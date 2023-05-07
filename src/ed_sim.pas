unit ed_sim;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MGlobal, MFunc,
  et_Global, et_Math, et_Objects;

type
  TTrajectoryPoint = record
    X, Y, Z: Float;
    Energy: Float;
  end;
  TTrajectory = array of TTrajectoryPoint;

  TDetectionEvent = procedure(AElectronNo: Integer; const Electron: TAugerElectron) of object;
  TTrajectoryCompleteEvent = procedure(AElectronID: String; ATrajectory: TTrajectory) of object;

  TSimulation = class
  private
    FElectronSource: TElectronSource;
    FSample: TSample;
    FAnalyzer: TAnalyzer;
    FSubstrate: TMaterial;
    FLayer: TMaterial;
    FMaxPrimElectrons: Integer;
    FNumPrimElectrons: Integer;
    FAborted: Boolean;
    FErrorCode: Integer;
    FOnDetection: TDetectionEvent;
    FOnTrajectoryComplete: TTrajectoryCompleteEvent;
  protected
    procedure CalcTrajectory;
    function InitMaterial(AMaterials: TMaterialsList; AMaterialName: String): TMaterial;
    procedure InitObjs(const AParams: TSimParams);
    procedure DoneObjs;
    procedure TraceElectron(APrimCount, ASecCount: Integer; Electron: TElectron; Energy: float);
  public
    constructor Create(const AParams: TSimParams);
    destructor Destroy; override;
    function Execute(AMaxPrimElectrons: Integer): Integer;
    property OnDetection: TDetectionEvent read FOnDetection write FOnDetection;
    property OnTrajectoryComplete: TTrajectoryCompleteEvent read FOnTrajectoryComplete write FOnTrajectoryComplete;
  end;

implementation

constructor TSimulation.Create(const AParams: TSimParams);
begin
  InitObjs(AParams);
end;

destructor TSimulation.Destroy;
begin
  DoneObjs;
  inherited;
end;

{ Traces a primary electron which enters the sample with energy <E> at point
  <Electron.Ray.Point> and in direction <Electron.Ray.Dir>.
  The trajectory is traced by means of the Monte-Carlo routine FSample.Scatter()
  until the electron's energy has dropped below the minimum energy <EMin>, or
  until the electron has left the sample.
  If the electron has left the sample the function result becomes true, and the
  field <Electron.Ray> gets the exit point and the exit direction, and <E> gets
  the energy at exit.
  If the trajectory intersects the surface an Auger electron is "emitted" by
  calling the method FSample.EmitAugerEl(). If the Auger electron leaves the
  sample (i.e., the result of EmitAugerEl() is true), then it is checked by
  calling the function Analyzer.Detect whether the electon enters the analyzer.
  The coordinates along the path as well as the energy are stored in the
  array <trajectory> which is passed to the handler of the event
  OnTrajectoryComplete. }
function TSimulation.CalcTrajectory(ElectronID: String; var Electron: TElectron;
  var E: float; Emin: float): Boolean;
const
  FROM_INSIDE = false;
  BLOCK_SIZE = 1000;
var
  AugerEl    : TAugerElectron;
  P          : TVector3;
  Ray        : TRay;
  Finished   : boolean;
  Matrix     : TMatrix;
  n          : Integer = 0;
  nMax       : Integer = 0;
  zInterface : Float;
  trajectory : array of TTrajectoryPoint = nil;
begin
  Result := false;

  if FErrorCode <> etOK then
    exit;

  zInterface := -abs(FSample.LayerThickness);
  finished := false;
  n := 0;

  with Electron.Ray do
  begin
    repeat
      if n >= Length(trajectory) then
        SetLength(trajectory, Length(trajectory) + BLOCK_SIZE);
      trajectory[n].X := Point.X;
      trajectory[n].Y := Point.Y;
      trajectory[n].Z := Point.Z;
      trajectory[n].Energy := E;
      inc(n);
      if FSample.Outside(Point) then
      begin
        Ray := Electron.Ray;       // The electron has left the sample
        VecMulSc(Ray.Dir, -1.0);   // Determine the point of emission
        if FSample.Intersection(Ray, P, FROM_INSIDE) then
          Point := P;
        finished := true;
      end;

      if GreaterThan(Point.Z, zInterface, FloatEps) then
        ChangeMaterial(Layer)
      else
      if LessThan(Point.Z, zInterface, FloatEps) or Equal(Point.Z, zInterface, FloatEps) then
        ChangeMaterial(Substrate);

      AugerEl.GenByBkScEl := LessThan(E, ElectronSource.Energy, FloatEps);
      if FSample.EmitAugerEl(Point, E, AugerEl) then
        FAnalyzer.Detect(AugerEl);
      if not FSample.OnlyDirect then
        FSampleScatter(Electron, E);
      if (E < EMin) or FAborted then
        finished := true;
    until finished or (FErrorCode <> etOK) or OnlyDirect;
  end;
  SetLength(trajectory, n);
  if Assigned(FOnTrajectoryComplete) then
    FOnTrajectoryComplete(ElectronID, trajectory);

  Result := (E >= Emin) and (not FSample.OnlyDirect);
  { Only when electron has exited the sample E is greater than Emin }
end;


procedure TSimulation.DoneObjs;
begin
  FreeAndNil(FElectronSource);
  FreeAndNil(FAnalyzer);
  FreeAndNil(FSample);
  FreeAndNil(FSubstrate);
  FreeAndNil(FLayer);
end;

function TSimulation.Execute(AMaxPrimElectrons: Integer): Integer;
var
  electron: TElectron;
  energy: Float;
begin
  // To silence the compiler...
  electron := Default(TElectron);
  energy := 0.0;

  FErrorCode := etOK;

  FNumPrimElectrons := 0;
  FMaxPrimElectrons := AMaxPrimElectrons;
  while (FNumPrimElectrons < FMaxPrimElectrons) do
  begin
    inc(FNumPrimElectrons);
    FElectronSource.GenEl(electron, energy);
    Trace(FNumPrimElectrons, 0, electron, energy);
    if (FErrorCode <> etOK) or FAborted then
      break;
  end;

  Result := FErrorCode;
end;

function TSimulation.InitMaterial(AMaterials: TMaterialsList;
  AMaterialName: String): TMaterial;
var
  P: TMaterialParams;
begin
  P := AMaterials.SearchName(AMaterialName);
  if P = nil then
    raise Exception.Create('Unknown material ' + AMaterialName);
  with P do
    Result := TMaterial.Create(Z, A, Massdensity, CoreLevel, AugerEnergy);
end;

procedure TSimulation.InitObjs(const AParams: TSimParams);
var
  Materials: TMaterialsList;
begin
  DoneObjs;
  with AParams do
  begin
    // Electron source
    FElectronSource := TElectronSource.Create(TiltAngle, PrimaryEnergy, BeamDiameter, Focus);

    // Analyzer
    FAnalyzer := TAnalyzer.Create(AnalyzerType, TiltAngle, 0.0);
    FAnalyzer.UseHoeslerAp := UseHoeslerAperture;
    if not Zero(SectorStart, FloatEps) and not Zero(SectorEnd, FloatEps) then
      FAnalyzer.Restrict(SectorStart, SectorEnd);

    // Sample
    Materials := NewMaterialsList;
    try
      FSubstrate := InitMaterial(Materials, SimParams.SubstrateName);
      FLayer := InitMaterial(Materials, SimParams.LayerName);
    finally
      Materials.Free;
    end;

    case Topography of
      ttContactHole:
        FSample := TContactHole.Create(LayerThickness, Width*0.5, Depth, TrajectoryFileName);
      ttStripe:
        FSample := TStripe.Create(LayerThickness, Width, Depth, TrajectoryFileName);
      ttStep:
        FSample := TStep.Create(LayerThickness, Depth, StepDir, TrajectoryFileName);
    end;
    FSample.OnlyDirect := OnlyDirect;
  end;
end;

procedure TSimulation.TraceElectron(APrimCount, ASecCount: Integer; Electron: TElectron;
  Energy: float);
var
  Point: TVector3 = (X:0.0; Y:0.0; Z:0.0);
  id: String;
  isPrimaryElectron: Boolean;
  minEnergy: float;
begin
  if (FErrorCode <> etOK) or FAborted then
    exit;

  minEnergy := MinF(Layer.Eb, Substrate.Eb);
  isPrimaryElectron := (ASecCount = 0);
  if Sample.Intersection(Electron.Ray, Point, isPrimaryElectron) then
  begin
    Electron.Ray.Point := Point;
    id := 'el_' + IntToStr(APrimCount);
    if ASecCount <> 0 then
      id := id + '_' + IntToStr(ASecCount);
    if (ASecCount < 10) and FSample.Trace(id, Electron, Energy, MinEnergy) then
      TraceElectron(APrimCount, ASecCount+1, Electron, Energy);
  end;
end;

end.

