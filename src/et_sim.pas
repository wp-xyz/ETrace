unit et_Sim;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  et_Global, et_Math, et_Objects;

type
  TSimulation = class;

  TTrajectoryPoint = record
    X, Y, Z: Float;
    Energy: Float;
  end;
  TTrajectory = array of TTrajectoryPoint;

  TCancelEvent = procedure(Simulation: TSimulation; var Cancel: Boolean) of object;

  TDetectionEvent = procedure(Simulation: TSimulation;
    AElectronNo: Integer; const Electron: TAugerElectron) of object;

  TTrajectoryCompleteEvent = procedure(Simulation: TSimulation;
    const AElectronID: String; const ATrajectory: TTrajectory) of object;

  TSimulation = class
  private
    FElectronSource: TElectronSource;
    FSample: TSample;
    FAnalyzer: TAnalyzer;
    FSubstrate: TMaterial;
    FLayer: TMaterial;
    FMaxPrimElectrons: Integer;
    FNumPrimElectrons: Integer;
    FSampleHitPoint: TVector3;
    FAborted: Boolean;
    FErrorCode: Integer;
    FOnCancel: TCancelEvent;
    FOnDetection: TDetectionEvent;
    FOnTrajectoryComplete: TTrajectoryCompleteEvent;
  private
    function GetSampleHitPoint: TVector3;
  protected
    procedure CalcTrajectory(ElectronID: String; EMin: Float;
      var Electron: TElectron; var E: float; out ExitsSample: Boolean);
    function InitMaterial(AMaterials: TMaterialsList; AMaterialName: String): TMaterial;
    procedure InitObjs(const AParams: TSimParams);
    procedure DoneObjs;
    procedure TraceElectron(APrimCount, ASecCount: Integer;
      Electron: TElectron; Energy: float);
  public
    constructor Create(const AParams: TSimParams);
    destructor Destroy; override;
    function Execute(AMaxPrimElectrons: Integer): Integer;

    property Analyzer: TAnalyzer read FAnalyzer;
    property ElectronSource: TElectronSource read FElectronSource;
    property Layer: TMaterial read FLayer;
    property Sample: TSample read FSample;
    property SampleHitPoint: TVector3 read GetSampleHitPoint;
    property Substrate: TMaterial read FSubstrate;
    property OnCancel: TCancelEvent read FOnCancel write FOnCancel;
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
procedure TSimulation.CalcTrajectory(ElectronID: String; EMin: Float;
  var Electron: TElectron; var E: float; out ExitsSample: Boolean);
const
  FROM_INSIDE = false;
  BLOCK_SIZE = 1000;
var
  AugerEl    : TAugerElectron;
  P          : TVector3;
  Ray        : TRay;
  Finished   : boolean;
  zIntf      : Float;
  n          : Integer = 0;
  nDetected  : Integer;
  oldDetected: Integer;
  trajectory : TTrajectory = nil;
begin
  if FErrorCode <> etOK then
    exit;

  ExitsSample := false;
  finished := false;
  n := 0;

  zIntf := FSample.zInterface;

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
        ExitsSample := true;
        finished := true;
        // Do not "break" here because an Auger electron may be emitted at the exit point.
      end;

      if GreaterThan(Point.Z, zIntf, FloatEps) then
        FSample.ChangeMaterial(FLayer)
      else
      if LessThan(Point.Z, zIntf, FloatEps) or Equal(Point.Z, zIntf, FloatEps) then
        FSample.ChangeMaterial(FSubstrate);

      AugerEl.GenByBkScEl := LessThan(E, FElectronSource.Energy, FloatEps);
      if FSample.EmitAugerEl(Point, E, AugerEl) then
      begin
        oldDetected := FAnalyzer.Detected;
        nDetected := FAnalyzer.Detect(AugerEl);
        if (oldDetected <> nDetected) and Assigned(FOnDetection) then
          FOnDetection(Self, nDetected, AugerEl);
      end;

      if not (FSample.OnlyDirect or ExitsSample) then
        FSample.Scatter(Electron, E);

      if (E < EMin) or FAborted then
        finished := true;
    until finished or (FErrorCode <> etOK) or FSample.OnlyDirect;
  end;

  SetLength(trajectory, n);
  if Assigned(FOnTrajectoryComplete) then
    FOnTrajectoryComplete(Self, ElectronID, trajectory);
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
    FElectronSource.GenerateElectron(electron, energy);
    TraceElectron(FNumPrimElectrons, 0, electron, energy);
    if Assigned(FOnCancel) and (FNumPrimElectrons mod 500 = 0) then
      FOnCancel(Self, FAborted);
    if (FErrorCode <> etOK) or FAborted then
      break;
  end;

  Result := FErrorCode;
end;

function TSimulation.GetSampleHitPoint: TVector3;
var
  ray: TRay;
begin
  if (FElectronSource = nil) or (FSample = nil) then
    Result := Vector3(NaN, NaN, NaN)
  else
  begin
    ray.Point := FElectronSource.FocusedPoint;
    ray.Dir := FElectronSource.Axis;
    VecMulSc(ray.Dir, -1);
    FSample.Intersection(ray, Result, true);
  end;
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
        FSample := TContactHole.Create(FSubstrate, FLayer, PrimaryEnergy, LayerThickness, Width*0.5, Depth);
      ttStripe:
        FSample := TStripe.Create(FSubstrate, FLayer, PrimaryEnergy, LayerThickness, Width, Depth);
      ttStep:
        FSample := TStep.Create(FSubstrate, FLayer, PrimaryEnergy, LayerThickness, Depth, StepDir);
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
  exitsSample: Boolean;
begin
  if (FErrorCode <> etOK) or FAborted then
    exit;

  minEnergy := Min(FLayer.CoreLevelEnergy, FSubstrate.CoreLevelEnergy);
  isPrimaryElectron := (ASecCount = 0);

  if FSample.Intersection(Electron.Ray, Point, isPrimaryElectron) then
  begin
    if Point.X < -3 then
      id := '';
    Electron.Ray.Point := Point;
    id := 'el_' + IntToStr(APrimCount);
    if ASecCount <> 0 then
      id := id + '_' + IntToStr(ASecCount);
    if (ASecCount < 10) then
    begin
      CalcTrajectory(id, MinEnergy, electron, energy, exitsSample);
      if exitsSample then
        TraceElectron(APrimCount, ASecCount+1, Electron, Energy);
    end;
  end;
end;

end.

