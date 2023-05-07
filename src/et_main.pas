unit et_Main;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, StrUtils, IniFiles,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, ComCtrls,
  // TAChart
  TAGraph, TACustomSeries, TASeries, TASources,
  // Math lib
  MGlobal, MFunc,
  // project units
  et_Global, et_Math, et_File, et_Objects, et_Info, et_Sim, TACustomSource;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRunSim: TButton;
    EmissionPointsSeries: TLineSeries;
    Label2: TLabel;
    ResultsPageControl: TPageControl;
    PageControl2: TPageControl;
    TrajectoryOptionsPanel: TPanel;
    ParamsPanel: TPanel;
    pgEmissionPoints: TTabSheet;
    pgTrajectories: TTabSheet;
    EmissionPointsMemo: TMemo;
    seTrajectories: TSpinEdit;
    pgPlot: TTabSheet;
    pgValues: TTabSheet;
    TrajectoriesChart: TChart;
    ComboBox1: TComboBox;
    gbAnalyzer: TGroupBox;
    Label1: TLabel;
    lblBeamDiam: TLabel;
    lblFocusX: TLabel;
    lblFocusY: TLabel;
    lblFocusZ: TLabel;
    rgProjection: TRadioGroup;
    seFocusX: TFloatSpinEdit;
    seFocusY: TFloatSpinEdit;
    seFocusZ: TFloatSpinEdit;
    sePrimEnergy: TFloatSpinEdit;
    gbEGun: TGroupBox;
    lblPrimElCount: TLabel;
    sePrimElCount: TSpinEdit;
    lblPrimEnergy: TLabel;
    seBeamDiam: TFloatSpinEdit;
    EmissionPointsChart: TChart;
    EmissionPointsSource: TUserDefinedChartSource;
    procedure Button1Click(Sender: TObject);
    procedure btnRunSimClick(Sender: TObject);
    procedure EmissionPointsSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgProjectionClick(Sender: TObject);
    procedure TrajectoryGetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    nPrim: Integer;
    TraceFileName: String;
    EmPtsFileName: String;
    FEmissionPoints: array of TVector3;
    FTrajectories: array of TTrajectory;
    function GetProjection: TProjection;
    procedure GUIToParams(var AParams: TSimParams);
    function InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
    procedure InitParams;
    procedure ParamsToGUI(const AParams: TSimParams);
    procedure PrepareSim;
    procedure RunSimulation;
    procedure SaveParams;
    procedure Trace(prim, sec: Integer; Electron: TElectron; Energy: float);

    // Event handlers
    procedure DetectionHandler(Simulation: TSimulation;
      AElectronCount: Integer; const AElectron: TAugerElectron);
    procedure TrajectoryCompleteHandler(Simulation: TSimulation;
      const AElectronID: String; const ATrajectory: TTrajectory);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  TITLE_MASK = '%7s %9s %9s %9s %12s %12s %12s %6s';
  VALUE_MASK = '%7d %9.3f %9.3f %9.3f %12.*f %12.*f %12.*f %6s';
var
  MaxEl: Integer = 100;  // Number of iterations
  BottomIntens: Float = 0.0;
  TopIntens: Float = 0.0;
  WallIntens: Float = 0.0;
  nDet: Integer = 0;    // Number of detected events
  nDetOld: Integer = 0; // Number of detected events found in EmPts file
  nPrim: Integer = 0;   // current iteration counter (= number of primary electrons
  EscDepth: float;
  TraceFileName: String = '';
  EmPtsFileName: String = '';
  Aborted: Boolean = false;

procedure EvalIntensities(const ASimParams: TSimParams; EscDepth: Float;
  Point: TVector3; dI: Float; var TopIntens, BottomIntens, WallIntens: Float);
var
  R2: Float;
  Tol: Float;
begin
  Tol := 6 * EscDepth;
  {
  Tol := EscDepth + EscDepth;
  Tol := Tol + Tol + EscDepth;
  }
  with Point do
  begin
    case ASimParams.Topography of
      ttContactHole :
        if Equal(Z, 0.0, Tol) then
        begin
          R2 := Sqr(0.5 * ASimParams.Width);
          if LessThan(X*X + Y*Y, R2, Tol) then
            BottomIntens := BottomIntens + dI
          else
            TopIntens := TopIntens + dI;
        end else
        if Equal(Z, -ASimParams.Depth, Tol) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStripe :
        if Zero(Z, Tol) and Zero(X, ASimParams.Width*0.5) then
          TopIntens := TopIntens + dI
        else
        if Equal(Z, -ASimParams.Depth, Tol) and not Zero(X, ASimParams.Width*0.5) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStep :
        if Zero(X, Tol) then
          WallIntens := WallIntens + dI
        else
        if GreaterThan(X, 0.0, Tol) then
          case ASimParams.StepDir of
            sdUp   : TopIntens := TopIntens + dI;
            sdDown : BottomIntens := BottomIntens + dI;
          end
        else
        if LessThan(X, 0.0, Tol) then
          case ASimParams.StepDir of
            sdUp   : BottomIntens := BottomIntens + dI;
            sdDown : TopIntens := TopIntens + dI;
          end;
    end;
  end;
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  RunSimulation;
end;

procedure TForm1.btnRunSimClick(Sender: TObject);
begin
  RunSimulation;
end;

procedure TForm1.DetectionHandler(Simulation: TSimulation;
  AElectronCount: Integer; const AElectron: TAugerElectron);
const
  CROSS: array[boolean] of String[1] = (' ', 'X');
var
  Decs: Byte;
  MaxIntens: Float;
begin
  SetLength(FEmissionPoints, Length(FEmissionPoints) + 1);
  FEmissionPoints[High(FEmissionPoints)] := AElectron.Ray.Point;

  EmissionPointsSource.PointsNumber := Length(FEmissionPoints);

  with AElectron do
  begin
    EvalIntensities(SimParams, EscDepth, Ray.Point, Weight, TopIntens, BottomIntens, WallIntens);
    MaxIntens := MaxF(BottomIntens, MaxF(TopIntens, WallIntens));
    if MaxIntens < 10 then Decs := 3 else
      if MaxIntens < 100 then Decs := 2 else
        if MaxIntens < 1000 then Decs := 1 else
          Decs := 0;
    with Ray.Point do
      EmissionPointsMemo.Lines.Add(VALUE_MASK, [
        AElectronCount, X, Y, Z, decs, TopIntens, decs, WallIntens, decs, BottomIntens, CROSS[GenByBkScEl]
      ]);
  end;
end;

procedure TForm1.EmissionPointsSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FEmissionPoints[AIndex].X;
  AItem.Y := FEmissionPoints[AIndex].Y;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitParams;
  ParamsToGui(SimParams);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GuiToParams(SimParams);
  SaveParams;
end;

function TForm1.GetProjection: TProjection;
begin
  Result := TProjection(rgProjection.ItemIndex);
end;

procedure TForm1.rgProjectionClick(Sender: TObject);
const
  TITLE_X: array[TProjection] of String = ('X', 'X', 'Y', '3D');
  TITLE_Y: array[TProjection] of String = ('Y', 'Z', 'Z', '3D');
var
  i: Integer;
  ser: TChartSeries;
begin
  for i := 0 to TrajectoriesChart.SeriesCount-1 do
    if (TrajectoriesChart.Series[i] is TChartSeries) then
    begin
      ser := TChartSeries(TrajectoriesChart.Series[i]);
      if ser.Source is TUserDefinedChartSource then
        TUserDefinedChartSource(ser.Source).Reset;
    end;
  TrajectoriesChart.LeftAxis.Title.Caption := TITLE_Y[GetProjection];
  TrajectoriesChart.BottomAxis.Title.Caption := TITLE_X[GetProjection];
end;

procedure TForm1.GUIToParams(var AParams: TSimParams);
begin
  MaxEl := sePrimElCount.Value;
  AParams.PrimaryEnergy := sePrimEnergy.Value;
  AParams.BeamDiameter := seBeamDiam.Value;
  AParams.Focus.X := seFocusX.Value;
  AParams.Focus.Y := seFocusY.Value;
  AParams.Focus.Z := seFocusZ.Value;
end;

function TForm1.InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
var
  P: TMaterialParams;
begin
  P := AMaterials.SearchName(AName);
  if P = nil then
    raise Exception.Create('Unknown material ' + AName);
  with P do
    Result := TMaterial.Create(Z, A, Massdensity, CoreLevel, AugerEnergy);
end;

procedure TForm1.InitParams;
var
  cfg: TCustomIniFile;
  s: String;
  section: String;
  savedFormatSettings: TFormatSettings;
begin
  savedFormatSettings := FormatSettings;
  cfg := TIniFile.Create('calc_et.cfg');
  try
    FormatSettings.DecimalSeparator := '.';

    section := 'Params';
    MaxEl := cfg.ReadInteger(section, 'Iterations', MaxEl);
    SimParams.TiltAngle := cfg.ReadFloat(section, 'Tilt angle', SimParams.TiltAngle);
    SimParams.PrimaryEnergy := cfg.ReadFloat(section, 'Primary energy', SimParams.PrimaryEnergy);
    SimParams.BeamDiameter := cfg.ReadFloat(section, 'Beam diameter', SimParams.BeamDiameter);
    s := cfg.ReadString(section, 'Analyzer type', '');
    case Uppercase(s) of
      'CMA', '': SimParams.AnalyzerType := atCMA;
      'CHA': SimParams.AnalyzerType := atCHA;
      else SimParams.AnalyzerType := atNone;
    end;
    SimParams.SectorStart := cfg.ReadFloat(section, 'Analyzer Aperture Min', SimParams.SectorStart);
    SimParams.SectorEnd := cfg.ReadFloat(section, 'Analyzer Aperture Max', SimParams.SectorEnd);
    SimParams.useHoeslerAperture := cfg.ReadBool(section, 'Hoesler aperature', SimParams.UseHoeslerAperture);
    SimParams.Focus.X := cfg.ReadFloat(section, 'Focus X', SimParams.Focus.X);
    SimParams.Focus.Y := cfg.ReadFloat(section, 'Focus Y', SimParams.Focus.Y);
    SimParams.Focus.Z := cfg.ReadFloat(section, 'Focus Z', SimParams.Focus.Z);
    s := cfg.ReadString(section, 'Topography', '');
    case Uppercase(s) of
      '',
      'CONTACT HOLE': SimParams.Topography := ttContactHole;
      'STRIPE': SimParams.Topography := ttStripe;
      'STEP': SimParams.Topography := ttStep;
      else SimParams.Topography := ttNone;
    end;
    case SimParams.Topography of
      ttContactHole:
        begin
          SimParams.Width := cfg.ReadFloat(section, 'Contact hole diameter', SimParams.Width);
          SimParams.Depth := cfg.ReadFloat(section, 'Contact hole depth', SimParams.Depth);
        end;
      ttStripe:
        begin
          SimParams.Width := cfg.ReadFloat(section, 'Stripe width', SimParams.Width);
          SimParams.Depth := cfg.ReadFloat(section, 'Stripe height', SimParams.Depth);
        end;
      ttStep:
        begin
          SimParams.Depth := cfg.ReadFloat(section, 'Step height', SimParams.Depth);
          s := cfg.ReadString(section, 'Step dir', '');
          case Uppercase(s) of
            'UP', '': SimParams.StepDir := sdUp;
            'DOWN': SimParams.StepDir := sdDown;
          end;
        end;
    end;
    SimParams.SubstrateName := cfg.ReadString(section, 'Substrate material', SimParams.SubstrateName);
    SimParams.LayerName := cfg.ReadString(section, 'Layer material', SimParams.LayerName);
    SimParams.LayerThickness := cfg.ReadFloat(section, 'LayerThickness', SimParams.LayerThickness);
    SimParams.OnlyDirect := cfg.ReadBool(section, 'Only Direct', SimParams.OnlyDirect);
    SimParams.TrajectoryFileName := cfg.ReadString(section, 'Trajectory file name', SimParams.TrajectoryFileName);
    TraceFileName := cfg.ReadString(section, 'Trajectory file name', TraceFileName);
    EmPtsFileName := cfg.ReadString(section, 'Emission points file name', EmPtsFileName)

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;

  (*
  nPrim := 0;
  TopIntens := 0.0;
  BottomIntens := 0.0;
  WallIntens := 0.0;
  *)
  (*
  IF Continue THEN BEGIN
    CheckFileParams(Tracename, OK);
    CheckFileParams(EmPtFName, OK);
    IF NOT OK THEN
      HaltError('Die aktuellen Parameter unterscheiden sich von '+
        'denen in der Datei.');
  END;
  IF (MaxEl>500) AND (TraceName<>'') THEN BEGIN
    Write('Es sollen mehr als 500 Trajektorien gespeichert werden. OK (j/n)? ');
    IF UpCase(ReadKey)='N' THEN Halt(0);
  END;
  *)
end;

procedure TForm1.ParamsToGUI(const AParams: TSimParams);
begin
  sePrimElCount.Value := MaxEl;
  sePrimEnergy.Value := AParams.PrimaryEnergy;
  seBeamDiam.Value := AParams.BeamDiameter;
  seFocusX.Value := AParams.Focus.X;
  seFocusY.Value := AParams.Focus.Y;
  seFocusZ.Value := AParams.Focus.Z;
end;

procedure TForm1.PrepareSim;
var
  s6, s7, s9, s12: String;
  i: Integer;
  ser: TChartSeries;
begin
  // Clear emission points data
  SetLength(FEmissionPoints, 0);

  // Clear trajectory data
  SetLength(FTrajectories, 0);

  // Clear emission points memo
  EmissionPointsMemo.Lines.Clear;

  // Write header of emission points memo
  EmissionPointsMemo.Lines.Add(TITLE_MASK, ['No.', 'X', 'Y', 'Z', 'I(top)', 'I(wall)', 'I(bottom)', 'BkSc']);
  s6 := StringOfChar('-', 6);
  s7 := StringOfChar('-', 7);
  s9 := StringOfChar('-', 9);
  s12 := StringOfChar('-', 12);
  EmissionPointsMemo.Lines.Add(TITLE_MASK, [s7, s9, s9, s9, s12, s12, s12, s6]);

  // Remove trajectory series
  for i := 0 to TrajectoriesChart.SeriesCount-1 do
    if TrajectoriesChart.Series[i] is TChartSeries then
    begin
      ser := TChartSeries(TrajectoriesChart.Series[i]);
      if ser.Source is TUserDefinedChartSource then
        TUserDefinedChartSource(ser.Source).Free;
    end;
  TrajectoriesChart.ClearSeries;

  // Clear emission points chart
  EmissionPointsSource.PointsNumber := 0;
end;

procedure TForm1.RunSimulation;
var
  sim: TSimulation;
begin
  GuiToParams(SimParams);
  PrepareSim;
  sim := TSimulation.Create(SimParams);
  try
    if seTrajectories.Value > 0 then
      sim.OnTrajectoryComplete := @TrajectoryCompleteHandler;
    sim.OnDetection := @DetectionHandler;
    BottomIntens := 0;
    TopIntens := 0;
    WallIntens := 0;
    EscDepth := sim.EscDepth;
    sim.Execute(sePrimElCount.Value);
  finally
    sim.Free;
  end;
end;

procedure TForm1.SaveParams;
var
  cfg: TCustomIniFile;
  s: String;
  section: String;
  savedFormatSettings: TFormatSettings;
begin
  savedFormatSettings := FormatSettings;
  cfg := TIniFile.Create('calc_et.cfg');
  try
    FormatSettings.DecimalSeparator := '.';

    section := 'Params';
    cfg.EraseSection(section);
    cfg.WriteInteger(section, 'Iterations', MaxEl);
    cfg.WriteFloat(section, 'Tilt angle', SimParams.TiltAngle);
    cfg.WriteFloat(section, 'Primary energy', SimParams.PrimaryEnergy);
    cfg.WriteFloat(section, 'Beam diameter', SimParams.BeamDiameter);
    case SimParams.AnalyzerType of
      atCMA: s := 'CMA';
      atCHA: s := 'CHA';
      else   s := '';
    end;
    if s <> '' then cfg.WriteString(section, 'Analyzer type', s);
    cfg.WriteFloat(section, 'Analyzer Aperture Min', SimParams.SectorStart);
    cfg.WriteFloat(section, 'Analyzer Aperture Max', SimParams.SectorEnd);
    cfg.WriteBool(section, 'Hoesler aperature', SimParams.UseHoeslerAperture);
    cfg.WriteFloat(section, 'Focus X', SimParams.Focus.X);
    cfg.WriteFloat(section, 'Focus Y', SimParams.Focus.Y);
    cfg.WriteFloat(section, 'Focus Z', SimParams.Focus.Z);
    case SimParams.Topography of
      ttContactHole: s := 'Contact hole';
      ttStripe: s := 'Stripe';
      ttStep: s := 'Step';
      else s := '';
    end;
    if s <> '' then cfg.WriteString(section, 'Topography', s);
    case SimParams.Topography of
      ttContactHole:
        begin
          cfg.WriteFloat(section, 'Contact hole diameter', SimParams.Width);
          cfg.WriteFloat(section, 'Contact hole depth', SimParams.Depth);
        end;
      ttStripe:
        begin
          cfg.WriteFloat(section, 'Stripe width', SimParams.Width);
          cfg.WriteFloat(section, 'Stripe height', SimParams.Depth);
        end;
      ttStep:
        begin
          cfg.WriteFloat(section, 'Step height', SimParams.Depth);
          case SimParams.StepDir of
            sdUp: s := 'Up';
            sdDown: s := 'Down';
            else s := '';
          end;
          if s <> '' then cfg.WriteString(section, 'Step dir', s);
        end;
    end;
    cfg.WriteString(section, 'Substrate material', SimParams.SubstrateName);
    cfg.WriteString(section, 'Layer material', SimParams.LayerName);
    cfg.WriteFloat(section, 'LayerThickness', SimParams.LayerThickness);
    cfg.WriteBool(section, 'Only Direct', SimParams.OnlyDirect);
    cfg.WriteString(section, 'Trajectory file name', SimParams.TrajectoryFileName);
    cfg.WriteString(section, 'Trajectory file name', TraceFileName);
    cfg.WriteString(section, 'Emission points file name', EmPtsFileName)

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;
end;

procedure TForm1.Trace(prim, sec: Integer; Electron: TElectron; Energy: float);
var
  Point: TVector3;
  s: String;
  IsPrimEl: BOOLEAN;
  MinEnergy: float;
begin
  if etError = etOK then
  begin
    MinEnergy := MinF(Layer.Eb, Substrate.Eb);
    IsPrimEl := (sec=0);
    if Sample.Intersection(Electron.Ray, Point, IsPrimEl) then
    begin
      Electron.Ray.Point := Point;
      s := 'el_' + IntToStr(prim);
      if sec <> 0 then
        s := s + '_' + IntToStr(sec);
      if (sec < 10) and Sample.Trace(s, Electron, Energy, MinEnergy) then
        Trace(prim, sec+1, Electron, Energy);
    end;
    //IF KeyPressed THEN ProcessUserInput;
  end;
end;

procedure TForm1.TrajectoryCompleteHandler(Simulation: TSimulation;
  const AElectronID: String;
  const ATrajectory: TTrajectory);
var
  ser: TLineSeries;
  udcs: TUserDefinedChartSource;
  i, idx: Integer;
begin
  SetLength(FTrajectories, Length(FTrajectories) + 1);
  idx := High(FTrajectories);
  FTrajectories[idx] := ATrajectory;

  udcs := TUserDefinedChartSource.Create(Self);
  udcs.Tag := idx;
  udcs.PointsNumber := Length(ATrajectory);
  udcs.OnGetChartDataItem := @TrajectoryGetChartDataItemHandler;

  ser := TLineSeries.Create(TrajectoriesChart);
  ser.Title := AElectronID;
  ser.Source := udcs;

  TrajectoriesChart.AddSeries(ser);

  if idx >= seTrajectories.Value then
    Simulation.OnTrajectoryComplete := nil;
end;

procedure TForm1.TrajectoryGetChartDataItemHandler(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  trajectory: TTrajectory;
begin
  trajectory := FTrajectories[ASource.Tag];
  case GetProjection of
    XYproj:
      begin
        AItem.X := trajectory[AIndex].X;
        AItem.Y := trajectory[AIndex].Y;
      end;
    XZproj:
      begin
        AItem.X := trajectory[AIndex].X;
        AItem.Y := trajectory[AIndex].Z;
      end;
    YZproj:
      begin
        AItem.X := trajectory[AIndex].Y;
        AItem.Y := trajectory[AIndex].Z;
      end;
  end;
end;


end.

