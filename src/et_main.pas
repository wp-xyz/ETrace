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

  { TMainForm }

  TMainForm = class(TForm)
    btnRunSim: TButton;
    cbUseHoeslerAp: TCheckBox;
    SummaryMemo: TMemo;
    EmissionPointsSeries: TLineSeries;
    Label2: TLabel;
    ResultsPageControl: TPageControl;
    PageControl2: TPageControl;
    pgSummary: TTabSheet;
    TrajectoryOptionsPanel: TPanel;
    ParamsPanel: TPanel;
    pgEmissionPoints: TTabSheet;
    pgTrajectories: TTabSheet;
    EmissionPointsMemo: TMemo;
    seTrajectories: TSpinEdit;
    pgPlot: TTabSheet;
    pgValues: TTabSheet;
    TrajectoriesChart: TChart;
    cmbAnalyzerType: TComboBox;
    gbAnalyzer: TGroupBox;
    lblAnalyzerType: TLabel;
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
    procedure btnRunSimClick(Sender: TObject);
    procedure EmissionPointsSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgProjectionClick(Sender: TObject);
    procedure TrajectoryGetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    FEmissionPoints: array of TVector3;
    FTrajectories: array of TTrajectory;
    FRunning: Boolean;
    FAborted: Boolean;
    procedure DisplaySummary(ASimulation: TSimulation);
    function GetProjection: TProjection;
    procedure GUIToParams(var AParams: TSimParams);
    function InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
    procedure LoadParamsFromCfg;
    procedure ParamsToGUI(const AParams: TSimParams);
    procedure PrepareSim;
    procedure RunSimulation;
    procedure SaveParamsToCfg;

    // Event handlers
    procedure CancelHandler(ASimulation: TSimulation; var Cancel: Boolean);
    procedure DetectionHandler(ASimulation: TSimulation;
      AElectronCount: Integer; const AElectron: TAugerElectron);
    procedure TrajectoryCompleteHandler(Simulation: TSimulation;
      const AElectronID: String; const ATrajectory: TTrajectory);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  APP_CAPTION = 'Electron Tracer';

  TITLE_MASK = '%7s %9s %9s %9s %12s %12s %12s %6s';
  VALUE_MASK = '%7d %9.3f %9.3f %9.3f %12.*f %12.*f %12.*f %6s';

var
  BottomIntens: Float = 0.0;
  TopIntens: Float = 0.0;
  WallIntens: Float = 0.0;
  DepthTol: float = 0.0;

procedure EvalIntensities(const ASimParams: TSimParams; Point: TVector3; dI: Float;
  var TopIntens, BottomIntens, WallIntens: Float);
var
  R2: Float;
begin
  with Point do
  begin
    case ASimParams.Topography of
      ttContactHole :
        if Equal(Z, 0.0, DepthTol) then
        begin
          R2 := Sqr(0.5 * ASimParams.Width);
          if LessThan(X*X + Y*Y, R2, DepthTol) then
            BottomIntens := BottomIntens + dI
          else
            TopIntens := TopIntens + dI;
        end else
        if Equal(Z, -ASimParams.Depth, DepthTol) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStripe :
        if Zero(Z, DepthTol) and Zero(X, ASimParams.Width*0.5) then
          TopIntens := TopIntens + dI
        else
        if Equal(Z, -ASimParams.Depth, DepthTol) and not Zero(X, ASimParams.Width*0.5) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStep :
        if Zero(X, DepthTol) then
          WallIntens := WallIntens + dI
        else
        if GreaterThan(X, 0.0, DepthTol) then
          case ASimParams.StepDir of
            sdUp   : TopIntens := TopIntens + dI;
            sdDown : BottomIntens := BottomIntens + dI;
          end
        else
        if LessThan(X, 0.0, DepthTol) then
          case ASimParams.StepDir of
            sdUp   : BottomIntens := BottomIntens + dI;
            sdDown : TopIntens := TopIntens + dI;
          end;
    end;
  end;
end;


{ TMainForm }

procedure TMainForm.btnRunSimClick(Sender: TObject);
var
  t: TDateTime;
begin
  if not FRunning then
  begin
    FRunning := true;
    FAborted := false;
    btnRunSim.Caption := 'Abort';
    Caption := APP_CAPTION + ' [running]';
    t := Now;
    RunSimulation;
    t := Now - t;
    FRunning := false;
    if FAborted then
      Caption := APP_CAPTION + ' [aborted]'
    else
      Caption := APP_CAPTION + ' [completed, ' + FormatDateTime('h:nn:ss.zzz', t) + ']';
    btnRunSim.Caption := 'Run simulation';
  end else
    FAborted := true;
end;

procedure TMainForm.CancelHandler(ASimulation: TSimulation; var Cancel: Boolean);
begin
  Application.ProcessMessages;
  Cancel := FAborted;
end;

procedure TMainForm.DetectionHandler(ASimulation: TSimulation;
  AElectronCount: Integer; const AElectron: TAugerElectron);
const
  CROSS: array[boolean] of String[1] = (' ', 'X');
var
  Decs: Byte;
  MaxIntens: Float;
begin
  // Store in emission points array
  SetLength(FEmissionPoints, Length(FEmissionPoints) + 1);
  FEmissionPoints[High(FEmissionPoints)] := AElectron.Ray.Point;

  // Update the chart
  EmissionPointsSource.PointsNumber := Length(FEmissionPoints);

  // Update the memo
  with AElectron do
  begin
    EvalIntensities(SimParams, Ray.Point, Weight, TopIntens, BottomIntens, WallIntens);
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

procedure TMainForm.DisplaySummary(ASimulation: TSimulation);
var
  intens: Float;
  src: TElectronSource;
  analyzer: TAnalyzer;
begin
  src := ASimulation.ElectronSource;
  analyzer := ASimulation.Analyzer;
  intens := analyzer.Intensity;

  if SummaryMemo.Lines.Count > 0 then
    SummaryMemo.Lines.Add('------------------------------------------------------------------');

  SummaryMemo.Lines.Add('PARAMETERS');
  SummaryMemo.Lines.Add('  ELECTRON SOURCE');
  SummaryMemo.Lines.Add('    Primary energy: %.3f keV', [src.Energy]);
  SummaryMemo.Lines.Add('    Beam diameter: %.3f µm', [src.BeamRadius*2]);
  SummaryMemo.Lines.Add('    Focus: X=%.3f µm, Y=%.3f µm', [src.FocusedPoint.X, src.FocusedPoint.Y]);
  SummaryMemo.Lines.Add('  ANALYZER');
  if analyzer.Restricted then
    SummaryMemo.Lines.Add('    %s, restricted', [ANALYZERTYPE_NAME[analyzer.AnalyzerType]])
  else
    SummaryMemo.Lines.Add('    %s', [ANALYZERTYPE_NAME[analyzer.AnalyzerType]]);
  if analyzer.UseHoeslerAp then
    SummaryMemo.Lines.Add('    Hoesler aperture applied');
  SummaryMemo.Lines.Add('');

  SummaryMemo.Lines.Add('RESULTS');
  SummaryMemo.Lines.Add('  ELECTRON SOURCE');
  SummaryMemo.Lines.Add('    Number of primary electrons: %20d', [src.NumFired]);
  SummaryMemo.Lines.Add('  ANALYZER');
  SummaryMemo.Lines.Add('    Number of Auger electrons:   %20d', [ASimulation.Analyzer.Detected]);
  SummaryMemo.Lines.Add('    Totel intensity:             %23.2f (%6.2f%%)', [intens, 100.0]);
  if intens <> 0 then
  begin
    SummaryMemo.Lines.Add('    Top surface intensity:       %23.2f (%6.2f%%)', [TopIntens, TopIntens / intens * 100.0]);
    SummaryMemo.Lines.Add('    Bottom surface intensity:    %23.2f (%6.2f%%)', [BottomIntens, BottomIntens / intens * 100.0]);
    SummaryMemo.Lines.Add('    Sidewall intensity:          %23.2f (%6.2f%%)', [WallIntens, WallIntens / intens * 100.0]);
  end;

  SummaryMemo.SelStart := Length(SummaryMemo.Text)-1;
  SummaryMemo.SelLength := 0;
end;

procedure TMainForm.EmissionPointsSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FEmissionPoints[AIndex].X;
  AItem.Y := FEmissionPoints[AIndex].Y;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  btnRunSim.Constraints.MinWidth := btnRunSim.Width;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SummaryMemo.Lines.Clear;
  sePrimElCount.MaxValue := MaxInt;

  LoadParamsFromCfg;
  ParamsToGui(SimParams);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GuiToParams(SimParams);
  SaveParamsToCfg;
end;

function TMainForm.GetProjection: TProjection;
begin
  Result := TProjection(rgProjection.ItemIndex);
end;

procedure TMainForm.rgProjectionClick(Sender: TObject);
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

procedure TMainForm.GUIToParams(var AParams: TSimParams);
begin
  AParams.NumElectrons := sePrimElCount.Value;
  AParams.PrimaryEnergy := sePrimEnergy.Value;
  AParams.BeamDiameter := seBeamDiam.Value;
  AParams.Focus.X := seFocusX.Value;
  AParams.Focus.Y := seFocusY.Value;
  AParams.Focus.Z := seFocusZ.Value;

  AParams.AnalyzerType := TAnalyzerType(cmbAnalyzerType.ItemIndex);
  AParams.UseHoeslerAperture := cbUseHoeslerAp.Checked;;
end;

function TMainForm.InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
var
  P: TMaterialParams;
begin
  P := AMaterials.SearchName(AName);
  if P = nil then
    raise Exception.Create('Unknown material ' + AName);
  with P do
    Result := TMaterial.Create(Z, A, Massdensity, CoreLevel, AugerEnergy);
end;

procedure TMainForm.LoadParamsFromCfg;
var
  cfg: TCustomIniFile;
  section: String;
  savedFormatSettings: TFormatSettings;
begin
  savedFormatSettings := FormatSettings;

  cfg := TIniFile.Create(CFG_FILE_NAME);
  try
    FormatSettings.DecimalSeparator := '.';

    section := 'Params';
    SimParams.TiltAngle := cfg.ReadFloat(section, 'Tilt angle', SimParams.TiltAngle);
    SimParams.PrimaryEnergy := cfg.ReadFloat(section, 'Primary energy', SimParams.PrimaryEnergy);
    SimParams.BeamDiameter := cfg.ReadFloat(section, 'Beam diameter', SimParams.BeamDiameter);
    SimParams.NumElectrons := cfg.ReadInteger(section, 'NumElectrons', SimParams.NumElectrons);
    case Uppercase(cfg.ReadString(section, 'Analyzer type', '')) of
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
    case Uppercase(cfg.ReadString(section, 'Topography', '')) of
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
          case Uppercase(cfg.ReadString(section, 'Step dir', '')) of
            'UP', '': SimParams.StepDir := sdUp;
            'DOWN': SimParams.StepDir := sdDown;
          end;
        end;
    end;
    SimParams.SubstrateName := cfg.ReadString(section, 'Substrate material', SimParams.SubstrateName);
    SimParams.LayerName := cfg.ReadString(section, 'Layer material', SimParams.LayerName);
    SimParams.LayerThickness := cfg.ReadFloat(section, 'LayerThickness', SimParams.LayerThickness);
    SimParams.OnlyDirect := cfg.ReadBool(section, 'Only Direct', SimParams.OnlyDirect);

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;
end;

procedure TMainForm.ParamsToGUI(const AParams: TSimParams);
begin
  sePrimElCount.Value := AParams.NumElectrons;
  sePrimEnergy.Value := AParams.PrimaryEnergy;
  seBeamDiam.Value := AParams.BeamDiameter;
  seFocusX.Value := AParams.Focus.X;
  seFocusY.Value := AParams.Focus.Y;
  seFocusZ.Value := AParams.Focus.Z;

  cmbAnalyzerType.ItemIndex := ord(AParams.AnalyzerType);
  cbUseHoeslerAp.Checked := AParams.UseHoeslerAperture;
end;

procedure TMainForm.PrepareSim;
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

procedure TMainForm.RunSimulation;
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
    sim.OnCancel := @CancelHandler;
    BottomIntens := 0;
    TopIntens := 0;
    WallIntens := 0;
    DepthTol := MaxF(sim.Layer.EscapeDepth, sim.Substrate.EscapeDepth) * 6;
    sim.Execute(sePrimElCount.Value);
    DisplaySummary(sim);
  finally
    sim.Free;
  end;
end;

procedure TMainForm.SaveParamsToCfg;
var
  cfg: TCustomIniFile;
  s: String;
  section: String;
  savedFormatSettings: TFormatSettings;
begin
  savedFormatSettings := FormatSettings;
  cfg := TIniFile.Create(CFG_FILE_NAME);
  try
    FormatSettings.DecimalSeparator := '.';

    section := 'Params';
    cfg.EraseSection(section);
    cfg.WriteInteger(section, 'NumElectrons', SimParams.NumElectrons);
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

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;
end;

procedure TMainForm.TrajectoryCompleteHandler(Simulation: TSimulation;
  const AElectronID: String; const ATrajectory: TTrajectory);
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

procedure TMainForm.TrajectoryGetChartDataItemHandler(
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

