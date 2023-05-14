unit et_Main;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, IniFiles, Math,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, ComCtrls, Buttons,
  // TAChart
  TAGraph, TAChartUtils, TAGeometry, TADrawUtils,
  TACustomSource, TASources,
  TACustomSeries, TASeries, SpinEx,
  // project units
  et_Global, et_Math, et_Objects, et_Sim;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    Bevel1: TBevel;
    btnRunSim: TButton;
    cbUseHoeslerAp: TCheckBox;
    cbAnnularAperture: TCheckBox;
    cmbTopography: TComboBox;
    cmbSubstrate: TComboBox;
    cmbLayer: TComboBox;
    lblTiltAngle: TLabel;
    Panel1: TPanel;
    seEmissionPointsHorMax: TFloatSpinEdit;
    seTiltAngle: TFloatSpinEdit;
    seTrajectoryHorMin: TFloatSpinEdit;
    seTrajectoryHorMax: TFloatSpinEdit;
    seEmissionPointsHorMin: TFloatSpinEdit;
    TrajectoryChartPanel: TPanel;
    ProgressBar: TProgressBar;
    seSectorFrom: TFloatSpinEdit;
    seSectorTo: TFloatSpinEdit;
    gbSample: TGroupBox;
    lblFrom: TLabel;
    lblTo: TLabel;
    lblDeg: TLabel;
    lblWidth: TLabel;
    lblLayerThickness: TLabel;
    lblDepth: TLabel;
    lblSubstrate: TLabel;
    lblTopography: TLabel;
    lblLayer: TLabel;
    lblWidth1: TLabel;
    seLayerThickness: TFloatSpinEdit;
    seDepth: TFloatSpinEdit;
    seWidth: TFloatSpinEdit;
    sbDirUp: TSpeedButton;
    sbDirDown: TSpeedButton;
    SummaryMemo: TMemo;
    EmissionPointsSeries: TLineSeries;
    Label2: TLabel;
    ResultsPageControl: TPageControl;
    EmissionPointsPageControl: TPageControl;
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
    procedure cbAnnularApertureChange(Sender: TObject);
    procedure cmbAnalyzerTypeChange(Sender: TObject);
    procedure cmbTopographyChange(Sender: TObject);
    procedure EmissionPointsSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgProjectionClick(Sender: TObject);
    procedure EmissionPointsChartExtentChange(Sender: TObject);
    procedure TrajectoriesChartExtentChange(Sender: TObject);
    procedure TrajectoriesChartAfterDraw(ASender: TChart; ADrawer: IChartDrawer);
    procedure TrajectoryGetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    FEmissionPoints: array of TVector3;
    FTrajectories: array of TTrajectory;
    FSampleHitPoint: TVector3;
    FRunning: Boolean;
    FAborted: Boolean;
    procedure DisplaySummary(ASimulation: TSimulation);
    function GetProjection: TProjection;
    function GetSelectedAnalyzer: TAnalyzerType;
    function GetSelectedTopography: TTopoType;
    procedure GUIToParams(var AParams: TSimParams);
    function InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
    procedure LoadParamsFromCfg;
    procedure ParamsToGUI(const AParams: TSimParams);
    procedure PopulateMaterialsCombo(ACombobox: TCombobox);
    procedure PrepareSim;
    procedure RunSimulation;
    procedure SaveParamsToCfg;
    procedure UpdateCtrlState(AEnabled: Boolean);

    // Event handlers
    procedure CancelHandler(ASimulation: TSimulation; var Cancel: Boolean);
    procedure DetectionHandler({%H-}ASimulation: TSimulation;
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

{ Set font style of Groupbox caption to bold, but keep items normal }
procedure BoldGroup(AControl: TCustomGroupBox);
var
  i: Integer;
begin
  AControl.Font.Style := [fsBold];
  for i:=0 to AControl.ControlCount-1 do
    AControl.Controls[i].Font.Style := [];
end;

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
    Progressbar.Position := 0;
    Progressbar.Max := sePrimElCount.Value;
    Progressbar.Show;
    UpdateCtrlState(false);
    t := Now;

    RunSimulation;
    t := Now - t;

    FRunning := false;
    if FAborted then
      Caption := APP_CAPTION + ' [aborted]'
    else
      Caption := APP_CAPTION + ' [completed, ' + FormatDateTime('h:nn:ss.zzz', t) + ']';
    UpdateCtrlState(true);
    Progressbar.Hide;
    btnRunSim.Caption := 'Run simulation';
  end else
    FAborted := true;
end;

procedure TMainForm.cbAnnularApertureChange(Sender: TObject);
begin
  cmbAnalyzerTypeChange(nil);
end;

procedure TMainForm.CancelHandler(ASimulation: TSimulation; var Cancel: Boolean);
begin
  Progressbar.Position := ASimulation.ElectronSource.NumFired;
  Application.ProcessMessages;
  Cancel := FAborted;
end;

procedure TMainForm.cmbAnalyzerTypeChange(Sender: TObject);
var
  isCMA: Boolean;
begin
  isCMA := cmbAnalyzerType.ItemIndex = 0;
  cbAnnularAperture.Enabled := isCMA and not FRunning;
  lblFrom.Enabled := isCMA and cbAnnularAperture.Checked and not FRunning;
  seSectorFrom.Enabled := lblFrom.Enabled and not FRunning;
  lblTo.Enabled := lblFrom.Enabled and not FRunning;
  seSectorTo.Enabled := lblFrom.Enabled and not FRunning;
  lblDeg.Enabled := lblFrom.Enabled and not FRunning;
  cbUseHoeslerAp.Enabled := isCMA and not FRunning;
end;

procedure TMainForm.cmbTopographyChange(Sender: TObject);
var
  topo: TTopoType;
begin
  topo := GetSelectedTopography;

  lblWidth.Enabled := (topo <> ttStep) and not FRunning;
  seWidth.Enabled := (topo <> ttStep) and not FRunning;
  sbDirUp.Enabled := (topo = ttStep) and not FRunning;
  sbDirDown.Enabled := sbDirUp.Enabled;

  case topo of
    ttContactHole:
      begin
        lblDepth.Caption := 'Depth (µm)';
        lblWidth.Caption := 'Diameter (µm)';
      end;
    ttStripe:
      begin
        lblDepth.Caption := 'Height (µm)';
        lblWidth.Caption := 'Width (µm)';
      end;
    ttStep:
      begin
        lblDepth.Caption := 'Height (µm)';
        lblWidth.Caption := '(not used)';
      end;
  end;
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
    MaxIntens := Max(BottomIntens, Max(TopIntens, WallIntens));
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
const
  STEP_DIR: array[TStepDir] of String = ('', 'up', 'down');
var
  intens: Float;
  src: TElectronSource;
  analyzer: TAnalyzer;
  sample: TSample;
begin
  src := ASimulation.ElectronSource;
  analyzer := ASimulation.Analyzer;
  sample := ASimulation.Sample;
  intens := analyzer.Intensity;

  TrajectoriesChart.Title.Text.Clear;
  TrajectoriesChart.Title.Text.Add(Format('Primary energy: %.3g keV', [src.Energy]));
  TrajectoriesChart.Title.Text.Add(Format('Beam diameter: %.3g µm', [src.BeamRadius*2]));
  TrajectoriesChart.Title.Text.Add(Format('Incident angle: %.3g°', [src.PolarAngle]));
  TrajectoriesChart.Title.Text.Add(Format('Sample: %s, %.3g µm %s on %s', [
    cmbTopography.Items[cmbTopography.ItemIndex],
    abs(sample.zInterface),
    GetElementName(sample.Layer.Z, true),
    GetElementName(sample.Substrate.Z, true)
  ]));

  EmissionPointsChart.Title.Text.Text := TrajectoriesChart.Title.Text.Text;

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
  SummaryMemo.Lines.Add('  SAMPLE');
  SummaryMemo.Lines.Add('    Substrate: %s, Layer: %s', [GetElementName(sample.Substrate.Z, false), GetElementName(sample.Layer.Z, false)]);
  if sample is TContactHole then
    SummaryMemo.Lines.Add('    Topography: Contact hole (diameter=%.3f µm, depth=%.3f µm)', [TContactHole(sample).Radius*2, TContactHole(sample).Depth]);
  if sample is TStripe then
    SummaryMemo.Lines.Add('    Topography: Stripe (width=%.3f µm, height=%.3f µm)', [TStripe(sample).Width, TStripe(sample).Height]);
  if sample is TStep then
    SummaryMemo.Lines.Add('    Topography: Step (height=%.3f µm, %s)', [TStep(sample).Height, STEP_DIR[TStep(sample).Dir]]);
  SummaryMemo.Lines.Add  ('    Title angle: %.0°', [src.PolarAngle]);

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
  ParamsPanel.Constraints.MinHeight := gbSample.Top + gbSample.Height +
    btnRunSim.BorderSpacing.Top + btnRunSim.Height;
  Constraints.MinHeight := ParamsPanel.Constraints.MinHeight + 2*ParamsPanel.BorderSpacing.Around;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ResultsPageControl.ActivePageIndex := 0;
  EmissionPointsPageControl.ActivePageIndex := 0;
  SummaryMemo.Lines.Clear;
  sePrimElCount.MaxValue := MaxInt;
  PopulateMaterialsCombo(cmbSubstrate);
  PopulateMaterialsCombo(cmbLayer);

  BoldGroup(gbEGun);
  BoldGroup(gbAnalyzer);
  BoldGroup(gbSample);
  BoldGroup(rgProjection);

  EmissionPointsChartExtentChange(nil);
  TrajectoriesChartExtentChange(nil);

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

function TMainForm.GetSelectedAnalyzer: TAnalyzerType;
begin
  Result := TAnalyzerType(cmbAnalyzerType.ItemIndex + 1);
end;

function TMainForm.GetSelectedTopography: TTopoType;
begin
  Result := TTopoType(cmbTopography.ItemIndex + 1);
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

procedure TMainForm.EmissionPointsChartExtentChange(Sender: TObject);
var
  xmin, xmax: Double;
begin
  xmin := seEmissionPointsHorMin.Value;
  xmax := seEmissionPointsHorMax.Value;
  if xmin <= xmax then
  begin
    EmissionPointsChart.Extent.XMin := xmin;
    EmissionPointsChart.Extent.XMax := xmax;
    EmissionPointsChart.Extent.UseXMin := true;
    EmissionPointsChart.Extent.UseXMax := true;
  end;
end;
procedure TMainForm.TrajectoriesChartExtentChange(Sender: TObject);
var
  xmin, xmax: Double;
begin
  xmin := seTrajectoryHorMin.Value;
  xmax := seTrajectoryHorMax.Value;
  if xmin <= xmax then
  begin
    TrajectoriesChart.Extent.XMin := xmin;
    TrajectoriesChart.Extent.XMax := xmax;
    TrajectoriesChart.Extent.UseXMin := true;
    TrajectoriesChart.Extent.UseXMax := true;
  end;
end;

procedure TMainForm.GUIToParams(var AParams: TSimParams);
begin
  AParams.NumElectrons := sePrimElCount.Value;
  AParams.PrimaryEnergy := sePrimEnergy.Value;
  AParams.BeamDiameter := seBeamDiam.Value;
  AParams.Focus.X := seFocusX.Value;
  AParams.Focus.Y := seFocusY.Value;
  AParams.Focus.Z := seFocusZ.Value;

  AParams.AnalyzerType := GetSelectedAnalyzer;
  AParams.UseHoeslerAperture := cbUseHoeslerAp.Checked;;

  AParams.SubstrateName := cmbSubstrate.Items[cmbSubstrate.ItemIndex];
  AParams.LayerName := cmbLayer.Items[cmbLayer.ItemIndex];
  AParams.LayerThickness := seLayerThickness.Value;
  AParams.Topography := GetSelectedTopography;
  AParams.Depth := seDepth.Value;
  AParams.Width := seWidth.Value;
  if sbDirUp.Down then AParams.StepDir := sdUp else AParams.StepDir := sdDown;
  AParams.TiltAngle := seTiltAngle.Value;
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
    SimParams.LayerThickness := cfg.ReadFloat(section, 'Layer thickness', SimParams.LayerThickness);
    SimParams.OnlyDirect := cfg.ReadBool(section, 'Only direct', SimParams.OnlyDirect);
    SimParams.TiltAngle := cfg.ReadFloat(section, 'Tilt angle', SimParams.TiltAngle);

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

  cmbAnalyzerType.ItemIndex := ord(AParams.AnalyzerType) + 1;
  cbUseHoeslerAp.Checked := AParams.UseHoeslerAperture;

  cmbSubstrate.ItemIndex := cmbSubstrate.Items.IndexOf(AParams.SubstrateName);
  cmbLayer.ItemIndex := cmbLayer.Items.IndexOf(AParams.LayerName);
  seLayerThickness.Value := AParams.LayerThickness;
  cmbTopography.ItemIndex := ord(AParams.Topography) - 1;
  seDepth.Value := AParams.Depth;
  seWidth.Value := AParams.Width;
  if AParams.StepDir = sdUp then sbDirUp.Down := true else sbDirDown.Down := true;
  seTiltAngle.Value := AParams.TiltAngle;

  cmbAnalyzerTypeChange(nil);
  cmbTopographyChange(nil);
end;

procedure TMainForm.PopulateMaterialsCombo(AComboBox: TComboBox);
var
  L: TMaterialsList;
  i: Integer;
begin
  AComboBox.Items.BeginUpdate;
  try
    AComboBox.Items.Clear;
    L := NewMaterialsList;
    try
      for i := 0 to L.Count-1 do
        AComboBox.Items.Add(GetElementName(TMaterialParams(L[i]).Z, false));
    finally
      L.Free;
    end;
  finally
    AComboBox.Items.EndUpdate;
  end;
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
    FSampleHitPoint := sim.SampleHitpoint;
    DepthTol := Max(sim.Layer.EscapeDepth, sim.Substrate.EscapeDepth) * 6;
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
    cfg.WriteFloat(section, 'Layer thickness', SimParams.LayerThickness);
    cfg.WriteBool(section, 'Only direct', SimParams.OnlyDirect);
    cfg.WriteFloat(section, 'Tilt angle', SimParams.TiltAngle);

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;
end;

procedure TMainForm.TrajectoriesChartAfterDraw(ASender: TChart;
  ADrawer: IChartDrawer);
var
  ext: TDoubleRect;
  ray, plane: TRay;
  v: TVector3;
  P: array of TPoint = nil;
  R: TRect;
  viewIndex: Integer;
  layerThk: Float;
  d: Float;
begin
  if Length(FTrajectories) = 0 then
    exit;

  ext := ASender.CurrentExtent;
  ADrawer.SetPenParams(psSolid, clRed, 3);
  ADrawer.SetBrushParams(bsClear, clNone);
  ADrawer.ClippingStart;

  if ASender = TrajectoriesChart then
    viewIndex := rgProjection.ItemIndex
  else
    viewIndex := 0;

  if seLayerThickness.Value < 0 then
    layerThk := seDepth.Value
  else
    layerThk := seLayerThickness.value;

  case cmbTopography.ItemIndex of
    0:  // contact hole
      case viewIndex of
        0: // x-y plane
          begin
            R.TopLeft := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, seWidth.Value/2));
            R.BottomRight := ASender.GraphToImage(DoublePoint(seWidth.Value/2, -seWidth.Value/2));
            ADrawer.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
          end;
        1, // x-z plane
        2: // y-z plane
          begin
            SetLength(P, 4);
            P[0] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, 0.0));
            P[1] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, -seDepth.Value));
            P[2] := ASender.GraphToImage(DoublePoint( seWidth.Value/2, -seDepth.Value));
            P[3] := ASender.GraphToImage(DoublePoint( seWidth.Value/2, 0.0));
            ADrawer.Polyline(P, 0, Length(P));
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, 0.0));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.x, 0.0));
            ADrawer.Line(P[0], P[1]);
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, -layerThk));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.x, -layerThk));
            ADrawer.Line(P[0], P[1]);
          end;
      end;
    1:  // Stripe
      case viewIndex of
        0:  // x-y plane
          begin
            SetLength(P, 2);
            P[0] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, ext.a.Y));
            P[1] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, ext.b.Y));
            ADrawer.Line(P[0], P[1]);
            P[0] := ASender.GraphToImage(DoublePoint(+seWidth.Value/2, ext.a.Y));
            P[1] := ASender.GraphToImage(DoublePoint(+seWidth.Value/2, ext.b.Y));
            ADrawer.Line(P[0], P[1]);
          end;
        1:  // x-z plane
          begin
            SetLength(P, 4);
            P[0] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, -seDepth.Value));
            P[1] := ASender.GraphToImage(DoublePoint(-seWidth.Value/2, 0.0));
            P[2] := ASender.GraphToImage(DoublePoint( seWidth.Value/2, 0.0));
            P[3] := ASender.GraphToImage(DoublePoint( seWidth.Value/2, -seDepth.Value));
            ADrawer.Polyline(P, 0, Length(P));
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, -seDepth.Value));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.x, -seDepth.Value));
            ADrawer.Line(P[0], P[1]);
          end;
        2:  // y-z plane
          begin
            SetLength(P, 2);
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.X, 0.0));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.X, 0.0));
            ADrawer.Line(P[0], P[1]);
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.X, -seDepth.Value));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.X, -seDepth.Value));
            ADrawer.Line(P[0], P[1]);
          end;
      end;
    2:  // Step
      case viewIndex of
        0:  // x-y plane
          begin
            SetLength(P, 2);
            P[0] := ASender.GraphToImage(DoublePoint(0.0, ext.a.Y));
            P[1] := ASender.GraphToImage(DoublePoint(0.0, ext.b.Y));
            ADrawer.line(P[0], P[1]);
          end;
        1:  // x-z plane
          begin
            SetLength(P, 4);
            if sbDirUp.Down then
            begin
              P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, -seDepth.Value));
              P[1] := ASender.GraphToImage(DoublePoint(0.0, -seDepth.Value));
              P[2] := ASender.GraphToImage(DoublePoint(0.0, 0.0));
              P[3] := ASender.GraphToImage(DoublePoint(ext.b.x, 0.0));
            end else
            begin
              P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, 0.0));
              P[1] := ASender.GraphToImage(DoublePoint(0.0, 0.0));
              P[2] := ASender.GraphToImage(DoublePoint(0.0, -seDepth.Value));
              P[3] := ASender.GraphToImage(DoublePoint(ext.b.x, -seDepth.Value));
            end;
            ADrawer.Polyline(P, 0, 4);
          end;
        2:   // y-z plane
          begin
            SetLength(P, 2);
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, 0.0));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.x, 0.0));
            ADrawer.Line(P[0], P[1]);
            P[0] := ASender.GraphToImage(DoublePoint(ext.a.x, -seDepth.Value));
            P[1] := ASender.GraphToImage(DoublePoint(ext.b.x, -seDepth.Value));
            ADrawer.Line(P[0], P[1]);
          end;
      end;
  end;

  if ValidVector(FSampleHitPoint) then
  begin
    // Draw electron beam
    ADrawer.SetPenParams(psDash, clBlue, 3);

    ray.Point := FSampleHitPoint;
    ray.Dir := Vector3(sin(DegToRad(seTiltAngle.Value)), 0, cos(DegToRad(seTiltAngle.Value)));
    plane.Point := Vector3(0, 0, ext.b.y);
    plane.Dir := Vector3(0, 0, 1);
    d := rayXplane(ray, plane, v{%H-});

                     (*
  writeLn('ray.point: ', ray.point.x:0:3, ' ', ray.Point.y:0:3, ' ', ray.point.Z:0:3);
  writeLn('ray.dir: ', ray.dir.x:0:3, ' ', ray.dir.y:0:3, ' ', ray.dir.Z:0:3);
  writeLn('plane.point: ', plane.point.x:0:3, ' ', plane.Point.y:0:3, ' ', plane.point.Z:0:3);
  writeLn('plane.dir: ', plane.dir.x:0:3, ' ', plane.dir.y:0:3, ' ', plane.dir.Z:0:3);
  writeLn('v: ', v.x:0:3, ' ', v.y:0:3, ' ', v.Z:0:3);
                       *)

    if not IsNaN(d) then
      case ViewIndex of
        0: ; // x-y plane
        1:   // x-z plane
          begin
            P[0] := ASender.GraphToImage(DoublePoint(ray.Point.X, ray.Point.Z));
            P[1] := ASender.GraphToImage(DoublePoint(v.X, v.Z));
            ADrawer.Line(P[0], P[1]);
          end;
        2:   // y-z plane
          begin
            P[0] := ASender.GraphToImage(DoublePoint(ray.Point.Y, ray.Point.Z));
            P[1] := ASender.GraphToImage(DoublePoint(v.Y, v.Z));
            ADrawer.Line(P[0], P[1]);
          end;
      end;
  end;

  ADrawer.ClippingStop;
end;

procedure TMainForm.TrajectoryCompleteHandler(Simulation: TSimulation;
  const AElectronID: String; const ATrajectory: TTrajectory);
var
  ser: TLineSeries;
  udcs: TUserDefinedChartSource;
  idx: Integer;
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

procedure TMainForm.UpdateCtrlState(AEnabled: Boolean);
begin
  gbEGun.Enabled := AEnabled;
  gbAnalyzer.Enabled := AEnabled;
  gbSample.Enabled := AEnabled;
end;

end.

