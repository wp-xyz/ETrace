unit et_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, IniFiles,
  MGlobal, MFunc,
  et_Global, et_Math, et_File, et_Objects, et_Info;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    gbAnalyzer: TGroupBox;
    Label1: TLabel;
    lblBeamDiam: TLabel;
    lblFocusX: TLabel;
    lblFocusY: TLabel;
    lblFocusZ: TLabel;
    ResultsMemo: TMemo;
    seFocusX: TFloatSpinEdit;
    seFocusY: TFloatSpinEdit;
    seFocusZ: TFloatSpinEdit;
    sePrimEnergy: TFloatSpinEdit;
    gbEGun: TGroupBox;
    lblPrimElCount: TLabel;
    sePrimElCount: TSpinEdit;
    lblPrimEnergy: TLabel;
    seBeamDiam: TFloatSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    nPrim: Integer;
    TraceFileName: String;
    EmPtsFileName: String;
    procedure DoneObjs;
    procedure GUIToParams;
    function InitMaterial(AMaterials: TMaterialsList; AName: String): TMaterial;
    procedure InitObjs;
    procedure InitParams;
    procedure ParamsToGUI;
    procedure RunSimulation;
    procedure SaveParams;
    procedure Trace(prim, sec: Integer; Electron: TElectron; Energy: float);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

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

procedure EvalIntensities(Point: TVector3; dI: Float);
var
  R2: Float;
  Tol: Float;
begin
  Tol := EscDepth + EscDepth;
  Tol := Tol + Tol + EscDepth;
  with Point do
  begin
    case Topography of
      ttContHole :
        if Equal(Z, 0.0, Tol) then
        begin
          R2 := Sqr(0.5*Width);
          if LessThan(X*X + Y*Y, R2, Tol) then
            BottomIntens := BottomIntens + dI
          else
            TopIntens := TopIntens + dI;
        end else
        if Equal(Z, -Depth, Tol) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStripe :
        if Zero(Z, Tol) and Zero(X, Width*0.5) then
          TopIntens := TopIntens + dI
        else
        if Equal(Z, -Depth, Tol) and not Zero(X, Width*0.5) then
          BottomIntens := BottomIntens + dI
        else
          WallIntens := WallIntens + dI;

      ttStep :
        if Zero(X, Tol) then
          WallIntens := WallIntens + dI
        else
        if GreaterThan(X, 0.0, Tol) then
          case StepDir of
            sdUp   : TopIntens := TopIntens + dI;
            sdDown : BottomIntens := BottomIntens + dI;
          end
        else
        if LessThan(X, 0.0, Tol) then
          case StepDir of
            sdUp   : BottomIntens := BottomIntens + dI;
            sdDown : TopIntens := TopIntens + dI;
          end;
    end;
  end;
end;

procedure DetectMessageProc(nr: LongInt; var Electron: TAugerElectron);
var
  Backsc: String[1];
  Decs: Byte;
  MaxIntens: Float;
begin
  with Electron do
  begin
    EvalIntensities(Ray.Point, Weight);
    if GenByBkScEl then
      Backsc :='X'
    else
      BackSc := ' ';
    MaxIntens := MaxF(BottomIntens, MaxF(TopIntens, WallIntens));
    nr := nr + nDetOld;
    (*
    IF Graphik THEN BEGIN
      IF EmPtFName<>'' THEN BEGIN
        WriteToGraphWindow(PlotWindow);
        WITH Ray.Point DO DrawEmPoint(X,Y,Z);
      END;
      Decs := 3;
      WriteToGraphWindow(GrResultWindow);
      MoveRel(TextWidth('detekt.Ereignisse: '),0);
      OutLn(RightStr(DecStr(nr), 10));
      OutLn(RightStr(FixedFloatStr(TopInt,Decs),10));
      OutLn(RightStr(FixedFloatStr(WallInt,Decs),10));
      OutLn(RightStr(FixedFloatStr(BotInt,Decs),10));
    END ELSE
    *)
    begin
      if MaxIntens < 10 then Decs := 3 else
        if MaxIntens < 100 then Decs := 2 else
          if MaxIntens < 1000 then Decs := 1 else
            Decs := 0;
      with Ray.Point do
        Form1.ResultsMemo.Lines.Add('%7d %9.3f %9.3f %9.3f %12.*f %12.*f %12.*f %6s', [
          nr, X, Y, Z, decs, TopIntens, decs, WallIntens, decs, BottomIntens, BackSc])
        {
      WriteToTextWindow(AugerElWindow);
      WITH Ray.Point DO
        WriteLn(nr:7, X:9:3, Y:9:3, Z:9:3, TopInt:11:Decs, Wallint:12:Decs,
          BotInt:12:Decs, Backsc:3);
      SaveCursor(AugerElWindow);
      }
    end;
  end;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  RunSimulation;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitParams;
  ParamsToGui;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GuiToParams;
  SaveParams;
  DoneObjs;
end;

procedure TForm1.DoneObjs;
begin
  FreeAndNil(EGun);
  FreeAndNil(Analyzer);
  FreeAndNil(Sample);
end;

procedure TForm1.GUIToParams;
begin
  MaxEl := sePrimElCount.Value;
  PrimEnergy := sePrimEnergy.Value;
  BeamDiam := seBeamDiam.Value;
  Focus.X := seFocusX.Value;
  Focus.Y := seFocusY.Value;
  Focus.Z := seFocusZ.Value;
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

procedure TForm1.InitObjs;
const
  MASK = '%7s %9s %9s %9s %12s %12s %12s %6s';
var
  Materials: TMaterialsList;
  s6, s7, s9, s12: String;
begin
  DoneObjs;

  Materials := NewMaterialsList;
  try
    Substrate := InitMaterial(Materials, SubstrateName);
    Layer := InitMaterial(Materials, LayerName);
  finally
    Materials.Free;
  end;

  EGun := TEGun.Create(TiltAngle, PrimEnergy, BeamDiam, Focus);
  Analyzer := TAnalyzer.Create(AnalyzerType, TiltAngle, 0.0);
  Analyzer.HoeslerAp := HoeslerAperture;
  if not Zero(SectorStart, FloatEps) and not Zero(SectorEnd, FloatEps) then
    Analyzer.Restrict(SectorStart, SectorEnd);

  case Topography of
    ttContHole : Sample := TContactHole.Create(LayerThickness, et_Global.Width*0.5, Depth, TraceFileName);
    ttStripe   : Sample := TStripe.Create(LayerThickness, et_Global.Width, Depth, TraceFileName);
    ttStep     : Sample := TStep.Create(LayerThickness, Depth, StepDir, TraceFileName);
  end;
  Sample.OnlyDirect := OnlyDirect;

  EscDepth   := MaxF(Layer.EscapeDepth, Substrate.EscapeDepth);
  DetectProc := @DetectMessageProc;
//  SaveProc   := @SaveDetectedElectronProc;
//  TrajProc   := @DrawTrajectory;

  ResultsMemo.Lines.Clear;
  ResultsMemo.Lines.Add(MASK, ['No.', 'X', 'Y', 'Z', 'I(top)', 'I(wall)', 'I(bottom)', 'BkSc']);
  s6 := StringOfChar('-', 6);
  s7 := StringOfChar('-', 7);
  s9 := StringOfChar('-', 9);
  s12 := StringOfChar('-', 12);
  ResultsMemo.Lines.Add(MASK, [s7, s9, s9, s9, s12, s12, s12, s6]);
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
    TiltAngle := cfg.ReadFloat(section, 'Tilt angle', TiltAngle);
    PrimEnergy := cfg.ReadFloat(section, 'Primary energy', PrimEnergy);
    BeamDiam := cfg.ReadFloat(section, 'Beam diameter', BeamDiam);
    s := cfg.ReadString(section, 'Analyzer type', '');
    case Uppercase(s) of
      'CMA', '': AnalyzerType := atCMA;
      'CHA': AnalyzerType := atCHA;
      else AnalyzerType := atNone;
    end;
    SectorStart := cfg.ReadFloat(section, 'Analyzer Aperture Min', SectorStart);
    SectorEnd := cfg.ReadFloat(section, 'Analyzer Aperture Max', SectorEnd);
    HoeslerAperture := cfg.ReadBool(section, 'Hoesler aperature', HoeslerAperture);
    Focus.X := cfg.ReadFloat(section, 'Focus X', Focus.X);
    Focus.Y := cfg.ReadFloat(section, 'Focus Y', Focus.Y);
    Focus.Z := cfg.ReadFloat(section, 'Focus Z', Focus.Z);
    s := cfg.ReadString(section, 'Topography', '');
    case Uppercase(s) of
      '',
      'CONTACT HOLE': Topography := ttContHole;
      'STRIPE': Topography := ttStripe;
      'STEP': Topography := ttStep;
      else Topography := ttNone;
    end;
    case Topography of
      ttContHole:
        begin
          et_Global.Width := cfg.ReadFloat(section, 'Contact hole diameter', et_Global.Width);
          et_Global.Depth := cfg.ReadFloat(section, 'Contact hole depth', et_Global.Depth);
        end;
      ttStripe:
        begin
          et_Global.Width := cfg.ReadFloat(section, 'Stripe width', et_Global.Width);
          et_Global.Depth := cfg.ReadFloat(section, 'Stripe height', et_Global.Depth);
        end;
      ttStep:
        begin
          et_Global.Depth := cfg.ReadFloat(section, 'Step height', et_Global.Depth);
          s := cfg.ReadString(section, 'Step dir', '');
          case Uppercase(s) of
            'UP', '': StepDir := sdUp;
            'DOWN': StepDir := sdDown;
          end;
        end;
    end;
    SubstrateName := cfg.ReadString(section, 'Substrate material', SubstrateName);
    LayerName := cfg.ReadString(section, 'Layer material', LayerName);
    LayerThickness := cfg.ReadFloat(section, 'LayerThickness', LayerThickness);
    OnlyDirect := cfg.ReadBool(section, 'Only Direct', OnlyDirect);
    TraceFileName := cfg.ReadString(section, 'Trajectory file name', TraceFileName);
    EmPtsFileName := cfg.ReadString(section, 'Emission points file name', EmPtsFileName)

  finally
    cfg.Free;
    FormatSettings := savedFormatSettings;
  end;

  nPrim := 0;
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

procedure TForm1.ParamsToGUI;
begin
  sePrimElCount.Value := MaxEl;
  sePrimEnergy.Value := PrimEnergy;
  seBeamDiam.Value := BeamDiam;
  seFocusX.Value := Focus.X;
  seFocusY.Value := Focus.Y;
  seFocusZ.Value := Focus.Z;
end;

procedure TForm1.RunSimulation;
var
  electron: TElectron;
  energy: Float;
begin
  InitParams;
  GUIToParams;
  InitObjs;
  while (nPrim < MaxEl) and not Aborted do
  begin
    inc(nPrim);
    //WritePrimEl(nPrim);
    EGun.GenEl(electron, energy);
    Trace(nPrim, 0, electron, energy);
  end;
end;

procedure TForm1.SaveParams;
var
  cfg: TCustomIniFile;
  topo: Word;
  s: String;
  ans: String;
  section: String;
  savedFormatSettings: TFormatSettings;
begin
  savedFormatSettings := FormatSettings;
  cfg := TIniFile.Create('calc_et.cfg');
  try
    FormatSettings.DecimalSeparator := '.';

    section := 'Params';
    cfg.WriteInteger(section, 'Iterations', MaxEl);
    cfg.WriteFloat(section, 'Tilt angle', TiltAngle);
    cfg.WriteFloat(section, 'Primary energy', PrimEnergy);
    cfg.WriteFloat(section, 'Beam diameter', BeamDiam);
    case AnalyzerType of
      atCMA: s := 'CMA';
      atCHA: s := 'CHA';
      else   s := '';
    end;
    if s <> '' then cfg.WriteString(section, 'Analyzer type', s);
    cfg.WriteFloat(section, 'Analyzer Aperture Min', SectorStart);
    cfg.WriteFloat(section, 'Analyzer Aperture Max', SectorEnd);
    cfg.WriteBool(section, 'Hoesler aperature', HoeslerAperture);
    cfg.WriteFloat(section, 'Focus X', Focus.X);
    cfg.WriteFloat(section, 'Focus Y', Focus.Y);
    cfg.WriteFloat(section, 'Focus Z', Focus.Z);
    case Topography of
      ttContHole: s := 'Contact hole';
      ttStripe: s := 'Stripe';
      ttStep: s := 'Step';
      else s := '';
    end;
    if s <> '' then cfg.WriteString(section, 'Topography', s);
    case Topography of
      ttContHole:
        begin
          cfg.WriteFloat(section, 'Contact hole diameter', et_Global.Width);
          cfg.WriteFloat(section, 'Contact hole depth', et_Global.Depth);
        end;
      ttStripe:
        begin
          cfg.WriteFloat(section, 'Stripe width', et_Global.Width);
          cfg.WriteFloat(section, 'Stripe height', et_Global.Depth);
        end;
      ttStep:
        begin
          cfg.WriteFloat(section, 'Step height', et_Global.Depth);
          case StepDir of
            sdUp: s := 'Up';
            sdDown: s := 'Down';
            else s := '';
          end;
          if s <> '' then cfg.WriteString(section, 'Step dir', s);
        end;
    end;
    cfg.WriteString(section, 'Substrate material', SubstrateName);
    cfg.WriteString(section, 'Layer material', LayerName);
    cfg.WriteFloat(section, 'LayerThickness', LayerThickness);
    cfg.WriteBool(section, 'Only Direct', OnlyDirect);
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

end.

