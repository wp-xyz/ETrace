unit et_Global;

{$mode ObjFPC}{$H+}

interface

uses
  MGlobal;

type
  TDataType = (dtNone, dtTraj, dtEmPts);

  TTopoType = (ttNone, ttContactHole, ttStripe, ttStep);

  TAnalyzerType = (atNone, atCMA, atCHA);

  TNormIntensType = (niRaw, niPrimEl, niArea);

  TProjType = (XYproj, XZproj, YZproj, ThreeD);

  TStepDir = (sdNone, sdUp, sdDown);

  TVector3 = record
    X, Y, Z: float;
  end;

  TRay = record
    Point : TVector3;       { start point of ray }
    Dir   : TVector3;       { ray direction      }
  END;

  TMatrix3 = array[1..3] of TVector3;

  ExtStr = String[4];

const
  etOK          = 0;
  etIOError     = 1;
  etOutOfMemory = 2;

var
  etError: integer = etOK;

const
  {
  ET_ID         = 7000;
  cmTitleFnt    = ET_ID;
  cmSubtitleFnt = ET_ID + 1;
  cmxAxFnt      = ET_ID + 2;
  cmyAxFnt      = ET_ID + 3;

  cmRadialEval  = ET_ID + 10;
  cmRadialNorm  = ET_ID + 11;
  cmRadialDens  = ET_ID + 12;

  cmRadialGraph = 250;
  cmRadialSave  = 251;

  hyLoadFile    = 100;
  hyLoadTplFile = 101;
  hySaveTplFile = 102;

  hyRadialSave  = 120;

  hyTitle       = 200;
  hySubtitle    = 201;
  hyXAxLabel    = 202;
  hyYAxLabel    = 203;
}
  SingleEps     = 1E-4;      { data are stored in single precision }

type
  TSimParams = record
    // general
    zAxis: TVector3;
    // electron source
    PrimaryEnergy: Float;             // keV
    BeamDiameter: Float;              // µm
    Focus: TVector3;                  // µm
    // electron analyzer
    AnalyzerType: TAnalyzerType;
    SectorStart: Float;
    SectorEnd: Float;
    UseHoeslerAperture: Boolean;
    OnlyDirect: Boolean;
    // sample
    SubstrateName: String;
    LayerName: String;
    Topography: TTopoType;
    Width: Float;                     // µm
    Depth: Float;                     // µm
    LayerThickness: Float;            // µm
    StepDir: TStepDir;
    TiltAngle: Float;                 // degrees
  end;

const
  DefaultSimParams: TSimParams = (
    // general
    zAxis: (X:0.0; Y:0.0; Z:1.0);
    // electron source
    PrimaryEnergy: 10.0;
    BeamDiameter: 0.3;
    Focus: (X:0.0; Y:0.0; Z:0.0);
    // electron analyzer
    AnalyzerType: atCMA;
    SectorStart: 0.0;
    Sectorend: 0.0;
    UseHoeslerAperture: false;
    OnlyDirect: false;
    // sample
    SubstrateName: 'Si';
    LayerName: 'SiO2';
    Topography: ttContactHole;
    Width: 1.0;
    Depth: 0.5;
    LayerThickness: -999.0;
    StepDir: sdUp;
    TiltAngle: 0.0
  );

var
  SimParams: TSimParams;
             (*
  DEFAULT_Primenergy = 10.0;          // keV
  DEFAULT_BeamDiam = 0.3;             // µm
  DEFAULT_AnalyzerType = atCMA;
  DEFAULT_SectorStart = 0.0;
  DEFAULT_SectorEnd = 0.0;
  DEFAULT_HoeslerAperture = false;
  DEFAULT_zAxis: TVector3 = (X:0.0; Y:0.0; Z:1.0);
  DEFAULT_Focus: TVector3 = (X:0.0; Y:0.0; Z:0.0);
  DEFAULT_Width = 1.0;
  DEFAULT_Depth = 0.1;
  DEFAULT_StepDir = sdDown;
  DEFAULT_Topography = ttContactHole;
  DEFAULT_LayerThickness = -999.0;    // <0 --> is equal to contact hole depth
  DEFAULT_SubstrateName = 'Si';
  DEFAULT_LayerName = 'SiO2';
  DEFAULT_OnlyDirect = false;

var
  TiltAngle      : float;
  PrimEnergy     : float;
  BeamDiam       : float;
  AnalyzerType   : TAnalyzerType;
  SectorStart    : float;
  SectorEnd      : float;
  HoeslerAperture: boolean;
  zAxis          : TVector3;
  Focus          : TVector3;
  Width          : float;
  Depth          : float;
  StepDir        : TStepDir;
  Topography     : TTopoType;
  LayerThickness : float;
  SubstrateName  : string;
  LayerName      : string;
  OnlyDirect     : Boolean;

  TplExt         : ExtStr        = '.TPL';
  TplBackupExt   : ExtStr        = '.BAK';
  CfgExt         : ExtStr        = '.CFG';
     *)
const
  CfgSignature   = 'ET-CFG';
  TplSignature   = 'ET-TPL';
  VersionID      = '2.1';

var
  EmPtFile: TEXT;
  TplFile: String[255];
  CfgFile: String[255];

function GetError: Integer;
procedure InitParams;

implementation

function GetError: Integer;
begin
  if mError <> mOK then
    Result := mError
  else
  if etError <> etOK then
    Result := etError
  else
    Result := 0;
end;

procedure InitParams;
begin
  SimParams := DefaultSimParams;
end;

initialization
  InitParams;
end.


