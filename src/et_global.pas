unit et_Global;

{$mode ObjFPC}{$H+}

interface

type
  Float = double;

  TDataType = (dtNone, dtTraj, dtEmPts);

  TTopoType = (ttNone, ttContactHole, ttStripe, ttStep);

  TAnalyzerType = (atNone, atCMA, atCHA);

  TNormIntensType = (niRaw, niPrimEl, niArea);

  TProjection = (XYproj, XZproj, YZproj, ThreeD);

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
  etAborted     = 3;

var
  etError: integer = etOK;

const
  ANALYZERTYPE_NAME: array[TAnalyzerType] of String = ('(none)', 'CMA', 'CHA');
  CFG_FILE_NAME = 'etracer.cfg';
  SingleEps     = 1E-4;           { data are stored in single precision }

type
  TSimParams = record
    // general
    zAxis: TVector3;
    // electron source
    PrimaryEnergy: Float;             // keV
    BeamDiameter: Float;              // µm
    Focus: TVector3;                  // µm
    NumElectrons: Integer;
    // electron analyzer
    AnalyzerType: TAnalyzerType;
    SectorStart: Float;
    SectorEnd: Float;
    UseHoeslerAperture: Boolean;
    // sample
    SubstrateName: String;
    LayerName: String;
    Topography: TTopoType;
    Width: Float;                     // µm
    Depth: Float;                     // µm
    LayerThickness: Float;            // µm
    StepDir: TStepDir;
    OnlyDirect: Boolean;
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
    NumElectrons: 1000;
    // electron analyzer
    AnalyzerType: atCMA;
    SectorStart: 0.0;
    Sectorend: 0.0;
    UseHoeslerAperture: false;
    // sample
    SubstrateName: 'Si';
    LayerName: 'SiO2';
    Topography: ttContactHole;
    Width: 1.0;
    Depth: 0.5;
    LayerThickness: -999.0;
    StepDir: sdUp;
    OnlyDirect: false;
    TiltAngle: 0.0
  );

var
  SimParams: TSimParams;

function GetError: Integer;

implementation

function GetError: Integer;
begin
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


