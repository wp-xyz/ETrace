{ Calculation parameter for the electron tracer }

unit et_Params;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  et_Global;

function CreateIni(ACfgFile: String): TCustomIniFile;
procedure ReadParamsFromCfg(ACfgFile: String);


const
  DEFAULT_MaxEl =100;
  DEFAULT
var
  MaxEl      : Integer  = 100;
  TraceName  : string   = '';
  EmPtFName  : string   = '';
  CfgName    : string   = 'CALC_ET.CFG';
  TopInt     : float    = 0.0;  { Intensity at top surface }
  WallInt    : float    = 0.0;  { Intensity at side walls }
  BotInt     : float    = 0.0;  { Intensity at bottom surface }
  EmPoints   : TMatrix  = nil;  { Matrix to store points where Auger electrons leave the sample }
  nDet       : Integer  = 0;    { Number of detected events }
  nDetOld    : Integer  = 0;    { Detected events in EmPt file }
  nPrim      : Integer  = 0;    { current number of iterations (= primary electron counter) }
  StartTime  : TDateTime= 0;
  {
  SetStart   : LONGINT  = 1;
  Graphik    : BOOLEAN  = TRUE;
  HorRange   : Float    = 9.0;
  VertRange  : Float    = 6.0;
  Projection : TProjType= XYproj;
  PlotColor  : INTEGER  = Yellow;
  GrBkColor  : INTEGER  = BLUE;
  GrColor    : INTEGER  = Yellow;
  BeepOn     : BOOLEAN  = FALSE;
   }
implementation

end.

