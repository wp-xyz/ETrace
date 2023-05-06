(****************************************************************************)
(*                                 ET_Info                                  *)
(* ------------------------------------------------------------------------ *)
(* Unit for accessing the file header                                       *)
(*                                                                          *)
(* Structure of the file header:                                            *)
(* - MatLab format                                                          *)
(* - 1st variable: vector <Params> with the following elements              *)
(*    Params[1] = ID (5432.0 -> Trajectories)                               *)
(*                   (6543.0 -> Emission points)                            *)
(*    Params[2] = Sample type                                               *)
(*    Params[3] = Z (of the substrate)                                      *)
(*    Params[4] = Core energy (keV, for substrate)                          *)
(*    Params[5] = Contact hole radius/stripe width (µm)/Step dir            *)
(*    Params[6] = Contact hole depth/stripe eight/ step height (µm)         *)
(*    Params[7] = Auger energy (eV, for substrate)                          *)
(*    Params[8] = Atomic Mass, for substrate                                *)
(*    Params[9] = Mass density, for substrate                               *)
(*    Params[10] = Iteration count                                          *)
(*    Params[11..13] = e-gun axis (x,y,z)                                   *)
(*    Params[14..16] = e-gun focus (x,y,z)                                  *)
(*    Params[17] = e-gun FWHM/2                                             *)
(*    Params[18] = Primary energy (keV)                                     *)
(*    Params[19..20] not used                                               *)
(*    Params[21] = Analyzer min acceptance angle (deg)  (Acc1)              *)
(*    Params[22] = Analyzer max acceptance angle (deg)  (Acc2)              *)
(*    Params[23..25] = Analyzer axis (x,y,z)                                *)
(*    Params[26] = Hösler aperture active                                   *)
(*    Params[27..29] not used                                               *)
(*    Params[30] = Layer Thickness                                          *)
(*    Params[31] = Z for layer                                              *)
(*    Params[32] = CoreEnergy for layer (keV)                               *)
(*    Params[33] = Auger energy for layer (keV)                             *)
(*    Params[34] = Atomic Mass for layer                                    *)
(*    Params[35] = Mass density for layer                                   *)
(*    Params[37..40] not used                                               *)
(****************************************************************************)

unit et_Info;

{$mode ObjFPC}{$H+}


interface

uses
  MGlobal, MFunc, MMatrix, ET_Global, ET_Math;

const
  SubstrateID = 1;
  LayerID     = 2;

function  GetDataType(Info: TMatrix): TDataType;
function  GetTopoType(Info: TMatrix): TTopoType;
function  GetZ(Info: TMatrix; ID: byte): float;
function  GetCoreEnergy(Info: TMatrix; ID: byte): float;
function  GetContHoleRadius(Info: TMatrix): float;
function  GetContHoleDepth(Info: TMatrix) : float;
function  GetStepDir(Info: TMatrix): TStepDir;
function  GetStepHeight(Info: TMatrix): float;
function  GetStripeWidth(Info: TMatrix): float;
function  GetStripeHeight(Info: TMatrix): float;
function  GetLayerThickness(Info: TMatrix): float;
function  GetAugerEnergy(Info: TMatrix; ID: Byte): float;
function  GetAtomicMass(Info: TMatrix; ID: Byte): float;
function  GetMassDensity(Info: TMatrix; ID: Byte): float;
function  GetIterations(Info:TMatrix): LongInt;
function  GetEgunTheta(Info: TMatrix): float;
procedure GetEgunAxis(Info: TMatrix; var Axis: TVector3);
procedure GetEgunFocus(Info: TMatrix; var Focus: TVector3);
function  GetBeamDiameter(Info: TMatrix): float;
function  GetPrimEnergy(Info: TMatrix): float;
function  GetAnalyzerType(Info: TMatrix): TAnalyzerType;
function  GetAnalyzerTheta(Info: TMatrix): float;
function  GetHoeslerAperture(Info: TMatrix): BOOLEAN;


implementation

function ReadLayerInfo(Info: TMatrix; SubstrIndex, Layerindex: Integer): Float;
begin
  Result := Info.GetValue(LayerIndex, 1);
  if Result = 0.0 then
    Result := Info.GetValue(SubstrIndex, 1);
end;

function GetDataType(Info: TMatrix): TDataType;
var
  FID: float;  { " file ID" }
begin
  FID := Info.GetValue(1,1);
  if Equal(FID, 5432.0, SingleEps) then
    GetDataType := dtTraj
  else
  if Equal(FID, 6543.0, SingleEps) then
    GetDataType := dtEmPts
  else
    GetDataType := dtNone;
end;

function GetTopoType(Info: TMatrix): TTopoType;
var
  value: float;
begin
  value := Info.GetValue(2,1);
  case Round(value) of
    1  : Result := ttContHole;
    2  : Result := ttStripe;
    3  : Result := ttStep;
    else Result := ttNone;
  end;
end;

function GetZ(Info: TMatrix; ID: Byte): float;
begin
  case ID of
    LayerID     : Result := ReadLayerInfo(Info, 3, 31);
    SubstrateID : Result := Info.Getvalue(3, 1);
  end;
end;

function GetCoreEnergy(Info: TMatrix; ID: Byte): float;
begin
  case ID of
    LayerID     : Result := ReadlayerInfo(Info, 4, 32);
    SubstrateID : Result := Info.GetValue(4, 1);
  end;
end;

function GetContHoleRadius(Info: TMatrix): float;
begin
  if GetTopoType(Info) = ttContHole then
    Result := Info.GetValue(5,1)
  else
    Result := mEmpty;
end;

function GetContHoleDepth(Info: TMatrix): float;
begin
  if GetTopoType(Info) = ttContHole then
    Result := Info.GetValue(6,1)
  else
    Result := mEmpty;
end;

function GetStepDir(Info: TMatrix): TStepDir;
begin
  Result := sdNone;
  if GetTopoType(Info) = ttStep then
    case Round(Info.GetValue(5, 1)) of
      1: Result := sdUp;
      2: Result := sdDown;
    end;
end;

function GetStepHeight(Info: TMatrix): float;
begin
  if GetTopoType(Info) = ttStep then
    Result := Info.Getvalue(6, 1)
  else
    Result := mEmpty;
end;

function GetStripeWidth(Info: TMatrix): float;
begin
  if GetTopoType(Info) = ttStripe then
    Result := Info.GetValue(5, 1)
  else
    Result := mEmpty;
end;

function GetStripeHeight(Info: TMatrix): float;
begin
  if GetTopoType(Info) = ttStripe then
    Result := Info.GetValue(6, 1)
  else
    Result := mEmpty;
end;

function GetLayerThickness(Info: TMatrix): float;
begin
  Result := Info.GetValue(30, 1);
end;

function GetAugerEnergy(Info: TMatrix; ID: Byte): float;
begin
  case ID of
    SubstrateID : Result := Info.Getvalue(7, 1);
    LayerID     : Result := ReadLayerinfo(Info, 7, 33);
  end;
end;

function GetAtomicMass(Info: TMatrix; ID: Byte): float;
begin
  case ID of
    SubstrateID : Result := Info.Getvalue(8, 1);
    LayerID     : Result := ReadLayerinfo(Info, 8, 34);
  end;
end;

function GetMassDensity(Info: TMatrix; ID: byte): float;
begin
  case ID of
    SubstrateID : Result := Info.GetValue(9, 1);
    LayerID     : Result := ReadLayerInfo(Info, 9, 35);
  end;
end;

function GetIterations(Info: TMatrix): LongInt;
begin
  Result := Round(Info.GetValue(10, 1));
end;

{ Polar angle between e-gun axis and sample normal, in radians }
function GetEgunTheta(Info: TMatrix): float;
var
  Axis: TVector3;
begin
  GetEgunAxis(Info, Axis);
  Normalize(Axis);
  Result := VecAngle(Axis, zAxis);
end;

procedure GetEgunAxis(Info: TMatrix; var Axis: TVector3);
begin
  Axis.X := Info.GetValue(11, 1);
  Axis.Y := Info.GetValue(12, 1);
  Axis.Z := Info.GetValue(13, 1);
end;

procedure GetEgunFocus(Info: TMatrix; var Focus: TVector3);
begin
  Focus.X := Info.GetValue(14, 1);
  Focus.Y := Info.GetValue(15, 1);
  Focus.Z := Info.GetValue(16, 1);
end;

function GetBeamDiameter(Info: TMatrix): float;
begin
  Result := Info.GetValue(17, 1) * 2.0;
end;

function GetPrimEnergy(Info: TMatrix): float;
begin
  Result := Info.GetValue(18, 1);
end;

function GetAnalyzerType(Info: TMatrix): TAnalyzerType;
var
  Acc1, Acc2 : float;
begin
  Acc1 := Info.GetValue(21, 1);
  Acc2 := Info.GetValue(22, 1);
  if Zero(42.3-6.0-Acc1, SingleEps) and Zero(42.3+6.0-Acc2, SingleEps) then
    Result := atCMA
  else
  if Zero(Acc1, SingleEps) and Zero(Acc2-15.0, SingleEps) then
    Result := atCHA
  else
    Result := atNone;
end;

{ Polar angle between analyzer axis and sample normal, in radians }
function GetAnalyzerTheta(Info: TMatrix): float;
var
  Axis: TVector3;
begin
  Axis.X := Info.GetValue(23, 1);
  Axis.Y := Info.GetValue(24, 1);
  Axis.Z := Info.GetValue(25, 1);
  Normalize(Axis);
  result := VecAngle(Axis, zAxis);
end;

function GetHoeslerAperture(Info: TMatrix): boolean;
begin
  Result := abs(Info.GetValue(26, 1)) > 0.001;
end;

end.

