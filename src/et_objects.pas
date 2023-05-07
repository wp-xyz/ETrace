unit et_Objects;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Contnrs, SysUtils,
  MGlobal, MFunc, MMatrix,
  et_Global, et_Math, et_File;

const
  Avogadro = 6.02214076E23;   { Avogadro constant }

type
  TElectron = record
    Ray: TRay;
    Weight: Float;
  end;

  TAugerElectron = record
    Ray: TRay;
    Weight: float;
    GenByBkscEl: Boolean;
  end;

  TElectronSource = class
    Energy      : float;
    Axis        : TVector3;
    theta       : float;
    BeamR       : float;       { Bream radius = std deviation of the normal distribution }
    FocusedPoint: TVector3;
    Fired       : LongInt;
    constructor Create(PolarAngle, E, BeamDiam: float; Focus: TVector3);
    destructor Destroy; override;
    procedure  GenEl(var Electron: TElectron; var E: float);
  end;

  TAnalyzer = class
    Axis         : TVector3;   { Analyzer axis, as seen in the sample coordinate system }
    Acc1, Acc2   : float;      { cosine of the accepted range }
    SectorFrom   : float;
    SectorTo     : float;
    Restricted   : boolean;
    UseHoeslerAp : boolean;
    Detected     : LongInt;
    Intensity    : float;
    constructor Create(AnalyzerType: TAnalyzerType; PolarAngle, AzimAngle: float);
    function Detect(var Electron: TAugerElectron): Integer;
    procedure Restrict(ASectorFrom, ASectorTo: Float);
  end;

  TMaterialParams = class
    Z           : float;
    A           : float;
    MassDensity : float;
    CoreLevel   : float;
    AugerEnergy : float;
    constructor Create(theZ, theMolecMass, theMassDensity, theCoreLevel, theAugerEnergy: float);
  end;

  TMaterialsList = class(TFPObjectList)
    function SearchZ(Z: float): TMaterialParams;
    function SearchName(Name: String): TMaterialParams;
  end;

  TMaterial = class
    Z              : float;
    A              : float;       { Atomic/moleculare mass }
    MassDensity    : float;       { Density in g/cm3 }
    NumDensity     : float;       { Density in atoms/cm3 }
    ElemDensity    : float;       { at/cm3 of the element to be analyzed }
    ScreeningParam : float;       { Screening paremter Alpha0 }
    RutherfordParam: float;       { Pre-factor for Rutherford cross-section }
    StoppingPowParam:float;       { Factor for stopping power }
    J              : float;       { average ionization potential (keV) }
    Eb             : float;       { Bindung energy of an electron shell (keV) }
    AugerEnergy    : float;       { Auger energy, eV }
    EscapeDepth    : float;       { Auger escape depth (µm) }
    constructor Create(theZ, theMolecMass, theMassDensity, theCoreLevel,
                  theAugerEnergy: float);
    function CalcAugerCrossSection(E: float): float;
    function CalcAugerEscapeDepth(E: float): float;
    function CalcStoppingPower(E: float): float;
    procedure InitParams;
    procedure RutherfordScattParams(E: float; var Sigma, Alpha: float);
  end;

  TSample = class
  private
    FLayer: TMaterial;
    FSubstrate: TMaterial;
    FPrimaryEnergy: Float;
  public
    Material    : TMaterial;
    zInterface  : float;
    maxStepLen  : float;
    TraceFile   : String;
    Intensfact  : float;
    Trajectory  : TMatrix;
    OnlyDirect  : boolean;
    constructor Create(ASubstrate, ALayer: TMaterial;
      APrimaryEnergy, ALayerThickness: float; ATraceFile: string);
    destructor  Destroy; override;
    function    CancelTrace: Boolean;
    procedure   ChangeMaterial(NewMaterial: TMaterial);
    procedure   DrawSample(Projection: TProjection); virtual;
    function    EmitAugerEl(Point: TVector3; E: float; var Electron: TAugerElectron): boolean;
    function    Intersection(Ray: TRay; var Point:TVector3; FromOutside: Boolean): Boolean; virtual;
    function    OnSurface(Point: TVector3): boolean; virtual; abstract;
    function    Outside(Point: TVector3): boolean; virtual; abstract;
    procedure   Scatter(var Electron: TElectron; var E: float);
    procedure   Save(Point: TVector3; E: float; n:integer);
    procedure   SurfNormal(Point:TVector3; var Normal: TVector3); virtual; abstract;
    function    Trace(Name: String; var Electron: TElectron; var E: float; Emin: Float): Boolean;
  end;

  TContactHole = class(TSample)
    Radius      : float;
    Depth       : float;
    constructor Create(ASubstrate, ALayer: TMaterial;
      APrimaryEnergy, ALayerThickness, ARadius, ADepth: float; ATraceFile: string);
    procedure   DrawSample(Projection: TProjection); override;
    function    InHole(Point: TVector3): Boolean;
    function    Intersection(Ray: TRay; var Point: TVector3; FromOutside: Boolean): Boolean; override;
    function    OnSurface(Point: TVector3): Boolean; override;
    function    Outside(Point: TVector3): boolean; override;
    procedure   SurfNormal(Point: TVector3; var Normal: TVector3); override;
  end;

  TStripe = class(TSample)
    Width       : float;
    Height      : float;
    constructor Create(ASubstrate, ALayer: TMaterial;
      APrimaryEnergy, ALayerThickness, AWidth, AHeight: float; ATraceFile: String);
    procedure   DrawSample(Projection: TProjection); override;
    function    Intersection(Ray: TRay; var Point: TVector3; FromOutside: boolean): Boolean; override;
    function    OnSurface(Point: TVector3): Boolean; override;
    function    Outside(Point: TVector3): Boolean; override;
    procedure   SurfNormal(Point: TVector3; var Normal: TVector3); override;
  end;

  TStep = class(TSample)
    Height      : Float;
    Dir         : TStepDir;
    constructor Create(ASubstrate, ALayer: TMaterial;
      APrimaryEnergy, ALayerThickness, AHeight: Float; ADir: TStepDir; ATracefile: string);
    procedure   DrawSample(Projection: TProjection); override;
    function    Intersection(Ray: TRay; var Point: TVector3; FromOutside: Boolean): Boolean; override;
    function    OnSurface(Point: TVector3) : BOOLEAN; override;
    function    Outside(Point: TVector3) : BOOLEAN; override;
    procedure   SurfNormal(Point: TVector3; var Normal: TVector3); override;
  end;

type
  TDetectMsgProc   = procedure(nr: LongInt; var Electron: TAugerElectron);
  TCancelTraceFunc = function: Boolean;
  TSaveDetElProc   = procedure(var Electron: TAugerElectron);
  TTrajectoryProc  = procedure(Trajectory: TMatrix);

const
  Analyzer        : TAnalyzer       = nil;
  ElectronSource  : TElectronSource = nil;
  DetectProc      : Pointer         = nil;
  CancelTraceFunc : Pointer         = nil;
  SaveProc        : Pointer         = nil;
  TrajProc        : Pointer         = nil;
  Substrate       : TMaterial       = nil;
  Layer           : TMaterial       = nil;
  Sample          : TSample         = nil;

function GetElementName(Z: float; AllowSubscripts: Boolean): String;

function GetAtomsPerMolecule(Z: float): Integer;

function GetConcentration(Z: float): float;

function NewMaterialsList: TMaterialsList;

function NewMaterial(theZ, theMolecMass, theMassDensity, theCoreLevel,
           theAugerEnergy: float): TMaterialParams;

implementation

(****************************************************************************)
(*                        Identifiers of chemical elements                  *)
(****************************************************************************)

type
  TElementName = string[2];   // Type used for element names

const
  psMaxZ = 103;    //Count of chemical elements used in array ElementName
  ElementName: array[1..psMaxZ] of TElementName =
    ('H' ,'He','Li','Be','B' ,'C' ,'N' ,'O' ,'F' , 'Ne',
     'Na','Mg','Al','Si','P' ,'S', 'Cl','Ar','K' , 'Ca',
     'Sc','Ti','V' ,'Cr','Mn','Fe','Co','Ni','Cu', 'Zn',
     'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y',  'Zr',
     'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','IN', 'Sn',
     'Sb','Te','J', 'Xe','Cs','Ba','La','Ce','Pr', 'Nd',
     'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm', 'Yb',
     'Lu','Hf','Ta','W' ,'Re','Os','Ir','Pt','Au', 'Hg',
     'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac', 'Th',
     'Pa','U' ,'Np','Pu','Am','Cm','Bk','Cf','ES', 'Fm',
     'Md','No','Lw');
     // The index into this array is the atomic number Z of the corresponding element

const
  Z_SiO2  = 10.1;   { Z is used as "Index", in order to assign parameters to these materials. }
  Z_Si3N4 = 10.01;
  Z_Al2O3 = 9.99;

function GetElementName(Z: float; AllowSubscripts: Boolean): String;
begin
  if (frac(Z) = 0) and (Z > 0) and (Z <= psMaxZ) then
    Result := ElementName[Round(Z)]
  else
  if Equal(Z, Z_SiO2, SingleEps) then
  begin
    if AllowSubscripts then
      Result := 'SiO<sub>2</sub>'
    else
      Result := 'SiO2';
  end else
  if Equal(Z, Z_Si3N4, SingleEps) then
  begin
    if AllowSubscripts then
      Result := 'Si<sub>3</sub>N<sub>4</sub>'
    else
      Result := 'Si3N4';
  end else
  if Equal(Z, Z_Al2O3, SingleEps) then
  begin
    if AllowSubScripts then
      Result := 'Al<sub>2</sub>O<sub>3</sub>'
    else
      Result := 'Al2O3';
  end else
    Result := '';
end;

function GetConcentration(Z: float): float;
begin
  if (frac(Z) = 0) and (Z > 0) and (Z <= psMaxZ) then
    Result:= 1.0
  else
  if Equal(Z, Z_SiO2, SingleEps) then
    Result := 1.0 {/3.0 <<<<<<<<<<<<<<<<<<<}  // ???? why not ????
  else
  if Equal(Z, Z_Si3N4, SingleEps) then
    Result := 3.0/7.0
  else
  if Equal(Z, Z_Al2O3, SingleEps) then
    Result := 2.0/5.0
  else
    raise Exception.Create('[GetConcentration] Z is not defined.');
end;

function GetAtomsPerMolecule(Z: float): Integer;
begin
  if Equal(Z, Z_SiO2, SingleEps) then
    Result := 3   { SiO2 }
  else
  if Equal(Z, Z_Si3N4, SingleEps) then
    Result := 7  { Si3N4 }
  else
  if Equal(Z, Z_Al2O3, SingleEps) then
    Result := 5  { Al2O3 }
    { *** The molecules above have a total Z of 10. In order to be able to
      classify the material on the basis of the Z, we are using slightly
      modified Z here - see declaration of the Z_XXXX constants.
    }
  else
  if (frac(Z) = 0) then
    Result := 1
  else
    raise Exception.Create('[GetAtomsPerMolecule] No molecule is assigned to this Z.');
end;


(****************************************************************************)
(*                                   TElectronSource                                  *)
(* ------------------------------------------------------------------------ *)
(* The electron gun: The constructor defines the geometrical relations      *)
(* (Orientation with respect to the sample, hitting point of the beam,      *)
(* beam diameter.                                                           *)
(* The method GenEl generates an electron.                                  *)
(****************************************************************************)

{ It is assumed that the primary beam is focused to point <Focus> on the xz plane.
  <BeamDiam> is the beam diameter, <PolarAngle> the angle between electron
  beam and sample normal (> 0 --> incident from the right, < 0 --> incient from
  the left, as seen in a top view of the sample).
  <E> is the electron energy in keV.
  The coordinate system is defined by the sample. }
constructor TElectronSource.Create(PolarAngle, E, BeamDiam: float; Focus: TVector3);
begin
  inherited Create;
  theta := DegToRad(PolarAngle);
  BeamR := BeamDiam*0.5;
  SphToCart(theta, 0.0, Axis.X, Axis.Y, Axis.Z);  { |Axis| = 1 }
  FocusedPoint := Focus;
  Energy := E;
  Fired  := 0;
end;

destructor TElectronSource.Destroy;
begin
  inherited;
end;

{ Creates an electron which hits the sample at point <Focus> with a
  normal distribution of width 2*BeamR.
  The coordinates of the <Ray> refer to the sample. }
procedure TElectronSource.GenEl(var Electron: TElectron; var E: float);
var
  r, phi: float;
  V: TVector3;
begin
  r := Random_Gauss * BeamR;
  phi := Random * TwoPi;
  V.Z := 0.0;
  CylToCart(r,phi, V.X, V.Y);
  RotateY(V, theta);
  Electron.Ray.Dir := Axis;
  VecMulSc(Electron.Ray.Dir, -1.0);
  VecAdd(FocusedPoint, V, Electron.Ray.Point);
  VecAdd(Axis, Electron.Ray.Point, Electron.Ray.Point);
  Electron.Weight := 1.0;                               { not at surface }
  E := Energy;
  inc(Fired);
end;

(****************************************************************************)
(*                               TAnalyzer                                  *)
(* ------------------------------------------------------------------------ *)
(* the electron analyzer: The constructor defines the type of the analyzer  *)
(* (CMA or CHA), as well as its orientation.                                *)
(* The procedure Detect() increments the internal counter <Detected> by 1   *)
(* as well as the <Intensity> by the value <Weight> of th electron, if an   *)
(* electron enters the acceptance range of the anylyzer.                    *)
(* If the string theEmPointFName has been specified as non-empty during     *)
(* construction then the coordinates of the emission points as well as the  *)
(* <weight> value of this electron are stored in this file (Save).          *)
(****************************************************************************)

{ PolarAngle, AzimAngle specify the orientation of the analyzer axis with
  respect to the sample. }
constructor TAnalyzer.Create(AnalyzerType: TAnalyzerType;
  PolarAngle, AzimAngle: float);
const
  CMA_ANGLE = 42.3;
begin
  inherited Create;
  SphToCart(DegToRad(PolarAngle), DegToRad(AzimAngle), Axis.X, Axis.Y, Axis.Z);
  case AnalyzerType of
    atCMA : begin
              Acc1 := Cos(DegToRad(CMA_ANGLE - 6.0));
              Acc2 := Cos(DegToRad(CMA_ANGLE + 6.0));
            end;
    atCHA : begin
              Acc1 := Cos(DegToRad(0.0));
              Acc2 := Cos(DegToRad(15.0));
            end;
  end;
  Restricted := false;
  Detected   := 0;
  Intensity  := 0.0;
end;

function TAnalyzer.Detect(var Electron: TAugerElectron): Integer;
const
  Cos_HoeslerAngle = 0.173648;
var
  tmp: float;
  AX, P: TVector3;
begin
  tmp := DotProduct(Electron.Ray.Dir, Axis);
  if (tmp <= Acc1) and (tmp >= Acc2) then
  begin

    { The following code implements the Hösler aperture: Electrons can reach
      the analyzer only if the emission angle with respect to the surface
      normal is > 80 degrees. }
    if UseHoeslerAp and (VecAngle(Electron.Ray.Dir, SimParams.zAxis) < DegToRad(80.0)) then
      Exit;

    { The following code is executed if the analyzer acceptance is restricted
      azimutally. }

    if Restricted then
    begin
      with Axis do VecAssign(AX, X,Y,Z);
      Normalize(Electron.Ray.Dir);
      VecMulSc(AX, DotProduct(Electron.Ray.Dir, AX));
        { component of the beam projected onto the axis }
      VecSub(Electron.Ray.Dir, AX, P);
      Normalize(P);
      { P is the projection of the electron beam onto the plane normal to the
        analyzer axis. }

      VecAssign(AX, Axis.Z, 0.0, -Axis.X);
      Normalize(AX);
      tmp := VecAngle(AX, P);    { Angle between projection and x plane }
      if (tmp < SectorFrom) or (tmp > SectorTo) then
        Exit;
    end;

    inc(Detected);
    Intensity := Intensity + Electron.Weight;
    if SaveProc <> nil then  TSaveDetElProc(SaveProc)(Electron);
    if DetectProc <> nil then TDetectMsgProc(DetectProc)(Detected, Electron);
  end;

  Result := Detected;
end;

{ ASectorFrom, ASectorTo describe the aszimuthal acceptance range (in degrees).
  See CMA aperture. If both values are zero, the full circle from 0 to 2 pi
  is accepted.
  Reference plane is defined by x axis and analyzer axis. }
procedure TAnalyzer.Restrict(ASectorFrom, ASectorTo: Float);
begin
  if ASectorTo < ASectorFrom then
    SwapFloat(ASectorFrom, ASectorTo);
  if Zero(ASectorTo-ASectorFrom, FloatEps) or Equal(Abs(ASectorTo-ASectorFrom), 360.0, FloatEps) then
  begin
    ASectorfrom := 0.0;
    ASectorTo   := 0.0;
    Restricted  := false;
  end else
    Restricted  := true;
  SectorFrom := DegToRad(ASectorFrom);
  SectorTo   := DegToRad(ASectorTo);
end;


(****************************************************************************)
(*                             TMaterialParams                              *)
(* ------------------------------------------------------------------------ *)
(* Material parameters for the simulation. Are stored in the MaterialsList  *)
(****************************************************************************)

constructor TMaterialParams.Create(theZ, theMolecMass, theMassDensity,
  theCoreLevel, theAugerEnergy: float);
begin
  inherited Create;
  Z := theZ;
  A := theMolecMass;
  MassDensity := theMassDensity;
  CoreLevel := theCoreLevel;
  AugerEnergy := theAugerEnergy;
end;


(****************************************************************************)
(*                           TMaterialsList                                 *)
(* ------------------------------------------------------------------------ *)
(* a simple database with material parameters needed by the simulation.     *)
(* SearchZ and SearchName allow to find a material with the given Z or name.*)
(****************************************************************************)

function TMaterialsList.SearchZ(Z: float): TMaterialParams;
var
  i: Integer;
  mp: TMaterialParams;
begin
  for i := 0 to Count-1 do
  begin
    mp := TMaterialParams(Items[i]);
    if Equal(mp.Z, Z, 1E-3) then
    begin
      Result := mp;
      Exit;
    end;
  end;
  Result := nil;
end;

function TMaterialsList.SearchName(Name: String): TMaterialParams;
const
  NO_SUBSCRIPTS = false;
var
  i: Integer;
  mp: TMaterialParams;
begin
  for i := 0 to Count-1 do
  begin
    mp := TMaterialParams(Items[i]);
    if GetElementName(mp.Z, NO_SUBSCRIPTS) = Name then
    begin
      Result := mp;
      Exit;
    end;
  end;
  Result := nil;
end;


(****************************************************************************)
(*                        NewMaterial, NewMaterialsList                     *)
(* ------------------------------------------------------------------------ *)
(* These function create a simple database with material parameters for     *)
(* the simulation.                                                          *)
(****************************************************************************)

function NewMaterial(theZ, theMolecMass, theMassDensity, theCoreLevel,
  theAugerEnergy: float): TMaterialParams;
begin
  Result := TMaterialParams.Create(theZ, theMolecMass, theMassDensity,
    theCoreLevel, theAugerEnergy);
end;

function NewMaterialsList: TMaterialsList;
begin
  Result := TMaterialsList.Create(true); //New(PMaterialCollection, Init(50,10));

  with Result do
  begin
    { 1st value = Z,
      2nd value = atomic/molecular mass
      3rd value = density (g/cm^3)
      4th value = core energy (keV)
      5th value = Auger energy (eV) }
    Add(NewMaterial(13.0,  26.0, 2.70, 1.487, 1620));  { Al }
    Add(NewMaterial(14.0,  28.0, 2.33, 1.839, 1617));  { Si }
    Add(NewMaterial(10.1,  60.0, 2.19, 1.839, 1609));  { SiO2 }
    Add(NewMaterial(28.0,  55.8, 7.86, 0.707,  702));  { Fe }
    Add(NewMaterial(79.0, 197.0, 19.3, 2.291, 2016));  { Au }   { Eb correct ? }
    Add(NewMaterial(29.0,  63.5, 8.93, 0.932,  918));  { Cu }
    Add(NewMaterial(28.0,  58.7,  8.9, 0.853,  846));  { Ni }
    Add(NewMaterial(78.0, 195.1, 21.5, 2.202, 1961));  { Pt }   { Eb correct ? }
    Add(NewMaterial(47.0, 107.9, 10.5, 0.368,  358));  { Ag }
    Add(NewMaterial(24.0,  52.0,  6.9, 0.574,  527));  { Cr }
    Add(NewMaterial(42.0,  95.9, 10.2, 2.867, 2038));  { Mo }   { Eb correct ? }

  { Complete energies here !!!
    Add(NewMaterial($$$$,  $$$$, $$$$, 1.839, 1612));  { Si3N4 }
    Add(NewMaterial($$$$,  $$$$, $$$$, 1.487, 1388));  { Al2O3 }
    Add(NewMaterial(12.0,  24.3, 1.74, $$$$$, 1186));  { Mg }
  }

  end;
end;


(****************************************************************************)
(*                                 TMaterial                                *)
(* ------------------------------------------------------------------------ *)
(* contains the required parameters for the sample materials and provides   *)
(* functions for the Monte-Carlo calculation.                               *)
(****************************************************************************)

constructor TMaterial.Create(theZ, theMolecMass, theMassDensity, theCoreLevel,
  theAugerEnergy: float);
begin
  Z := theZ;
  A := theMolecMass;
  Eb := theCoreLevel;
  MassDensity := theMassDensity;
  AugerEnergy := theAugerEnergy;
  InitParams;
end;

{ Calculates the ionization cross section for the exicitation energy E and the
  binding energy Eb, according to Gryzinski.
  Pre-factors are omitted.
  Eq. (11) in Ze-jun et al. SIA, 10, 253 (1987) }
function TMaterial.CalcAugerCrossSection(E: float): float;
begin
  if E < Eb then
    Result := 0.0
  else
    Result :=
      1.0/(Eb*E) * Power((E-Eb)/(E+Eb), 1.5) *
      (1.0 + TwoThirds * (1.0-Eb/(E+E)) * Ln(2.7 + Sqrt(E/Eb-1.0)) );
end;

{ Calculates the escape depth (in µm) for an Auger electron having energy E
  (according to Seah&Dench. }
function TMaterial.CalcAugerEscapeDepth(E: float): float;
var
  n: float;
begin
  n := NumDensity * 1E-21;   { Atoms per nm^3 }
  Result := 538E-3 * Power(n,-OneThird)/(E*E) + 0.41E-3 * Sqrt(E/n);
  { Divide pre-factors by 1000, because result must be in µm. }
end;

procedure TMaterial.RutherfordScattParams(E: float; var Sigma, Alpha: Float);
{ Calculates the Rutherford cross-section <sigma> in cm², as well as the
  screening factor <alpha>, for energe E (in keV).
  See: Eli Napchan, p10 }
begin
  Alpha := ScreeningParam/E;
  Sigma := RutherfordParam * Sqr((E+511)/((E+1022)*E)) / (Alpha*(1.0+Alpha));
end;

{ Calculates the loss in energy, dE, of an electron at energy E (keV) after
  passing a distance S (in µm): Uses modified Bethe equation.
  See Eli Napchan, p9 }
function TMaterial.CalcStoppingPower(E: float): float;
begin
  Result := StoppingPowParam / E * Ln(1.166*E/J+1.0);
end;

procedure TMaterial.InitParams;
begin
  NumDensity := (Avogadro * MassDensity * GetAtomsPerMolecule(Z)) / A;
  ElemDensity := NumDensity * GetConcentration(Z);
  ScreeningParam := 3.4E-3 * Power(Z, TwoThirds);
  RutherfordParam := 5.21E-21 * FourPi * Z * Z;
  if Z > 13 then
    J := 9.76E-3*Z + 58.5E-3 * Power(Z,-0.19)
  else
    J := 11.5E-3 * Z;   { * 1E-3 for energy in keV }
  StoppingPowParam := -7.85 * MassDensity * Z * GetAtomsPerMolecule(Z) / A;
    { Dropped 1E4 for conversion from cm to µm! }
  EscapeDepth := CalcAugerEscapeDepth(AugerEnergy);
end;


(****************************************************************************)
(*                                 TSample                                  *)
(* ------------------------------------------------------------------------ *)
(* The ancestor of the sample. Traces the path of a primary electron by     *)
(* means of a Monte-Carlo simulation. Describes the emission of Auger       *)
(* electrons.                                                               *)
(* Must be overridden to define the geometry of th esurface.                *)
(* When the parameter <theTraceFName> is specifed as non-empty in the       *)
(* contructor the trajectories are saved in this file (Save).               *)
(* If <OnlyDirect> is true, no trajectories are calculated for              *)
(* (back-) scattered electrons.                                             *)
(****************************************************************************)

constructor TSample.Create(ASubstrate, ALayer: TMaterial;
  APrimaryEnergy: Float; ALayerThickness: float; ATraceFile: String);
const
  nMax = 2000;
begin
  FLayer := ALayer;
  FSubstrate := ASubstrate;
  FPrimaryEnergy := APrimaryEnergy;

  if (FLayer = nil) or (FSubstrate = nil) then
    raise Exception.Create('[TSample.Create] Materials have not been initialized.');

  zInterface := -abs(ALayerThickness);
  TraceFile  := ATraceFile;

  if Zero(zInterface, SingleEps) then
    ChangeMaterial(FSubstrate)
  else
    ChangeMaterial(FLayer);

  with FSubstrate do
    IntensFact := 1.0 / (ElemDensity * EscapeDepth * CalcAugerCrossSection(FPrimaryEnergy));

  { Every detected Auger electrons adds the amount
           Intensfact * AugerCrossSctn * EscapeDepth
    to the total intensity. This value becomes a bit more handy by multiplication
    with the factor <IntensFact> (Normalization to the ionization cross-section
    at the primary energy, substrate density and escape depth of the substrate. }

  if TraceFile = '' then
    Trajectory := nil
  else
    Trajectory := TMatrix.Create(nMax, 4);
  if mError = mOutOfMemory then
    etError := etOutOfMemory
end;

destructor TSample.Destroy;
begin
  ClearMat(Trajectory);
  inherited;
end;

function TSample.CancelTrace: Boolean;
var
  cancelFunc: TCancelTraceFunc;
begin
  Result := false;
  if CancelTraceFunc <> nil then
  begin
    cancelFunc := TCancelTraceFunc(CancelTraceFunc);
    Result := cancelFunc();
  end;
end;

procedure TSample.ChangeMaterial(NewMaterial: TMaterial);
var
  sigma, alpha: float;
begin
  if NewMaterial <> Material then
  begin
    Material := NewMaterial;
    Material.RutherfordScattParams(FPrimaryEnergy, sigma, alpha);
    MaxStepLen := -1E4 / (Material.NumDensity * sigma) * Ln(0.001);
      { 1E4 to account for conversion from cm (sigma, NumDensity) to µm }
  end;
end;

procedure TSample.DrawSample(Projection: TProjection);
begin
end;

{ A primary electron arriving with energy <E> at point <Point> of the surface
  creates an Auger electron <Electron>.
  The emission direction is uniformly distributed.
  The contribution of the emitted Auger electron to the overall intensity in the
  detector (<Electron.Weight>) depends on
  - the ionization cross-section (calculated by Material.CalcAugerCrossSection)
  - the escape depth because sub-surface emission is neglected.
  The field <GenByPrimEl> is true if the Auger electron has been created directly
  by a primary electron, or by a backscattered electron.
  The function result is false if the electron does not leave the sample. }
function TSample.EmitAugerEl(Point: TVector3; E: float;
  var Electron: TAugerElectron): boolean;
var
  theta : float;
  phi   : float;
  RotAx : TVector3;
  Normal: TVector3;
begin
  EmitAugerEl := false;
  if OnSurface(Point) then
  begin
    // Create a random vector uniformly distributed over the half-sphere
    phi := Random * TwoPi;
    repeat
      theta := Random*Pi_2;
    until Random <= sin(theta);
    // Emission direction of the electron
    with Electron.Ray.Dir do
      SphToCart(theta, phi, X, Y, Z);
    SurfNormal(Point, Normal);
    // Emission point of the electron
    Electron.Ray.Point := Point;
    // Contribution of electron to overall detected intensity
    Electron.Weight :=
      IntensFact *
      Material.CalcAugerCrossSection(E) *
      Material.ElemDensity *
      Material.EscapeDepth *
      DotProduct(Electron.Ray.Dir, Normal);
    EmitAugerEl := (Electron.Weight > 0.0)      { for example if E < CoreLevel }
      and (not Intersection(Electron.Ray, Point, false));
  end;
end;

{ Returns true if the beam <ray> intersects the sample surface. Returns in
  <Point> the coordinates of the intersection point.
  By means of the parameter <FromOutside> it can be distinguished whether the
  ray hits the surface from outside or inside the surface.
  Must be overridden. }
function TSample.Intersection(Ray: TRay; var Point: TVector3;
  FromOutside: boolean): boolean;
begin
  //
end;

{ An electron flying in direction <Electron.Ray.Dir> at energy <E> is scattered
  at the location <Electron.Ray.Point>.
  The procedure returns in <Electron.Ray.Dir> the new direction, and in <E> the
  new (smaller) energy after the scattering process.
  Scattering is simulated by the "single scattering model" (see Eli Napchan,
  or Joy (Scanning Microscopy 3 (1991 329).
  NOTE: The direction must be a unit vector. }
procedure TSample.Scatter(var Electron: TElectron; var E: float);
var
  alpha     : float = 0.0;   { screening factor }
  sigma     : float = 0.0;   { scattering cross-section }
  r         : float;         { random number }
  RotAx     : TVector3;
  Dir       : TVector3;
  Point     : TVector3;
  Cos_theta : float;         { polar scattering angle }
  Sin_theta : float;
  phi       : float;         { azimuthal scattering angle }
  Cos_phi   : float;
  Sin_phi   : float;
  psi       : float;         { angle between z axis and old direction }
  step      : float;         { scattering distance (in µm) }
begin
  Material.RutherfordScattParams(E, sigma, alpha);

  { *** 1 - Polar scattering angle **** }
  r := Random;
  Cos_theta := 1.0 - 2*alpha*r/(1.0 + alpha - r);
  Sin_theta := Sqrt(1.0 - Cos_theta*Cos_theta);

  { *** 2 - Azimuthal scattering angle **** }
  phi := TwoPi*Random;
  Cos_phi := Cos(phi);
  Sin_phi := Sin(phi);

  { *** 3 - Path length **** }
  step := -1E4 / (Material.NumDensity*sigma) * Ln(Random);
    { 1E4 to take care of conversion from cm (sigma, NumDensity) to µm }

  { *** 4 - Create the scattered direction vector ***
    At first, create direction vector in a coordinate system in which the
    direction is oriented along the z axis. Then rotate this vector around the
    angle enclosed between the sample z axis and the old direction vector.
    The rotation axis is perpendicular to z and the old direction vector.
    ( --> RotAx = (-Dir.Y, Dir.X, 0)  }

  { Dir = scattered vector in the electron's coordinate system }
  SphToCart(Cos_theta, Sin_theta, Cos_phi, Sin_phi, Dir.X, Dir.Y, Dir.Z);

  { Angle between old direction and z axis }
  psi := VecAngle(Electron.Ray.Dir, SimParams.zAxis);

  if Equal(psi, Pi, 1E-5) then     { old direction is anti-parallel to z }
    VecMulSc(Dir, -1.0)
  else
  if not Zero(psi, 1E-5) then      { old direction is arbitrary }
  begin
    with Electron.Ray.Dir do VecAssign(RotAx, -Y, X, 0.0);
    Rotate(Dir, RotAx, psi);
  end;
  Electron.Ray.Dir := Dir;
  VecMulSc(Dir, Step);
  Point := Electron.Ray.Point;
  VecAdd(Point, Dir, Electron.Ray.Point);

  { *** 5 - Final energy: Energy loss = path length * stopping power **** }

  E := E + step * Material.CalcStoppingPower(E);  { Note stopping power is < 0 }
end;

{ Saves the trajectory and the energy of the electron inside the sample to
  matrix <Trajectory> which later can be written to disk in the MatLab format. }
procedure TSample.Save(Point: TVector3; E: float; n: Integer);
begin
  if Trajectory <> NIL then
  begin
    Trajectory.PutValue(n,1, Point.X);
    Trajectory.Putvalue(n,2, Point.Y);
    Trajectory.Putvalue(n,3, Point.Z);
    Trajectory.Putvalue(n,4, E);
  end;
end;

{ Traces a primary electron which enters the sample with energy <E> at point
  <Electron.Ray.Point> and in direction <Electron.Ray.Dir>.
  The path is traced by means of the Monte-Carlo routine Scatter() until the
  electron's energy has dropped below the minimum energy <EMin>, or until the
  electron has left the sample.
  If the electron has left the sample the function result becomes true, and the
  field <Electron.Ray> gets the exit point and the exit direction, and <E> gets
  the energy at exit.
  If the trajectory intersects the surface an Auger electron is "emitted" by
  calling the method EmitAugerEl(). If the Auger electron leaves the sample
  (i.e., the result of EmitAugerEl() is true), then it is checked by calling the
  function Analyzer.Detect whether the electon enters the analyzer.
  The coordinates along the path as well as the energy are added as columns to
  the Matlab file <TraceFile> (which already must be opened)
  <Name> is the name of the MatLab variable in the file. }
function TSample.Trace(Name: String; var Electron: TElectron; var E: float;
  Emin: float): Boolean;
const
  FROM_INSIDE = false;
var
  AugerEl    : TAugerElectron;
  P          : TVector3;
  Ray        : TRay;
  Finished   : boolean;
  Matrix     : TMatrix;
  n          : Integer = 0;
  nMax       : Integer = 0;
begin
  finished := false;
  if etError = etOK then
    with Electron.Ray do
    begin
      if Trajectory <> nil then
        Trajectory.Size(nMax, n)
      else
        nMax := MaxInt;
      n := 0;
      repeat
        inc(n);
        Save(Point, E, n);
        if Outside(Point) then
        begin
          Ray := Electron.Ray;       // the electron has left the sample
          VecMulSc(Ray.Dir, -1.0);   // Determine the point of emission
          if Intersection(Ray, P, FROM_INSIDE) then Point := P;   {<<< was: TRUE >>> }
          Finished := true;
        end;

        if GreaterThan(Point.Z, zInterface, FloatEps) then
          ChangeMaterial(Layer)
        else
        if LessThan(Point.Z, zInterface, FloatEps) or Equal(Point.Z, zInterface, FloatEps) then
          ChangeMaterial(Substrate);

        AugerEl.GenByBkScEl := LessThan(E, ElectronSource.Energy, FloatEps);
        if EmitAugerEl(Point, E, AugerEl) then
          Analyzer.Detect(AugerEl);
        if not OnlyDirect then
          Scatter(Electron, E);
        if (E < EMin) or CancelTrace() then
          Finished := true;
      until Finished or (etError <> etOK) or (n > nMax) or OnlyDirect;

      if Trajectory <> nil then
      begin
        Matrix := TMatrix.SubMatrixOf(Trajectory, 1,1,n,4);
        MatlabFile_Append(TraceFile, Name, Matrix);
        if TrajProc <> nil then
          TTrajectoryProc(TrajProc)(Matrix);
        ClearMat(Matrix);
      end;
    end;
  Result := (E >= Emin) and (not OnlyDirect);
  { Only when electron has exited the sample E is greater than Emin }
end;


(****************************************************************************)
(*                               TContactHole                               *)
(* ------------------------------------------------------------------------ *)
(* Implements the description of a contact hole having radius <Radius> and  *)
(* depth <Depth>.                                                           *)
(* The z axis point along the surfe normal away from the sample.            *)
(* The origin is at the top plane of the surface, i.e. point on the contact *)
(* hole bottom have negative z coordinates.                                 *)
(* If LayerThickness < 0, the LayerThickness is assumed to be equal to the  *)
(* contact hole depth.                                                      *)
(****************************************************************************)

constructor TContactHole.Create(ASubstrate, ALayer: TMaterial;
  APrimaryEnergy, ALayerThickness, ARadius, ADepth: float;
  ATraceFile: String);
begin
  if LessThan(ALayerThickness, 0.0, SingleEps) then
    ALayerThickness := ADepth;

  inherited Create(ASubstrate, ALayer, APrimaryEnergy, ALayerThickness, ATraceFile);

  Radius := ARadius;
  Depth  := -Abs(ADepth);    // negative z coordinate at the bottom
end;

procedure TContactHole.DrawSample(Projection: TProjection);
begin
  // must be redone for LCL
end;
(*
var
  vp : ViewPortType;
  x,y,rx,ry : INTEGER;
BEGIN
  IF GetGraphMode>=0 THEN BEGIN
    GetViewSettings(vp);
    GRAPH.SetColor(White);
    GRAPH.SetLineStyle(SolidLn, 0, ThickWidth);
    x := xPix(0.0);
    y := yPix(0.0);
    rx := xPix(Radius)-x;
    ry := y-yPix(radius);
    CASE Projection OF
      XYproj :
        BEGIN
          GRAPH.Ellipse(x,y, 0, 360, rx, ry);
        END;
      XZproj,
      YZproj :
        BEGIN
          Line(0, y, vp.x2-vp.x1, y);
          Line(x-rx, y, x-rx, yPix(Depth));
          Line(x-rx, yPix(Depth), x+rx, yPix(Depth));
          Line(x+rx, yPix(Depth), x+rx, y);
        END;
    END;
  END;
END;
*)

function TContactHole.InHole(Point: TVector3): Boolean;
begin
  with Point do
    InHole := ValidVector(Point) and (X*X + Y*Y - Radius*Radius < 0.0);
end;

function TContactHole.Intersection(Ray: TRay; var Point: TVector3;
  FromOutside: Boolean): Boolean;
var
  Plane: TRay;
  R2, dc, dt, db: float;
begin
  R2 := sqr(Radius);

  // Intersection with cylinder
  dc := rayXcyl(ray, Radius, Point, FromOutside);
  if (dc <> mEmpty) and Between(Point.Z, Depth, 0.0, FloatEps) then
  begin
    Result := true;
    exit;
  end;

  // Intersection with top plane (z = 0)
  Plane.Dir := SimParams.zAxis;
  VecAssign(Plane.Point, 0.0, 0.0, 0.0);
  dt := rayXplane(ray, Plane, Point);
  if (dt <> mEmpty) and GreaterThan(sqr(Point.X) + sqr(Point.Y), R2, FloatEps) then
  begin
    Result := true;
    exit;
  end;

  // Intersection with bottom plan (z = Depth)
  Plane.Point.Z := Depth;
  db := rayXplane(ray, Plane, Point);
  if (db <> mEmpty) and LessThan(sqr(Point.X) + sqr(Point.Y), R2, FloatEps) then
  begin
    Result := true;
    exit;
  end;

  Result := false;
  VecAssign(Point, mEmpty, mEmpty, mEmpty);
         (*
  R2 := Sqr(radius);
  dc := rayXcyl(ray, Radius, Point, FromOutside);         { Cylinder }
  if (dc <> mEmpty) and (not Between(Point.Z, Depth, 0.0, FloatEps)) then
    dc := mEmpty;

  Plane.Dir := zAxis;                                     { upper-most plane }
  VecAssign(Plane.Point, 0.0, 0.0, 0.0);
  dt := rayXplane(ray, Plane, Point);
  if (dt <> mEmpty) and LessThan(Sqr(Point.X) + Sqr(Point.Y), R2, FloatEps) then
    dt := mEmpty;

  Plane.Point.Z := Depth;                                 { lower plane }
  db := rayXplane(ray, Plane, Point);
  if (db <> mEmpty) and GreaterThan(Sqr(Point.X) + Sqr(Point.Y), R2, FloatEps) then
    db := mEmpty;

  dc := MinF(dc, MinF(dt, db));
  if dc <> mEmpty then
  begin
    VecMulSc(Ray.Dir, dc);
    VecAdd(Ray.Point, Ray.Dir, Point);
    Result := true;
  end else
  begin
    VecAssign(Point, mEmpty, mEmpty, mEmpty);
    Result := false;
  end;
  *)
end;

function TContactHole.OnSurface(Point: TVector3): Boolean;
var
  diff: float;
begin
  diff := Sqr(Point.X) + Sqr(Point.Y) - Sqr(Radius);
  if Zero(diff, FloatEps) then                    { at the contact hole wall }
    Result := Between(Point.Z, Depth,0.0, FloatEps)
  else
  if diff > 0.0 then                              { at the top surface}
    Result := Zero(Point.Z, FloatEps)
  else                                            { at the contact hole bottom }
    Result := Equal(Point.Z, Depth, FloatEps);
end;

{ Returns true when the point <Point> is outside the sample. }
function TContactHole.Outside(Point: TVector3): boolean;
begin
  if InHole(Point) then
    Result := GreaterThan(Point.Z, Depth, FloatEps)
  else
    Result := GreaterThan(Point.Z, 0.0, FloatEps);
end;

{ calculates the surface normal vector at the specified point. }
procedure TContactHole.SurfNormal(Point: TVector3; var Normal: TVector3);
begin
  Normal.X := mEmpty;
  Normal.Y := mEmpty;
  Normal.Z := mEmpty;
  if Equal(Point.Z, 0.0, FloatEps) and (not InHole(Point)) then
  begin
    Normal.X := 0.0;
    Normal.Y := 0.0;
    Normal.Z := 1.0;
  end else
  if Equal(Point.Z, Depth, FloatEps) and InHole(Point) then
  begin
    Normal.X := 0.0;
    Normal.Y := 0.0;
    Normal.Z := 1.0;
  end else
  if Equal(Point.X*Point.X + Point.Y*Point.Y, Radius*Radius, FloatEps) then
  begin
    Normal.X := -Point.X;
    Normal.Y := -Point.Y;
    Normal.Z := 0.0;
  end;
end;


(****************************************************************************)
(*                               TStripe                                    *)
(* ------------------------------------------------------------------------ *)
(* Implements the description of a stripe (conductor line) having height    *)
(* <Height> and width <Width>.                                              *)
(* The stripe runs along the y axis.                                        *)
(* The z axis points along the sample normal away from the sample.          *)
(* The origin is at the top plane of the sample, i.e. substrate points      *)
(* have negative z coordinates.                                             *)
(* If LayerThickness < 0, the layer thickness is assumed to be equal to the *)
(* stripe height.                                                           *)
(****************************************************************************)

constructor TStripe.Create(ASubstrate, ALayer: TMaterial;
  APrimaryEnergy, ALayerThickness, AWidth, AHeight: float;
  ATraceFile: String);
begin
  if LessThan(ALayerThickness, 0.0, FloatEps) then
    ALayerThickness := AHeight;

  inherited Create(ASubstrate, ALayer, APrimaryEnergy, ALayerThickness, ATraceFile);

  Width := AWidth;
  Height := -abs(AHeight);    // negative z coordinates at the bottom
end;

procedure TStripe.DrawSample(Projection: TProjection);
begin
  // To be implemented for LCL
end;
(*
var
  vp    : ViewportType;
  x1,x2 : Integer;
  y1,y2 : Integer;
BEGIN
  IF GetGraphMode>=0 THEN BEGIN
    GetViewSettings(vp);
    SetLineStyle(SolidLn, 0, ThickWidth);
    SetColor(white);
    x1 := xPix(-Width*0.5);
    x2 := xPix(Width*0.5);
    y1 := yPix(0.0);
    y2 := yPix(Height);
    CASE Projection OF
      XYproj :
        BEGIN
          Line(x1, 0, x1, vp.y2-vp.y1);
          Line(x2, 0, x2, vp.y2-vp.y1);
        END;
      XZproj :
        BEGIN
          Line(0, y2, vp.x2-vp.x1, y2);
          Line(x1,y2, x1, y1);
          Line(x1,y1, x2, y1);
          Line(x2,y1, x2, y2);
        END;
      YZproj :
        BEGIN
          Line(0,y1, vp.x2-vp.x1, y1);
          Line(0,y2, vp.x2-vp.x1, y2);
        END;
    END;
  END;
END;
*)

{---------------------------------------------------------------------------}

function TStripe.Intersection(Ray: TRay; var Point: TVector3;
  FromOutside: Boolean): Boolean;
var
  Plane : TRay;
  W     : float;
  dt,db : float;
  DL,dr : float;
begin
  W  := Width*0.5;

  VecAssign(Plane.Point, 0.0, 0.0, 0.0);    { Intersection at the top }
  Plane.Dir := SimParams.zAxis;
  dt := rayXplane(ray, Plane, Point);
  if (dt <> mEmpty) and (not Zero(Point.X, W)) then
    dt := mEmpty;

  Plane.Point.Z := Height;                  { Intersection at the bottom }
  db := rayXplane(ray, Plane, Point);
  if (db <> mEmpty) and Zero(Point.X, W) then
    db := mEmpty;

  VecAssign(Plane.Point, W, 0.0, 0.0);      { right sidewall }
  VecAssign(Plane.Dir, 1.0, 0.0, 0.0);
  dr := rayXplane(ray, Plane, Point);
  if (dr <> mEmpty) and (not Between(Point.Z, Height, 0.0, floateps)) then
    dr := mEmpty;

  Plane.Point.X := -W;                      { left sidewall }
  Plane.Dir.X   := -1.0;
  dl := rayXplane(ray, Plane, Point);
  if (dl <> mEmpty) and (not Between(Point.Z, Height, 0.0, floateps)) then
    dl := mEmpty;

  dt := MinF(dt, MinF(db, MinF(dr,DL)));
  if dt <> mEmpty then
  begin
    VecMulSc(Ray.Dir, dt);
    VecAdd(Ray.Point, Ray.Dir, Point);
    Result := true;
  end else
  begin
    VecAssign(Point, mEmpty, mEmpty, mEmpty);
    Result := false
  end;
end;

function TStripe.OnSurface(Point: TVector3): boolean;
var
  W: float;
begin
  W := Width*0.5;
  Result :=
    (Zero(Point.Z, FloatEps) and Zero(Point.X, W))             { Upper surface }
    or
    (Equal(Point.Z, Height, FloatEps) and (not Zero(Point.X, W))) { Substrate}
    or
    (Equal(Abs(Point.X), W, FloatEps) and Between(Point.Z, Height, 0.0, floateps)); {Side walls }
end;

{ Return true if the point <Point> is outside the sample. }
function TStripe.Outside(Point: TVector3): Boolean;
var
  W: float;
begin
  W := Width  * 0.5;
  if Zero(Point.X, W + FloatEps) then
    Result := GreaterThan(Point.Z, 0.0, FloatEps)
  else
    result := GreaterThan(Point.Z, Height, FloatEps);
end;

procedure TStripe.SurfNormal(Point: TVector3; var Normal: TVector3);
var
  W : float;
begin
  W := Width * 0.5;
  if Equal(Abs(Point.X), W, FloatEps) then
    VecAssign(Normal, sgn(Point.X), 0.0, 0.0)
  else
    VecAssign(Normal, 0.0, 0.0, 1.0);
end;


(****************************************************************************)
(*                                TStep                                     *)
(* ------------------------------------------------------------------------ *)
(* implements the description of a step of height <Height>.                 *)
(* Depending on <Dir> the step can go up or go down towards the right.      *)
(*                       ------              --------                       *)
(*            Dir=sdUp   |*****              ****** | Dir=sdDown            *)
(*                       |*****              ****** |                --> x  *)
(*               ---------*****              ****** --------                *)
(*               **************              ***************                *)
(*                                                                          *)
(* The edge runs along the y axis.                                          *)
(* The z axis always points along the sample surface normal away from the   *)
(* sample.                                                                  *)
(* The origin is in the upper plane.                                        *)
(* If LayerThickness < 0, it is assumed that the layer thickness is equal   *)
(* to the step height.                                                      *)
(****************************************************************************)

constructor TStep.Create(ASubstrate, ALayer: TMaterial;
  APrimaryEnergy, ALayerthickness, AHeight: Float; ADir: TStepDir;
  ATraceFile: String);
begin
  if LessThan(ALayerThickness, 0.0, FloatEps) then
    ALayerThickness := AHeight;
  inherited Create(ASubstrate, ALayer, APrimaryEnergy, ALayerThickness, ATraceFile);
  Dir := ADir;
  Height:= -abs(AHeight);     // negative z coordinates at the bottom!
END;

procedure TStep.DrawSample(Projection:TProjection);
begin
  { Must be implemented for LCL }
end;
{
VAR
  vp : ViewportType;
  x,y1,y2 : INTEGER;
BEGIN
  IF GetGraphMode>=0 THEN BEGIN
    GetViewSettings(vp);
    SetLineStyle(SolidLn, 0, ThickWidth);
    SetColor(White);
    y1 := yPix(0.0);
    y2 := yPix(Height);
    x  := xPix(0.0);
    CASE Projection OF
      XYproj : Line(x, 0, x, vp.y2-vp.y1);
      XZproj :
        BEGIN
          Line(0, y2, vp.x2-vp.x1, y2);
          IF Dir=sdUP THEN BEGIN
            Line(x, y1, x, y2);
            Line(x, y1, vp.x2-vp.x1, y1);
          END ELSE BEGIN
            Line(0, y1, x, y1);
            Line(x, y2, x, y1);
          END;
        END;
      YZproj :
        BEGIN
          Line(0, yPix(0.0), vp.x2-vp.x1, yPix(0.0));
          Line(0, yPix(Height), vp.x2-vp.x1, yPix(Height));
        END;
    END;
  END;
END;
}

function TStep.Intersection(Ray: TRay; var Point: TVector3;
  FromOutside: Boolean): Boolean;
var
  Plane    : TRay;
  dt,db,dw : Float;
begin
  VecAssign(Plane.Point, 0.0, 0.0, 0.0);    { Intersection at the top }
  Plane.Dir := SimParams.zAxis;
  dt := rayXplane(ray, Plane, Point);
  if (dt <> mEmpty) then
    case Dir of
      sdUp   : if not GreaterThan(Point.X, 0.0, FloatEps) then dt := mEmpty;
      sdDown : if not LessThan(Point.X, 0.0, FloatEps) then dt := mEmpty;
    end;

  Plane.Point.Z := Height;                  { Intersection at bottom }
  db := rayXplane(ray, Plane, Point);
  if (db <> mEmpty) then
    case Dir of
      sdUp   : if not LessThan(Point.X, 0.0, FloatEps) then db := mEmpty;
      sdDown : if not GreaterThan(Point.X, 0.0, FloatEps) then db := mEmpty;
    end;

  VecAssign(Plane.Point, 0.0, 0.0, 0.0);    { edge }
  SurfNormal(Plane.Point, Plane.Dir);
  dw := rayXplane(ray, Plane, Point);
  if (dw <> mEmpty) and (not Between(Point.Z, Height,0.0, Floateps)) then
    dw := mEmpty;

  dt := MinF(dt, MinF(db, dw));
  if dt <> mEmpty then
  begin
    VecMulSc(Ray.Dir, dt);
    VecAdd(Ray.Point, Ray.Dir, Point);
    Result := true;
  end else
  begin
    VecAssign(Point, mEmpty, mEmpty, mEmpty);
    Result := false;
  end;
end;

function TStep.OnSurface(Point: TVector3): Boolean;
begin
  if Zero(Point.X, FloatEps) then                     { x at edge }
    Result := Between(Point.Z, Height, 0.0, floateps)
  else
  if GreaterThan(Point.X, 0.0, FloatEps) then         { x at the right of edge }
    case Dir of
      sdUp   : OnSurface := Zero(Point.Z, FloatEps);
      sdDown : OnSurface := Equal(Point.Z, Height, FloatEps);
    end
  else                                                { x at the left of edge }
    case Dir of
      sdUp   : OnSurface := Equal(Point.Z, Height, FloatEps);
      sdDown : OnSurface := Zero(Point.Z, FloatEps);
    end;
end;

function TStep.Outside(Point: TVector3): Boolean;
var
  y: Float;
begin
  if Zero(Point.X, FloatEps) then
    Result := GreaterThan(Point.Z, 0.0, FloatEps)
  else
  if GreaterThan(Point.X, 0.0, FloatEps) then
  begin
    y := FirstIfTrue(Dir=sdUp, 0.0, Height);
    Result := GreaterThan(Point.Z, Y, FloatEps);
  end else
  begin
    y := FirstIfTrue(Dir=sdUp, Height, 0.0);
    Result := GreaterThan(Point.Z, Y, FloatEps);
  end;
end;

procedure TStep.SurfNormal(Point: TVector3; var Normal: TVector3);
begin
  if not Zero(Point.X, FloatEps) then
    VecAssign(Normal, 0.0, 0.0, 1.0)
  else
    case Dir of
      sdUp   : VecAssign(Normal, -1.0, 0.0, 0.0);
      sdDown : VecAssign(Normal,  1.0, 0.0, 0.0);
    end;
end;

end.

