(****************************************************************************)
(*                               MFunc                                      *)
(* ------------------------------------------------------------------------ *)
(* Unit with important math functions.                                      *)
(* Special function in MSpFunc                                              *)
(****************************************************************************)

unit MFunc;

{$mode ObjFPC}{$H+}

interface

uses
  MGlobal;

function ArcCos(x: Float): Float;
function ArcSin(x: Float): Float;
function Between(x, lo, hi, tol: Float): boolean;
function DegToRad(x: Float): Float;
function Equal(x, y, Tol: Float): boolean;
function FirstIfTrue(P: Boolean; A, B: Float): Float;
function GreaterThan(x, y, tol: Float): Boolean;
function LessThan(x, y, tol: Float): Boolean;
function MaxF(x,y: Float): Float;
function MinF(x, y: Float): Float;
function Power(x, y: float): float;
function RadToDeg(x: Float): Float;
function Sign(x: Float): Float;
function Sgn(x: Float): Float;
procedure SwapFloat(var x, y: Float);
function Zero(X, Tol: Float): boolean;

implementation

function ArcCos(x: Float): Float;          { ArcCos }
var
  y: Float;
begin
  if (x <= 1.0) and (x >= -1.0) then
  begin
    y := Sqrt(1.0 - x*x);
    if abs(x) <= y then
      Result  := Pi_2 - ArcTan(x/y)
    else
      Result := ArcTan(y/x) + FirstIfTrue(x < 0.0, Pi, 0.0);
  end else
  begin
    Result := mEmpty;
    mError := mUndefined;
  end;
end;

function ArcSin(x: Float): Float;          { ArcSin }
var
  y: Float;
begin
  if (x <= 1.0) and (x >= -1.0) then
  begin
    y := Sqrt(1.0 - x*x);
    if abs(X) <= Y then
      Result := ArcTan(x/y)
    else
      Result := sgn(x)*Pi_2 - ArcTan(y/x);
  end else
  begin
    Result := mEmpty;
    mError := mUndefined;
  end;
end;

{ Returns true, if <x> is between the limits <lo> and <hi>. Equality is not
  considered. }
function Between(x, lo, hi, Tol: Float): boolean;
begin
  Result := (x > lo + Tol) and (x < Hi - Tol);
end;

function DegToRad(x: Float): Float;
begin
  Result := X * D2R;
end;

{ Returns true, when the two number X and Y are equal within the specified
  tolerance. Avoids rounding errors. }
function Equal(x, y, Tol: Float): boolean;
begin
  Result := abs(x - y) < Tol;
end;

function FirstIfTrue(P: Boolean; A, B: Float): Float;
begin
  if P then
    Result := A
  else
    Result := B;
end;

function GreaterThan(x, y, tol: Float): Boolean;
begin
  Result := (x > y) and not Equal(x, y, tol);
end;

function LessThan(x, y, tol: Float): Boolean;
begin
  Result := (x < y) and not Equal(x, y, tol);
end;

function MaxF(x,y: Float): Float;
begin
  if x >= y then
    Result := x
  else
    Result := y;
end;

function MinF(x, y: Float): Float;
begin
  if x <= y then
    Result := x
  else
    Result := y;
end;

{ Calculates x^y }
function Power(x, y: float): float;
var
  I: Integer;
begin
  Result := mEmpty;
  if x > 0.0 then
  begin
    if (frac(y) = 0.0) and (abs(y) <= 10.0) then begin   { integer exponent }
      Result := 1.0;
      for I := 1 to round(abs(Y)) do  Result := Result * X;
      if Y < 0.0 then
        Result := 1.0 / Result;
    end else                                          { fractional exponent }
      Result := exp(ln(X)*Y);
  end else
  if X = 0.0 then                                                     { 0^y }
  begin
    if y < 0.0 then
      mError := mUndefined
    else
    if y = 0.0 then
      Result := 1.0
    else
      Result := 0.0;
  end else
  if x < 0.0 then
  begin
    if frac(y*0.5) = 0.0 then
      Result := exp(y*ln(-x))
    else
    if abs(frac(y*0.5)) = 0.5 then
      Result := -exp(y*ln(-x))
    else
    if frac(y) <> 0.0 then
      mError := mUndefined;
  end;
  if mError <> 0 then
    Result := mEmpty;
end;

function RadToDeg(x: Float): Float;
begin
  Result := x * R2D;
end;

function Sgn(x: Float): Float;
begin
  if x = 0 then
    Result := 0.0
  else
    Result := FirstIfTrue(x > 0, 1.0, -1.0);
end;

function Sign(x: Float): Float;
begin
  Result := Sgn(X);
end;

{ Exchanges the values x and y }
procedure SwapFloat(var x, y: Float);
var
  tmp: Float;
begin
  tmp := x;
  x := y;
  y := tmp;
end;

{ Returns true when the number x is zero within the specified tolerance. }
function Zero(X, Tol: Float): Boolean;
begin
  Zero := Equal(X, 0.0, Tol);
end;

end.

