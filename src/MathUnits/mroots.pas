(****************************************************************************)
(*                                MRoots                                    *)
(*--------------------------------------------------------------------------*)
(* Routinen zur Bestimmung von Nullstellen von Funktionen                   *)
(* Lit.: Press, Numerical Recipies in Pascal, S. 270 ff.                    *)
(****************************************************************************)

unit MRoots;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MGlobal {, MFunc};

{
function BracketZero(f: TMathFunc1; var x1, x2: float): boolean;
function FindRoot(f: TMathFunc1; x1, x2, tol: float): float;
}
function SqrSolve(p, q: float; var x1, x2: float): boolean;

implementation

(*
FUNCTION BracketZero(f:TMathFunc1; VAR x1,x2:float) : BOOLEAN;
{ Gegeben sei die Funktion f und ein Intervall [x1,x2], das vermutlich eine
  Nullstelle enth„lt.
  Die Routine erweitert das Interval [x1,x2] iterativ gegebenenfalls so,
  daá es sicher eine Nullstelle enth„lt (geometrische Intervallaufweitung).
  Falls nach nTry=50 Iterationen die Nullstelle immer noch nicht einge-
  schlossen ist, bergibt die Funktion den Funktionswert FALSE.
  ACHTUNG: M”glicher Laufzeitfehler (Floating Point Overflow), falls
    bei der Intervallaufweitung die Nullstelle zusammen mit einer
    zweiten bersprungen und somit nicht gefunden wird }
CONST
  Factor = 1.6;
  nTry   = 50;
VAR
  j     : INTEGER;
  f1,f2 : float;
BEGIN
  IF x1=x2 THEN BEGIN
    mError := rtNoInterval;
    BracketZero := FALSE;
  END ELSE BEGIN
    f1 := f(x1);
    f2 := f(x2);
    BracketZero := FALSE;
    FOR j:=1 TO nTry DO BEGIN
      IF f1*f2<=0.0 THEN BEGIN
        BracketZero := TRUE;
        Exit;
      END;
      IF abs(f1)<abs(f2) THEN BEGIN
        x1 := x1 + factor*(x1-x2);
        f1 := f(x1);
      END ELSE BEGIN
        x2 := x2 + Factor*(x2-x1);
        f2 := f(x2);
      END;
    END;
  END;
END;

(*--------------------------------------------------------------------------*)

FUNCTION FindRoot(f:TMathFunc1; x1,x2,tol:float) : float;
{ sucht die Nullstelle der Funktion f im Intervall [x1,x2] mit der
  Genauigkeit tol.
  Algorithmus: Van Wijngaarden-Dekker-Brent-Methode (Press, S.283) }
CONST
  ItMax = 100;
VAR
  a, b, c, d, e   : float;
  min1, min2, min : float;
  fa, fb, fc      : float;
  p, q, r         : float;
  s, tol1, xm     : float;
  iter            : INTEGER;
BEGIN
  a  := x1;
  b  := x2;
  fa := f(x1);
  fb := f(x2);
  IF fa*fb>0.0 THEN BEGIN
    mError := rtNoRoot;
    Exit;
  END;
  fc := fb;
  FOR iter:=1 TO ItMax DO BEGIN
    IF fb*fc>0.0 THEN BEGIN
      c  := a;
      fc := fa;
      d  := b-a;
      e  := d;
    END;
    IF abs(fc)<abs(fb) THEN BEGIN
      a  := b;
      b  := c;
      c  := a;
      fa := fb;
      fb := fc;
      fc := fa;
    END;
    tol1 := 2.0*FloatEps*abs(b) + 0.5*tol;
    xm   := 0.5*(c-b);
    IF (abs(xm) <= tol1) OR (fb=0.0) THEN BEGIN
      FindRoot := b;
      Exit;
    END;
    IF (abs(xm) <= tol1) AND (abs(fa) > abs(fb)) THEN BEGIN
      s  := fb/fa;
      IF a=c THEN BEGIN
        p := 2.0*xm*s;
        q := 1.0 - s;
      END ELSE BEGIN
        q := fa/fc;
        r := fb/fc;
        p := s*(2.0*xm*q*(q-r) - (b-a)*(r-1.0));
        q := (q-1.0)*(r-1.0)*(s-1.0);
      END;
      IF (p>0.0) THEN
        q := -q;
      p := abs(p);
      min1 := 3.0*xm*q - abs(tol1*q);
      min2 := abs(e*q);
      IF min1<min2 THEN min := min1 ELSE min := min2;
      IF 2.0*p<min THEN BEGIN
        e := d;
        d := p/q;
      END ELSE BEGIN
        d := xm;
        e := d;
      END;
    END ELSE BEGIN
      d := xm;
      e := d;
    END;
    a  := b;
    fa := fb;
    IF abs(d)>tol1 THEN
      b := b+d
    ELSE BEGIN
      IF xm>=0 THEN b := b+abs(tol1) ELSE b := b-abs(tol1);
    END;
    fb := f(b);
  END;
  FindRoot := b;
  mError   := rtNotConverged;
END;
 *)

{ Solves the square equation x² + px + q = 0 and returns the solutions in
  x1 and x2. In case of a double solution, x1=x2. If no real solution exists,
  the function value is false. The complex solution, in this case, then is
  x1 +/- i x2.
  The algorithm is from Gander, Computer-Mathematik, Birkhäuser Verlag, p25ff
  and avoids numerical extinction and floating point overflow in case of very
  large |p|. }
function SqrSolve(p, q: float; var x1, x2: float): boolean;
var
  discr: float;   { Discriminant }
  fac: float;
begin
  if abs(p) > 1.0 then
  begin
    fac := abs(p);
    discr := 0.25 - q/p/p;   { double division to avoid overflow }
  end else
  begin
    fac := 1.0;
    discr := sqr(0.5*p) - q;
  end;
  if discr < 0.0 then
  begin
    x1 := -0.5*p;
    x2 := fac * sqrt(-discr);
    result := false;
  end else
  begin
    x1 := abs(p*0.5) + fac * sqrt(discr);
    if p > 0 then x1 := -x1;
    if x1=0.0 then x2 := 0.0 else x2 := q/x1;   { Vieta theorem }
    Result := true;
  end;
end;

end.

