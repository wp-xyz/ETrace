unit et_Math;

{$mode ObjFPC}{$H+}

interface

uses
  Math,
  et_Global;

const
  FloatEps  = 3.4E-9;
  Pi_2      = 1.5707963267948966192313;    // pi/2
  TwoPi     = 6.2831853071795864769253;    // 2*pi
  FourPi    = 12.5663706143591729538506;   // 4*pi
  OneThird  = 1.0/3;                       // 1/3
  TwoThirds = 2.0/3;                       // 2/3

function Between(x, lo, hi, Tol: Float): boolean;
function Equal(x, y, Tol: Float): Boolean;
function GreaterThan(x, y, tol: Float): Boolean;
function LessThan(x, y, tol: Float): Boolean;
function Random_Gauss: Float;
function Random_Cos: Float;
function SqrSolve(p, q: float; var x1, x2: float): boolean;
function SqrSolve(a, b, c: float; var x1, x2: float): boolean;
procedure SwapFloat(var x, y: Float);
function Zero(x, Tol: Float): Boolean;

function Vector3(x, y, z: Float): TVector3;
function EmptyVector3: TVector3;
//function IsEmptyVector3(V: TVector3): Boolean;

//procedure VecAssign(var A: TVector3; x, y, z: float);
function ValidVector(V: TVector3): boolean;
function DotProduct(const A, B: TVector3): float;
function CrossProduct(const A, B: TVector3): TVector3; overload;
function VecLength(const A: TVector3): float;
function VecNormalize(const A: TVector3): TVector3; overload;
function VecNormalize(X, Y, Z: Float): TVector3; overload;
function VecAngle(const A, B: TVector3): float;
function VecRotate(const V, A: TVector3; phi: Float): TVector3;
procedure Rotate(var V: TVector3; A: TVector3; Cos_phi,Sin_phi: Float);
function VecRotateY(const V: TVector3; phi: float): TVector3;
function Distance(const A, B: TVector3): float;

operator + (A, B: TVector3): TVector3;
operator - (A, B: TVector3): TVector3;
operator - (A: TVector3): TVector3;
operator * (A: TVector3; x: Float): TVector3;

function rayXplane(ray: TRay; Plane: TRay; var Point: TVector3): float;
function rayXcyl(ray: TRay; r: float; var Point:TVector3; FarPoint: boolean): Float;

procedure CartToSph(x, y, z: float; var theta, phi: float);
procedure SphToCart(theta, phi: float; var x, y, z: float);
procedure SphToCart(Cos_theta, Sin_theta, Cos_phi, Sin_phi: float; var x, y, z: float);
procedure CartToCyl(x, y: float; var rho, phi: float);
procedure CylToCart(rho, phi: float; var x, y: float);


implementation

{ Returns true, if <x> is between the limits <lo> and <hi>. Equality is not
  considered. }
function Between(x, lo, hi, Tol: Float): boolean;
begin
  Result := (x > lo + Tol) and (x < Hi - Tol);
end;

{ Returns true, when the two number X and Y are equal within the specified
  tolerance. Avoids rounding errors. }
function Equal(x, y, Tol: Float): boolean;
begin
  Result := abs(x - y) < Tol;
end;

function GreaterThan(x, y, tol: Float): Boolean;
begin
  Result := (x > y) and not Equal(x, y, tol);
end;

function LessThan(x, y, tol: Float): Boolean;
begin
  Result := (x < y) and not Equal(x, y, tol);
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

(****************************************************************************)
(*                          Random numbers                                  *)
(****************************************************************************)

const
  PendingGauss: boolean = false;
var
  PrevNumber: float;

{ Returns a random number with normal distribution having mean 0 and standard
  deviation 1.
  From:  Numerical Recipies in Pascal, p225 }
function Random_Gauss: float;
var
  fac, r, v1, v2: float;
begin
  if not PendingGauss then
  begin
    repeat
      v1 := 2.0 * Random - 1.0;
      v2 := 2.0 * Random - 1.0;
      r  := Sqr(v1) + Sqr(v2);
    until (r < 1.0) and (r > 0.0);
    fac := sqrt(-2.0 * ln(r) / r);
    PrevNumber   := v1 * fac;
    Result := v2 * fac;
    PendingGauss := true;
  end else
  begin
    Result := PrevNumber;
    PendingGauss := false;
  end;
end;

{ Creates a cosine-distibuted variable between 0 and pi/2 from a uniformly
  distributes variable.
  See Numerical Recipies, chapter 7.2 }
function Random_Cos: float;
begin
  Result := ArcSin(Random);
end;


(****************************************************************************)
(*                          Solving equations                               *)
(****************************************************************************)

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

{ Returns in x1 and x2 the real solution of the square equation ax² + bx + c = 0.
  If there is no real solution the function value becomes false. In case of
  a double root, x1 and x2 are equal. }
function SqrSolve(a, b, c: float; var x1, x2: float): boolean;
var
  p, q: float;
  ok: Boolean;
begin
  ok := false;
  if (a = 0.0) then
  begin
    if b <> 0.0 then
    begin
      x1 := -c/b;
      x2 := x1;
      ok := true;
    end;
  end else
  begin
    p := b/a;
    q := c/a;
    ok := SqrSolve(p,q, x1,x2);
  end;
  if not ok then
  begin
    x1 := NaN;
    x2 := NaN;
  end;
  Result := ok;
end;


(****************************************************************************)
(*                             Vector math                                  *)
(****************************************************************************)

function Vector3(x, y, z: Float): TVector3;
begin
  Result.x := x;
  Result.Y := y;
  Result.Z := z;
end;

function EmptyVector3: TVector3;
begin
  Result.X := NaN;
  Result.Y := NaN;
  Result.Z := NaN;
end;
  {
function IsEmptyVector3(V: TVector3): Boolean;
begin
  Result := IsNaN(V.X) or IsNaN(V.Y) or IsNaN(V.Z);
end;

procedure VecAssign(var A: TVector3; x, y, z: float);
begin
  A.X := x;
  A.Y := y;
  A.Z := z;
end;
}
{ Checks whether there is any component of the vector V which is mEmpty.
  Such a vector might be created in case of the intersection routines, if
  there is no intersection of the line with the geometric object. }
function ValidVector(V: TVector3): boolean;
begin
  Result := not IsNaN(V.X) and not IsNaN(V.Y) and not IsNaN(V.Z);
end;

{ Calculates the dot product of the vector A and B. }
function DotProduct(const A, B: TVector3): float;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

{ Calculates the cross product of the vector A and B. }
function CrossProduct(const A, B: TVector3): TVector3;
begin
  Result.X := A.Y*B.Z - A.Z*B.Y;
  Result.Y := A.Z*B.X - A.X*B.Z;
  Result.Z := A.X*B.Y - A.Y*B.X;
end;

{ Calculates the length of vector A }
function VecLength(const A: TVector3): float;
begin
  Result := Sqrt(DotProduct(A, A));
end;

{ Normalizes the vector A to length 1 }
function VecNormalize(const A: TVector3): TVector3;
begin
  Result := A * (1.0 / VecLength(A));
end;

function VecNormalize(X, Y, Z: Float): TVector3;
begin
  Result := VecNormalize(Vector3(X, Y, Z));
end;

{ Calculates the angle (in radians) between the unit vectors A and B. }
function VecAngle(const A, B: TVector3): float;
begin
  Result := arccos(DotProduct(A, B));
end;

{ Rotates the vector V by the angle phi (in radians) around the axis A and
  returns the rotated vector in V.
  A must be unit vector. }
function VecRotate(const V, A: TVector3; phi: Float): TVector3;
var
  cPhi, sPhi: Float;
begin
//  Normalize(A);
  SinCos(phi, sPhi, cPhi);
  Result := V;
  Rotate(Result, A, cPhi, sPhi);
end;

{ Rotates the vector V by the angle phi (in radians) around the axis A and
  returns the rotated vector in V.
  A must be a unit vector.
  Note that the rotation angle phi is not given explicitely, but only by its
  cosine and sine values in order to be able to call the vector rotation from
  the Monte-Carlo simulation efficiently since it provides the angle just like
  this. }
procedure Rotate(var V: TVector3; A: TVector3; Cos_phi,Sin_phi: Float);
var
  V1, V2 : TVector3;
begin
  Cos_phi := 1.0 - Cos_phi;
  V1 := CrossProduct(A, V);
  V2 := CrossProduct(A, V1);
  V.X  := V.X + Sin_phi*V1.X + Cos_phi*V2.X;
  V.Y  := V.Y + Sin_phi*V1.Y + Cos_phi*V2.Y;
  V.Z  := V.Z + Sin_phi*V1.Z + Cos_phi*V2.Z;
end;

{ Rotates the vector V around the y axis by the angle phi and returns the
  rotated angle. phi is positive for a counter-clockwise rotation viewed along
  the y axis towards positive values. }
function VecRotateY(const V: TVector3; phi: float): TVector3;
var
  SPhi, CPhi: float;
begin
  SinCos(phi, SPhi, CPhi);
  Result.X := CPhi*V.X + SPhi*V.Z;
  Result.Y := V.Y;
  Result.Z := -SPhi*V.X + CPhi*V.Z;
end;

{ Calculates the distance between the points A and B (= length of vector B-A) }
function Distance(const A, B: TVector3): float;
begin
  result := VecLength(A - B);
end;


{ Vector operator overloading }

{ Sum of vectors A and B }
operator + (A, B: TVector3): TVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

{ Difference of vectors A and B }
operator - (A, B: TVector3): TVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

{ Inversion of vector A }
operator - (A: TVector3): TVector3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

{ Product of vector A with scalar x }
operator * (A: TVector3; x: Float): TVector3;
begin
  Result.X := A.X * x;
  Result.Y := A.Y * x;
  Result.Z := A.Z * x;
end;


(****************************************************************************)
(*            Intersection of a straight line with geometric objects        *)
(****************************************************************************)

{ Returns in <Point> the intersection of a ray (<ray>, starting at <ray>.Point
  in direction <ray.Dir>) with the plain <plane> going through the point
  <plane>.Point and having normal vector <plane>Dir.
  The function result is the distance between <ray.Point> and the intersection
  point expressed in units of vector <ray.dir>.
  Considers only that intersection point which is in positive ray direction.
  ray.Point must not be on the plane.
  If not such intersection point exists (or if the line is parallel to the
  plane, then the resulting <Point> has the coordinates (mEmpty, mEmpty, mEmpty),
  and the function value is mEmpty as well. }
function rayXplane(ray: TRay; Plane: TRay; var Point: TVector3): float;
var
  lambda: float;
  V: TVector3;
  dot: float;
begin
  Point := EmptyVector3;
  Result := NaN;

  V := Plane.Point - Ray.Point;
  dot := DotProduct(Plane.Dir, Ray.Dir);
  lambda := -1.0;
  if not Zero(dot, FloatEps) then
  begin
    lambda := DotProduct(Plane.Dir, V)/dot;
    if not Zero(lambda, FloatEps) then
    begin
      Point := Ray.Point + Ray.dir * lambda;
      Result := lambda;
    end;
  end;
  if Zero(lambda, FloatEps) or (lambda < 0.0) then
  begin
    Point := EmptyVector3;
    Result := NaN;
  end;
end;

{ Returns in <point> the intersection point of the ray <ray> (starting at point
  <ray.Point> and going in direction <ray.Dir>) with a cylinder oriented along
  the z axis and having radius <r>.
  Considers only the intersection point which is closest to ray.Point if
  FarPoint is false, or the other ray.Point if FarPoint is true, always seen
  along the direction ray.Dir.
  The function value is the distance between <ray.Point> and the intersection
  point, expressed in units of the vector <rax.dir>.
  If no such intersection point exists, <point> becomes an "empty vector", and
  the function result is mEmpty as well.

  Theory:
    - Equation of a straight line in 3D space: v = v0 + µ vd
         (v: vector to any point on the line
          µ: variable along the line )
    - Equation of a cylinder in 3D space
                 | r cos phi |
          cyl =  | r sin phi |
                 |    tau    |
    For the intersection point the line equation is inserted into the cylinder
    equation which result in a quadratic equation system:
          A µ² + B µ + C = 0
          v0(z) + µ vd(z) = tau
    with
          A = vd(x)² + vd(y)²
          B = 2 ( v0(x)*vd(x) + v0(y)*vc(y) )
          c = v0(x)² + v0(y)² - r²                }
function rayXcyl(ray: TRay; r: float; var Point:TVector3; FarPoint: boolean): Float;
var
  A, B, C, my, my1: float;
begin
  Point := EmptyVector3();
  Result := NaN;

  with Ray do
  begin
    with Dir do A := X*X + Y*Y;
    B := Point.X*Dir.X + Point.Y*Dir.Y;
    B := B + B;               { avoid multiplication by 2 }
    with Point do C := X*X + Y*Y - r*r;
  end;
  if SqrSolve(A,B,C, my, my1) then
  begin
    if (my < 0.0) and (my1 < 0.0) then Exit;
    if FarPoint then
      my := Max(my, my1)
    else
    if (my > 0.0) and (my1 > 0.0) then
      my := Min(my,my1)
    else
    if my <= 0.0 then
      my := my1;
    Point := Ray.Point + Ray.Dir * my;
    Result := my;
  end;
end;


(****************************************************************************)
(*                         Coordinate systems                               *)
(****************************************************************************)

{ Converts the cartesian coordinates (x, y, z) (unit size) to spherical
  coordinates (theta, phi). }
procedure CartToSph(x, y, z: float; var theta, phi: float);
begin
  if Equal(z, 1.0, FloatEps) then
    theta := 0.0
  else
  if Equal(z, -1.0, FloatEps) then
    theta := Pi
  else
     theta := ArcCos(z);

  if Equal(x, 0.0, FloatEps) then
    phi := Sign(y)*Pi_2
  else
  if (x > 0.0) then
    phi := ArcTan(y/x)
  else
    phi := Pi + ArcTan(y/x);
end;

{ Converts spherical coordinates (theta, phi) given by their sine and cosine
  values to cartesion coordinates (x, y, z). }
procedure SphToCart(Cos_theta, Sin_theta, Cos_phi, Sin_phi: float;
  var x, y, z: float);
begin
  x := Sin_theta*Cos_phi;
  y := Sin_theta*Sin_phi;
  z := Cos_theta;
end;

{ Converts the spherical coordinates (theta, phi) to cartesian coordinates (x, y, z). }
procedure SphToCart(theta, phi: float; var x, y, z: float);
begin
  SphToCart(Cos(theta),Sin(theta),Cos(phi),Sin(phi), x,y,z);
end;

procedure CartToCyl(x, y: float; var rho, phi: float);
begin
  rho := Sqrt(x*x + y*y);
  if x > 0.0 then
    phi := ArcTan(y/x)
  else
  if x < 0.0 then
    phi := Pi + ArcTan(y/x)
  else
  if y > 0 then
    phi := Pi_2
  else
  if y<0 then
    phi := 3.0 * Pi_2
  else
    phi := 0.0;
end;

procedure CylToCart(rho, phi: float; var x, y: float);
begin
  x := rho*Cos(phi);
  y := rho*Sin(phi);
end;

end.

