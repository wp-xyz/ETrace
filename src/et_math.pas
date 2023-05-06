unit et_Math;

{$mode ObjFPC}{$H+}

interface

uses
  MGlobal, MFunc, MRoots,
  et_Global;

const
  ETMathID = 31000;

const
  OneThird  = 1.0/3;
  TwoThirds = 2.0/3;

function Random_Gauss: float;
function Random_Cos: float;

function SqrSolve(a, b, c: float; var x1, x2: float): boolean;

procedure VecAssign(var A: TVector3; x, y, z: float);
function ValidVector(V: TVector3): boolean;
function DotProduct(const A, B: TVector3): float;
procedure CrossProduct(const A, B: TVector3; var C: TVector3);
procedure VecAdd(const A,B: TVector3; var C: TVector3);
procedure VecMulSc(VAR A:TVector3; x:float);
procedure VecSub(const A, B: TVector3; var C: TVector3);
function VecLength(const A: TVector3): float;
procedure Normalize(var A: TVector3);
function VecAngle(const A, B: TVector3): float;
procedure Rotate(var V, A: TVector3; phi: Float);
procedure Rotate(var V: TVector3; A: TVector3; Cos_phi,Sin_phi: Float);
procedure RotateY(var V: TVector3; phi: float);
function Distance(const A, B: TVector3): float;
procedure MatMul(const Matrix: TMatrix3; var V, Res: TVector3);

function rayXplane(ray: TRay; Plane: TRay; var Point: TVector3): float;
function rayXcyl(ray: TRay; r: float; var Point:TVector3; FarPoint: boolean): Float;

procedure CartToSph(x, y, z: float; var theta, phi: float);
procedure SphToCart(theta, phi: float; var x, y, z: float);
procedure SphToCart(Cos_theta, Sin_theta, Cos_phi, Sin_phi: float; var x, y, z: float);
procedure CartToCyl(x, y: float; var rho, phi: float);
procedure CylToCart(rho, phi: float; var x, y: float);


implementation

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
    ok := MRoots.SqrSolve(p,q, x1,x2);
  end;
  if not ok then
  begin
    x1 := mEmpty;
    x2 := mEmpty;
  end;
  Result := ok;
end;


(****************************************************************************)
(*                             Vector math                                  *)
(****************************************************************************)

procedure VecAssign(var A: TVector3; x, y, z: float);
begin
  A.X := x;
  A.Y := y;
  A.Z := z;
end;

{ Checks whether there is any component of the vector V which is mEmpty.
  Such a vector might be created in case of the intersection routines, if
  there is no intersection of the line with the geometric object. }
function ValidVector(V: TVector3): boolean;
begin
  Result := (V.X <> mEmpty) and (V.Y <> mEmpty) and (V.Z <> mEmpty);
end;

{ Calculates the dot product of the vector A and B. }
function DotProduct(const A, B: TVector3): float;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

{ Calculates the cross product of the vector A and B. }
procedure CrossProduct(const A, B: TVector3; var C: TVector3);
begin
  C.X := A.Y*B.Z - A.Z*B.Y;
  C.Y := A.Z*B.X - A.X*B.Z;
  C.Z := A.X*B.Y - A.Y*B.X;
end;

{ Vector sum C = A + B }
procedure VecAdd(const A,B: TVector3; var C: TVector3);
begin
  C.X := A.X + B.X;
  C.Y := A.Y + B.Y;
  C.Z := A.Z + B.Z;
end;

{ Multiplies the vector A by the scalar x }
procedure VecMulSc(VAR A:TVector3; x:float);
begin
  A.X := A.X * x;
  A.Y := A.Y * x;
  A.Z := A.Z * x;
end;

{ Vector difference C = A-B }
procedure VecSub(const A, B: TVector3; var C: TVector3);
begin
  C.X := A.X - B.X;
  C.Y := A.Y - B.Y;
  C.Z := A.Z - B.Z;
end;

function VecLength(const A: TVector3): float;
begin
  Result := Sqrt(DotProduct(A, A));
end;

{ Normalizes the vector A to length 1 }
procedure Normalize(var A: TVector3);
var
  L: float;   { reciprocal length of the vector A }
begin
  L := 1.0 / VecLength(A);
  VecMulSc(A, L);
end;

{ Calculates the angle (in radians) between the unit vectors A and B. }
function VecAngle(const A, B: TVector3): float;
begin
  Result := arccos(DotProduct(A, B));
end;

{ Rotates the vector V by the angle phi (in radians) around the axis A and
  returns the rotated vector in V. }
procedure Rotate(var V, A: TVector3; phi: Float);
begin
  Normalize(A);
  Rotate(V, A, cos(phi), sin(phi));
end;

{ Rotates the vector V by the angle phi (in radians) around the axis A and
  returns the rotated vector in V.
  Note that the rotation angle phi is not given explicitely, but only by its
  cosine and sine values in order to be able to call the vector rotation from
  the Monte-Carlo simulation efficiently since it provides the angle just like
  this. }
procedure Rotate(var V: TVector3; A: TVector3; Cos_phi,Sin_phi: Float);
var
  V1, V2 : TVector3;
begin
  Normalize(A);
  Cos_phi := 1.0 - Cos_phi;
  CrossProduct(A,V, V1);
  CrossProduct(A,V1,V2);
  V.X  := V.X + Sin_phi*V1.X + Cos_phi*V2.X;
  V.Y  := V.Y + Sin_phi*V1.Y + Cos_phi*V2.Y;
  V.Z  := V.Z + Sin_phi*V1.Z + Cos_phi*V2.Z;
end;

{ Rotates the vector V around the y axis by the angle phi and returns the
  rotated angle in V. }
procedure RotateY(var V: TVector3; phi: float);
var
  SPhi, CPhi: float;
  tmp: TVector3;
begin
  SPhi := Sin(phi);
  CPhi := Cos(phi);
  tmp.X := CPhi*V.X + SPhi*V.Z;
  tmp.Y := V.Y;
  tmp.Z := -SPhi*V.X + CPhi*V.Z;
  V := tmp;
end;

{ Calculates the distance between the points A and B (= length of vector B-A) }
function Distance(const A, B: TVector3): float;
var
  C: TVector3;
begin
  VecSub(A, B, C);
  result := VecLength(C);
end;

{ Multiplies the given matrix by the vector V and returns the resulting vector
  in Res. }
procedure MatMul(const Matrix: TMatrix3; var V, Res: TVector3);
begin
  Res.X := Matrix[1].X*V.X + Matrix[2].X*V.Y + Matrix[3].X*V.Z;
  Res.Y := Matrix[1].Y*V.X + Matrix[2].Y*V.Y + Matrix[3].Y*V.Z;
  Res.Z := Matrix[1].Z*V.X + Matrix[2].Z*V.Y + Matrix[3].Z*V.Z;
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
  parall: float;
begin
  VecAssign(Point, mEmpty, mEmpty, mEmpty);
  Result := mEmpty;

  VecSub(Plane.Point, Ray.Point, V);
  parall := DotProduct(Plane.Dir, Ray.Dir);
  lambda := -1.0;
  if not Zero(parall, FloatEps) then
  begin
    lambda := DotProduct(Plane.Dir, V)/parall;
    if not Zero(lambda, FloatEps) then
    begin
      VecMulSc(Ray.Dir, lambda);
      VecAdd(Ray.Point, Ray.Dir, Point);
      Result := lambda;
    end;
  end;
  if Zero(Lambda,FloatEps) or (lambda<0.0) then
  begin
    VecAssign(Point, mEmpty, mEmpty, mEmpty);
    Result := mEmpty;
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
  VecAssign(Point, mEmpty, mEmpty, mEmpty);
  Result := mEmpty;

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
      my := MaxF(my, my1)
    else
    if (my > 0.0) and (my1 > 0.0) then
      my := MinF(my,my1)
    else
    if my <= 0.0 then
      my := my1;
    VecMulSc(Ray.Dir, my);
    VecAdd(Ray.Point, Ray.Dir, Point);
    Result := my;
  end;
end;


(****************************************************************************)
(*                         Coordiante systems                               *)
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
    phi := Sgn(y)*Pi_2
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

