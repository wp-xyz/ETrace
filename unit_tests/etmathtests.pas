unit etMathTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, math,
  ET_Global, ET_Math;

type

  TMathTests= class(TTestCase)
  published
    procedure TestVectorMath;
    procedure TestIntersection_RayPlane;
  end;

implementation

procedure TMathTests.TestVectorMath;
const
  EPS = 1E-9;
var
  a, b, c, expected: TVector3;
  f, float_expected: Float;
begin
  // Vector addition
  a := Vector3(1, 2, 4);
  b := Vector3(-1, 2, 0);
  c := a + b;
  expected := Vector3(0, 4, 4);
  AssertEquals('Vector addition result mismatch x', expected.x, c.x);
  AssertEquals('Vector addition result mismatch y', expected.y, c.y);
  AssertEquals('Vector addition result mismatch z', expected.z, c.z);

  // Vector subtraction
  a := Vector3(1, 2, 4);
  b := Vector3(-1, 2, 0);
  c := a - b;
  expected := Vector3(2, 0, 4);
  AssertEquals('Vector subtraction result mismatch x', expected.x, c.x);
  AssertEquals('Vector subtraction result mismatch y', expected.y, c.y);
  AssertEquals('Vector subtraction result mismatch z', expected.z, c.z);

  // Vector scalar multiplication
  a := Vector3(-1, 2, 4);
  c := a * 3;
  expected := Vector3(-3, 6, 12);
  AssertEquals('Vector scalar multiplication result mismatch x', expected.x, c.x);
  AssertEquals('Vector scalar multiplication result mismatch y', expected.y, c.y);
  AssertEquals('Vector scalar multiplication result mismatch z', expected.z, c.z);

  // Dot product
  a := Vector3(1, 2, 4);
  b := Vector3(-1, 2, 0);
  f := DotProduct(a, b);
  float_expected := 3;  // calculated by Wolfram Alpha
  AssertEquals('Dot product result mismatch', float_expected, f);

  // Cross product
  a := Vector3(1, 2, 4);
  b := Vector3(-1, 2, 0);
  c := CrossProduct(a, b);
  expected := Vector3(-8, -4, 4);  // calculated by Wolfram Alpha
  AssertEquals('Cross product result mismatch x', expected.x, c.x);
  AssertEquals('Cross product result mismatch y', expected.y, c.y);
  AssertEquals('Cross product result mismatch z', expected.z, c.z);

  // Vector normalization
  a := Vector3(1, 2, 4);
  b := VecNormalize(a);
  expected := Vector3(1/sqrt(21), 2/sqrt(21), 4/sqrt(21));  // calculated by Wolfram Alpha
  AssertEquals('Vector normalization result mismatch x', expected.x, b.x);
  AssertEquals('Vector normalization result mismatch y', expected.y, b.y);
  AssertEquals('Vector normalization result mismatch z', expected.z, b.z);

  // Angle between vectors
  a := Vector3( 8, 4, 1);
  b := Vector3(-1, 2, 0);
  f := VecAngle(VecNormalize(a), VecNormalize(b));
  float_expected := pi/2;
  AssertEquals('Angle between vectors result mismatch', float_expected, f, EPS);

  // Rotate a vector around the y axis by 90°
  a := Vector3( 8, 4, 1);
  b := VecRotateY(a, pi/2);
  expected := Vector3(1, 4, -8);    // Result from https://www.vcalc.com/wiki/vector-rotation
  AssertEquals('Vector rotation around y axis result mismatch x', expected.x, b.x);
  AssertEquals('Vector rotation around y axis result mismatch y', expected.y, b.y);
  AssertEquals('Vector rotation around y axis result mismatch z', expected.z, b.z);

  // Rotate a vector around the y axis by 45°
  a := Vector3(8, 4, 1);
  b := VecRotateY(a, pi/4);
  expected := Vector3(6.3639610307, 4, -4.9497474683);  // from https://www.vcalc.com/wiki/vector-rotation
  AssertEquals('Vector rotation around y axis result mismatch x', expected.x, b.x, EPS);
  AssertEquals('Vector rotation around y axis result mismatch y', expected.y, b.y, EPS);
  AssertEquals('Vector rotation around y axis result mismatch z', expected.z, b.z, EPS);

  // General vector rotation
  a := Vector3(8, 4, 1);
  b := Vector3(-1, 2, 0);
  c := VecRotate(a, VecNormalize(b), pi/4);
  // Expected result calculated at https://www.vcalc.com/wiki/vector-rotation
  expected := Vector3(6.2893097815, 3.1446548908, -5.6174485392);
  AssertEquals('General vector rotation result mismatch x', expected.x, c.x, EPS);
  AssertEquals('General vector rotation result mismatch y', expected.y, c.y, EPS);
  AssertEquals('General vector rotation result mismatch z', expected.z, c.z, EPS);
end;

procedure TMathTests.TestIntersection_RayPlane;
const
  EPS = 1e-9;
var
  ray: TRay;
  plane: TRay;
  v: TVector3;
  d: Float;
begin
  // Test #1: ray going down along z towards xy plane and to the origin
  ray.Point := Vector3(0, 0, 10);
  ray.Dir := Vector3(0, 0, -1);
  plane.Point := Vector3(1, 1, 0);
  plane.Dir := Vector3(0, 0, 1);
  d := RayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #1: distance mismatch', 10, d);
  AssertEquals('Ray-plane intersection test #1: intersection point x mismatch', 0, v.x);
  AssertEquals('Ray-plane intersection test #1: intersection point y mismatch', 0, v.y);
  AssertEquals('Ray-plane intersection test #1: intersection point z mismatch', 0, v.z);

  // Test #2: ray is x axis, plane is xy plane, distance 1 --> no intersection
  ray.Point := Vector3(0, 0, 1);
  ray.Dir := Vector3(1, 0, 0);
  plane.Point := Vector3(0, 0, 0);
  plane.Dir := Vector3(0, 0, 1);
  d := RayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #2: distance mismatch', true, IsNaN(d));
  AssertEquals('Ray-plane intersection test #2: intersection point x mismatch', true, IsNaN(v.x));
  AssertEquals('Ray-plane intersection test #2: intersection point y mismatch', true, IsNaN(v.y));
  AssertEquals('Ray-plane intersection test #2: intersection point z mismatch', true, isNaN(v.z));

  // Test #3: ray.Point=(3,4,0), ray.Dir = (-1,-2,1); plane: 3x+5y-2z=-1 --> plane.Point=(0,0,0.5), plane.Dir=(-3,-5,2) --> S = (1,0,2)
  // Values from https://www.abiweb.de/mathematik-analytische-geometrie-lineare-algebra-agla/schnitte/schnitt-ebene-gerade.html
  ray.Point := Vector3(3, 4, 0);
  ray.Dir := VecNormalize(-1, -2, 1);
  plane.Point := Vector3(0, 0, 0.5);
  plane.Dir := VecNormalize(-3, -5, 2);       // from 3x + 5y -2z = -1
  d := rayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #3: distance mismatch', sqrt((sqr(3-1)+sqr(4-0)+sqr(0-2))), d, EPS);
  AssertEquals('Ray-plane intersection test #3: intersection point x mismatch', 1.0, v.x, EPS);
  AssertEquals('Ray-plane intersection test #3: intersection point y mismatch', 0.0, v.y, EPS);
  AssertEquals('Ray-plane intersection test #3: intersection point z mismatch', 2.0, v.z, EPS);

  // Test #4: ray going down inclined by 45° in xz plane, targetted at origin. xy plane
  ray.Point := Vector3(10, 0, 10); // 10/10 --> 45°
  ray.Dir := Vector3(-sin(degToRad(45)), 0, -cos(DegToRad(45)));
  plane.Point := Vector3(0, 0, 0);
  plane.Dir := Vector3(0, 0, 1);
  d := RayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #4: distance mismatch', sqrt(sqr(10) + sqr(0) + sqr(10)), d);
  AssertEquals('Ray-plane intersection test #4: intersection point x mismatch', 0.0, v.x, EPS);
  AssertEquals('Ray-plane intersection test #4: intersection point y mismatch', 0.0, v.y, EPS);
  AssertEquals('Ray-plane intersection test #4: intersection point z mismatch', 0.0, v.z, EPS);

  // Test #5: like test #4, but plane shifted up to z=1
  ray.Point := Vector3(10, 0, 10); // 10/10 --> 45°
  ray.Dir := VecNormalize(-1, 0, -1);
  plane.Point := Vector3(0, 0, 1);
  plane.Dir := Vector3(0, 0, 1);
  d := RayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #5: distance mismatch', sqrt(sqr(10.0-1) + sqr(0) + sqr(10-1)), d);
  AssertEquals('Ray-plane intersection test #5: intersection point x mismatch', 1.0, v.x, EPS);
  AssertEquals('Ray-plane intersection test #5: intersection point y mismatch', 0.0, v.y, EPS);
  AssertEquals('Ray-plane intersection test #5: intersection point z mismatch', 1.0, v.z, EPS);

  // Test #6: Ray startin at (0,0,0), upward in xz plane, polar angle 60°, yz plane at x=2.5 --> P = (2.5,0,1.25)
  ray.Point := Vector3(0,0,0);
  ray.Dir := Vector3(sin(DegToRad(60)), 0, cos(DegToRad(60)));
  plane.Point := Vector3(2.5, 0, 0);
  plane.Dir := Vector3(1, 0, 0);
  d := RayXPlane(ray, plane, v);
  AssertEquals('Ray-plane intersection test #6: distance mismatch', 2.5/sin(DegToRad(60)), d, EPS);
  AssertEquals('Ray-plane intersection test #6: intersection point x mismatch', 2.5, v.x, EPS);
  AssertEquals('Ray-plane intersection test #6: intersection point y mismatch', 0.0, v.y, EPS);
  AssertEquals('Ray-plane intersection test #6: intersection point z mismatch', 2.5/tan(DegToRad(60)), v.z, EPS);

end;



initialization

  RegisterTest(TMathTests);
end.

