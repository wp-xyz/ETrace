unit etSampleTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, fpcunit, testutils, testregistry,
  et_Global, et_Objects, et_Math;

type

  TSampleTests= class(TTestCase)
  protected
    FSubstrate: TMaterial;
    FLayer: TMaterial;
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestIntersection_ContactHole;
    procedure TestIntersection_Stripe;
  end;

implementation

procedure TSampleTests.Setup;
var
  materials: TMaterialsList;
  P: TMaterialParams;
begin
  materials := NewMaterialsList;
  try
    P := materials.SearchName('Si');
    FSubstrate := TMaterial.Create(P.Z, P.A, P.Massdensity, P.CoreLevel, P.AugerEnergy);
    P := materials.SearchName('SiO2');
    FLayer := TMaterial.Create(P.Z, P.A, P.Massdensity, P.CoreLevel, P.AugerEnergy);
  finally
    materials.Free;
  end;
end;

procedure TSampleTests.TearDown;
begin
  FSubstrate.Free;
  FLayer.Free;
end;

procedure TSampleTests.TestIntersection_ContactHole;
const
  EPS = 1E-6;
  R = 0.5;
  DEPTH = 1.0;
var
  sample: TContacthole;
  ray: TRay;
  p: TVector3;
  expected: TVector3;
  intersects: Boolean;
begin
  sample := TContactHole.Create(FSubstrate, FLayer, 10, -1, R, DEPTH);

  // Test #1: Electron flying downward in center of contact hole, starting outside
  ray.Point := Vector3(0, 0, 1);
  ray.Dir := Vector3(0, 0, -1);
  intersects := sample.Intersection(ray, P, true);        // from outside
  expected := Vector3(0, 0, -1);
  AssertEquals('Intersection not found in test #1', true, intersects);
  AssertEquals('Intersection point mismatch in test #1', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #1', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #1', expected.Z, p.Z, EPS);

  // Test #1a: Electron flying downward in center of contact hole, starting below bottom
  ray.Point := Vector3(0, 0, -DEPTH - 0.1);
  ray.Dir := Vector3(0, 0, -1);
  intersects := sample.Intersection(ray, P, false);        // from inside
  AssertEquals('Non-existent intersection found in test #1a', false, intersects);

  // Test #2: Electron flying horizontally from left to right to c.h. center at half depth
  ray.Point := Vector3(-2, 0, -DEPTH/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, false);      // from inside
  expected := Vector3(-R, 0, -DEPTH/2);
  AssertEquals('Intersection not found in test #2', true, intersects);
  AssertEquals('Intersection point mismatch in test #2', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #2', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #2', expected.Z, p.Z, EPS);

  // Test #3: Electron flying horizontally from c.h. center at half depth to right
  ray.Point := Vector3(0, 0, -DEPTH/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, false);      // from inside
  expected := Vector3(R, 0, -DEPTH/2);
  AssertEquals('Intersection not found in test #3', true, intersects);
  AssertEquals('Intersection point mismatch in test #3', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #3', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #3', expected.Z, p.Z, EPS);

  // Test #4: Electron flying horizontally from c.h. center at half depth to right: changed "from inside" to "from outside"
  ray.Point := Vector3(0, 0, -DEPTH/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, true);      // from outside        // --> "from inside"/"from outside" parameter not necessary
  expected := Vector3(R, 0, -DEPTH/2);
  AssertEquals('Intersection not found in test #4', true, intersects);
  AssertEquals('Intersection point mismatch in test #4', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #4', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #4', expected.Z, p.Z, EPS);

  // Test #5 Electron flying from center of c.h. bottom upwards under 45°
  ray.Point := Vector3(0, 0, -DEPTH);
  ray.Dir := VecNormalize(1, 0, 1);
  intersects := sample.Intersection(ray, P, true);
  expected := Vector3(R, 0, -DEPTH + R);
  AssertEquals('Intersection not found in test #5', true, intersects);
  AssertEquals('Intersection point mismatch in test #5', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #5', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #5', expected.Z, p.Z, EPS);

  // Test #6 Electron flying from hit point of Test #6 upwards under 45°
  ray.Point := Vector3(R+1e-9, 0, -DEPTH + R);   // +1e-9 --> move away from contact hole wall
  ray.Dir := VecNormalize(1, 0, 1);
  intersects := sample.Intersection(ray, P, false);
  expected := Vector3(DEPTH, 0, 0);
  AssertEquals('Intersection not found in test #6', true, intersects);
  AssertEquals('Intersection point mismatch in test #6', expected.X, p.X, EPS);
  AssertEquals('Intersection point mismatch in test #6', expected.Y, p.Y, EPS);
  AssertEquals('Intersection point mismatch in test #6', expected.Z, p.Z, EPS);

  sample.Free;
end;

procedure TSampleTests.TestIntersection_Stripe;
const
  EPS = 1E-6;
  WIDTH = 0.5;
  HEIGHT = 1.0;
var
  sample: TStripe;
  ray: TRay;
  p: TVector3;
  expected: TVector3;
  intersects: Boolean;
begin
  sample := TStripe.Create(FSubstrate, FLayer, 10, -1, WIDTH, HEIGHT);

  // Test #1: Electron flying downward in center of stripe, starting outside
  ray.Point := Vector3(0, 0, 1);
  ray.Dir := Vector3(0, 0, -1);
  intersects := sample.Intersection(ray, P, true);        // from outside
  expected := Vector3(0, 0, 0);
  AssertEquals('Stripe intersection not found in test #1', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #1', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #1', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #1', expected.Z, p.Z, EPS);

  // Test #1a: Electron flying downward in center of stripe, starting inside
  ray.Point := Vector3(0, 0, 0);
  ray.Dir := Vector3(0, 0, -1);
  intersects := sample.Intersection(ray, P, false);
  AssertEquals('Non-existent stripe intersection found in test #1a', false, intersects);

  // Test #2: Electron flying horizontally from left to right to stripe center at half height
  ray.Point := Vector3(-2, 0, -HEIGHT/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, false);      // from inside
  expected := Vector3(-WIDTH/2, 0, -HEIGHT/2);
  AssertEquals('Stripe intersection not found in test #2', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #2', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #2', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #2', expected.Z, p.Z, EPS);

  // Test #3: Electron flying horizontally from stripe center at half depth to right
  ray.Point := Vector3(0, 0, -HEIGHT/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, false);      // from inside
  expected := Vector3(WIDTH/2, 0, -HEIGHT/2);
  AssertEquals('Stripe intersection not found in test #3', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #3', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #3', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #3', expected.Z, p.Z, EPS);

  // Test #4: Electron flying horizontally from stripe center at half depth to right: changed from "from inside" to "from outside"
  ray.Point := Vector3(0, 0, -HEIGHT/2);
  ray.Dir := Vector3(1, 0, 0);
  intersects := sample.Intersection(ray, P, true);      // from outside        // --> "from inside"/"from outside" parameter not necessary
  expected := Vector3(WIDTH/2, 0, -HEIGHT/2);
  AssertEquals('Stripe intersection not found in test #4', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #4', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #4', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #4', expected.Z, p.Z, EPS);

  // Test #5 Electron flying from center of stripe bottom upwards under 45°
  ray.Point := Vector3(0, 0, -HEIGHT);
  ray.Dir := VecNormalize(1, 0, 1);
  intersects := sample.Intersection(ray, P, false);
  expected := Vector3(WIDTH/2, 0, -HEIGHT + WIDTH/2);
  AssertEquals('Stripe intersection not found in test #5', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #5', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #5', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #5', expected.Z, p.Z, EPS);

  // Test #6 Electron flying from hit point of Test #5 downwards under 45°
  ray.Point := Vector3(WIDTH/2 + 1e-9, 0, -HEIGHT + WIDTH/2);   // +1e-9 --> move away from stripe wall
  ray.Dir := VecNormalize(1, 0, -1);
  intersects := sample.Intersection(ray, P, true);
  expected := Vector3(WIDTH, 0, -HEIGHT);
  AssertEquals('Stripe intersection not found in test #6', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #6', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #6', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #6', expected.Z, p.Z, EPS);

  // Test #7 Electron flying towards right sidewall under 60°, focused (intentionally) to bottom of stripe (0, 0, -HEIGHT)
  ray.Dir := Vector3(-sin(DegToRad(60)), 0, -cos(DegToRad(60)));
  ray.Point := Vector3(0, 0, -HEIGHT) + (-ray.Dir)*10.0;
  intersects := sample.Intersection(ray, P, true);
  expected := Vector3(WIDTH/2, 0, -HEIGHT + WIDTH/2/tan(DegToRad(60)));
  AssertEquals('Stripe intersection not found in test #7', true, intersects);
  AssertEquals('Stripe intersection point x mismatch in test #7', expected.X, p.X, EPS);
  AssertEquals('Stripe intersection point y mismatch in test #7', expected.Y, p.Y, EPS);
  AssertEquals('Stripe intersection point z mismatch in test #7', expected.Z, p.Z, EPS);

  sample.Free;
end;

initialization
  RegisterTest(TSampleTests);

end.

