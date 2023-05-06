(****************************************************************************)
(*                                MGLOBAL                                   *)
(*--------------------------------------------------------------------------*)
(*  global declarations for math units                                      *)
(*  (c) WP 11/1991; parts by J.W.Rider                                      *)
(****************************************************************************)

unit MGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  float = Double;
  xfloat = Extended;

const
  MaxFloat = 1.7E308;
  MaxFloatIndex = 6551;
  FloatEps: float = 3.4E-9;

type
  PFloatArray = ^TFloatArray;
  TFloatArray = array[1..MaxFloatIndex] of float;

  TMathFunc1  = function(X: float): float;
  TMathFunc2  = function(X, Y: float): float;
  TMathFunc3  = function(X, Y, Z: float): float;

  TMathFuncX1 = function(X: xfloat): xfloat;
  TMathFuncX2 = function(X, Y: xfloat): xfloat;
  TMathFuncX3 = function(X, Y, Z: xfloat): xfloat;

CONST
  CbRt2     = 1.2599210498948731647672;            { cube root of 2          }
  CbRt3     = 1.4422495703074083823216;            { cube root of 3          }
  D2R       = 0.017453292519943295769237;          { # radians in a degree   }
  E         = 2.7182818284590452353603;            { base of natural logs    }
  EulerC    = 0.5772156649015328606065;            { Euler's constant        }
  Ln2       = 0.6931471805599453094172;            { natural logarithm of 2  }
  Ln3       = 1.0986122886681096913952;            { natural logarithm of 3  }
  Ln10      = 2.3025850929940456840280;            { natural logarithm of 10 }
  LnPi      = 1.1447298858494001741434;            { natural logarithm of Pi }
  Log2E     = 1.4426950408889634073599;            { 1/Ln(2)                 }
  Log10E    = 0.4342944819032518276511;            { 1/Ln(10)                }
  M2R       = 0.000290888208665721596154;          {  # rads in a minute     }
  Pi        = 3.1415926535897932384626;            { plain ol' Round Pi      }
  Pi_2      = 1.5707963267948966192313;            { Pi/2                    }
  Pi_4      = 0.7853981633974483096156;            { Pi/4                    }
  R1_E      = 0.3678794411714423215955;            { 1/E                     }
  R1_Pi     = 0.3183098861837906715378;            { 1/Pi                    }
  R1_SqrtPi = 0.5641895835477562869481;            { 1/Sqrt(Pi)              }
  R2_Pi     = 0.6366197723675813430755;            { 2/Pi                    }
  R2_SqrtPi = 1.12837916709551257390;              { 2/Sqrt(Pi)              }
  R2D       = 57.295779513082320876798;            { # degrees in a radian   }
  S2R       = 0.000004848136811095359936;          { # rads in a second      }
  SqrPI     = 9.8696044010893586188345;            { Sqr(Pi) (not round!)    }
  Sqrt_2    = 0.707106781186547524401;             { Sqrt(1/2)               }
  Sqrt2     = 1.4142135623730950488017;            { Sqrt(2)                 }
  Sqrt2Pi   = 2.5066282746310005024158;            { Sqrt(2*Pi)              }
  Sqrt3     = 1.7320508075688772935;               { Sqrt(3)                 }
  Sqrt5     = 2.2360679774997896964;               { Sqrt(5)                 }
  SqrtPi    = 1.7724538509055160272982;            { Sqrt(Pi) = gamma(1/2)   }
  TwoPi     = 6.2831853071795864769253;            { 2*Pi                    }
  FourPi    = 12.5663706143591729538506;           { 4*Pi                    }

const
  mOK                = 0;     { everything correct                           }
  mUndefined         = 1;     { Function is not defined, e.g. log(-1))       }
  mNotConverged      = 2;     { does not converge                            }
  mOverFlow          = 3;     { Overflow                                     }
  mIllegalOperation  = 4;     { Operation not allowed                        }
  mDivByZero         = 5;     { Division by zero                             }
  mOutOfMemory       = 6;     { Memory overflow in allocation                }
  mNILPointer        = 7;     { Returned pointer is nil                      }

  matOutOfMem        = 201;   { Memory overflow                              }
  matIllegalIndex    = 202;   { invalid index                                }
  matNoVector        = 203;   { no vector (no nx1 matrix)                    }
  matDimError        = 204;   { incorrect dimension for requested operation  }
  matIncomplete      = 205;   { matrix incomplete                            }
  matIllegalOp       = 206;   { Operation not allowed                        }
  matSingular        = 207;   { Matrix is singular                           }

  ipEqualX           = 301;   { x values are equal                           }
  ipPole             = 302;   { Pole at x value                              }
  ipNoData           = 303;   { no data for interpolation                    }
  ipOutOfMemory      = 304;   { not enough memory for spline preparation     }

  rtNoInterval       = 400;   { no interval for zero search                  }
  rtNoRoot           = 401;   { Interval does not contain a zero             }
  rtNotConverged     = 402;   { Non-convergent zero search                   }

  spBessOrderError   = 500;   { Illegal order of bessel function             }

const
  mError: integer   = mOK;       { general error variableriable               }
  mEmpty: float     = MaxFloat;  { replacement for empty field in matrixix    }

procedure MResetError;


implementation

procedure MResetError;
begin
  mError := mOK;
end;

end.

