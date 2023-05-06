(***************************************************************************)
(*                                 MMatrix                                 *)
(*-------------------------------------------------------------------------*)
(* Unit for vectors and matices of "arbitray" size on the heap.            *)
(* Vectors and matrices are implemented as object.                         *)
(* Moreover, the unit implements elemtnal computational operations.        *)
(* Convention:                                                             *)
(*  Object methods only change the content of the matices, but not their   *)
(*  size, and also do not create any new matrices.                         *)
(*                                                                         *)
(* Version:  1.2       (WP / May 1992)                                     *)
(* Compiler: TP 6.0                                                        *)
(***************************************************************************)

unit MMatrix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, MGlobal;

const
  MMatrixID = 14000;

type
  PIntArray = ^TIntArray;
  TIntArray = array[1..High(SmallInt)] of Integer;

  TMatrix = class
    DataPtr     : Pointer;
    Cols,Rows   : Integer;
    Transposed  : Boolean;
    Parent      : TMatrix;
    Mi1,Mj1     : Integer;
    Mi2,Mj2     : Integer;
    constructor Create(n, m: Integer);
    constructor Init(n, m: Integer);
    (*
    CONSTRUCTOR CopyOf(A:PMatrix);
    CONSTRUCTOR ProductOf(A,B:PMatrix);
    *)
    constructor SubMatrixOf(M: TMatrix; theI1, theJ1, theI2, theJ2: integer);
    (*
    CONSTRUCTOR Zeros(n,m:integer);
    CONSTRUCTOR Load(var S:TStream);
    *)
    destructor Destroy; override;
    (*
    PROCEDURE   AddMat(M:PMatrix);
    PROCEDURE   AddSc(x:float);
    FUNCTION    AllZero : BOOLEAN;
    FUNCTION    AnyZero : BOOLEAN;
    FUNCTION    ColumnMax(c:integer):float;
    FUNCTION    ColumnMin(c:integer):float;
    PROCEDURE   DivSc(x:float);
    PROCEDURE   Exp;
    FUNCTION    GetColumnPtr(j:integer) : Pointer;
    *)
    PROCEDURE   GetMem(n,m:integer);
    FUNCTION    GetValue(i, j:integer): float; virtual;
    (*
    PROCEDURE   Ln;
    FUNCTION    Max : float;
    FUNCTION    Min : float;
    PROCEDURE   MulSc(x:float);
    *)
    PROCEDURE   PutValue(i, j: integer; z: float); virtual;
    (*
    PROCEDURE   Recip(x:float);
    FUNCTION    RowMax(c:integer):float;
    FUNCTION    RowMin(c:integer):float;
    *)
    procedure   Size(var n, m: integer); virtual;
    (*
    PROCEDURE   Store(var S:TStream); VIRTUAL;
    PROCEDURE   SubMat(M:PMatrix);
    PROCEDURE   SubSc(x:float);
    PROCEDURE   Transpose;
    *)
  private
    (*
    PROCEDURE   OpMat(Mat:PMatrix; Op:byte);
    PROCEDURE   OpSc(x:float; Op:byte);
    PROCEDURE   OpFunc(op:byte);
    *)
  end;

procedure ClearMat(var A: TMatrix);

implementation

type
  PPointerArray = ^TPointerArray;
  TPointerArray = Array[1..65500 div SizeOf(Pointer)] of Pointer;

(***************************************************************************)
(*                               TMatrix                                   *)
(*-------------------------------------------------------------------------*)
(* nxm matrix (n = number of rows, m = number of columns), allocated       *)
(* dynamically on the heap.                                                *)
(* Memory is allocated at construction (Init).                             *)
(* Matrix elements can be written by the PutValue method and read by the   *)
(* GetValue method.                                                        *)
(* The size of the matrix is returned by the Size method.                  *)
(* Internally, the matrix is implemented as a collection of PArrayObj,     *)
(* where the array are the columns of the matrix, i.e. m array of length n *)
(* Index number always begin with 1.                                       *)
(***************************************************************************)

constructor TMatrix.Create(n, m: integer);
var
  i, j: INTEGER;
begin
  inherited Create;
  Transposed := false;
  GetMem(n, m);
  if mError = mOK then
    for i := 1 to n do
      for j := 1 to m do
        PutValue(i, j, mEmpty)
  else
    Fail;
end;

constructor TMatrix.Init(n, m: Integer);
begin
  Create(n, m);
end;

{ Creates a matrix which is the submatrix of matrix <M> between the matrix
  elements at (theI1, theJ1) and (theI2, theJ2).
  No more memory is allocated for the matrix elements.
  Every change in the fields of the matrix is propagated to the parent
  matrix M !!! }
constructor TMatrix.SubMatrixOf(M: TMatrix; theI1, theJ1, theI2, theJ2: integer);
var
  tmp: Integer;
begin
  inherited Create;
  Transposed := false;
  Parent := M;

  if theI1 > theI2 then
  begin
    tmp := theI1;
    theI1 := theI2;
    theI2 := tmp;
  end;

  if theJ1>theJ2 then
  begin
    tmp := theJ1;
    theJ1 := theJ2;
    theJ2 := tmp;
  end;

  Mi1 := theI1;
  Mj1 := theJ1;
  Mi2 := theI2;
  Mj2 := theJ2;
end;

{ NOTE: Needs the physical matrix dimensions rather than the values returned by
  Size() which could be swapped due to "Transposed".
  Matrices created by SubMatrixOf() did not allocate any memory and do not
  release anything here. }
destructor TMatrix.Destroy;
var
  j: Integer;
  Arr: PFloatArray;
begin
  if (Parent = nil) and (DataPtr <> nil) then
  begin
    for j := 1 to Cols do
    begin
      Arr := PFloatArray(PPointerArray(DataPtr)^[j]);
      if Arr <> NIL then
        FreeMem(Arr, Rows*SizeOf(Float));
    end;
    FreeMem(DataPtr, Cols*SizeOf(Pointer));
  end;
  inherited;
end;

{ Allocates memory on the heap for n rows and m columns. }
procedure TMatrix.GetMem(n, m: integer);
var
  //MatMem: LongInt;
  j: Integer;
begin
  {
  MatMem := LongInt(n) * m * SizeOf(Float);
  if (MaxAvail-16*LowMemSize) < MatMem + m*SizeOf(Pointer) THEN BEGIN
    mError  := matOutOfMem;
    DataPtr := NIL;
  END ELSE
  }
  begin
    SYSTEM.GetMem(DataPtr, m * SizeOf(Pointer));
    Cols := m;
    Rows := n;
    for j := 1 to m do
      SYSTEM.GetMem(PPointerArray(DataPtr)^[j], n*SizeOf(Float));
  end;
  Parent := nil;
end;

function TMatrix.GetValue(i, j: integer): float;
var
  n, m: INTEGER;
begin
  if mError <> mOK then
  begin
    Result := mEmpty;
    Exit;
  end;

  Size(n,m);
  if (i < 1) or (i > n) or (j < 1) or (j > m) then
  begin
    mError := matIllegalIndex;
    Result := mEmpty;
  end else
  begin
    if Parent = nil then
    begin
      if DataPtr = nil then
      begin
        mError := matOutOfMem;
        Result := mEmpty;
      end else
      begin
        if Transposed then
          Result := PFloatArray(PPointerArray(DataPtr)^[i])^[j]
        else
          Result := PFloatArray(PPointerArray(DataPtr)^[j])^[i];
      end;
    end else
      Result := Parent.GetValue(i+Mi1-1, j+Mj1-1);
  end;
end;

procedure TMatrix.PutValue(i, j: Integer; z: float);
var
  n, m: Integer;
begin
  if mError <> mOK then
    Exit;

  Size(n,m);
  if (i < 1) or (i > n) or (j < 1) or (j > m) then
    mError := matIllegalIndex
  else
  begin
    if Parent = nil then
    begin
      if (DataPtr = nil) then
        mError := matOutOfMem
      else
      begin
        if Transposed then
          PFloatArray(PPointerArray(DataPtr)^[i])^[j] := z
        else
          PFloatArray(PPointerArray(DataPtr)^[j])^[i] := z;
      end;
    end else
      Parent.PutValue(i+Mi1-1, j+Mj1-1, z);
  end;
end;

procedure TMatrix.Size(var n, m: integer);
var
  tmp: Integer;
begin
  if mError <> mOK then
  begin
    n := 0;
    m := 0;
    Exit;
  end;

  if Parent = nil then
  begin
    m := Cols;
    n := Rows;
  end else
  begin
    m := Mj2 - Mj1 + 1;
    n := Mi2 - Mi1 + 1;
  end;

  if Transposed then
  begin
    tmp := n;
    n := m;
    m := tmp;
  end;
end;



(****************************************************************************)
(*                                                                          *)
(*                        elemental matrix operations                       *)
(*                                                                          *)
(****************************************************************************)

{ Destroys the matrix. }
procedure ClearMat(var A: TMatrix);
begin
  A.Free;
  A := nil;
end;

end.

