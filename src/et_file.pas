{ Saves data in a MatLab file  }

unit et_File;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, MGlobal, MMatrix, et_Global;

const
  DoublePrec   = 0;
  SinglePrec   = 1;
  Long         = 2;
  Int          = 3;
  UnsignedInt  = 4;

var
  SavePrecision: Integer = SinglePrec;

function MatlabFile_Create(FileName, MatName: String; Data: TMatrix): Boolean;
function MatlabFile_Append(FileName, MatName: String; Data: TMatrix) : Boolean;
function MatlabFile_Write(FileName: String; Pos: LongInt; MatName: String; Data: TMatrix): Boolean;
procedure MatlabFile_Read(FileName: String; var Pos: LongInt; var MatName: String; var Data: TMatrix);
function MatlabFile_ReadInfo(FileName: String; var Pos: LongInt; var Info: TMatrix): Boolean;
function MatlabFile_GetVarName(FileName: String; var Pos: LongInt): String;

implementation

type
  TMatLabHeader = record
    MType    : LongInt;
    MRows    : LongInt;
    MCols    : LongInt;
    ImagFlag : LongInt;
    NameLen  : LongInt;
  end;

const
  INTEL     = 0;
  MOTOROLA  = 1;
  OTHER     = -1;

procedure SwapBytes(var b; Count: Word);
var
  Old: array[0..7] of byte;
  New: array[0..7] of byte;
  i: byte;
begin
  if Count <= 8 then
  begin
    Move(b, Old, Count);
    for i := 0 to Count-1 do
      New[i] := Old[Count - i - 1];
    Move(New, b, Count);
  end else
    raise Exception.Create('Too many bytes for et_File.SwapBytes');
end;

{ Checks whether the header can correspond to that of a Matlab file.
  Takes care of big-endian/little-endian byte order.
  Returns 0 for Intel (little-endian) or 1 for Motorola (big endian).
  If the header is not reckognized the function value is -1. }
function CheckHeader(HeaderType: LongInt): Integer;
var
  M, O, P, T: LongInt;

  procedure CalcMOPT;
  begin
    M := HeaderType div 1000;
    O := (HeaderType mod 1000) div 100;
    P := (HeaderType mod 100) div 10;
    T := HeaderType mod 10;
  END;

  function IsOK: BOOLEAN;
  begin
    Result := (M >= 0) and (M <= 3) and ((O = 0) or (O = 1)) and
              (P >= 0) and (P <= 4) and ((T = 0) or (T = 1));
  end;

begin
  CalcMOPT;
  if IsOK then
    Result := INTEL
  else
  begin
    SwapBytes(HeaderType, 4);
    CalcMOPT;
    if IsOK then
      Result := MOTOROLA
    else
      Result := OTHER;
  end;
end;

procedure SaveToFile(var F: File; MatName: String; Data: TMatrix;
  Prec: Integer);
const
  MaxDouble = 65000 div 8;
  MaxSingle = 65000 div 4;
type
  TDoubleArray = array[1..MaxDouble] of Double;
  TSingleArray = array[1..MaxSingle] of Single;
  PDoubleArray = ^TDoubleArray;
  PSingleArray = ^TSingleArray;
var
  Header: TMatlabHeader;
  res: WORD;
  i, j: Integer;
  Z: Double;
  Arr: Pointer;
  n, m: Integer;
  BytesPer: Integer;
begin
  if (Data = nil) then Exit;

  MatName := MatName + #0;
  Header.MType := Prec * 10;
  Data.Size(n,m);
  Header.MRows := n;
  Header.MCols := m;
  Header.ImagFlag := 0;
  Header.NameLen := Length(MatName);

  case Prec of
    DoublePrec: BytesPer := 8;
    SinglePrec: BytesPer := 4;
    else
      raise Exception.Create('Can save only in double or single precision');
  end;

  {$I-}
  BlockWrite(F, Header, SizeOf(Header), res);
  if res <> SizeOf(Header) then
  begin
    etError := etIOError;
    Exit;
  end;
  BlockWrite(F, MatName[1], Length(MatName), res);
  if res <> Length(MatName) then
  begin
    etError := etIOError;
    Exit;
  end;

  n := Header.MRows * BytesPer;
  GetMem(Arr, n);

  for j:=1 to Header.MCols do
  begin
    if etError = etOK then
    begin
      for i := 1 to Header.MRows do
      begin
        case Prec of
          DoublePrec: PDoubleArray(Arr)^[i] := Data.GetValue(i,j);
          SinglePrec: PSingleArray(Arr)^[i] := Data.GetValue(i,j);
        end;
      end;
      BlockWrite(F, Arr^, n, res);
      if res <> n then etError := etIOError;
    end;
  end;
  FreeMem(Arr, n);
end;

{ Reads the MatLab variable at byte position <Pos> from the file <FileName>
  and returns in <MatName> the name of the variable and in <Data> then data
  (as matrix). }
procedure MatlabFile_Read(FileName: String; var Pos: LongInt;
  var MatName: String; var Data: TMatrix);
const
  BufSize = 1600;
var
  F         : File;
  Header    : TMatlabHeader;
  Machine   : Integer;
  Prec      : Byte;
  HasCols   : Boolean;
  IsText    : Boolean;
  Z         : float;          { Matrixelement after conversion to float }
  Zd        : double;         { Matrixelement, as double precision value }
  Zs        : Single;         { Matrixelement, as single precision value }
  Zl        : LongInt;        { Matrixelement, as longint value }
  Zi        : SmallInt;       { Matrixelement, as Int value }
  Zw        : word;           { Matrixelement, as unsigned int value }
  BytesPer  : word;           { Amount of bytes needed by a matrix element }
  Count     : LongInt;        { Number of elements in the matrix }
  TotalBytes: LongInt;        { Number of bytes in the matrix }
  FileBytes : Int64;          { Size of the file, in bytes }
  k         : Integer;        { Index of byte in buffer }
  ReadBytes : word;           { Number of bytes to be read into buffer }
  Offs      : LongInt;        { Offset within the matrix on the file }
  i,j       : Integer;        { Indices for the matrix element }
  Buffer    : array[0..BufSize-1] of Byte;
  res       : word;
begin
  MatName := '';
  Data := nil;
  if FileName='' then
    Exit;

  Assign(F, FileName);
  {$I-}
  Reset(F, 1);
  Seek(F, Pos);
  BlockRead(F, Header, SizeOf(Header), res);
  if (IOResult <> 0) then etError := etIOError;
  if (etError <> etOK) or (res <> SizeOf(Header)) then
    Exit;

  Machine := CheckHeader(Header.MType);
  case Machine of
    INTEL:
      ;
    MOTOROLA:
      begin
        SwapBytes(Header.MType, 4);
        SwapBytes(Header.MRows, 4);
        SwapBytes(Header.MCols, 4);
        SwapBytes(Header.ImagFlag, 4);
        SwapBytes(Header.NameLen, 4);
      end;
    OTHER:
      Exit;
  end;

  BlockRead(F, MatName[1], Header.NameLen);
  SetLength(MatName, Header.NameLen-1);
//  MatName[0] := Chr(Header.NameLen-1);
  if Header.ImagFlag=1 then
    Exit;

  Prec := (Header.MType mod 100) div 10;
  HasCols := (Header.MType mod 1000) div 100 = 0;
  IsText := (Header.MType mod 10) = 1;
  case Prec of
    DoublePrec : BytesPer := 8;
    SinglePrec,
    Long       : BytesPer := 4;
    Int,
    UnsignedInt: BytesPer := 2;
  end;
  TotalBytes := BytesPer * Header.MRows * Header.MCols;
  Data := TMatrix.Create(Header.MRows, Header.MCols);

  if mError=mOutOfMemory then
  begin
    etError := etOutOfMemory;
    mResetError;
  end else
  if mError=mOK then
  begin
    i := 1;
    j := 1;
    Offs := 0;
    ReadBytes := BufSize;
    while (Offs < TotalBytes) and (etError = etOK) and(not EoF(F)) do
    begin
      if not IsText then
      begin
        if (Offs + BufSize) >= TotalBytes-1 then
          ReadBytes := TotalBytes - Offs;
        BlockRead(F, Buffer, ReadBytes, res);
        if IOResult <> 0 then
          etError := etIOError
        else
        if (res<>0) then
        begin
          k := 0;
          repeat
            case Prec of
              DoublePrec:
                begin
                  Move(Buffer[k], Zd, BytesPer);
                  if Machine = MOTOROLA then SwapBytes(Zd, 8);
                  Z := Zd;
                end;
              SinglePrec:
                begin
                  Move(Buffer[k], Zs, BytesPer);
                  if Machine = MOTOROLA then
                    SwapBytes(Zs, 4);
                  Z := Zs;
                end;
              Long:
                begin
                  Move(Buffer[k], Zl, BytesPer);
                  if Machine = MOTOROLA then SwapBytes(Zl, 4);
                  Z := 1.0*Zl;
                end;
              Int:
                begin
                  Move(Buffer[k], Zi, BytesPer);
                  if Machine = MOTOROLA then Zi := Swap(Zi);
                  Z := 1.0*Zi;
                end;
              UnsignedInt :
                begin
                  Move(Buffer[k], Zw, BytesPer);
                  if Machine = MOTOROLA then Zw := Swap(Zw);
                  Z := 1.0*Zw;
                end;
            end;
            inc(k, BytesPer);
            Data.PutValue(i, j, Z);
            if HasCols then
            begin
              inc(i);
              if (i > Header.MRows) then
              begin
                i := 1;
                inc(j);
              end;
            end else
            begin
              inc(j);
              if (j > Header.MCols) then
              begin
                j := 1;
                inc(i);
              end;
            end;
          until (k >= res);
        end;
      end;  { if not IsText }
      inc(Offs, ReadBytes);
    end; { while }
    if etError <> etIOError then
      Pos := FilePos(F);
    Close(F);
    if etError <> 0 then ClearMat(Data);
  end;
  {$I+}
end;

function MatlabFile_ReadInfo(FileName: String; var Pos: LongInt;
  var Info: TMatrix): Boolean;
var
  P: LongInt;
  name: String;
begin
  P := 0;
  MatlabFile_Read(FileName, P, Name, Info);
  if Info <> nil then
  begin
    Pos := P;
    Result := True;
  end else
    Result := false;
end;

function MatlabFile_Write(FileName: String; Pos: LongInt; MatName: String;
  Data: TMatrix): Boolean;
var
  F: File;
begin
  Result := False;
  if (FileName <> '') then
  begin
    etError := etOK;
    Assign(F, FileName);
    {$I-}
    Reset(F,1);
    if IOResult = 0 then
    begin
      Seek(F, Pos);
      if Data <> nil then
        SaveToFile(F, MatName, Data, SavePrecision);
      Close(F);
      Result := (etError=etOK);
    end else
      etError := etIOError;
    {$I+}
  end;
end;

function MatlabFile_Create(FileName, MatName: String; Data: TMatrix): Boolean;
var
  F: File;
begin
  Result := false;
  if (FileName <> '') then
  begin
    etError := etOK;
    Assign(F, FileName);
    {$I-}
    Rewrite(F, 1);
    if IOResult = 0 then
    begin
      if Data <> NIL then
        SaveToFile(F, MatName, Data, SavePrecision);
      Close(F);
      MatlabFile_Create := etError=etOK;
    end else
      etError := etIOError;
    {$I+}
  end;
end;

function MatlabFile_Append(FileName, MatName: String; Data: TMatrix): Boolean;
var
  F: File;
begin
  Result := false;
  if (Data <> nil) and (FileName <> '') and (MatName <> '') then
  begin
    Assign(F, FileName);
    {$I-}
    Reset(F, 1);
    if IOResult = 0 then
    begin
      Seek(F, FileSize(F));
      SaveToFile(F, MatName, Data, SavePrecision);
      Close(F);
      Result := etError=etOK;
    end else
      etError := etIOError;
    {$I+}
  end;
end;

function MatlabFile_GetVarName(FileName: String; var Pos: LongInt): String;
var
  MatName: String;
  F: File;
  res: Word;
  Header: TMatlabHeader;
  Machine: Integer;
  Prec: Byte;
  HasCols: Boolean;
  IsText: Boolean;
  BytesPer: Word;             { Bytes needed by a matrix element }
  TotalBytes: LongInt;        { Bytes in the matrix }
begin
  Result := '';
  MatName := '';
  if FileName <> '' then
  begin
    Assign(F, FileName);
    {$I-}
    Reset(F, 1);
    Seek(F, Pos);
    BlockRead(F, Header, SizeOf(Header), res);
    if IOResult <> 0 then
    begin
      etError := etIOError;
      Exit;
    end;
    if (res = SizeOf(Header)) then
    begin
      Machine := CheckHeader(Header.MType);
      if Machine <> OTHER then
      begin
        if Machine = MOTOROLA then
        begin
          SwapBytes(Header.MType, 4);
          SwapBytes(Header.MRows, 4);
          SwapBytes(Header.MCols, 4);
          SwapBytes(Header.ImagFlag, 4);
          SwapBytes(Header.NameLen, 4);
        end;
        BlockRead(F, MatName[1], Header.NameLen);
        SetLength(MatName, Header.NameLen-1);
        //MatName[0] := Chr(Header.NameLen-1);
        Prec := (Header.MType mod 100) div 10;
        HasCols := (Header.MType mod 1000) div 100 = 0;
        IsText := (Header.MType mod 10) = 1;
        case Prec of
          DoublePrec : BytesPer := 8;
          SinglePrec,
          Long       : BytesPer := 4;
          Int,
          UnsignedInt: BytesPer := 2;
        end;
        TotalBytes := BytesPer * Header.MRows * Header.MCols;
        Pos := FilePos(F) + TotalBytes;
      end;
    end;
    Close(F);
  end;
  Result := MatName;
  {$I+}
end;

end.

