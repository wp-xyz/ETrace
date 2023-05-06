unit ed_sim;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  et_Global, et_Math, et_Objects;

type
  TSimulation = class
  private
    FElectronSource: TElectronSource;
    FSample: TSample;
    FAnalyzer: TAnalyzer;
  protected
    procedure DoneObjs;
  public
    destructor Destroy; override;
  end;

implementation

destructor TSimulation.Destroy;
begin
  DoneObjs;
  inherited;
end;

procedure TSimulation.DoneObjs;
begin
  FreeAndNil(FElectronSource);
  FreeAndNil(FAnalyzer);
  FreeAndNil(FSample);
end;

end.

