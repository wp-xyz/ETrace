program et_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, etMathTests, etSampleTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

