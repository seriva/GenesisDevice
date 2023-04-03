program Demo;

{$MODE Delphi}

uses
  // heaptrc,
  SysUtils,
  Forms, 
  Interfaces,
  uConfiguration in 'uConfiguration.pas' {ConfigurationForm},
  uGDEngine,
  uMain in 'Main.pas';

label start;

begin
  // DeleteFile('leaks.txt');
  // SetHeapTraceOutput('leaks.txt');
  Application.Initialize;
  Application.Scaled:=true;
  start:
  if SettingsExecute() then
  begin
    GDEngine.Init(@InitGame);
    While not(GDEngine.Done) do
      GDEngine.Loop(@GameLoop);
    GDEngine.Clear(@ClearGame);
    goto start;
  end;
end.
