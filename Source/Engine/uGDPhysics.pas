unit uGDPhysics;

{$mode delphi}

interface

uses
  SysUtils,
  newton,
  uGDConstants;

type
  TGDPhysics = Class
  private
    FInitialized : boolean;
    FWorld : NewtonWorld;
  public
    property Initialized : boolean read FInitialized;

    constructor Create();
    destructor  Destroy(); override;

    procedure Update();
  end;

implementation

uses
  uGDEngine;

constructor TGDPhysics.Create();
var
  iI : Integer;
begin
  Inherited;
  GDConsole.Write('.....Initializing physics');
  try
    FInitialized := false;
    GDTiming.Start();

    //check newton version
    iI := NewtonWorldGetVersion();
    GDConsole.Write('  Version: ' + IntToStr(iI));
    if (iI <> MRS_NEWTON_VERSION) then
      Raise Exception.Create('Newton version ' + IntToStr(MRS_NEWTON_VERSION) + ' required.');

    //create the newton world
    FWorld := NewtonCreate();

    GDTiming.Stop();
    GDConsole.Write('.....Done initializing physics (' + GDTiming.TimeInSeconds + ' Sec)');
    FInitialized := true;
  except
    on E: Exception do
    begin
      FInitialized := false;
      GDConsole.Write('Failed to initialize Physics: ' + E.Message);
    end;
  end;
end;


destructor TGDPhysics.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  GDConsole.Write('Shutting down physics...');
  try
    iResult := true;
    NewtonDestroy(FWorld);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  GDConsole.WriteOkFail(iResult, iError);
end;


procedure TGDPhysics.Update();
begin
  if not(FInitialized) then exit;
end;

end.

