 unit uGDTiming;

{$MODE Delphi}

interface

uses
  sdl2,
  SysUtils;

type
  TGDTiming = class
  private
    FDemoStart   : Integer;
    FElapsedTime : Integer;
    FFrameTime   : Integer;
    FLastTime    : Integer;
    FStart       : Integer;
    FStop        : Integer;
  public
    property ElapsedTime : Integer read FElapsedTime write FElapsedTime;
    property FrameTime   : Integer read FFrameTime write FFrameTime;

    constructor Create();
    destructor  Destroy(); override;

    procedure CalculateFrameTime();

    Procedure Start();
    Procedure Stop();
    Function  TimeInSeconds() : String;
    Function  TimeInMilliSeconds() : String;
    function  GetTime(): integer;
  end;

implementation

constructor TGDTiming.Create();
begin
  inherited;
  FDemoStart   := SDL_GetTicks();
  FElapsedTime := 0;
  FLastTime    := 0;
  FFrameTime   := 0;
  FStart       := 0;
  FStop        := 0;
end;


destructor TGDTiming.Destroy(); 
begin
  inherited;
end;


procedure TGDTiming.CalculateFrameTime();
begin
  FLastTime    := FElapsedTime;
  FElapsedTime := SDL_GetTicks() - FDemoStart;
  FElapsedTime := (FLastTime + FElapsedTime) div 2;
  FFrameTime   := FELapsedTime - FLastTime;
end;


Procedure TGDTiming.Start();
begin
  FStart := SDL_GetTicks();
end;


Procedure TGDTiming.Stop();
begin
  FStop := SDL_GetTicks();
end;


Function TGDTiming.TimeInSeconds() : String;
begin
  result := FormatFloat('0.###' ,(Fstop - FStart)/1000);
end;


Function TGDTiming.TimeInMilliSeconds() : String;
begin
  result := FormatFloat('0.###',((Fstop - FStart)));
end;


function TGDTiming.GetTime(): integer;
begin
  result := SDL_GetTicks();
end;

end.
