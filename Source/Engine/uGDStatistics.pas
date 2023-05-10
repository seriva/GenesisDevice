 unit uGDStatistics;

{$MODE Delphi}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  SDL2,
  dglOpenGL,
  uGDGUI,
  uGDConstants;

type
  TGDStatistics = class(TObject)
  private
    FFPSTime          : Integer;
    FLastTime         : Integer;
    FFrameCount       : integer;
    FFpsCount         : Integer;
    FFrameTimeSlice   : String;
  public
    property FrameCount       : integer read FFrameCount write FFrameCount;
    property FpsCount         : integer read FFpsCount write FFpsCount;
    property FrameTimeSlice   : string read FFrameTimeSlice write FFrameTimeSlice;

    constructor Create();

    procedure FrameStart();
    procedure FrameStop();

    procedure Update();
    procedure Render();
  end;

implementation

uses
  uGDEngine;

constructor TGDStatistics.Create();
begin
  FLastTime := GDTiming.GetTime()+1000;
end;


procedure TGDStatistics.FrameStart();
begin
  GDTiming.Start();
end;


procedure TGDStatistics.FrameStop();
begin
  GDTiming.Stop();
  FFrameTimeSlice := GDTiming.TimeInMilliSeconds();
end;


procedure TGDStatistics.Update();
begin
  //calculate fps
  FrameCount   := FrameCount + 1;
  if SDL_TICKS_PASSED(GDTiming.GetTime(), FLastTime) then
  begin
    FLastTime := GDTiming.GetTime()+1000;
    FpsCount   := FrameCount;
    FrameCount := 0;
    FFPSTime   := 0;
  end;
end;


procedure TGDStatistics.Render();
begin
  RenderFlatQuad(20, 20, 375, 195);
  GDRenderer.RenderState( RS_TEXTS );

  GDGUI.DefaultFont.Color := GDGUI.FontColor.Copy();
  GDGUI.DefaultFont.Render(25,215-32,0.4,'FPS');
  GDGUI.DefaultFont.Render(25,190-32,0.4,'TRIS');
  GDGUI.DefaultFont.Render(25,165-32,0.4,'VOBJ');
  GDGUI.DefaultFont.Render(25,140-32,0.4,'FTIME');
  GDGUI.DefaultFont.Render(25,115-32,0.4,'X');
  GDGUI.DefaultFont.Render(25,90-32,0.4,'Y');
  GDGUI.DefaultFont.Render(25,65-32,0.4,'Z');
  GDGUI.DefaultFont.Render(150,215-32,0.4,': ' + IntToStr(FFpsCount));
  GDGUI.DefaultFont.Render(150,190-32,0.4,': ' + IntToStr(GDMap.TriangleCount()));
  GDGUI.DefaultFont.Render(150,165-32,0.4,': ' + IntToStr(GDMap.ObjectCount()));
  GDGUI.DefaultFont.Render(150,140-32,0.4,': ' + FFrameTimeSlice + ' ms' );;
  GDGUI.DefaultFont.Render(150,115-32,0.4,': ' + IntToStr( Round(GDCamera.Position.X) ));
  GDGUI.DefaultFont.Render(150,90-32,0.4,': ' + IntToStr( Round(GDCamera.Position.Y) ));
  GDGUI.DefaultFont.Render(150,65-32,0.4,': ' + IntToStr( Round(GDCamera.Position.Z) ));

  glDisable(GL_BLEND);
end;

end.
