{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine.                             *
*                                                                              *
*  The Genesis Device Engine is free software: you can redistribute            *
*  it and/or modify it under the terms of the GNU Lesser General Public        *
*  License as published by the Free Software Foundation, either version 3      *
*  of the License, or any later version.                                       *
*                                                                              *
*  The Genesis Device Engine is distributed in the hope that                   *
*  it will be useful, but WITHOUT ANY WARRANTY; without even the               *
*  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
*  See the GNU Lesser General Public License for more details.                 *
*                                                                              *
*  You should have received a copy of the GNU General Public License           *
*  along with Genesis Device.  If not, see <http://www.gnu.org/licenses/>.     *
*                                                                              *
*******************************************************************************}   
unit GDStatistics;

{$MODE Delphi}

{******************************************************************************}
{* This units holds classes for statistics                                    *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  SDL2,
  dglOpenGL,
  GDConstants,
  GDGUI;

type

{******************************************************************************}
{* Statistics class                                                           *}
{******************************************************************************}

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
  GDEngine;

{******************************************************************************}
{* Create                                                                     *}
{******************************************************************************}

constructor TGDStatistics.Create();
begin
  FLastTime := Engine.Timing.GetTime()+1000;
end;

{******************************************************************************}
{* Start and stop procedures for gathering time information                   *}
{******************************************************************************}

procedure TGDStatistics.FrameStart();
begin
  Engine.Timing.Start();
end;

procedure TGDStatistics.FrameStop();
begin
  Engine.Timing.Stop();
  FFrameTimeSlice := Engine.Timing.TimeInMilliSeconds();
end;


{******************************************************************************}
{* Update the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Update();
begin
  //calculate fps
  FrameCount   := FrameCount + 1;
  if SDL_TICKS_PASSED(Engine.Timing.GetTime(), FLastTime) then
  begin
    FLastTime := Engine.Timing.GetTime()+1000;
    FpsCount   := FrameCount;
    FrameCount := 0;
    FFPSTime   := 0;
  end;
end;

{******************************************************************************}
{* Render the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Render();
begin
  RenderFlatQuad(20, 20, 375, 195);
  Engine.Renderer.RenderState( RS_TEXTS );
  with Engine.GUI do
  begin
    Font.Color := Engine.GUI.FontColor.Copy();
    Font.Render(25,215-32,0.4,'FPS');
    Font.Render(25,190-32,0.4,'TRIS');
    Font.Render(25,165-32,0.4,'VOBJ');
    Font.Render(25,140-32,0.4,'FTIME');
    Font.Render(25,115-32,0.4,'X');
    Font.Render(25,90-32,0.4,'Y');
    Font.Render(25,65-32,0.4,'Z');
    Font.Render(150,215-32,0.4,': ' + IntToStr(FFpsCount));
    Font.Render(150,190-32,0.4,': ' + IntToStr(Engine.Map.TriangleCount()));
    Font.Render(150,165-32,0.4,': ' + IntToStr(Engine.Map.ObjectCount()));
    Font.Render(150,140-32,0.4,': ' + FFrameTimeSlice + ' ms' );;
    Font.Render(150,115-32,0.4,': ' + IntToStr( Round(Engine.Camera.Position.X) ));
    Font.Render(150,90-32,0.4,': ' + IntToStr( Round(Engine.Camera.Position.Y) ));
    Font.Render(150,65-32,0.4,': ' + IntToStr( Round(Engine.Camera.Position.Z) ));
  end;
  glDisable(GL_BLEND);
end;

end.
