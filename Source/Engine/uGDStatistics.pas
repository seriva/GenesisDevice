{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine                              *
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
unit uGDStatistics;

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
  uGDGUI,
  uGDConstants;

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
  uGDEngine;

{******************************************************************************}
{* Create                                                                     *}
{******************************************************************************}

constructor TGDStatistics.Create();
begin
  FLastTime := GDTiming.GetTime()+1000;
end;

{******************************************************************************}
{* Start and stop procedures for gathering time information                   *}
{******************************************************************************}

procedure TGDStatistics.FrameStart();
begin
  GDTiming.Start();
end;

procedure TGDStatistics.FrameStop();
begin
  GDTiming.Stop();
  FFrameTimeSlice := GDTiming.TimeInMilliSeconds();
end;


{******************************************************************************}
{* Update the statistic                                                       *}
{******************************************************************************}

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

{******************************************************************************}
{* Render the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Render();
begin
  RenderFlatQuad(20, 20, 375, 195);
  GDRenderer.RenderState( RS_TEXTS );

  GDGUI.Font.Color := GDGUI.FontColor.Copy();
  GDGUI.Font.Render(25,215-32,0.4,'FPS');
  GDGUI.Font.Render(25,190-32,0.4,'TRIS');
  GDGUI.Font.Render(25,165-32,0.4,'VOBJ');
  GDGUI.Font.Render(25,140-32,0.4,'FTIME');
  GDGUI.Font.Render(25,115-32,0.4,'X');
  GDGUI.Font.Render(25,90-32,0.4,'Y');
  GDGUI.Font.Render(25,65-32,0.4,'Z');
  GDGUI.Font.Render(150,215-32,0.4,': ' + IntToStr(FFpsCount));
  GDGUI.Font.Render(150,190-32,0.4,': ' + IntToStr(GDMap.TriangleCount()));
  GDGUI.Font.Render(150,165-32,0.4,': ' + IntToStr(GDMap.ObjectCount()));
  GDGUI.Font.Render(150,140-32,0.4,': ' + FFrameTimeSlice + ' ms' );;
  GDGUI.Font.Render(150,115-32,0.4,': ' + IntToStr( Round(GDCamera.Position.X) ));
  GDGUI.Font.Render(150,90-32,0.4,': ' + IntToStr( Round(GDCamera.Position.Y) ));
  GDGUI.Font.Render(150,65-32,0.4,': ' + IntToStr( Round(GDCamera.Position.Z) ));

  glDisable(GL_BLEND);
end;

end.
