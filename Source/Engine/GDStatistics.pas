{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
*                         luukvanvenrooij84@gmail.com                          *
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
  LCLIntf, LCLType,
  SysUtils,
  MMSystem,
  dglOpenGL,
  GDRenderer,
  GDCellManager,
  GDSkyDome,
  GDFont,
  GDConstants,
  GDCamera,
  GDTiming;

type

{******************************************************************************}
{* Statistics class                                                           *}
{******************************************************************************}

  TGDStatistics = class(TObject)
  private
    FFrameTiming      : TGDPerformanceTiming;
    FUpdateTimer      : Integer;
    FFrameCount       : integer;
    FFpsCount         : Integer;
    FTriangleCount    : integer;
    FOBJCount         : integer;
    FFrameTimeSlice   : String;
  public
    property FrameCount       : integer read FFrameCount write FFrameCount;
    property FpsCount         : integer read FFpsCount write FFpsCount;
    property TriangleCount    : integer read FTriangleCount write FTriangleCount;
    property OBJCount         : integer read FOBJCount write FOBJCount;
    property FrameTimeSlice   : string read FFrameTimeSlice write FFrameTimeSlice;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitStatistics();

    procedure FrameStart();
    procedure FrameStop();

    procedure Update();
    procedure Render();
  end;

var
  Statistics : TGDStatistics;

  procedure UpdateStatisticsCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;

implementation

{******************************************************************************}
{* Create the statistic class                                                 *}
{******************************************************************************}

constructor TGDStatistics.Create();
begin
  FFrameTiming   := TGDPerformanceTiming.Create();
  FUpdateTimer   := TimeSetEvent(S_UPDATE_TIME, 0, @UpdateStatisticsCallBack, 0, TIME_PERIODIC);
  InitStatistics();
end;

{******************************************************************************}
{* Destroy the statistic class                                                *}
{******************************************************************************}

destructor  TGDStatistics.Destroy();
begin
  TimeKillEvent(FUpdateTimer);
  FreeAndNil(FFrameTiming);
end;

{******************************************************************************}
{* Init the statistic class                                                   *}
{******************************************************************************}

procedure TGDStatistics.InitStatistics();
begin
  FFrameCount       := 0;
  FFpsCount         := 0;
  FTriangleCount    := 0;
  FOBJCount         := 0;
  FFrameTimeSlice   := '0.000';
end;

{******************************************************************************}
{* Start and stop procedures for gathering time information                   *}
{******************************************************************************}

procedure TGDStatistics.FrameStart();
begin
  FFrameTiming.Start();
end;

procedure TGDStatistics.FrameStop();
begin
  FFrameTiming.Stop();
  FFrameTimeSlice := FFrameTiming.TimeInMilliSeconds();
end;


{******************************************************************************}
{* Update the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Update();
begin
  Statistics.FpsCount   := Statistics.FrameCount;
  Statistics.FrameCount := 0;
end;

{******************************************************************************}
{* Render the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Render();
begin
  Renderer.RenderState( RS_COLOR );

  FTriangleCount := 0;
  FTriangleCount := SkyDome.TriangleCount +
                    CellManager.TriangleCount;

  FOBJCount := 0;
  FOBJCount := CellManager.VisibleCells.Count +
               CellManager.VisibleWaterCells.Count;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glColor4f(0.4,0.4,0.4,0.5);
  glBegin(GL_QUADS);
    glVertex2f(20, 215);
    glVertex2f(20, 25);
    glVertex2f(400, 25);
    glVertex2f(400, 215);
  glEnd;
  glColor4f(1,1,1,1);
  glBegin(GL_LINE_LOOP);
    glVertex2f(400, 25);
    glVertex2f(400, 215);
    glVertex2f(20, 215);
    glVertex2f(20, 25);
  glEnd;

  Renderer.RenderState( RS_TEXTS );
  SystemFont.Render(25,215,1,'FPS');
  SystemFont.Render(25,190,1,'TRIS');
  SystemFont.Render(25,165,1,'VOBJ');
  SystemFont.Render(25,140,1,'FTIME');
  SystemFont.Render(25,115,1,'X');
  SystemFont.Render(25,90,1,'Y');
  SystemFont.Render(25,65,1,'Z');
  SystemFont.Render(150,215,1,': ' + IntToStr(FFpsCount));
  SystemFont.Render(150,190,1,': ' + IntToStr(FTriangleCount));
  SystemFont.Render(150,165,1,': ' + IntToStr(FOBJCount));
  SystemFont.Render(150,140,1,': ' + FFrameTimeSlice + ' ms' );;
  SystemFont.Render(150,115,1,': ' + IntToStr( Round(Camera.Position.X) ));
  SystemFont.Render(150,90,1,': ' + IntToStr( Round(Camera.Position.Y) ));
  SystemFont.Render(150,65,1,': ' + IntToStr( Round(Camera.Position.Z) ));
  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Update Statistics Callback                                                 *}
{******************************************************************************}

procedure UpdateStatisticsCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
begin
  Statistics.Update();
end;

end.
