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
  dglOpenGL,
  GDRenderer,
  GDCellManager,
  GDSkyDome,
  GDConstants,
  GDCamera,
  GDGUI,
  GDTiming;

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

implementation

{******************************************************************************}
{* Create the statistic class                                                 *}
{******************************************************************************}

constructor TGDStatistics.Create();
begin
  InitStatistics();
end;

{******************************************************************************}
{* Destroy the statistic class                                                *}
{******************************************************************************}

destructor  TGDStatistics.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Init the statistic class                                                   *}
{******************************************************************************}

procedure TGDStatistics.InitStatistics();
begin
  FLastTime         := Timing.GetTime();
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
  Timing.Start();
end;

procedure TGDStatistics.FrameStop();
begin
  Timing.Stop();
  FFrameTimeSlice := Timing.TimeInMilliSeconds();
end;


{******************************************************************************}
{* Update the statistic                                                       *}
{******************************************************************************}

procedure TGDStatistics.Update();
var
  iDT, iTime : Integer;
begin
  //calculate fps
  Statistics.FrameCount := Statistics.FrameCount + 1;
  iTime        := Timing.GetTime();
  iDT          := iTime - FLastTime;
  FLastTime    := iTime;
  FFPSTime  := FFPSTime + iDT;
  if (FFPSTime >= 1000) then
  begin
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
  FTriangleCount := SkyDome.TriangleCount + CellManager.TriangleCount;
  FOBJCount := CellManager.VisibleCells.Count + CellManager.VisibleWaterCells.Count;

  Renderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4fv(GUI.FillColor.ArrayPointer());
  glBegin(GL_QUADS);
    glVertex2f(20, 215);
    glVertex2f(20, 25);
    glVertex2f(400, 25);
    glVertex2f(400, 215);
  glEnd;
  glColor4fv(GUI.OutlineColor.ArrayPointer());
  glBegin(GL_LINE_LOOP);
    glVertex2f(400, 25);
    glVertex2f(400, 215);
    glVertex2f(20, 215);
    glVertex2f(20, 25);
  glEnd;

  Renderer.RenderState( RS_TEXTS );
  GUI.Font.Color := GUI.FontColor.Copy();
  GUI.Font.Render(25,215-32,0.4,'FPS');
  GUI.Font.Render(25,190-32,0.4,'TRIS');
  GUI.Font.Render(25,165-32,0.4,'VOBJ');
  GUI.Font.Render(25,140-32,0.4,'FTIME');
  GUI.Font.Render(25,115-32,0.4,'X');
  GUI.Font.Render(25,90-32,0.4,'Y');
  GUI.Font.Render(25,65-32,0.4,'Z');
  GUI.Font.Render(150,215-32,0.4,': ' + IntToStr(FFpsCount));
  GUI.Font.Render(150,190-32,0.4,': ' + IntToStr(FTriangleCount));
  GUI.Font.Render(150,165-32,0.4,': ' + IntToStr(FOBJCount));
  GUI.Font.Render(150,140-32,0.4,': ' + FFrameTimeSlice + ' ms' );;
  GUI.Font.Render(150,115-32,0.4,': ' + IntToStr( Round(Camera.Position.X) ));
  GUI.Font.Render(150,90-32,0.4,': ' + IntToStr( Round(Camera.Position.Y) ));
  GUI.Font.Render(150,65-32,0.4,': ' + IntToStr( Round(Camera.Position.Z) ));
  glDisable(GL_BLEND);
end;

end.
