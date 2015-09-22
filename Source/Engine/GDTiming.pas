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
unit GDTiming;

{$MODE Delphi}

{******************************************************************************}
{* Holds timing classse                                                       *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  Windows,
  SysUtils;

type

{******************************************************************************}
{* timing class                                                               *}
{******************************************************************************}

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

{******************************************************************************}
{* Create timing class                                                        *}
{******************************************************************************}

constructor TGDTiming.Create();
begin
  inherited;
  FDemoStart   := GetTickCount();
  FElapsedTime := 0;
  FLastTime    := 0;
  FFrameTime   := 0;
  FStart       := 0;
  FStop        := 0;
end;

{******************************************************************************}
{* Destroy timing class                                                       *}
{******************************************************************************}

destructor TGDTiming.Destroy(); 
begin
  inherited;
end;

{******************************************************************************}
{* Calculate frametime                                                        *}
{******************************************************************************}

procedure TGDTiming.CalculateFrameTime();
begin
  FLastTime    := FElapsedTime;
  FElapsedTime := GetTickCount() - FDemoStart;
  FElapsedTime := (FLastTime + FElapsedTime) div 2;
  FFrameTime   := FELapsedTime - FLastTime;
end;

{******************************************************************************}
{* Start timing                                                               *}
{******************************************************************************}

Procedure TGDTiming.Start();
begin
  FStart := GetTickCount();
end;

{******************************************************************************}
{* Stop timing                                                                *}
{******************************************************************************}

Procedure TGDTiming.Stop();
begin
  FStop := GetTickCount();
end;

{******************************************************************************}
{* Returns the time in seconds                                                *}
{******************************************************************************}

Function TGDTiming.TimeInSeconds() : String;
begin
  result := FormatFloat('0.###' ,(Fstop - FStart)/1000);
end;

{******************************************************************************}
{* Returns the time in miliseconds                                            *}
{******************************************************************************}

Function TGDTiming.TimeInMilliSeconds() : String;
begin
  result := FormatFloat('0.###',((Fstop - FStart)));
end;

{******************************************************************************}
{* Get time tick                                                              *}
{******************************************************************************}

function TGDTiming.GetTime(): integer;
begin
  result := GetTickCount();
end;

end.
