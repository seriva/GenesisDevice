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
{* performancetiming class                                                    *}
{******************************************************************************}

  TGDPerformanceTiming = class
  private
    FStart : Int64;
    Fstop  : Int64;
    FFreq  : Int64;
  public
    constructor Create();
    destructor  Destroy(); override;

    Procedure Start();
    Procedure Stop();
    Function  TimeInSeconds() : String;
    Function  TimeInMilliSeconds() : String;
  end;

{******************************************************************************}
{* timing class                                                               *}
{******************************************************************************}

  TGDTiming = class
  private
    FDemoStart   : Integer;
    FElapsedTime : Integer;
    FFrameTime   : Integer;
    FLastTime    : Integer;
  public
    property DemoStart   : Integer read FDemoStart write FDemoStart;
    property ElapsedTime : Integer read FElapsedTime write FElapsedTime;
    property FrameTime   : Integer read FFrameTime write FFrameTime;
    property LastTime    : Integer read FLastTime write FLastTime;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitTiming();
    procedure CalculateFrameTime();
  end;
  
var
  Timer  : TGDPerformanceTiming;
  Timing : TGDTiming;

implementation

{******************************************************************************}
{* Create performancetiming class                                             *}
{******************************************************************************}

constructor TGDPerformanceTiming.Create();
begin
  QueryPerformanceFrequency(FFreq);
end;

{******************************************************************************}
{* Destroy performancetiming class                                            *}
{******************************************************************************}

destructor  TGDPerformanceTiming.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Start performancetiming                                                    *}
{******************************************************************************}

Procedure TGDPerformanceTiming.Start();
begin
  QueryPerformanceCounter(FStart);
end;

{******************************************************************************}
{* Stop performancetiming                                                     *}
{******************************************************************************}

Procedure TGDPerformanceTiming.Stop();
begin
  QueryPerformanceCounter(Fstop);
end;

{******************************************************************************}
{* Returns the time in seconds                                                *}
{******************************************************************************}

Function TGDPerformanceTiming.TimeInSeconds() : String;
begin
  result := FormatFloat('0.###' ,(Fstop - FStart) / FFreq);
end;

{******************************************************************************}
{* Returns the time in miliseconds                                            *}
{******************************************************************************}

Function TGDPerformanceTiming.TimeInMilliSeconds() : String;
begin
  result := FormatFloat('0.###',(1000 * (Fstop - FStart) / FFreq));
end;

{******************************************************************************}
{* Create timing class                                                        *}
{******************************************************************************}

constructor TGDTiming.Create();
begin
  inherited;
end;

{******************************************************************************}
{* Destroy timing class                                                       *}
{******************************************************************************}

destructor TGDTiming.Destroy(); 
begin
  inherited;
end;

{******************************************************************************}
{* Init timing class                                                          *}
{******************************************************************************}

procedure TGDTiming.InitTiming();
begin
  FDemoStart   := GetTickCount();
  FElapsedTime := 0;
  FLastTime    := 0;
  FFrameTime   := 0;
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

end.
