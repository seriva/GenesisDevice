{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
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
unit uGDPhysics;

{$mode delphi}

interface

uses
  SysUtils,
  newton,
  uGDConstants;

{******************************************************************************}
{* Physics class                                                              *}
{******************************************************************************}

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

{******************************************************************************}
{* Create physics class                                                       *}
{******************************************************************************}

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

{******************************************************************************}
{* Destroy physics class                                                      *}
{******************************************************************************}

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

{******************************************************************************}
{* Update the sound engine                                                    *}
{******************************************************************************}

procedure TGDPhysics.Update();
begin
  if not(FInitialized) then exit;
end;

end.

