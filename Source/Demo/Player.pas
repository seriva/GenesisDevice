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
unit Player;

{$MODE Delphi}

{******************************************************************************}
{* This units hold the playerobject for the game                              *}
{******************************************************************************}

interface

uses
  IniFiles,
  GDInterface,
  GDTypes,
  SysUtils;

type

{******************************************************************************}
{* Player class                                                               *}
{******************************************************************************}

  TPlayer = class
  private
    FMovementSpeed : Double;
    FMovementMultiplier : Integer;
    FSinkSpeed : Double;
    FSinkMultiplier : Integer;
    FRunning        : boolean;
  public
    property  MovementSpeed : Double read FMovementSpeed write FMovementSpeed;
    property  MovementMultiplier : Integer read FMovementMultiplier write FMovementMultiplier;
    property  SinkSpeed : Double read FSinkSpeed write FSinkSpeed;
    property  SinkMultiplier : Integer read FSinkMultiplier write FSinkMultiplier;
    property  Running : Boolean read FRunning write FRunning;

    Constructor Create();
    Destructor  Destroy(); override;

    procedure InitializePlayer( aIniPath : String );
    procedure ClearPlayer();

    procedure MoveForward();
    procedure MoveBackWard();
    procedure MoveLeft();
    procedure MoveRight();

    Procedure Run();
    Procedure Walk();

    Function  PlayerInWater() : boolean;
    Function  PlayerUnderWater() : boolean;
    procedure DoPlayerCollisionAndPhysics();
  end;

implementation

uses
  Main;

{******************************************************************************}
{* Create player class                                                        *}
{******************************************************************************}

Constructor TPlayer.Create();
begin
  inherited;
end;

{******************************************************************************}
{* Destroy player class                                                       *}
{******************************************************************************}

Destructor TPlayer.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Init the player                                                            *}
{******************************************************************************}

procedure TPlayer.InitializePlayer( aIniPath : String );
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( aIniPath );
  FMovementMultiplier := 400;
  FSinkMultiplier     := 50;
  FreeAndNil( iIniFile );
end;

{******************************************************************************}
{* Clear the player                                                           *}
{******************************************************************************}

procedure TPlayer.ClearPlayer();
begin
  FMovementMultiplier := 400;
  FSinkMultiplier     := 50;
end;

{******************************************************************************}
{*  Move forwards                                                             *}
{******************************************************************************}

procedure TPlayer.MoveForward();
begin
  if Not(Intro.FRenderIntroText) and Not(GamePause) then
    gdCameraMove( MovementSpeed );
end;

{******************************************************************************}
{* Move backwards                                                             *}
{******************************************************************************}

procedure TPlayer.MoveBackWard();
begin
  if Not(Intro.FRenderIntroText) and Not(GamePause) then
    gdCameraMove( -MovementSpeed );
end;

{******************************************************************************}
{* Move left                                                                  *}
{******************************************************************************}

procedure TPlayer.MoveLeft();
begin
  if Not(Intro.FRenderIntroText) and Not(GamePause) then
    gdCameraStrafe(MovementSpeed);
end;

{******************************************************************************}
{* Move right                                                                 *}
{******************************************************************************}

procedure TPlayer.MoveRight();
begin
  if Not(Intro.FRenderIntroText) and Not(GamePause) then
    gdCameraStrafe(-MovementSpeed);
end;

{******************************************************************************}
{* Set player running mode                                                    *}
{******************************************************************************}

Procedure TPlayer.Run();
begin
  FRunning := true;
  if PlayerInWater() then
    FMovementMultiplier := 600
  else
    FMovementMultiplier := 800;

  if not(Clip) then FMovementMultiplier := 2000;
end;

{******************************************************************************}
{* Set player walking mode                                                    *}
{******************************************************************************}

Procedure TPlayer.Walk();
begin
  FRunning := false;
  if PlayerInWater() then
    FMovementMultiplier := 400
  else
    FMovementMultiplier := 600;

  if not(Clip) then FMovementMultiplier := 2000;
end;

{******************************************************************************}
{* Check if the player is in water                                            *}
{******************************************************************************}

Function TPlayer.PlayerInWater() : boolean;
var
  aV : TGDVectorRecord;
begin
  aV := gdCameraGetPosition();
  if aV.Y < gdWaterHeight() + 100 then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Check if the under water                                                   *}
{******************************************************************************}

Function TPlayer.PlayerUnderWater() : boolean;
var
  aV : TGDVectorRecord;
begin
  aV := gdCameraGetPosition();
  if aV.Y < gdWaterHeight() then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Do player collision and physics                                            *}
{******************************************************************************}

procedure TPlayer.DoPlayerCollisionAndPhysics();
var
  iTerrainHeight : Double;
  aV : TGDVectorRecord;
begin
  FMovementSpeed := FMovementMultiplier * gdRenderSystemGetFrameTime() / 1000;
  FSinkSpeed     := FSinkMultiplier * gdRenderSystemGetFrameTime() / 1000;

  If Not(Clip) then exit;
  IF GamePause then exit;

  if PlayerInWater() then
  begin
    aV := gdCameraGetPosition();
    aV.Y := aV.Y - FSinkSpeed;
    gdCameraSetPosition(aV.X, aV.Y, aV.Z);
  end;

  aV := gdCameraGetPosition();
  iTerrainHeight := gdTerrainHeight( aV.X, aV.Z  );

  if iTerrainHeight > gdWaterHeight() then
  begin
    aV.Y := iTerrainHeight + 256;
    gdCameraSetPosition( aV.X, aV.Y, aV.z );
    exit;
  end
  else
  begin
    if iTerrainHeight + 170 > gdWaterHeight() then
    begin
      aV.Y := iTerrainHeight + 256;
      gdCameraSetPosition( aV.X, aV.Y, aV.z );
      exit;
    end;

    if aV.Y < iTerrainHeight+170 then
    begin
      aV.Y := iTerrainHeight+170;
      gdCameraSetPosition( aV.X, aV.Y, aV.z );
    end
    else
      if aV.Y > gdWaterHeight()+96 then
      begin
        aV.Y := gdWaterHeight()+96;
        gdCameraSetPosition( aV.X, aV.Y, aV.z );
      end;
  end;
End;

end.
