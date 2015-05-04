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
unit Sounds;

{$MODE Delphi}

{******************************************************************************}
{* This units manages the ingame sounds                                       *}
{******************************************************************************}


interface

uses
  IniFiles,
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDInterface;

type

{******************************************************************************}
{* The sounds class                                                           *}
{******************************************************************************}

  TSounds = class
  private
  public
    FAmbientId    : pointer;
    FUnderWaterId : pointer;
    FMusicId      : pointer;

    Constructor Create();
    Destructor  Destroy(); override;

    procedure InitializeSounds( aIniPath : String );
    procedure ClearSounds();
  end;

implementation

{******************************************************************************}
{* Create the sound class
{******************************************************************************}

Constructor TSounds.Create();
begin
  ClearSounds();
end;

{******************************************************************************}
{* Destroy the sound class                                                    *}
{******************************************************************************}

Destructor  TSounds.Destroy();
begin
  ClearSounds();
end;

{******************************************************************************}
{* Init the sounds                                                            *}
{******************************************************************************}

procedure TSounds.InitializeSounds( aIniPath : String );
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( aIniPath );

  FAmbientId    := gdSoundLoad( PChar( iIniFile.ReadString('GameSounds', 'AmbientSound', '')), ST_LOOP);
  FUnderWaterId := gdSoundLoad( PChar( iIniFile.ReadString('GameSounds', 'UnderWater', '')), ST_LOOP);
  FMusicId      := gdSoundLoad( PChar( iIniFile.ReadString('GameSounds', 'Music', '')), ST_LOOP);

  gdSoundPlay(FAmbientId);
  gdSoundPause(FAmbientId);
  gdSoundPlay(FUnderWaterId);
  gdSoundPause(FUnderWaterId);
  gdSoundPlay( FMusicId );

  FreeAndNil( iIniFile );
end;

{******************************************************************************}
{* Clear the sounds                                                           *}
{******************************************************************************}

procedure TSounds.ClearSounds();
begin
  gdSoundRemove(FAmbientId);
  gdSoundRemove(FUnderWaterId);
  gdSoundRemove(FMusicId);
  FMusicId := nil;
  FAmbientId := nil;
  FUnderWaterId := nil;
end;

end.
