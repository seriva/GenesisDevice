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
unit GDSound;

{$MODE Delphi}

{******************************************************************************}
{* Holds everything related to sound. It`s a wrapper for the FMOD sound       *}
{* library.                                                                   *}
{******************************************************************************}

interface

uses
  SysUtils,
  Math,
  Contnrs,
  fmod,
  fmoderrors,
  fmodpresets,
  fmodtypes,
  GDLog,
  GDConstants,
  GDSettings,
  GDObjectList;

Type

{******************************************************************************}
{* Sound class                                                                *}
{******************************************************************************}

  TGDSound = class
  private
    FSoundSystem   : FMOD_SYSTEM;
    FSoundSystemOk : Boolean;
  public
    Property System : FMOD_SYSTEM read FSoundSystem;

    constructor Create();
    destructor  Destroy(); override;

    function    CheckVersion() : boolean;
    function    GetNumberOfDrivers() : Integer;
    function    GetDriverName( aDriverNumber : Integer ) : String;

    function    InitSoundEngine() : boolean;
    function    ShutDownSoundEngine(): boolean;

    procedure   UpdateSound();
  end;

{******************************************************************************}
{* Soundfile class                                                            *}
{******************************************************************************}

  TGDSoundFile = class (TObject)
  private
    FSound   : FMOD_SOUND;
    FChannel : FMOD_CHANNEL;
  public
    constructor Create();
    destructor  Destroy(); override;

    function    InitSoundFile( aFileName : String; aType : TGDSoundTypes ) : boolean;
    procedure   Clear();

    procedure   Play();
    procedure   Pause();
    procedure   Resume();
  end;

var
  Sound : TGDSound;
  SoundList : TGDObjectList;

const
  CHANNEL_COUNT = 128;

implementation

{******************************************************************************}
{* Create sound class                                                         *}
{******************************************************************************}

constructor TGDSound.Create();
begin
  If FMOD_System_Create( FSoundSystem ) = FMOD_OK then
     FSoundSystemOk := true
  else
     FSoundSystemOk := false;
end;

{******************************************************************************}
{* Destroy sound class                                                        *}
{******************************************************************************}

destructor TGDSound.Destroy();
begin
  FMOD_System_Release(FSoundSystem);
end;

{******************************************************************************}
{* Check FMOD version                                                         *}
{******************************************************************************}

function   TGDSound.CheckVersion() : boolean;
var
  iVersion : Cardinal;
begin
  result := False;
  if Not(FSoundSystemOk) then Exit;
  if  FMOD_System_GetVersion( FSoundSystem, iVersion) = FMOD_OK  then
  begin
    If iVersion = FMOD_VERSION Then
       result := True
    else
      FSoundSystemOk := false;
  end
  else
    FSoundSystemOk := false;
end;

{******************************************************************************}
{* Get the number of available sound drivers                                  *}
{******************************************************************************}

function   TGDSound.GetNumberOfDrivers() : Integer;
var
  iDriverCount : integer;
begin
  result := -1;
  if Not(FSoundSystemOk) then Exit;
  if  FMOD_System_GetNumDrivers( FSoundSystem, iDriverCount) = FMOD_OK  then
       result := iDriverCount
  else
    FSoundSystemOk := false;
end;

{******************************************************************************}
{* Get the name of a driver                                                   *}
{******************************************************************************}

function   TGDSound.GetDriverName( aDriverNumber : Integer ) : String;
var
  iName  : array[0..256] of byte;
begin
  result := 'NO_DRIVER';
  if FMOD_System_getDriverName(FSoundSystem, aDriverNumber, @iName[0], 256 ) = FMOD_OK  then
  begin
    SetString(result, PAnsiChar(@iName[0]), 256);
  end
  else
    FSoundSystemOk := false;
end;

{******************************************************************************}
{* Init the sound engine                                                      *}
{******************************************************************************}

function   TGDSound.InitSoundEngine() : boolean;
var
  iError    : string;
begin
  Log.Write('Initializing soundengine...');
  try
    Result := true;

    if Not(FSoundSystemOk) then
      Raise Exception.Create('Error using FMOD!');

    If  Not(FMOD_System_setDriver( FSoundSystem, Settings.SoundDriver ) = FMOD_OK )then
      Raise Exception.Create('Unable to set the sounddriver!');

    If  Not(FMOD_System_Init(FSoundSystem, CHANNEL_COUNT, FMOD_INIT_NORMAL, 0 ) = FMOD_OK )then
      Raise Exception.Create('Unable to initialize the sounddriver!');

  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FSoundSystemOk := false;
    end;
  end;

  Log.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Shutdown the sound engine                                                  *}
{******************************************************************************}

function  TGDSound.ShutDownSoundEngine(): boolean;
var
  iError    : string;
begin
  Log.Write('Shutting down soundengine...');
  try
    result := true;

    If  Not(FMOD_System_Close( FSoundSystem ) = FMOD_OK )then
      Raise Exception.Create('Unable to release the sounddriver!');

  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Log.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Update the sound engine                                                    *}
{******************************************************************************}

procedure TGDSound.UpdateSound();
begin
  FMOD_System_Update( FSoundSystem );
end;

{******************************************************************************}
{* Create a soundfile class                                                   *}
{******************************************************************************}

constructor TGDSoundFile.Create();
begin
  FSound   := nil;
  FChannel := nil;
end;

{******************************************************************************}
{* Destroy a soundfile class                                                  *}
{******************************************************************************}

destructor  TGDSoundFile.Destroy();
begin
  Clear();
end;

{******************************************************************************}
{* Init the soundfile                                                         *}
{******************************************************************************}

function    TGDSoundFile.InitSoundFile( aFileName : String; aType : TGDSoundTypes ) : boolean;
var
  iError : string;
  iMode  : Cardinal;
begin
  Log.Write('Loading sound from file ' + aFileName + '...');
  try
    Clear();
    result := true;
    case aType of
      ST_SINGLE : if Not( FMOD_System_CreateSound(Sound.System, PChar(aFileName), FMOD_HARDWARE, 0, FSound) = FMOD_OK ) then
                      Raise Exception.Create('Problem loading file ' + aFileName);

      ST_LOOP   :  if Not( FMOD_System_CreateSound(Sound.System, PChar(aFileName), FMOD_HARDWARE or FMOD_LOOP_NORMAL, 0, FSound) = FMOD_OK ) then
                      Raise Exception.Create('Problem loading file ' + aFileName);
    end;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Log.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Clear the soundfile                                                        *}
{******************************************************************************}

procedure   TGDSoundFile.Clear();
begin
  FMOD_Sound_Release( FSound );
  FChannel := nil;
  FSound   := nil;
end;

{******************************************************************************}
{* Pause play the soundfile                                                   *}
{******************************************************************************}

procedure   TGDSoundFile.Play();
begin
  If Not(Settings.MuteSound) then
  begin
    FMOD_System_PlaySound(Sound.System, FMOD_CHANNEL_FREE, FSound, true, FChannel);
    FMOD_Channel_SetVolume(FChannel, Settings.SoundVolume);
    FMOD_Channel_SetPaused(FChannel,False);
  end;
end;

{******************************************************************************}
{* Pause play the soundfile                                                   *}
{******************************************************************************}

procedure   TGDSoundFile.Pause();
begin
  FMOD_Channel_SetPaused(FChannel,True);
end;

{******************************************************************************}
{* Resume play the soundfile                                                  *}
{******************************************************************************}

procedure   TGDSoundFile.Resume();
begin
  FMOD_Channel_SetPaused(FChannel,False);
end;


end.
