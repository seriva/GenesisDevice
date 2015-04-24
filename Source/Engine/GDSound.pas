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
  fmod,
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
    FInitialized : boolean;
    FSoundSystem : FMOD_SYSTEM;
  public
    property Initialized : boolean read FInitialized;
    Property System      : FMOD_SYSTEM read FSoundSystem;

    constructor Create();
    destructor  Destroy(); override;

    procedure   UpdateSound();

    function    GetNumberOfDrivers() : Integer;
    function    GetDriverName( aDriverNumber : Integer ) : String;
    function    InitSoundDriver() : boolean;
    function    ShutDownSoundDriver(): boolean;
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
var
  iError    : string;
  iVersion : Cardinal;
begin
  Log.Write('Initializing sound...');
  try
    FInitialized := true;
    If not(FMOD_System_Create( FSoundSystem ) = FMOD_OK) then
      Raise Exception.Create('Error initializing FMOD!');

    if not(FMOD_System_GetVersion( FSoundSystem, iVersion) = FMOD_OK) then
      Raise Exception.Create('Error getting FMOD version!');

    If not(iVersion = FMOD_VERSION) Then
      Raise Exception.Create('FMOD version is different!');
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
    end;
  end;

  Log.WriteOkFail(FInitialized, iError);
end;

{******************************************************************************}
{* Destroy sound class                                                        *}
{******************************************************************************}

destructor TGDSound.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  Log.Write('Shutting down sound...');
  try
    iResult := true;
    if not(FMOD_System_Release(FSoundSystem) = FMOD_OK) then
      Raise Exception.Create('Error shutting down FMOD!');
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Log.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Get the number of available sound drivers                                  *}
{******************************************************************************}

function TGDSound.GetNumberOfDrivers() : Integer;
var
  iDriverCount : integer;
begin
  result := -1;
  if  FMOD_System_GetNumDrivers( FSoundSystem, iDriverCount) = FMOD_OK  then
    result := iDriverCount
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
    SetString(result, PAnsiChar(@iName[0]), 256);
end;

{******************************************************************************}
{* Init the sound engine                                                      *}
{******************************************************************************}

function   TGDSound.InitSoundDriver() : boolean;
var
  iError    : string;
begin
  Log.Write('Initializing sound driver...');
  try
    Result := true;

    If  Not(FMOD_System_setDriver( FSoundSystem, Settings.SoundDriver ) = FMOD_OK )then
      Raise Exception.Create('Unable to set the sounddriver!');

    If  Not(FMOD_System_Init(FSoundSystem, CHANNEL_COUNT, FMOD_INIT_NORMAL, 0 ) = FMOD_OK )then
      Raise Exception.Create('Unable to initialize the sounddriver!');

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
{* Shutdown the sound engine                                                  *}
{******************************************************************************}

function  TGDSound.ShutDownSoundDriver(): boolean;
var
  iError    : string;
begin
  Log.Write('Shutting down sound driver...');
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
