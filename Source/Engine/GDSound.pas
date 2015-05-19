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
{* Simple sound system based on OpenAL. Plays back WAV files and later MP3    *}
{* streaming will be added. For now playback can only be on 16 sources and    *}
{* there is no support for 3D sound positioning.                              *}
{******************************************************************************}

interface

uses
  SysUtils,
  GDConsole,
  GDConstants,
  GDSettings,
  GDResource,
  openal,
  GDTiming,
  mpg123;

Type

{******************************************************************************}
{* Sound source class                                                         *}
{******************************************************************************}

  TGDSoundBuffer = class (TGDResource)
  private
    FBuffer  : TALuint;
  public
    constructor Create(aFileName : String);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* Sound source class                                                         *}
{******************************************************************************}

  TGDSoundSource = class
  private
    FSource  : TALuint;
    FBuffer  : TGDSoundBuffer;
  public
    constructor Create();
    destructor  Destroy(); override;

    function IsFree(): boolean;
  end;

{******************************************************************************}
{* Sound class                                                                *}
{******************************************************************************}

  TGDSound = class
  private
    FInitialized : boolean;
    FContext     : PALCcontext;
    FDevice      : PALCdevice;
    FSources     : array[0..S_MAX_SOURCES] of TGDSoundSource;
  public
    property Initialized : boolean read FInitialized;

    constructor Create();
    destructor  Destroy(); override;

    procedure   Update();

    function  Play(aBuffer : TGDSoundBuffer; aLoop : boolean): integer;
    procedure Stop(aIndex : Integer);
    procedure Pause(aIndex : Integer);
    procedure Resume(aIndex : Integer);
  end;

var
  Sound : TGDSound;

implementation

{******************************************************************************}
{* Create buffer class                                                        *}
{******************************************************************************}

constructor TGDSoundBuffer.Create( aFileName : String);
var
  iError : string;
  iFormat: TALEnum;
  iSize: TALSizei;
  iFreq: TALSizei;
  iLoop: TALInt;
  iData: TALVoid;
  iResult : boolean;
begin
  Console.Write('Loading sound ' + aFileName + '...');
  try
    iResult := true;
    AlGenBuffers(1, @FBuffer);
    AlutLoadWavFile(aFileName, iFormat, iData, iSize, iFreq, iLoop);
    AlBufferData(FBuffer, iFormat, iData, iSize, iFreq);
    AlutUnloadWav(iFormat, iData, iSize, iFreq);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Destroy buffer class                                                       *}
{******************************************************************************}

destructor  TGDSoundBuffer.Destroy();
begin
  inherited;
  AlDeleteBuffers(1, @FBuffer);
end;

{******************************************************************************}
{* Create source class                                                        *}
{******************************************************************************}

constructor TGDSoundSource.Create();
var
  iSourcePos: array [0..2] of TALfloat= ( 0.0, 0.0, 0.0 );
  iSourceVel: array [0..2] of TALfloat= ( 0.0, 0.0, 0.0 );
begin
  FBuffer := nil;
  AlGenSources(1, @FSource);
  AlSourcef( FSource, AL_PITCH, 1.0 );
  AlSourcef( FSource, AL_GAIN, Settings.SoundVolume);
  AlSourcefv( FSource, AL_POSITION, @iSourcePos);
  AlSourcefv( FSource, AL_VELOCITY, @iSourceVel);
end;

{******************************************************************************}
{* Destroy source class                                                       *}
{******************************************************************************}

destructor TGDSoundSource.Destroy();
begin
  AlDeleteSources(1, @FSource);
end;

{******************************************************************************}
{* Check if this source is playing a sound                                    *}
{******************************************************************************}

function TGDSoundSource.IsFree(): boolean;
var
  iState : TALCint;
begin
  if FBuffer = nil then
  begin
    result := true;
    exit;
  end;

  alGetSourcei(FSource, AL_SOURCE_STATE, @iState);
  if (iState = AL_PLAYING) or (iState = AL_PAUSED) then
    result := false
  else
    result := true;
end;

{******************************************************************************}
{* Create sound class                                                         *}
{******************************************************************************}

constructor TGDSound.Create();
var
  iError, iV : string;
  iALInt1, iALInt2 : TALCint;
  iI : Integer;
begin
  Timing.Start();
  Console.Write('......Initializing sound');
  try
    FInitialized := true;
    if not(InitOpenAL()) then
      Raise Exception.Create('Error initializing OpenAL! Is OpenAL installed?');
    FDevice := alcOpenDevice(nil); //for now only default device.
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error initializing sound device!');
    FContext := alcCreateContext(FDevice,nil);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error initializing sound context!');
    alcMakeContextCurrent(FContext);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error making the sound context current!');

    //Print specs
    Console.Write('Vendor: ' + String(AnsiString(alGetString(AL_VENDOR))));
    Console.Write('Renderer: ' + String(AnsiString(alGetString(AL_RENDERER))));
    Console.Write('Version: ' + String(AnsiString(alGetString(AL_VERSION))));

    //Check requirements
    //Version
    alcGetIntegerv(FDevice, ALC_MAJOR_VERSION, 1, @iALInt1);
    alcGetIntegerv(FDevice, ALC_MINOR_VERSION, 1, @iALInt2);
    iV := IntToStr(MRS_OPENAL_MAJOR_VERSION) + '.' + IntToStr(MRS_OPENAL_MINOR_VERSION);
    if iALInt1 < MRS_OPENAL_MAJOR_VERSION then
      Raise Exception.Create('To low OpenAL version! Minimal version ' + iV + ' needed.');
    if iALInt2 < MRS_OPENAL_MINOR_VERSION then
      Raise Exception.Create('To low OpenAL version! Minimal version ' + iV + ' needed.');

    //Create the sources.
    for iI := 0 to S_MAX_SOURCES-1 do
      FSources[iI] := TGDSoundSource.Create();
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
      Console.Write('Failed to initialize sound: ' + iError);
    end;
  end;

  If FInitialized then
  begin
    Timing.Stop();
    Console.Write('......Done initializing sound (' + Timing.TimeInSeconds + ' Sec)');
  end;
end;

{******************************************************************************}
{* Destroy sound class                                                        *}
{******************************************************************************}

destructor TGDSound.Destroy();
var
  iError  : string;
  iResult : boolean;
  iI : Integer;
begin
  inherited;
  Console.Write('Shutting down sound...');
  try
    iResult := true;
    for iI := 0 to S_MAX_SOURCES-1 do
      FreeAndNil(FSources[iI]);
    alcDestroyContext(FContext);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error destroying contect!');
    alcCloseDevice(FDevice);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error destroying device!');
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Update the sound engine                                                    *}
{******************************************************************************}

procedure TGDSound.Update();
var
  iListenerPos : array [0..2] of TALfloat= ( 0.0, 0.0, 0.0);
  iListenerVel : array [0..2] of TALfloat= ( 0.0, 0.0, 0.0);
  iListenerOri : array [0..5] of TALfloat= ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0);
begin
  //TODO: add positional sounds
  AlListenerfv ( AL_POSITION, @iListenerPos);
  AlListenerfv ( AL_VELOCITY, @iListenerVel);
  AlListenerfv ( AL_ORIENTATION, @iListenerOri);
end;

{******************************************************************************}
{* Play                                                                       *}
{******************************************************************************}

function TGDSound.Play(aBuffer : TGDSoundBuffer; aLoop : boolean): integer;
var
  iI : Integer;
  iSource : TGDSoundSource;
begin
  result := -1;
  If Not(Settings.MuteSound) then
  begin
    //find a free source for playing
    for iI := 0 to S_MAX_SOURCES-1 do
    begin
      iSource := FSources[iI];
      if iSource.IsFree() then
      begin
        result := iI;
        break;
      end
      else
        iSource := nil;
    end;

    if iSource <> nil then
    begin
      if aLoop then
        AlSourcei ( iSource.FSource, AL_LOOPING, AL_TRUE)
      else
        AlSourcei ( iSource.FSource, AL_LOOPING, AL_FALSE);
      AlSourcei(iSource.FSource, AL_BUFFER, aBuffer.FBuffer);
      AlSourcePlay(iSource.FSource);
      iSource.FBuffer := aBuffer;
    end
    else
      Console.WriteOkFail(false, 'Failed to find free source to play sound: ' + aBuffer.Name, false);
  end;
end;

{******************************************************************************}
{* Pause                                                                      *}
{******************************************************************************}

procedure TGDSound.Pause(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : TALCint;
begin
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := (FSources[aIndex] as TGDSoundSource);
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, @iState);
  if (iState = AL_PLAYING) then
    AlSourcePause(iSource.FSource);
end;

{******************************************************************************}
{* Resume                                                                     *}
{******************************************************************************}

procedure TGDSound.Resume(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : TALCint;
begin
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := (FSources[aIndex] as TGDSoundSource);
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, @iState);
  if (iState = AL_PAUSED) then
    AlSourcePlay(iSource.FSource);
end;

{******************************************************************************}
{* Stop                                                                       *}
{******************************************************************************}

procedure TGDSound.Stop(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : TALCint;
begin
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := (FSources[aIndex] as TGDSoundSource);
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, @iState);
  if (iState = AL_PLAYING) or (iState = AL_PAUSED) then
    AlSourceStop(iSource.FSource);
  iSource.FBuffer := nil;
end;

end.
