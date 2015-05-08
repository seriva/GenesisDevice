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
  GDConsole,
  GDConstants,
  GDSettings,
  GDResource,
  openal,
  mpg123;

Type

{******************************************************************************}
{* Sound class                                                                *}
{******************************************************************************}

  TGDSound = class
  private
    FInitialized : boolean;
    FContext     : PALCcontext;
    FDevice      : PALCdevice;
  public
    property Initialized : boolean read FInitialized;

    constructor Create();
    destructor  Destroy(); override;

    procedure   Update();
  end;

{******************************************************************************}
{* Soundfile class                                                            *}
{******************************************************************************}

  TGDSoundFile = class (TGDResource)
  private
    FBuffer  : TALuint;
    FSource  : TALuint;
    FPlaying : boolean;
  public
    constructor Create(aFileName : String; aType : TGDSoundTypes );
    destructor  Destroy(); override;

    procedure   Play();
    procedure   Pause();
    procedure   Resume();
    procedure   Stop();
  end;

var
  Sound : TGDSound;

implementation

{******************************************************************************}
{* Create sound class                                                         *}
{******************************************************************************}

constructor TGDSound.Create();
var
  iError    : string;
begin
  Console.Write('Initializing sound...');
  try
    FInitialized := true;
    InitOpenAL();
    FDevice := alcOpenDevice(nil);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error initializing device!');
    FContext := alcCreateContext(FDevice,nil);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error initializing context!');
    alcMakeContextCurrent(FContext);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error making the context current!');
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
    end;
  end;

  Console.WriteOkFail(FInitialized, iError);
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
  Console.Write('Shutting down sound...');
  try
    iResult := true;
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
  //TODO: add player data here later.
  AlListenerfv ( AL_POSITION, @iListenerPos);
  AlListenerfv ( AL_VELOCITY, @iListenerVel);
  AlListenerfv ( AL_ORIENTATION, @iListenerOri);
end;

{******************************************************************************}
{* Create                                                                     *}
{******************************************************************************}

constructor TGDSoundFile.Create( aFileName : String; aType : TGDSoundTypes );
var
  iError : string;
  iFormat: TALEnum;
  iSize: TALSizei;
  iFreq: TALSizei;
  iLoop: TALInt;
  iData: TALVoid;
  iResult : boolean;
  iSourcePos: array [0..2] of TALfloat= ( 0.0, 0.0, 0.0 );
  iSourceVel: array [0..2] of TALfloat= ( 0.0, 0.0, 0.0 );
begin
  Console.Write('Loading sound ' + aFileName + '...');
  try
    iResult := true;
    FPlaying := false;

    //Buffer sound
    AlGenBuffers(1, @FBuffer);
    AlutLoadWavFile(aFileName, iFormat, iData, iSize, iFreq, iLoop);
    AlBufferData(FBuffer, iFormat, iData, iSize, iFreq);
    AlutUnloadWav(iFormat, iData, iSize, iFreq);

    //Create Sources
    AlGenSources(1, @FSource);
    AlSourcei ( FSource, AL_BUFFER, FBuffer);
    AlSourcef ( FSource, AL_PITCH, 1.0 );
    AlSourcef ( FSource, AL_GAIN, Settings.SoundVolume);

    //TODO: Add positional sound.
    AlSourcefv ( FSource, AL_POSITION, @iSourcePos);
    AlSourcefv ( FSource, AL_VELOCITY, @iSourceVel);

    //Loop the file?
    case aType of
      ST_SINGLE : AlSourcei ( FSource, AL_LOOPING, AL_FALSE );
      ST_LOOP   : AlSourcei ( FSource, AL_LOOPING, AL_TRUE );
    end;
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
{* Destroy                                                                    *}
{******************************************************************************}

destructor  TGDSoundFile.Destroy();
begin
  inherited;
  AlDeleteBuffers(1, @FBuffer);
  AlDeleteSources(1, @FSource);
end;

{******************************************************************************}
{* Pause play                                                                 *}
{******************************************************************************}

procedure   TGDSoundFile.Play();
begin
  If Not(Settings.MuteSound) then
  begin
    if FPlaying = false then
      AlSourcePlay(FSource);
    FPlaying := true;
  end;
end;

{******************************************************************************}
{* Pause play                                                                 *}
{******************************************************************************}

procedure TGDSoundFile.Pause();
begin
  FPlaying := false;
  AlSourcePause(FSource);
end;

{******************************************************************************}
{* Resume play                                                                *}
{******************************************************************************}

procedure TGDSoundFile.Resume();
begin
  Play();
end;

{******************************************************************************}
{* Stop play the soundfile                                                  *}
{******************************************************************************}

procedure TGDSoundFile.Stop();
begin
  AlSourceStop(FSource);
end;

end.
