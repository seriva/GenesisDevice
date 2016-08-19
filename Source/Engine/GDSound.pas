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
  Classes,
  SysUtils,
  GDConstants,
  GDResource,
  openal,
  mpg123;

Type
 TGDSoundResource = class (TGDResource)
   ResourceType : TGDSoundResourceType
 end;

{******************************************************************************}
{* Sound stream class                                                         *}
{******************************************************************************}

  TGDSoundStream = class (TGDSoundResource)
  private
    FBuffers  : array[0..1] of TALUInt;
    FHandle   : Pmpg123_handle;
    FFormat   : TALEnum;
    FRate     : Integer;
    FChannels : Integer;
    FEncoding : Integer;
  public
    constructor Create(aFileName : String);
    destructor  Destroy(); override;

    function  Stream(aBuffer : TALUInt; aLoop : boolean = false): boolean;
    procedure ResetStream();
    procedure PreBuffer();
  end;

{******************************************************************************}
{* Sound buffer class                                                         *}
{******************************************************************************}

  TGDSoundBuffer = class (TGDSoundResource)
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
    FSource   : TALuint;
    FResource : TGDSoundResource;
    FLoop     : Boolean;
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

    function  Load(aFileName : String) : TGDSoundResource;
    procedure Remove(aSoundResource : TGDSoundResource);

    function  Play(aResource : TGDSoundResource; aLoop : boolean): integer;
    procedure Stop(aIndex : Integer);
    procedure Pause(aIndex : Integer);
    procedure Resume(aIndex : Integer);

    procedure Update();
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create stream class                                                        *}
{******************************************************************************}

constructor TGDSoundStream.Create( aFileName : String);
var
  iError : string;
  iResult : boolean;
begin
  Engine.Console.Write('Loading sound stream' + aFileName + '...');
  try
    iResult := true;
    FHandle := mpg123_new('MMX', nil);
    mpg123_open(FHandle, PChar(aFileName));
    mpg123_getformat(FHandle, @FRate, @FChannels, @FEncoding);
    mpg123_format_none(Fhandle);
    mpg123_format(Fhandle, Frate, FChannels, FEncoding);
    FFormat := AL_FORMAT_STEREO16;
    alGenBuffers(2, @FBuffers);
    ResourceType := SR_STREAM;
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Engine.Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Destroy stream class                                                       *}
{******************************************************************************}

destructor TGDSoundStream.Destroy();
begin
  inherited;
  alDeleteBuffers(2, @FBuffers);
  mpg123_close(Fhandle);
end;

{******************************************************************************}
{* Stream next part in the buffer                                             *}
{******************************************************************************}

function TGDSoundStream.Stream(aBuffer : TALUInt; aLoop : boolean = false): boolean;
const
  BUFFER_SIZE = 4096*32;
var
  lData: PByte;
  lD : Cardinal;
begin
  result := true;

  getmem(lData, BUFFER_SIZE);
  if mpg123_read(Fhandle, lData, BUFFER_SIZE, @lD) = MPG123_OK then
     alBufferData(aBuffer, FFormat, lData, BUFFER_SIZE, FRate)
  else
  begin
    if aLoop then
    begin
      mpg123_seek(Fhandle, 0, 0);
      result := Stream(aBuffer)
    end
    else
      result := false;
  end;
  freemem(lData);
end;

{******************************************************************************}
{* Reset the sounds stream                                                    *}
{******************************************************************************}

procedure TGDSoundStream.ResetStream();
begin
  mpg123_seek(Fhandle, 0, 0);
end;

{******************************************************************************}
{* Prebuffer the stream                                                       *}
{******************************************************************************}

procedure TGDSoundStream.PreBuffer();
begin
  Stream(FBuffers[0]);
  Stream(FBuffers[1]);
end;

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
  Engine.Console.Write('Loading sound ' + aFileName + '...');
  try
    iResult := true;
    {
    AlGenBuffers(1, @FBuffer);
    AlutLoadWavFile(aFileName, iFormat, iData, iSize, iFreq, iLoop);
    AlBufferData(FBuffer, iFormat, iData, iSize, iFreq);
    AlutUnloadWav(iFormat, iData, iSize, iFreq);
    ResourceType := SR_BUFFER;
    }
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Engine.Console.WriteOkFail(iResult, iError);
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
  FResource := nil;
  AlGenSources(1, @FSource);
  AlSourcef( FSource, AL_PITCH, 1.0 );
  AlSourcef( FSource, AL_GAIN, Engine.Settings.SoundVolume);
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
  if FResource = nil then
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
  iDefaultDevice: PALCubyte;
  iALInt1, iALInt2 : TALCint;
  iI : Integer;
begin
  Engine.Timing.Start();
  Engine.Console.Write('.....Initializing sound');
  try
    FInitialized := false;
    if not(InitOpenAL()) then
      Raise Exception.Create('OpenAL library is missing!');
    if not(InitMPG123()) then
      Raise Exception.Create('mpg123 library is missing!');
    iDefaultDevice := '';
    iDefaultDevice := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
    FDevice := alcOpenDevice(PChar(iDefaultDevice)); //for now only default device.
    //if not(alGetError() = AL_NO_ERROR) then
    //  Raise Exception.Create('Error initializing sound device!');
    FContext := alcCreateContext(FDevice,nil);
    //if not(alGetError() = AL_NO_ERROR) then
    //  Raise Exception.Create('Error initializing sound context!');
    alcMakeContextCurrent(FContext);
    //if not(alGetError() = AL_NO_ERROR) then
    //  Raise Exception.Create('Error making the sound context current!');

    //Print specs
    Engine.Console.Write('  Vendor: ' + String(AnsiString(alGetString(AL_VENDOR))));
    Engine.Console.Write('  Renderer: ' + String(AnsiString(alGetString(AL_RENDERER))));
    Engine.Console.Write('  Version: ' + String(AnsiString(alGetString(AL_VERSION))));

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

    //Init mpg123 for mp3 streaming.
    if mpg123_init <> 0 then
      Raise Exception.Create('Error initializing mpg123 library!');

    FInitialized := true;
  except
    on E: Exception do
    begin
      iError := E.Message;
      Engine.Console.Write('Failed to initialize sound: ' + iError);
      Engine.Console.Write('Sound will be disabled.');
    end;
  end;

  If FInitialized then
  begin
    Engine.Timing.Stop();
    Engine.Console.Write('.....Done initializing sound (' + Engine.Timing.TimeInSeconds + ' Sec)');
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
  Engine.Console.Write('Shutting down sound...');
  try
    iResult := true;
    mpg123_exit;
    for iI := 0 to S_MAX_SOURCES-1 do
      FreeAndNil(FSources[iI]);
    alcDestroyContext(FContext);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error destroying context!');
    alcCloseDevice(FDevice);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error destroying device!');

    FreeMPG123();
    //FreeOpenAL();
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Engine.Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Update the sound engine                                                    *}
{******************************************************************************}

procedure TGDSound.Update();
var
  iI,iProcessed : integer;
  iSource       : TGDSoundSource;
  iBuffer       : TALUInt;
  iState        : TALCint;
  iListenerPos  : array [0..2] of TALfloat= ( 0.0, 0.0, 0.0);
  iListenerVel  : array [0..2] of TALfloat= ( 0.0, 0.0, 0.0);
  iListenerOri  : array [0..5] of TALfloat= ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0);
begin
  if not(FInitialized) then exit;

  //TODO: add positional sounds
  AlListenerfv( AL_POSITION, @iListenerPos);
  AlListenerfv( AL_VELOCITY, @iListenerVel);
  AlListenerfv( AL_ORIENTATION, @iListenerOri);
  AlListenerf( AL_GAIN, Engine.Settings.SoundVolume);

  //Update possible steams
  for iI := 0 to S_MAX_SOURCES-1 do
  begin
    iSource := FSources[iI];
    if iSource.FResource = nil then continue;
    if iSource.FResource.ResourceType <> SR_STREAM then continue;
    alGetSourcei(iSource.FSource, AL_SOURCE_STATE, @iState);
    if (iState = AL_PAUSED) then  continue;
    alGetSourcei(iSource.FSource, AL_BUFFERS_PROCESSED, @iProcessed);
    if iProcessed > 0 then
      repeat
        alSourceUnqueueBuffers(iSource.FSource, 1, @iBuffer);
        if (iSource.FResource as TGDSoundStream).Stream(iBuffer, iSource.FLoop) then
           alSourceQueueBuffers(iSource.FSource, 1, @iBuffer)
        else
          Stop(iI);
        dec(iProcessed);
      until iProcessed <= 0;
  end;
end;

{******************************************************************************}
{* Load                                                                       *}
{******************************************************************************}

function  TGDSound.Load(aFileName : String) : TGDSoundResource;
begin
  result := nil;
  if not(FInitialized) then exit;
  if UpperCase(ExtractFileExt(aFileName)) = '.WAV' then
     result := Engine.Resources.LoadSoundBuffer(aFileName)
  else if UpperCase(ExtractFileExt(aFileName)) = '.MP3' then
     result := Engine.Resources.LoadSoundStream(aFileName)
end;

{******************************************************************************}
{* Remove                                                                     *}
{******************************************************************************}

procedure TGDSound.Remove(aSoundResource : TGDSoundResource);
begin
  if not(FInitialized) then exit;
  Engine.Resources.RemoveResource(TGDResource(aSoundResource));
end;

{******************************************************************************}
{* Play                                                                       *}
{******************************************************************************}

function TGDSound.Play(aResource : TGDSoundResource; aLoop : boolean): integer;
var
  iI : Integer;
  iSource : TGDSoundSource;
begin
  result := -1;
  if not(FInitialized) then exit;

  If Not(Engine.Settings.MuteSound) then
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
      iSource.FLoop := aLoop;
      if aResource.ClassType = TGDSoundBuffer then
      begin
        if aLoop then
          AlSourcei ( iSource.FSource, AL_LOOPING, AL_TRUE)
        else
          AlSourcei ( iSource.FSource, AL_LOOPING, AL_FALSE);
        AlSourcei(iSource.FSource, AL_BUFFER, TGDSoundBuffer(aResource).FBuffer);
      end
      else if aResource.ClassType = TGDSoundStream then
      begin
        TGDSoundStream(aResource).PreBuffer();
        alSourceQueueBuffers(iSource.FSource, 2, @TGDSoundStream(aResource).FBuffers[0]);
      end
      else
        Engine.Console.WriteOkFail(false, aResource.Name + ' is not a playeble resource.', false);

      iSource.FResource := aResource;
      AlSourcePlay(iSource.FSource);
    end
    else
      Engine.Console.WriteOkFail(false, 'Failed to find free source to play sound: ' + aResource.Name, false);
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
  if not(FInitialized) then exit;
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
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
  if not(FInitialized) then exit;
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
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
  if not(FInitialized) then exit;

  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, @iState);
  if (iState = AL_PLAYING) or (iState = AL_PAUSED) then
    AlSourceStop(iSource.FSource);
  if iSource.FResource.ResourceType = SR_STREAM then
    (iSource.FResource as TGDSoundStream).ResetStream();
  iSource.FResource := nil;
end;

end.
