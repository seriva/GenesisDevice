{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
*                         luukvanvenrooij84@gmail.com                          *
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
unit uGDSound;

{******************************************************************************}
{* Simple sound system based on OpenAL. Plays back WAV files and MP3.         *}
{* For now playback can only be on 16 sources and there is no support for     *}
{* 3D sound positioning.                                                      *}
{******************************************************************************}

interface

uses
  Classes,
  SysUtils,
  uGDConstants,
  uGDResource,
  openalsoft,
  libmpg123;

Type
 TGDSoundResource = class (TGDResource)
   ResourceType : TGDSoundResourceType
 end;

{******************************************************************************}
{* Sound stream class                                                         *}
{******************************************************************************}

  TGDSoundStream = class (TGDSoundResource)
  private
    FBuffers  : array[0..1] of ALUInt;
    FHandle   : Pmpg123_handle;
    FFormat   : ALEnum;
    FRate     : Integer;
    FChannels : Integer;
    FEncoding : Integer;
  public
    constructor Create(aFileName : String);
    destructor  Destroy(); override;

    function  Stream(aBuffer : ALUInt; aLoop : boolean = false): boolean;
    procedure ResetStream();
    procedure PreBuffer();
  end;

{******************************************************************************}
{* Sound buffer class                                                         *}
{******************************************************************************}

  TGDSoundBuffer = class (TGDSoundResource)
  private
    FBuffer  : ALuint;
  public
    constructor Create(aFileName : String);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* Sound source class                                                         *}
{******************************************************************************}

  TGDSoundSource = class
  private
    FSource   : ALuint;
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
  uGDEngine;

type
  //WAV file header
  TWAVHeader = record
    RIFFHeader: array[1..4] of AnsiChar;
    FileSize: Integer;
    WAVEHeader: array[1..4] of AnsiChar;
    FormatHeader: array[1..4] of AnsiChar;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
  end;

const
  WAV_STANDARD = $0001;
  WAV_IMA_ADPCM = $0011;
  WAV_MP3 = $0055;

function LoadWavStream(Stream: Tstream; var format: ALenum; var data: PALvoid;
  var size: ALsizei; var freq: ALsizei; var loop: ALint): Boolean;
var
  WavHeader: TWavHeader;
  readname: pansichar;
  name: ansistring;
  readint: integer;
begin
  Result := False;

  //Read wav header
  stream.Read(WavHeader, sizeof(TWavHeader));

  //Determine SampleRate
  freq := WavHeader.SampleRate;

  //Detemine waveformat
  if WavHeader.ChannelNumber = 1 then
    case WavHeader.BitsPerSample of
      8: format := AL_FORMAT_MONO8;
      16: format := AL_FORMAT_MONO16;
    end;

  if WavHeader.ChannelNumber = 2 then
    case WavHeader.BitsPerSample of
      8: format := AL_FORMAT_STEREO8;
      16: format := AL_FORMAT_STEREO16;
    end;

  //go to end of wavheader
  stream.seek((8 - 44) + 12 + 4 + WavHeader.FormatHeaderSize + 4, soFromCurrent);
    //hmm crappy...

  //loop to rest of wave file data chunks
  repeat
    //read chunk name
    getmem(readname, 4);
    stream.Read(readname^, 4);
    name := readname[0] + readname[1] + readname[2] + readname[3];
    if name = 'data' then
    begin
      //Get the size of the wave data
      stream.Read(readint, 4);
      size := readint;
      //if WavHeader.BitsPerSample = 8 then size:=size+1; //fix for 8bit???
      //Read the actual wave data
      getmem(data, size);
      stream.Read(Data^, size);

      //Decode wave data if needed
      if WavHeader.FormatCode = WAV_IMA_ADPCM then
      begin
        //TODO: add code to decompress IMA ADPCM data
      end;
      if WavHeader.FormatCode = WAV_MP3 then
      begin
        //TODO: add code to decompress MP3 data
      end;
      Result := True;
    end
    else
    begin
      //Skip unknown chunk(s)
      stream.Read(readint, 4);
      stream.Position := stream.Position + readint;
    end;
    freemem(readname);
  until stream.Position >= stream.size;
end;

procedure alutLoadWAVFile(fname: string; var format: ALenum; var data: PALvoid;
  var size: ALsizei; var freq: ALsizei; var loop: ALint);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(fname, $0000);
  LoadWavStream(Stream, format, data, size, freq, loop);
  Stream.Free;
end;

procedure alutLoadWAVMemory(memory: PALbyte; var format: ALenum; var data:
  PALvoid; var size: ALsizei; var freq: ALsizei; var loop: ALint);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.Write(memory, sizeof(memory^));
  LoadWavStream(Stream, format, data, size, freq, loop);
  Stream.Free;
end;

procedure alutUnloadWAV(format: ALenum; data: PALvoid; size: ALsizei; freq:
  ALsizei);
begin
  //Clean up
  if data <> nil then
    freemem(data);
end;

{******************************************************************************}
{* Create stream class                                                        *}
{******************************************************************************}

constructor TGDSoundStream.Create( aFileName : String);
var
  iError : string;
  iResult : boolean;
  lErrorCode : integer;
begin
  GDConsole.Write('Loading sound stream ' + aFileName + '...');
  try
    iResult := true;
    FHandle := mpg123_new(nil, lErrorCode);
    mpg123_open(FHandle, PChar(aFileName));
    mpg123_getformat(FHandle, FRate, FChannels, FEncoding);
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
  GDConsole.WriteOkFail(iResult, iError);
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

function TGDSoundStream.Stream(aBuffer : ALUInt; aLoop : boolean = false): boolean;
const
  BUFFER_SIZE = 131072;
var
  lD : Cardinal;
  lData : PALvoid;
begin
  result := true;

  getmem(lData, BUFFER_SIZE);
  if mpg123_read(Fhandle, lData, BUFFER_SIZE, @lD) = Integer(MPG123_OK) then
  begin
    alBufferData(aBuffer, FFormat, lData, BUFFER_SIZE, FRate);
    result := true;
  end
  else
  begin
    if aLoop then
    begin
      mpg123_seek(Fhandle, 0, 0);
      result := Stream(aBuffer);
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
  iFormat: ALEnum;
  iSize: ALSizei;
  iFreq: ALSizei;
  iLoop: ALInt;
  iData: PALVoid;
  iResult : boolean;
begin
  GDConsole.Write('Loading sound ' + aFileName + '...');
  try
    iResult := true;
    AlGenBuffers(1, @FBuffer);
    AlutLoadWavFile(aFileName, iFormat, iData, iSize, iFreq, iLoop);
    AlBufferData(FBuffer, iFormat, iData, iSize, iFreq);
    AlutUnloadWav(iFormat, iData, iSize, iFreq);
    ResourceType := SR_BUFFER;
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
  iSourcePos: array [0..2] of ALfloat= ( 0.0, 0.0, 0.0 );
  iSourceVel: array [0..2] of ALfloat= ( 0.0, 0.0, 0.0 );
begin
  FResource := nil;
  AlGenSources(1, @FSource);
  AlSourcef( FSource, AL_PITCH, 1.0 );
  AlSourcef( FSource, AL_GAIN, GDSettings.SoundVolume);
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
  iState : ALCint;
begin
  if FResource = nil then
  begin
    result := true;
    exit;
  end;

  alGetSourcei(FSource, AL_SOURCE_STATE, iState);
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
  iError, iV1, iV2 : string;
  lErrorCode : integer;
  iDefaultDevice: PALCchar;
  iALInt1, iALInt2 : ALCint;
  iI : Integer;
begin
  GDTiming.Start();
  GDConsole.Write('.....Initializing sound');
  try
    FInitialized := false;
    if not(LoadOpenALCoreLibrary('')) then
      Raise Exception.Create('OpenAL library is missing!');
    iDefaultDevice := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
    FDevice := alcOpenDevice(iDefaultDevice); //for now only default device.
    if FDevice = nil then
      Raise Exception.Create('Error initializing sound device!');
    FContext := alcCreateContext(FDevice,nil);
    if FContext = nil then
      Raise Exception.Create('Error initializing sound context!');
    alcMakeContextCurrent(FContext);
    if not(alGetError() = AL_NO_ERROR) then
      Raise Exception.Create('Error making the sound context current!');

    //Print specs
    GDConsole.Write('  Vendor: ' + String(AnsiString(alGetString(AL_VENDOR))));
    GDConsole.Write('  Renderer: ' + String(AnsiString(alGetString(AL_RENDERER))));
    GDConsole.Write('  Version: ' + String(AnsiString(alGetString(AL_VERSION))));

    //Check requirements
    alcGetIntegerv(FDevice, ALC_MAJOR_VERSION, 1, @iALInt1);
    alcGetIntegerv(FDevice, ALC_MINOR_VERSION, 1, @iALInt2);
    iV1 := IntToStr(iALInt1) + '.' + IntToStr(iALInt2);
    iV2 := IntToStr(MRS_OPENAL_MAJOR_VERSION) + '.' + IntToStr(MRS_OPENAL_MINOR_VERSION);
    if (iV1 <> iV2) then
      Raise Exception.Create('OpenAL version ' + iV2 + ' required.');

    //Create the sources.
    for iI := 0 to S_MAX_SOURCES-1 do
      FSources[iI] := TGDSoundSource.Create();

    //Init mpg123 for mp3 streaming.
    lErrorCode := mpg123_init();
    if lErrorCode <> Integer(MPG123_OK) then
       Raise Exception.Create('Error initializing mpg123 library: ' + mpg123_plain_strerror(lErrorCode));


    FInitialized := true;
    GDTiming.Stop();
    GDConsole.Write('.....Done initializing sound (' + GDTiming.TimeInSeconds + ' Sec)');
  except
    on E: Exception do
    begin
      iError := E.Message;
      GDConsole.Write('Failed to initialize sound: ' + iError);
      GDConsole.Write('Sound will be disabled.');
    end;
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
  GDConsole.Write('Shutting down sound...');
  try
    iResult := true;
    mpg123_exit;
    for iI := 0 to S_MAX_SOURCES-1 do
      FreeAndNil(FSources[iI]);
    alcMakeContextCurrent(nil);
    alcDestroyContext(FContext);
    if not(alcGetError(FDevice) = AL_NO_ERROR) then
      Raise Exception.Create('Error destroying context!');
    alcCloseDevice(FDevice);
    UnloadOpenALSoftLibrary();
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

procedure TGDSound.Update();
var
  iI,iProcessed : integer;
  iSource       : TGDSoundSource;
  iStream       : TGDSoundStream;
  iBuffer       : ALUInt;
  iState        : ALCint;
  iListenerPos  : array [0..2] of ALfloat= ( 0.0, 0.0, 0.0);
  iListenerVel  : array [0..2] of ALfloat= ( 0.0, 0.0, 0.0);
  iListenerOri  : array [0..5] of ALfloat= ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0);
begin
  if not(FInitialized) then exit;

  //TODO: add positional sounds
  AlListenerfv( AL_POSITION, @iListenerPos);
  AlListenerfv( AL_VELOCITY, @iListenerVel);
  AlListenerfv( AL_ORIENTATION, @iListenerOri);
  AlListenerf( AL_GAIN, GDSettings.SoundVolume);

  //Update possible steams
  for iI := 0 to S_MAX_SOURCES-1 do
  begin
    iSource := FSources[iI];
    if iSource.FResource = nil then continue;
    if iSource.FResource.ResourceType <> SR_STREAM then continue;
    alGetSourcei(iSource.FSource, AL_SOURCE_STATE, iState);
    if (iState = AL_PAUSED) then  continue;
    alGetSourcei(iSource.FSource, AL_BUFFERS_PROCESSED, iProcessed);

    iStream := iSource.FResource as TGDSoundStream;
    while (iProcessed > 0) do
    begin
      alSourceUnqueueBuffers(iSource.FSource, 1, @iBuffer);
      if iStream.Stream(iBuffer, iSource.FLoop) then
        alSourceQueueBuffers(iSource.FSource, 1, @iBuffer)
      else
        Stop(iI);
      dec(iProcessed);
    end;
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
     result := GDResources.LoadSoundBuffer(aFileName)
  else if UpperCase(ExtractFileExt(aFileName)) = '.MP3' then
     result := GDResources.LoadSoundStream(aFileName)
end;

{******************************************************************************}
{* Remove                                                                     *}
{******************************************************************************}

procedure TGDSound.Remove(aSoundResource : TGDSoundResource);
begin
  if not(FInitialized) then exit;
  GDResources.RemoveResource(TGDResource(aSoundResource));
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

  If Not(GDSettings.MuteSound) then
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
        GDConsole.WriteOkFail(false, aResource.Name + ' is not a playeble resource.', false);

      iSource.FResource := aResource;
      AlSourcePlay(iSource.FSource);
    end
    else
      GDConsole.WriteOkFail(false, 'Failed to find free source to play sound: ' + aResource.Name, false);
  end;
end;

{******************************************************************************}
{* Pause                                                                      *}
{******************************************************************************}

procedure TGDSound.Pause(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : ALCint;
begin
  if not(FInitialized) then exit;
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, iState);
  if (iState = AL_PLAYING) then
    AlSourcePause(iSource.FSource);
end;

{******************************************************************************}
{* Resume                                                                     *}
{******************************************************************************}

procedure TGDSound.Resume(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : ALCint;
begin
  if not(FInitialized) then exit;
  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, iState);
  if (iState = AL_PAUSED) then
    AlSourcePlay(iSource.FSource);
end;

{******************************************************************************}
{* Stop                                                                       *}
{******************************************************************************}

procedure TGDSound.Stop(aIndex : Integer);
var
  iSource : TGDSoundSource;
  iState : ALCint;
begin
  if not(FInitialized) then exit;

  if (aIndex < 0) or (aIndex > S_MAX_SOURCES-1) then exit;
  iSource := FSources[aIndex];
  alGetSourcei(iSource.FSource, AL_SOURCE_STATE, iState);
  if (iState = AL_PLAYING) or (iState = AL_PAUSED) then
    AlSourceStop(iSource.FSource);
  if iSource.FResource.ResourceType = SR_STREAM then
    (iSource.FResource as TGDSoundStream).ResetStream();
  iSource.FResource := nil;
end;

end.
