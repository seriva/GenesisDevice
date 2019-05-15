{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
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
unit uGDTexture;

{$MODE objfpc}

{******************************************************************************}
{* Holds the texture class and managing system                                *}
{******************************************************************************}

interface

Uses
 fgl,
 Classes,
 uGDResource,
 SysUtils,
 dglOpenGL,
 uGDConstants;

{******************************************************************************}
{* Records for loading DDS files                                              *}
{******************************************************************************}

const
     DDSD_CAPS        = $00000001;
     DDSD_HEIGHT      = $00000002;
     DDSD_WIDTH       = $00000004;
     DDSD_PITCH       = $00000008;
     DDSD_PIXELFORMAT = $00001000;
     DDSD_MIPMAPCOUNT = $00020000;
     DDSD_LINEARSIZE  = $00080000;
     DDSD_DEPTH       = $00800000;

     DDPF_ALPHAPIXELS = $00000001;
     DDPF_FOURCC      = $00000004;
     DDPF_RGB         = $00000040;

     DDSCAPS_COMPLEX  = $00000008;
     DDSCAPS_TEXTURE  = $00001000;
     DDSCAPS_MIPMAP   = $00400000;

     DDSCAPS2_CUBEMAP           = $00000200;
     DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
     DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
     DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
     DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
     DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
     DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
     DDSCAPS2_VOLUME            = $00200000;


  type
     TDDPIXELFORMAT = record
        dwSize,
        dwFlags,
        dwFourCC,
        dwRGBBitCount,
        dwRBitMask,
        dwGBitMask,
        dwBBitMask,
        dwRGBAlphaBitMask : Cardinal;
     end;

     TDDCAPS2 = record
        dwCaps1,
        dwCaps2 : Cardinal;
        Reserved : array[0..1] of Cardinal;
     end;

     TDDSURFACEDESC2 = record
        dwSize,
        dwFlags,
        dwHeight,
        dwWidth,
        dwPitchOrLinearSize,
        dwDepth,
        dwMipMapCount : Cardinal;
        dwReserved1 : array[0..10] of Cardinal;
        ddpfPixelFormat : TDDPIXELFORMAT;
        ddsCaps : TDDCAPS2;
        dwReserved2 : Cardinal;
     end;

     TDDSHeader = record
        Magic : Cardinal;
        SurfaceFormat : TDDSURFACEDESC2;
     end;

     TFOURCC = array[0..3] of char;

const
   FOURCC_DXT1 = $31545844; // 'DXT1'
   FOURCC_DXT3 = $33545844; // 'DXT3'
   FOURCC_DXT5 = $35545844; // 'DXT5'

{******************************************************************************}
{* Texture class                                                              *}
{******************************************************************************}

type
  TGDTexture = class (TGDResource)
  private
    FTexture: TGLuint;
  public
    property Texture : TGLuint read FTexture;

    constructor Create( aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter); overload;
    constructor Create( aType : GLEnum; aFormat: GLenum; aSizeW, aSizeH : integer ); overload;
    destructor  Destroy(); override;

    procedure BindTexture(aTU : GLEnum);
  end;

  TGDTextureList = specialize TFPGObjectList<TGDTexture>;

implementation

uses
  uGDEngine;

{******************************************************************************}
{* Create the texture class                                                   *}
{******************************************************************************}

constructor TGDTexture.Create(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter);
var
  iError : string;
  iResult : boolean;

type
  TDDSData = record
    OutputFormat  : Word;
    Factor        : Integer;
    Width         : Integer;
    Height        : Integer;
    NumMipMaps    : Integer;
    Components    : Integer;
    Data          : array of Byte;
  end;

function LoadTextureFromFile( aName: string; aDetail : TGDTextureDetail ): GLuint;
var
  iDDSD            : TDDSurfaceDesc2;
  iFileCode        : array[0..3] of AnsiChar;
  iBufferSize      : integer;
  iReadBufferSize  : integer;
  iDDSData         : TDDSData;
  iBlockSize       : Integer;
  iHeight          : Integer;
  iWidth           : Integer;
  iOffset          : Integer;
  iSize            : Integer;
  iI, iLevelOffset : Integer;
  iData            : TMemoryStream;
begin
  	result := 0;

    //check if the file exists
    if Not(FileExists( aName )) then
      Raise Exception.Create( aName + ' doesn`t exists.');

    //load the texture
    iData := TMemoryStream.Create();
    iData.LoadFromFile(aName);

    //verify if it is a true DDS file
    iData.Read(iFileCode, SizeOf(byte) * 4);
    if (iFileCode[0] + iFileCode[1] + iFileCode[2] <> 'DDS') then
      Raise Exception.Create('File ' + aName + ' is not a valid DDS file.');

    //read surface descriptor
    iData.Read(iDDSD, SizeOf(iDDSD));
    case iDDSD.ddpfPixelFormat.dwFourCC of
    FOURCC_DXT1 : begin
                    //DXT1's compression ratio is 8:1
                    iDDSData.OutputFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
                    iDDSData.Factor := 2;
                  end;
    FOURCC_DXT3 : begin
                    //DXT3's compression ratio is 4:1
                    iDDSData.OutputFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
                    iDDSData.Factor := 4;
                  end;
    FOURCC_DXT5 : begin
                    //DXT5's compression ratio is 4:1
                    iDDSData.OutputFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
                    iDDSData.Factor := 4;
                  end;
    else          begin
                    //Not compressed. Oh shit, didn't implement that!
                    Raise Exception.Create('File ' + aName + ' has no compression! Loading non-compressed not implemented.');
                  end;
    end;

    //how big will the buffer need to be to load all of the pixel data including mip-maps?
    if( iDDSD.dwPitchOrLinearSize = 0 ) then
      Raise Exception.Create('File ' + aName + ' dwLinearSize is 0.');

    //set the buffer size
    if( iDDSD.dwMipMapCount > 1 ) then
      iBufferSize := iDDSD.dwPitchOrLinearSize * iDDSData.Factor
    else
      iBufferSize := iDDSD.dwPitchOrLinearSize;

    //read the buffer data
    iReadBufferSize := iBufferSize * sizeof(Byte);
    setLength(iDDSData.Data, iReadBufferSize);
    iData.Read(iDDSData.Data[0], iReadBufferSize);
    FreeAndNil(iData);

    //more output info }
    iDDSData.Width      := iDDSD.dwWidth;
    iDDSData.Height     := iDDSD.dwHeight;
    iDDSData.NumMipMaps := iDDSD.dwMipMapCount;

    //do we have a fourth Alpha channel doc?
    if( iDDSD.ddpfPixelFormat.dwFourCC = FOURCC_DXT1 ) then
      iDDSData.Components := 3
    else
      iDDSData.Components := 4;

    glEnable(GL_TEXTURE_2D);
    glGenTextures(1, @FTexture);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTexture);

    if iDDSData.OutputFormat = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT then
      iBlockSize := 8
    else
      iBlockSize := 16;

    iHeight     := iDDSData.height;
    iWidth      := iDDSData.width;
    iOffset     := 0;

    iLevelOffset := 0;
    if iDDSData.NumMipMaps >= 3 then
    begin
      case aDetail of
      TD_LOW :    iLevelOffset := 2;
      TD_MEDIUM : iLevelOffset := 1;
      TD_HIGH :   iLevelOffset := 0;
      end;
    end;

    for iI := 0 to iDDSData.NumMipMaps-1 do
    begin
      if iWidth  = 0 then iWidth  := 1;
      if iHeight = 0 then iHeight := 1;

      iSize := ((iWidth+3) div 4) * ((iHeight+3) div 4) * iBlockSize;

      if iI >= iLevelOffset then
      begin
        glCompressedTexImage2DARB( GL_TEXTURE_2D,
                                   iI-iLevelOffset,
                                   iDDSData.Outputformat,
                                   iWidth,
                                   iHeight,
                                   0,
                                   iSize,
                                   @iDDSData.data[iOffset]);
      end;
      iOffset := iOffset  + iSize;
      iWidth  := (iWidth  div 2);
      iHeight := (iHeight div 2);
    end;
end;

begin
  GDConsole.Write('Loading texture ' + aFileName + '...');
  try
    iResult := True;

    LoadTextureFromFile(aFileName, aDetail);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    case aTextureFilter of
        TF_BILINEAR   : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_NEAREST);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
                        end;
        TF_TRILINEAR  : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_LINEAR);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
                        end;
        TF_AF2        : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_LINEAR);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2);
                        end;
        TF_AF4        : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_LINEAR);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 4);
                        end;
        TF_AF8        : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_LINEAR);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 8);
                        end;
        TF_AF16       : begin
                          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,  GL_LINEAR_MIPMAP_LINEAR);
                          glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 16);
                        end;
    end;
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;

  GDConsole.WriteOkFail(iResult, iError);
end;

constructor TGDTexture.Create( aType, aFormat : GLEnum; aSizeW, aSizeH : integer );
begin
  glGenTextures(1, @FTexture);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, aType, aSizeW, aSizeH, 0,aFormat, GL_UNSIGNED_BYTE, nil);

  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

  if aType = GL_RGBA then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end
  else if aType = GL_DEPTH_COMPONENT then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  end;
end;

{******************************************************************************}
{* Destroy the texture class                                                  *}
{******************************************************************************}

destructor  TGDTexture.Destroy();
begin
  glDeleteTextures(1, @FTexture);
end;

{******************************************************************************}
{* Bind the texture to a texture unit                                         *}
{******************************************************************************}

procedure TGDTexture.BindTexture(aTU : GLenum);
begin
  glActiveTexture(aTU);
  glBindTexture(GL_TEXTURE_2D, FTexture);
end;

end.
