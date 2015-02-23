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
unit GDFont;

{$MODE Delphi}

{******************************************************************************}
{* Hold the main font class the use of Fontstudio 3 files.                    *}
{* Fontstudio 3 can be found at http://www.nitrogen.za.org/                   *}
{******************************************************************************}

interface

Uses
 Classes,
 LCLIntf, LCLType, LMessages,
 Graphics, FileUtil,
 SysUtils,
 Contnrs,
 dglOpenGL,
 GDTypes,
 GDLog,
 GDObjectList;

type

{******************************************************************************}
{* Character record                                                           *}
{******************************************************************************}

  FCharRect = Record
    FT1,FU1,FT2,FU2, FW, FH, FKernW: single;
  end;

{******************************************************************************}
{* Font class                                                                 *}
{******************************************************************************}

  TGDFont = class
  private
    FChars         : array of FCharRect;
    FFontTexture   : glUInt;
    FStartChar     : integer;
    FFontLength    : integer;
    FCharOffset    : integer;
    FSpaceWidth    : single;
    FAverageHeight : single;
    FColor         : TGDColor;

    procedure   RenderChar(aX, aY, aWidth, aHeigt, aTU1, aTU2, aTV1,aTV2: Double);
    procedure   SetColor(aC : TGDColor);
  public
    Property Color : TGDColor read FColor write SetColor;

    constructor Create();
    destructor  Destroy(); override;
    function    InitFont( aFileName : string) : boolean;
    procedure   Clear();
    procedure   Render( aX, aY, aScale : Double; aString : string);
  end;

var
  SystemFont : TGDFont;
  FontList   : TGDObjectList;

implementation

{******************************************************************************}
{* Set the text color                                                         *}
{******************************************************************************}

procedure TGDFont.SetColor(aC : TGDColor);
begin
  FreeAndNil(FColor);
  FColor := aC;
end;

{******************************************************************************}
{* Create the font class                                                      *}
{******************************************************************************}

constructor TGDFont.Create();
begin
  SetLength(FChars,0);
  FColor := TGDColor.Create();
  FColor.White();
end;

{******************************************************************************}
{* Destroy the font class                                                     *}
{******************************************************************************}

destructor TGDFont.Destroy();
begin
  Clear();
  FreeAndNil(FColor);
  inherited;
end;

{******************************************************************************}
{* Render a single character                                                  *}
{******************************************************************************}

Procedure TGDFont.RenderChar(aX, aY, aWidth, aHeigt, aTU1, aTU2, aTV1,aTV2: Double);
begin
  aTV1 := 1-aTV1;
  aTV2 := 1-aTV2;
  glBegin(GL_QUADS);
    glTexCoord2f(aTU1, aTV2); glVertex2f(aX,        aY-aHeigt);
    glTexCoord2f(aTU2, aTV2); glVertex2f(aX+aWidth, aY-aHeigt);
    glTexCoord2f(aTU2, aTV1); glVertex2f(aX+aWidth, aY);
    glTexCoord2f(aTU1, aTV1); glVertex2f(aX,        aY);
  glEnd;
end;

{******************************************************************************}
{* Init the font                                                              *}
{******************************************************************************}

function TGDFont.InitFont( aFileName : string) : boolean;
Var
  iF1, iF2: TfileStream;
  iI, iIData: integer;
  iStPos, iCtPos: longint;
  iOther: string;
  iData : Array of Byte;
  iW, iWidth : Integer;
  iH, iHeight : Integer;
  iBMP : TBitmap;
  iJPG: TJPEGImage;
  iC : LongWord;
  iLine : PByteArray;
  iError : string;
begin
  result := true;
  Log.AddNewLine('Loading font from file ' + aFileName + '...');
  try
    Setlength(FChars, 1);
    FFontLength := 0;
    FSpaceWidth := 20;
    iOther := Copy(aFileName, 1, length(aFileName)-3)+'jpg';
    iF1 := TFileStream.Create(aFileName, $0000);
    iF2 := Tfilestream.Create(iOther, FmCreate or $0000);
    iF1.Seek(-(sizeof(longint)*2), soFromEnd);
    iF1.Read(iCtpos, sizeof(longint));
    iF1.Read(iStpos, sizeof(longint));
    iF1.Seek(iCtpos, soFromBeginning);
    iF2.CopyFrom(iF1, iStPos-iCtPos-1);
    iF2.Free;
    iF1.Seek(iStpos, soFromBeginning);
    iF1.Read(FFontLength, Sizeof(integer));
    iF1.Read(FStartChar, Sizeof(integer));
    iF1.Read(FCharOffset, Sizeof(integer));
    iF1.Read(iIData, Sizeof(integer));
    Setlength(FChars, FFontLength+1);
    FAverageHeight := 0;
    For iI := 0 to high(FChars) do
    begin
      iF1.Read(FChars[iI], sizeof(FCharRect));
      FAverageHeight := FAverageHeight + FChars[iI].FH;
    end;
    FAverageHeight := FAverageHeight / (FFontLength+1);
    iF1.Free;
    iJPG := TJPEGImage.Create;
    iJPG.LoadFromFile(aFileName);
    iBMP             := TBitmap.Create;
    iBMP.pixelformat := pf24bit;
    iBMP.width       := iJPG.width;
    iBMP.height      := iJPG.height;
    iBMP.canvas.draw(0,0,iJPG);
    iWidth  := iBMP.Width;
    iHeight := iBMP.Height;
    SetLength(iData, iWidth*iHeight*4);
    For iH:=0 to iHeight-1 do
    Begin
      iLine := iBMP.ScanLine[iHeight-iH-1];
      For iW:=0 to iWidth-1 do
      Begin
        iData[(iW*4)+(iH*iWidth*4)]   := iLine[iW*3];
        iData[(iW*4)+1+(iH*iWidth*4)] := iLine[iW*3+1];
        iData[(iW*4)+2+(iH*iWidth*4)] := iLine[iW*3+2];
      End;
    End;
    iJPG.LoadFromFile(iOther);
    iBMP.canvas.draw(0,0,iJPG);
    For iH:=0 to iHeight-1 do
    Begin
      iLine := iBMP.ScanLine[iHeight-iH-1];
      For iW:=0 to iWidth-1 do
      Begin
        iData[(iW*4)+3+(iH*iWidth*4)] := iLine[iW*3];
      End;
    End;
    iBMP.free;
    iJPG.free;
    glGenTextures(1, @FFontTexture);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, iWidth, iHeight, GL_RGBA, GL_UNSIGNED_BYTE, addr(iData[0]));
    DeleteFileUTF8(iOther); { *Converted from DeleteFile* }
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  If result then
  begin
    Log.AddToLastLine('Succeeded');
  end
  else
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: ' + iError);
  end;
end;

{******************************************************************************}
{* Clear the font                                                             *}
{******************************************************************************}

Procedure TGDFont.Clear();
begin
  SetLength(FChars,0);
  glDeleteTextures(1, @FFontTexture);
end;

{******************************************************************************}
{* Render a string                                                            *}
{******************************************************************************}

Procedure TGDFont.Render( aX, aY, aScale : Double; aString : string);
Var
  iI, iLet: integer;
  iX : Single;
begin
  if aString = '' then exit;
  iX := 0;
  glActiveTexture(GL_TEXTURE0);  
  glBindTexture(GL_TEXTURE_2D, FFontTexture);
  glColor4fv(FColor.ArrayPointer);
  glPushMatrix();
  glLoadIdentity();
  glTranslatef(aX, aY,0);
  For iI := 1 to length(aString) do
  begin
    if aString[iI] = ' ' then
    begin
      iX := iX + FSpaceWidth*aScale;
    end
    else
    begin
      iLet := Ord(aString[iI])-FStartChar;
      if ((iLet > -1) and not (iLet > FFontLength)) then
      begin
        RenderChar(iX,0,
                   FChars[iLet].FW*aScale,
                   FChars[iLet].FH*aScale,
                   FChars[iLet].FT1,
                   FChars[iLet].FT2,
                   FChars[iLet].FU1,
                   FChars[iLet].FU2);
         iX := iX + (FChars[iLet].FKernW*aScale);
       end
       else
       begin
         iX := iX + (FSpaceWidth*aScale);
       end;
    end;
  end;
  glPopMatrix();
end;

end.
