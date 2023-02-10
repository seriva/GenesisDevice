{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
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
unit uGDBmp;

{$mode objfpc}

interface

uses
  Math,
  Classes,
  Interfaces,
  SysUtils;

type
  TRGBA = packed record
    B, G, R, A: Byte;
  end;

  TGDBmp = class
  private
    FData : array of TRGBA;
    FWidth, FHeight : integer;
  public
    property Width : Integer read FWidth;
    property Height : Integer read FHeight;

    constructor Create( aName : String); overload;
    destructor  Destroy(); override;

    function GetRGBA(aX, aY : Integer): TRGBA;
		function GetInt(aX, aY : Integer): Integer;
  end;

implementation

constructor TGDBmp.Create( aName : String); overload;
type
	TBitmapFileHeader = packed record
  	bfType: Word;
  	bfSize: DWORD;
  	bfReserved1: Word;
  	bfReserved2: Word;
  	bfOffBits: DWORD;
	end;
  TBitmapInfoHeader = record
    biSize: DWORD;
    biWidth: LongInt;
    biHeight: LongInt;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: LongInt;
    biYPelsPerMeter: LongInt;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;
var
  iLen, iBits, iPX, iI, iJ, iC : Integer;
  iFileHeader : TBitmapFileHeader;
  iInfoHeader : TBitmapInfoHeader;
  iData       : TMemoryStream;
  iB          : byte;
  iLB, iL     : Integer;

  function IntToRGBA(aPixel : integer):TRGBA;
  begin
    result.B := Byte(aPixel);
    result.G := Byte(aPixel shr 8);
    result.R := Byte(aPixel shr 16);
    result.A := Byte(aPixel shr 24);
  end;
begin
  iData := TMemoryStream.Create();
  iData.LoadFromFile(aName);

  iData.Read(iFileHeader, SizeOf(iFileHeader));
  iData.Read(iInfoHeader, SizeOf(iInfoHeader));
  iData.Position := iFileHeader.bfOffBits;

  FWidth:= iInfoHeader.biWidth;
  FHeight:= iInfoHeader.biHeight;
  iLen:= FWidth * FHeight;
  iBits:= iInfoHeader.biBitCount div 8;
  SetLength(FData,iLen);

  iL := 0;
  iLB := Width * iBits;
  if iLB mod 4 > 0 then
   iL := (ceil(iLB / 4) * 4) - iLB;

  for iI := 0 to FHeight-1 do
  begin
    for iJ := 0 to FWidth-1 do
    begin
      iPX:= 0;
      for iC:= 1 to iBits do
      begin
        iData.Read(iB, SizeOf(Byte));
        iPX:= iPX + iB;
      end;
      if iBits = 4 then
        iPX:= iPX div 3
      else
        iPX:= iPX div iBits;
      if iPX <> 0 then
        FData[(iI*FHeight) + iJ] := InttoRGBA(iPX);
    end;

    for iC:= 0 to iL-1 do
    	iData.Read(iB, SizeOf(Byte));
  end;

  FreeAndNil(iData);
end;

destructor TGDBmp.Destroy();
begin
  SetLength(FData, 0);
end;

function TGDBmp.GetRGBA(aX, aY : Integer): TRGBA;
begin
  result := FData[((FWidth-aY-1) * FWidth) + aX];
end;

function TGDBmp.GetInt(aX, aY : Integer): Integer;
var
  iPX : TRGBA;
begin
  iPX := GetRGBA(aX, aY);
  result := (iPX.b or (iPX.g shl 8) or (iPX.r shl 16) or (iPX.a shl 24));
end;

end.

