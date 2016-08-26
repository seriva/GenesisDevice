{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
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
unit GDBmp;

{$mode objfpc}

interface

uses
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

    constructor Create( aFileName : String); overload;
    destructor  Destroy(); override;

    function GetRGBA(aX, aY : Integer): TRGBA;
		function GetInt(aX, aY : Integer): Integer;
  end;

implementation

uses
  GDEngine;

type
  TFile = file of Byte;

constructor TGDBmp.Create( aFileName : String); overload;
  function Read8(var f: TFile): integer;
  var
    b: byte;
  begin
    read(f,b);
    result:= b;
  end;
  function Read16(var f: TFile): integer;
  var
	  b: byte;
  begin
    read(f,b);
    result:= b;
    read(f,b);
    result:= result or (b shl 8);
  end;
  function Read24(var f: TFile): integer;
  var
    b: byte;
  begin
    read(f,b);
    result:= b;
    read(f,b);
    result:= result or (b shl 8);
    read(f,b);
    result:= result or (b shl 16);
  end;
  function Read32(var f: TFile): integer;
  var
    b: byte;
  begin
    read(f,b);
    result:= b;
    read(f,b);
    result:= result or (b shl 8);
    read(f,b);
    result:= result or (b shl 16);
    read(f,b);
    result:= result or (b shl 24);
  end;
  function IntToRGBA(aPixel : integer):TRGBA;
  begin
    result.B := Byte(aPixel);
    result.G := Byte(aPixel shr 8);
    result.R := Byte(aPixel shr 16);
    result.A := Byte(aPixel shr 24);
  end;
var
  offset,bits,i,c,px,len: integer;
  f: TFile;
begin
  Assign(f,aFileName);
  Reset(f);
  seek(f,10);
  offset:= read32(f);
  read32(f);
  FWidth:= read32(f);
  FHeight:= read32(f);
  read16(f);
  bits:= read16(f) div 8;
  seek(f,	offset);
  i:= 0;
  len:= FWidth * FHeight;
  SetLength(FData,len);


    while i < len do
    begin
      px:= 0;
      for c:= 1 to bits do
      begin
        px:= px + read8(f);
      end;
      if bits = 4 then
        px:= px div 3
      else
        px:= px div bits;
      if px <> 0 then
        FData[i] := InttoRGBA(px);
      inc(i);
    end;

  Close(f);
end;

destructor TGDBmp.Destroy();
begin
  SetLength(FData, 0);
end;

function TGDBmp.GetRGBA(aX, aY : Integer): TRGBA;
begin
  result := FData[(aY * FWidth) + aX]
end;

function TGDBmp.GetInt(aX, aY : Integer): Integer;
var
  iPX : TRGBA;
begin
  iPX := GetRGBA(aX, aY);
  result := (iPX.b or (iPX.g shl 8) or (iPX.r shl 16) or (iPX.a shl 24));
end;

end.

