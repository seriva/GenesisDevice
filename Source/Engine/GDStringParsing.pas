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
unit GDStringParsing;

{$mode delphi}

interface

uses
  LCLIntf,
  LCLType,
  Classes,
  GDTypes,
  IniFiles,
  SysUtils,
  FileUtil;

Var
  CommentString : String = '//';
  StringToken   : String = '"';

function CharacterIsWhiteSpace(const aChar : AnsiChar ): boolean;
function GetNextToken(const aFile : TMemoryStream): String;
function ReadColor(const aIniFile : TIniFile; aSection, aSubName : String): TGDColor;
function ReadVector(const aIniFile : TIniFile; aSection, aSubName : String): TGDVector;

implementation

function CharacterIsWhiteSpace(const aChar : AnsiChar ): boolean;
begin
  result := ((aChar = ' ') or (aChar = #9) or (aChar = #10) or (aChar = #13));
end;

function CharacterIsStringToken(const aChar : AnsiChar ): boolean;
begin
  result := (aChar = StringToken);
end;

function GetNextToken(const aFile : TMemoryStream): String;
var
  iC : AnsiChar;
  iStr : String;
begin
  //skip any whitespace
  while (aFile.Position < aFile.Size) do
  begin
    aFile.Read(iC, 1);
    if Not(CharacterIsWhiteSpace(iC)) then
    begin
      if aFile.Position > 0 then
        aFile.Position := aFile.Position - 1;
      break;
    end;
  end;

  //now read the next token
  result := '';
  aFile.Read(iC, 1);

  //are we dealing with a string token?
  if CharacterIsStringToken(iC) then
  begin
    //read the string string:)
    aFile.Read(iC, 1);
    while Not(CharacterIsStringToken(iC)) and (aFile.Position < aFile.Size) do
    begin
      result := result + String(iC);
      aFile.Read(iC, 1);
    end;
  end
  else
  begin
    //read the token.
    while Not(CharacterIsWhiteSpace(iC)) and (aFile.Position < aFile.Size) do
    begin
      result := result + String(iC);
      aFile.Read(iC, 1);
    end;
  end;

  // if we have read a comment then skip it and read the next token
  if length(result) >= 2 then
  begin
    iStr := Copy( result, 0, 2);
    if (iStr = CommentString) then
    begin
      //now skip the comment
      aFile.Read(iC, 1);
      while Not(iC = #10) and (aFile.Position < aFile.Size) do
        aFile.Read(iC, 1);

      //recursively read the next token
      result := GetNextToken(aFile);
    end;
  end;
end;

function ReadColor(const aIniFile : TIniFile; aSection, aSubName : String): TGDColor;
var
  lStr : String;
  lSplit : TStringList;
begin
  lSplit := TStringList.create;
  try
    lStr := aIniFile.ReadString(aSection, aSubName, '1,1,1,1' );
    ExtractStrings([','], [], PChar(lStr), lSplit);
    result.Reset(StrToFloat(lSplit.Strings[0]), StrToFloat(lSplit.Strings[1]),
                 StrToFloat(lSplit.Strings[2]), StrToFloat(lSplit.Strings[3]));
  finally
    lSplit.free;
  end;
end;

function ReadVector(const aIniFile : TIniFile; aSection, aSubName : String): TGDVector;
var
  lStr : String;
  lSplit : TStringList;
begin
  lSplit := TStringList.create;
  try
    lStr := aIniFile.ReadString(aSection, aSubName, '1,1,1' );
    ExtractStrings([','], [], PChar(lStr), lSplit);
    result.Reset(StrToFloat(lSplit.Strings[0]), StrToFloat(lSplit.Strings[1]), StrToFloat(lSplit.Strings[2]));
  finally
    lSplit.free;
  end;
end;

end.

