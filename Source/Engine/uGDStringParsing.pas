unit uGDStringParsing;

{$mode delphi}

interface

uses
  LCLIntf,
  LCLType,
  Classes,
  SysUtils,
  uGDTypes,
  FileUtil;

Var
  CommentString : String = '//';
  StringToken   : String = '"';

function CharacterIsWhiteSpace(const aChar : AnsiChar ): boolean;
function GetNextToken(const aFile : TMemoryStream): String;

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

end.

