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
unit GDLog;

{$MODE Delphi}

{******************************************************************************}
{* Holds the log class for the engine.                                        *}
{******************************************************************************}

interface

uses
  Classes,
  SysUtils,
  LCLIntf, LCLType,
  GDConstants;

type

{******************************************************************************}
{* Log class                                                                  *}
{******************************************************************************}

  TGDLog = class
  private
    FFileName : String;
    FText : TStringList;
    FUse  : Boolean;
    FSave : Boolean;
  public
    property Text : TStringList read FText;
    property Use : Boolean read FUse write FUse;
    property Save : Boolean read FSave write FSave;

    constructor Create(aFileName : String);
    destructor  Destroy(); override;

    procedure AddNewLine(aString : String);
    procedure AddToLastLine(aString : String);
    procedure ClearLog();
    procedure SaveLog();
  end;

var
  Log : TGDLog;

implementation

{******************************************************************************}
{* Create the log class                                                       *}
{******************************************************************************}

constructor TGDLog.Create(aFileName : String);
var
  iDateTime : TDateTime;
  iString : String;
begin
  FText := TStringList.Create();
  FUse := true;
  FSave := true;

  FFileName := aFileName;

  iString := 'Log started at ';
  iDateTime := Date();
  iString := iString + DateToStr(iDateTime) + ', ';
  iDateTime := Time();
  iString := iString + TimeToStr(iDateTime);
  AddNewLine(iString);
  AddNewLine('');

  AddNewLine('---Engine Information---');
  AddNewLine('Build: ' + ENGINE_BUILD);
  AddNewLine('Build Date: ' + ENGINE_BUILDDATE);
  AddNewLine('------------------------');
  AddNewLine('');
end;

{******************************************************************************}
{* Destroy the log class                                                      *}
{******************************************************************************}

destructor  TGDLog.Destroy();
var
  iDateTime : TDateTime;
  iString : String;
begin
  inherited;
  AddNewLine('');
  iString := 'Log ended at ';
  iDateTime := Date();
  iString := iString + DateToStr(iDateTime) + ', ';
  iDateTime := Time();
  iString := iString + TimeToStr(iDateTime);
  AddNewLine(iString);
  SaveLog();
  FreeAndNil(FText);
end;

{******************************************************************************}
{* Add a new line to the log                                                  *}
{******************************************************************************}

procedure TGDLog.AddNewLine(aString : String);
begin
  If FUse = False then exit;
  FText.Add(aString);
  SaveLog();
end;

{******************************************************************************}
{* Add behind the last log line                                               *}
{******************************************************************************}

procedure TGDLog.AddToLastLine(aString : String);
begin
  If FUse = False then exit;
  FText.Strings[FText.Count-1] := FText.Strings[FText.Count-1] + aString;
  SaveLog();
end;

{******************************************************************************}
{* Clear the log                                                              *}
{******************************************************************************}

procedure TGDLog.ClearLog();
begin
  FText.Clear();
  SaveLog();
end;

{******************************************************************************}
{* Save the log to a file                                                     *}
{******************************************************************************}

procedure TGDLog.SaveLog();
begin
  If FSave then FText.SaveToFile(FFileName);
end;

end.
