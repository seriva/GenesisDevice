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

    constructor Create(aFileName : String);
    destructor  Destroy(); override;

    procedure Write(aString : String; aNewLine : boolean = true);
    procedure WriteOkFail(aResult : boolean; aError : String);
  end;

var
  Log : TGDLog;

implementation

{******************************************************************************}
{* Create the log class                                                       *}
{******************************************************************************}

constructor TGDLog.Create(aFileName : String);
begin
  FText := TStringList.Create();
  FUse  := true;
  FSave := true;
  FFileName := aFileName;
  Write('Log started at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  Write('Build: ' + ENGINE_INFO);
end;

{******************************************************************************}
{* Destroy the log class                                                      *}
{******************************************************************************}

destructor  TGDLog.Destroy();
begin
  inherited;
  Write('Log ended at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  FreeAndNil(FText);
end;

{******************************************************************************}
{* Write to the log                                                           *}
{******************************************************************************}

procedure TGDLog.Write(aString : String; aNewLine : boolean = true);
begin
  If FUse = False then exit;
  if aNewLine then
    FText.Add(aString)
  else
    FText.Strings[FText.Count-1] := FText.Strings[FText.Count-1] + aString;
  If FSave then FText.SaveToFile(FFileName);
end;

{******************************************************************************}
{* Write Ok or fail to the log                                                *}
{******************************************************************************}

procedure TGDLog.WriteOkFail(aResult : boolean; aError : String);
begin
  If aResult then
  begin
    Log.Write('Ok', false);
  end
  else
  begin
    Log.Write('Failed', false);
    Log.Write('Error Message: ' + aError);
  end;
end;

end.
