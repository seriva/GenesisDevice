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
unit GDConsole;

{$MODE Delphi}

{******************************************************************************}
{* Holds the console class                                                    *}
{******************************************************************************}

interface

uses
  SysUtils,
  Classes,
  LCLIntf,
  LCLType,
  MMSystem,
  dglOpenGL,
  GDLog,
  GDFont,
  GDTypes,
  GDRenderer,
  GDConstants;

type

{******************************************************************************}
{* Console input record                                                       *}
{******************************************************************************}

  TGDConsoleInput = record
    LineR : Double;
    LineG : Double;
    LineB : Double;
    LineA : Double;
    BackR : Double;
    BackG : Double;
    BackB : Double;
    BackA : Double;
  end;

{******************************************************************************}
{* Console class                                                              *}
{******************************************************************************}

  TGDConsole = class
  private
    FShow              : Boolean;
    FBackGroundColor   : TGDColor;
    FLineColor         : TGDColor;
    FInputString       : String;
    FRow               : integer;
    FInputRow          : integer;
    FInputStringList   : TStringList;
    FExecuteCommand    : Boolean;
    FCursorUpdate      : boolean;
    FUpdateTimer  : Integer;
  public
    property Show : Boolean read FShow write FShow;
    property CommandString : String read FInputString write FInputString;
    property Row : Integer read FRow write FRow;
    property ExecuteCommand : Boolean read FExecuteCommand write FExecuteCommand;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitConsole( aInput : TGDConsoleInput );
    procedure Clear();

    procedure Render();
    procedure MoveUp();
    procedure MoveDown();
    procedure MoveInputUp();
    procedure MoveInputDown();
    procedure AddChar( aChar : Char );
    procedure RemoveChar();
    procedure AddLine();
    procedure Update();
  end;

var
  Console : TGDConsole;

  procedure UpdateConsoleCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;

implementation

{******************************************************************************}
{* Create the console class                                                   *}
{******************************************************************************}

constructor TGDConsole.Create();
begin
  FShow := False;
  FCursorUpdate := False;
  FLineColor := TGDColor.Create(1,1,1,1);
  FBackGroundColor := TGDColor.Create(1,1,1,1);
  FInputStringList := TStringList.Create();
  FUpdateTimer  := TimeSetEvent(C_CURSOR_TIME, 0, @UpdateConsoleCallBack, 0, TIME_PERIODIC);
end;

{******************************************************************************}
{* Destroy the console class                                                  *}
{******************************************************************************}

destructor  TGDConsole.Destroy();
begin
  TimeKillEvent(FUpdateTimer);
  FreeAndNil(FBackGroundColor);
  FreeAndNil(FLineColor);
  FreeAndNil(FInputStringList);
end;

{******************************************************************************}
{* Init the console                                                           *}
{******************************************************************************}

procedure TGDConsole.InitConsole( aInput : TGDConsoleInput );
begin
  Clear();
  FLineColor.Reset( aInput.LineR, aInput.LineG, aInput.LineB, aInput.LineA);
  FBackGroundColor.Reset(aInput.BackR, aInput.BackG, aInput.BackB, aInput.BackA);
  FRow := Log.Text.Count-1;
end;

{******************************************************************************}
{* Clear the console                                                          *}
{******************************************************************************}

procedure TGDConsole.Clear();
begin
  FExecuteCommand := false;
  FLineColor.Reset(1,1,1,1);
  FBackGroundColor.Reset(1,1,1,1);
end;

{******************************************************************************}
{* Render the console                                                         *}
{******************************************************************************}

procedure TGDConsole.Render();
var
  iI,iJ : Integer;
begin
  If Not(FShow) then
  begin
    FRow := Log.Text.Count-1;
    exit;
  end;

  Renderer.RenderState( RS_COLOR );
  glColor4fv(FBackGroundColor.ArrayPointer);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glBegin(GL_QUADS);
    glVertex2f(0, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, R_HUDHEIGHT);
    glVertex2f(0, R_HUDHEIGHT);
  glEnd;
  glDisable(GL_BLEND);
  glColor4fv(FLineColor.ArrayPointer);
  glBegin(GL_LINES);
    glVertex2f(0,          (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, (R_HUDHEIGHT/2)-7);
    glVertex2f(0,          (R_HUDHEIGHT/2)+25);
    glVertex2f(R_HUDWIDTH, (R_HUDHEIGHT/2)+25);
  glEnd;

  Renderer.RenderState(RS_TEXTS);
  iJ := 0;
  For iI := FRow downto FRow-C_MAX_LINES do
  begin
    If  ((iI) >= 0) then
    begin
      If copy(Uppercase(Log.Text.Strings[iI]), 0, 5) = 'ERROR' then
        SystemFont.Color.Red
      else
        SystemFont.Color.White;
      SystemFont.Render(0, (R_HUDHEIGHT/2)+63+(iJ*25), 1, Log.Text.Strings[iI] );
      iJ := iJ + 1;
    end
  end;

  SystemFont.Color.White;
  if FCursorUpdate then
     SystemFont.Render(0, (R_HUDHEIGHT/2)+32, 1, FInputString + '_' )
  else
     SystemFont.Render(0, (R_HUDHEIGHT/2)+32, 1, FInputString );

  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Move the shown text up                                                     *}
{******************************************************************************}

procedure TGDConsole.MoveUp();
begin
  If Not(FShow) then Exit;
  If Log.Text.Count = 0 then exit;
  FRow := FRow - 1;
  If FRow < 0 then FRow := 0;
end;

{******************************************************************************}
{* Move the shown text down                                                   *}
{******************************************************************************}

procedure TGDConsole.MoveDown();
begin
  If Not(FShow) then Exit;
  If Log.Text.Count = 0 then exit;
  FRow := FRow + 1;
  If FRow > Log.Text.Count-1 then FRow := Log.Text.Count-1;
end;

{******************************************************************************}
{* Add a character to the console command input                               *}
{******************************************************************************}

procedure TGDConsole.AddChar( aChar : Char );
begin
  If Not(FShow) then Exit;
  If Not(((Ord(aChar) >= 32) and (Ord(aChar) <= 126))) then Exit;
  If aChar = '`' then Exit;
  FInputString := FInputString + aChar;
end;

{******************************************************************************}
{* Remove a character to the console command input                            *}
{******************************************************************************}

procedure TGDConsole.RemoveChar();
begin
   If Not(FShow) then Exit;
   SetLength(FInputString, Length(FInputString)-1);
end;

{******************************************************************************}
{* Add a line to the console                                                  *}
{******************************************************************************}

procedure TGDConsole.AddLine();
var
  iI : Integer;
  iBool : Bool;
begin
  If Not(FShow) then Exit;
  Log.AddNewLine(FInputString);

  iBool := false;
  For iI := 0 to FInputStringList.Count-1 do
  begin
    If FInputStringList.Strings[iI] = FInputString then
      iBool := true;
  end;

  If iBool = false then
    FInputStringList.Add(FInputString);

  FRow := Log.Text.Count-1;
end;

{******************************************************************************}
{* Select old command                                                         *}
{******************************************************************************}

procedure TGDConsole.MoveInputUp();
begin
  If Not(FShow) then Exit;
  If FInputStringList.Count = 0 then exit;

  FInputRow := FInputRow - 1;

  If FInputRow < 0 then
    FInputRow := FInputStringList.Count-1;

  FInputString :=  FInputStringList.Strings[FInputRow]
end;

{******************************************************************************}
{* Select old command                                                         *}
{******************************************************************************}

procedure TGDConsole.MoveInputDown();
begin
  If Not(FShow) then Exit;
  If FInputStringList.Count = 0 then exit;

  FInputRow := FInputRow + 1;

  If FInputRow > FInputStringList.Count-1 then
    FInputRow := 0;

  FInputString :=  FInputStringList.Strings[FInputRow]
end;

{******************************************************************************}
{* Update the console                                                         *}
{******************************************************************************}

procedure TGDConsole.Update();
begin
  FCursorUpdate := Not( FCursorUpdate );
end;

{******************************************************************************}
{* Update Console Callback                                                 *}
{******************************************************************************}

procedure UpdateConsoleCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
begin
  Console.Update();
end;

end.
