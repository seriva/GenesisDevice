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
{* Holds the console class for logging commands                               *}
{******************************************************************************}

interface

uses
  FGL,
  SysUtils,
  Classes,
  LCLIntf,
  LCLType,
  MMSystem,
  dglOpenGL,
  GDFont,
  GDTypes,
  GDConstants,
  GDStringParsing;

type

{******************************************************************************}
{* Console class                                                              *}
{******************************************************************************}

  TGDCommandType = (CT_BOOLEAN, CT_INTEGER, CT_FLOAT, CT_FUNCTION);

  PBoolean  = ^Boolean;
  PInteger  = ^Integer;
  PFloat    = ^Single;
  PFunction = procedure();

  TGDCommand = record
    Command     : String;
    Help        : String;
    CommandType : TGDCommandType;
    Bool        : PBoolean;
    Int         : PInteger;
    Float       : PFloat;
    Func        : PFunction;
  end;

  TGDCommandMap<TKey, TGDCommand> = class(TFPGMap<TKey, TGDCommand>)
  end;

{******************************************************************************}
{* Console class                                                              *}
{******************************************************************************}

  TGDConsole = class
  private
    FShow            : Boolean;
    FInputString     : String;
    FRow             : integer;
    FInputRow        : integer;
    FInputStringList : TStringList;
    FExecuteCommand  : Boolean;
    FCursorUpdate    : boolean;
    FUpdateTimer     : Integer;

    FFileName : String;
    FText : TStringList;
    FUse  : Boolean;
    FSave : Boolean;

    FCommandMap : TGDCommandMap<String, TGDCommand>;
  public
    property Show : Boolean read FShow write FShow;
    property CommandString : String read FInputString write FInputString;
    property Row : Integer read FRow write FRow;

    property Text : TStringList read FText;
    property Use : Boolean read FUse write FUse;

    constructor Create(aLogName : String);
    destructor  Destroy(); override;

    procedure InitConsole();
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

    procedure Write(aString : String; aNewLine : boolean = true);
    procedure WriteOkFail(aResult : boolean; aError : String; aIncludeFailed : boolean = true);

    procedure AddCommand(const aCommand, aHelp : String; const aType : TGDCommandType; const aPointer : Pointer );
    procedure ExecuteCommand();
  end;

var
  Console : TGDConsole;

  procedure UpdateConsoleCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;

implementation

uses
  GDRenderer;

{******************************************************************************}
{* Create the console class                                                   *}
{******************************************************************************}

constructor TGDConsole.Create(aLogName : String);
begin
  FText := TStringList.Create();
  FUse  := true;
  FSave := true;
  FFileName := aLogName;
  Write('Log started at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  Write('Build: ' + ENGINE_INFO);

  FShow := False;
  FCursorUpdate := False;
  FInputStringList := TStringList.Create();
  FUpdateTimer  := TimeSetEvent(C_CURSOR_TIME, 0, @UpdateConsoleCallBack, 0, TIME_PERIODIC);

  FCommandMap := TGDCommandMap<String, TGDCommand>.Create();
end;

{******************************************************************************}
{* Destroy the console class                                                  *}
{******************************************************************************}

destructor  TGDConsole.Destroy();
begin
  TimeKillEvent(FUpdateTimer);
  FreeAndNil(FInputStringList);
  FreeAndNil(FCommandMap);

  Write('Log ended at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  FreeAndNil(FText);
end;

{******************************************************************************}
{* Init the console                                                           *}
{******************************************************************************}

procedure TGDConsole.InitConsole();
begin
  Clear();
  FRow := Console.Text.Count-1;
end;

{******************************************************************************}
{* Clear the console                                                          *}
{******************************************************************************}

procedure TGDConsole.Clear();
begin
  FExecuteCommand := false;
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
    FRow := Console.Text.Count-1;
    exit;
  end;

  Renderer.RenderState( RS_COLOR );
  glColor4f(0.4,0.4,0.4,0.7);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glBegin(GL_QUADS);
    glVertex2f(0, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, R_HUDHEIGHT);
    glVertex2f(0, R_HUDHEIGHT);
  glEnd;
  glDisable(GL_BLEND);
  glColor4f(1,1,1,1);
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
      If copy(Uppercase(Text.Strings[iI]), 0, 5) = 'ERROR' then
        Font.Color.Red
      else
       Font.Color.White;
      Font.Render(0, (R_HUDHEIGHT/2)+28+(iJ*25), 0.40, Text.Strings[iI] );
      iJ := iJ + 1;
    end
  end;

  Font.Color.White;
  if FCursorUpdate then
     Font.Render(0, (R_HUDHEIGHT/2)-3, 0.40, FInputString + '_' )
  else
     Font.Render(0, (R_HUDHEIGHT/2)-3, 0.40, FInputString );

  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Move the shown text up                                                     *}
{******************************************************************************}

procedure TGDConsole.MoveUp();
begin
  If Not(FShow) then Exit;
  If Console.Text.Count = 0 then exit;
  FRow := FRow - 1;
  If FRow < 0 then FRow := 0;
end;

{******************************************************************************}
{* Move the shown text down                                                   *}
{******************************************************************************}

procedure TGDConsole.MoveDown();
begin
  If Not(FShow) then Exit;
  If Console.Text.Count = 0 then exit;
  FRow := FRow + 1;
  If FRow > Console.Text.Count-1 then FRow := Console.Text.Count-1;
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
  Console.Write(FInputString);

  iBool := false;
  For iI := 0 to FInputStringList.Count-1 do
  begin
    If FInputStringList.Strings[iI] = FInputString then
      iBool := true;
  end;

  If iBool = false then
    FInputStringList.Add(FInputString);

  FRow := Console.Text.Count-1;
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


procedure TGDConsole.Write(aString : String; aNewLine : boolean = true);
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

procedure TGDConsole.WriteOkFail(aResult : boolean; aError : String; aIncludeFailed : boolean = true);
begin
  If aResult then
  begin
    Console.Write('Ok', false);
  end
  else
  begin
    if aIncludeFailed then Console.Write('Failed', false);
    Console.Write('Error: ' + aError);
  end;
end;

{******************************************************************************}
{* Add a command to the console.                                              *}
{******************************************************************************}

procedure TGDConsole.AddCommand(const aCommand, aHelp : String; const aType : TGDCommandType; const aPointer : Pointer );
var
  iCommand : TGDCommand;
begin
  iCommand.Command      := lowercase(aCommand);
  iCommand.Help         := aHelp;
  iCommand.CommandType  := aType;
  case iCommand.CommandType of
    CT_BOOLEAN        : iCommand.Bool  := aPointer;
    CT_INTEGER        : iCommand.Int   := aPointer;
    CT_FLOAT          : iCommand.Float := aPointer;
    CT_FUNCTION       : iCommand.Func  := aPointer;
  end;
  FCommandMap.Add(iCommand.Command,iCommand);
  FCommandMap.Sort;
end;

{******************************************************************************}
{* Add a command to the console.                                              *}
{******************************************************************************}

procedure TGDConsole.ExecuteCommand();
var
  iIdx : Integer;
  iI : Integer;
  iCommand : TGDCommand;
  iCommandStr  : String;
  iCommandPara : String;
  iStrPos : Integer;

function GetNextCommand(const aStr : String): String;
var
  iC   : AnsiChar;
begin
  result := '';
  while (iStrPos <= Length(aStr)) do
  begin
    iC := AnsiChar(aStr[iStrPos]);
    if CharacterIsWhiteSpace(iC) then
    begin
      Inc(iStrPos);
      Break;
    end
    else
    begin
      result := result + String(iC);
      Inc(iStrPos);
    end;
  end;
end;

begin
  //no command string so exit
  if CommandString = '' then exit;

  //add command string
  Write(CommandString);
  FInputStringList.Add(CommandString);

  //get the command parameters
  iStrPos := 1;
  iCommandStr  := lowercase(GetNextCommand(CommandString));
  iCommandPara := lowercase(GetNextCommand(CommandString));

  //execute the commands
  if FCommandMap.Find(iCommandStr, iIdx) then
  begin
    iCommand := FCommandMap.Data[iIdx];
    if (iCommand.Bool = nil) and (iCommand.Int = nil) and
       (iCommand.Float = nil) and not(assigned(iCommand.Func)) then
      WriteOkFail(false, 'Command pointer nul!', false)
    else
    begin
      case iCommand.CommandType of
        CT_BOOLEAN   : begin
                         if iCommandPara = '0' then
                           iCommand.Bool^ := false
                         else if iCommandPara = '1' then
                           iCommand.Bool^ := true
                         else
                           WriteOkFail(false, 'Unknown Parameter!', false);
                       end;
        CT_INTEGER   : begin
                         try
                           iCommand.Int^ := StrToInt(iCommandPara);
                         except
                           WriteOkFail(false, 'Unknown Parameter!', false);
                         end;
                       end;
        CT_FLOAT     : begin
                         try
                           iCommand.Float^ := StrToFloat(iCommandPara);
                         except
                           WriteOkFail(false, 'Unknown Parameter!', false);
                         end;
                       end;
        CT_FUNCTION  : begin
                         try
                           iCommand.Func();
                         except
                           WriteOkFail(false, 'Unknown Parameter!' ,false);
                         end;
                       end;
      end;
    end;
  end
  else
    WriteOkFail(false, 'Unknown Command!', false);

  //reset some stuff
  CommandString := '';
  FRow := Text.Count-1;
end;

end.
