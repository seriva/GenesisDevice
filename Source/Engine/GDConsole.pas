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
  GDGUI,
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
    FShow           : Boolean;
    FUse            : Boolean;
    FRow            : integer;
    FCursorUpdate   : boolean;
    FCursorPos      : integer;
    FUpdateTimer    : Integer;
    FLogText        : TStringList;
    FCommand        : String;
    FCommandRow     : integer;
    FCommandHistory : TStringList;
  public
    CommandMap : TGDCommandMap<String, TGDCommand>;
    property Use  : Boolean read FUse write FUse;
    property Show : Boolean read FShow write FShow;
    property Command : String read FCommand write FCommand;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitConsole();
    procedure Clear();

    procedure Render();
    procedure Update();

    procedure AddChar( aChar : Char );
    procedure Control( aKey : Integer );

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
{* Show help                                                                  *}
{******************************************************************************}

procedure Help();
var
  iK : Integer;
  iCommand : TGDCommand;
begin
  Console.Write('');
  for ik := 0 to Console.CommandMap.Count - 1 do
  begin
    iCommand := Console.CommandMap.Data[ik];
    Console.Write(iCommand.Command + ' - ' + iCommand.Help);
  end;
  Console.Write('');
end;

{******************************************************************************}
{* Create the console class                                                   *}
{******************************************************************************}

constructor TGDConsole.Create();
begin
  FUse          := True;
  FShow         := False;
  FLogText      := TStringList.Create();
  FCommandHistory := TStringList.Create();
  CommandMap    := TGDCommandMap<String, TGDCommand>.Create();
  FCursorUpdate := False;
  AddCommand('Help', 'Show help', CT_FUNCTION, @Help);
  FUpdateTimer  := TimeSetEvent(C_CURSOR_TIME, 0, @UpdateConsoleCallBack, 0, TIME_PERIODIC);
  Write('Log started at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  Write('Build: ' + ENGINE_INFO);
end;

{******************************************************************************}
{* Destroy the console class                                                  *}
{******************************************************************************}

destructor  TGDConsole.Destroy();
begin
  Write('Log ended at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  TimeKillEvent(FUpdateTimer);
  FreeAndNil(CommandMap);
  FreeAndNil(FLogText);
  FreeAndNil(FCommandHistory);
end;

{******************************************************************************}
{* Init the console                                                           *}
{******************************************************************************}

procedure TGDConsole.InitConsole();
begin
  Clear();
  FRow := FLogText.Count-1;
  FShow := false;
  FCommand := '';
end;

{******************************************************************************}
{* Clear the console                                                          *}
{******************************************************************************}

procedure TGDConsole.Clear();
begin
end;

{******************************************************************************}
{* Render the console                                                         *}
{******************************************************************************}

procedure TGDConsole.Render();
var
  iI,iJ,iX : Integer;
begin
  If Not(FShow) then
  begin
    FRow := FLogText.Count-1;
    exit;
  end;

  Renderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4fv(GUI.FillColor.ArrayPointer());
  glBegin(GL_QUADS);
    glVertex2f(0, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, (R_HUDHEIGHT/2)-7);
    glVertex2f(R_HUDWIDTH, R_HUDHEIGHT);
    glVertex2f(0, R_HUDHEIGHT);
  glEnd;
  glColor4fv(GUI.OutlineColor.ArrayPointer());
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
      If copy(Uppercase(FLogText.Strings[iI]), 0, 5) = 'ERROR' then
        GUI.Font.Color.Red
      else
        GUI.Font.Color := GUI.FontColor.Copy();
      GUI.Font.Render(0, (R_HUDHEIGHT/2)+28+(iJ*25), 0.40, FLogText.Strings[iI] );
      iJ := iJ + 1;
    end
  end;

  GUI.Font.Color := GUI.FontColor.Copy();
  GUI.Font.Render(0, (R_HUDHEIGHT/2)-3, 0.40, FCommand);
  iX := GUI.Font.TextWidth(Copy(FCommand, 1, FCursorpos-1), 0.40);
  if FCursorUpdate then
     GUI.Font.Render(iX, (R_HUDHEIGHT/2)-3, 0.40, '_' );

  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Add a character to the console command input                               *}
{******************************************************************************}

procedure TGDConsole.AddChar( aChar : Char );
begin
  If Not(FShow) then Exit;
  If Not(((Ord(aChar) >= 32) and (Ord(aChar) <= 126))) then Exit;
  If aChar = '`' then Exit;
  Insert(aChar, FCommand, FCursorPos);
  FCursorPos := FCursorPos + 1;
end;

{******************************************************************************}
{* Control the console                                                        *}
{******************************************************************************}

procedure TGDConsole.Control( aKey : Integer );
begin
  if aKey = 192 then
  begin
    FShow := not(FShow);
    If Not(FShow) then
      Exit
    else
      FCursorPos := length(FCommand)+1;
  end;

  case aKey of
    VK_PRIOR  : begin
                  If FLogText.Count = 0 then exit;
                  FRow := FRow - 1;
                  If FRow < 0 then FRow := 0;
                end;
    VK_NEXT   : begin
                  If FLogText.Count = 0 then exit;
                  FRow := FRow + 1;
                  If FRow > FLogText.Count-1 then FRow := FLogText.Count-1;
                end;
    VK_UP     : begin
                  If FCommandHistory.Count = 0 then exit;
                  FCommandRow := FCommandRow - 1;
                  If FCommandRow < 0 then
                    FCommandRow := FCommandHistory.Count-1;
                  FCommand :=  FCommandHistory.Strings[FCommandRow];
                  FCursorPos := length(FCommand)+1;
                end;
    VK_DOWN   : begin
                  If FCommandHistory.Count = 0 then exit;
                  FCommandRow := FCommandRow + 1;
                  If FCommandRow > FCommandHistory.Count-1 then
                    FCommandRow := 0;
                  FCommand :=  FCommandHistory.Strings[FCommandRow];
                  FCursorPos := length(FCommand)+1;
                end;
    VK_BACK   : begin
                  if FCursorPos = 1 then exit;
                  Delete(FCommand, FCursorPos-1, 1);
                  FCursorPos := FCursorPos - 1;
                end;
    VK_LEFT   : begin
                  if (FCursorPos = 1) then exit;
                  FCursorPos := FCursorPos - 1
                end;
    VK_RIGHT  : begin
                  if (FCursorPos = (length(FCommand) + 1)) then exit;
                  FCursorPos := FCursorPos + 1
                end;
    VK_RETURN : ExecuteCommand();
  end;
end;

{******************************************************************************}
{* Update the console                                                         *}
{******************************************************************************}

procedure TGDConsole.Update();
begin
  FCursorUpdate := Not( FCursorUpdate );
end;

{******************************************************************************}
{* Update Console Callback                                                    *}
{******************************************************************************}

procedure UpdateConsoleCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
begin
  if Console <> nil then
     Console.Update();
end;

{******************************************************************************}
{* Write to the log                                                           *}
{******************************************************************************}

procedure TGDConsole.Write(aString : String; aNewLine : boolean = true);
begin
  If FUse = False then exit;
  if aNewLine then
    FLogText.Add(aString)
  else
    FLogText.Strings[FLogText.Count-1] := FLogText.Strings[FLogText.Count-1] + aString;
  FLogText.SaveToFile(ENGINE_LOG);
end;

{******************************************************************************}
{* Write Ok or fail to the log                                                *}
{******************************************************************************}

procedure TGDConsole.WriteOkFail(aResult : boolean; aError : String; aIncludeFailed : boolean = true);
begin
  If aResult then
  begin
    Write('Ok', false);
  end
  else
  begin
    if aIncludeFailed then Write('Failed', false);
    Write('Error: ' + aError);
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
  CommandMap.Add(iCommand.Command,iCommand);
  CommandMap.Sort;
end;

{******************************************************************************}
{* Add a command to the console.                                              *}
{******************************************************************************}

procedure TGDConsole.ExecuteCommand();
var
  iI : Integer;
  iIdx : Integer;
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
  if FCommand = '' then exit;

  //add command string
  Write(FCommand);
  If Not(FCommandHistory.Find( FCommand, iI )) then
    FCommandHistory.Add(FCommand);

  //get the command parameters
  iStrPos := 1;
  iCommandStr  := lowercase(GetNextCommand(FCommand));
  iCommandPara := lowercase(GetNextCommand(FCommand));

  //execute the commands
  if CommandMap.Find(iCommandStr, iIdx) then
  begin
    iCommand := CommandMap.Data[iIdx];
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
  FCommand := '';
  FRow := FLogText.Count-1;
  FCursorPos := 1;
end;

end.
