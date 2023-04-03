unit uGDConsole;

{$MODE objfpc}

interface

uses
  FGL,
  SysUtils,
  Classes,
  SDL2,
  dglOpenGL,
  uGDGUI,
  uGDConstants,
  uGDStringParsing;

type
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

  TGDCommandMap = specialize TFPGMap<String, TGDCommand>;

  TGDConsole = class
  private
    FLastTime       : Integer;
    FCursorUpdate   : Boolean;
    FShow           : Boolean;
    FUse            : Boolean;
    FRow            : integer;
    FCursorPos      : integer;
    FLogText        : TStringList;
    FCommand        : String;
    FCommandRow     : integer;
    FCommandHistory : TStringList;
  public
    CommandMap       : TGDCommandMap;
    property Use     : Boolean read FUse write FUse;
    property Show    : Boolean read FShow write FShow;
    property Command : String read FCommand write FCommand;

    constructor Create();
    destructor  Destroy(); override;

    procedure Render();

    procedure AddChar( aChar : Char );
    procedure Control( aKey : Integer );

    procedure Write(aString : String; aNewLine : boolean = true);
    procedure WriteOkFail(aResult : boolean; aError : String; aIncludeFailed : boolean = true);

    procedure AddCommand(aCommand, aHelp : String; aType : TGDCommandType; aPointer : Pointer );
    procedure ExecuteCommand(aCommand : String);
  end;

implementation

uses
  uGDEngine;


procedure Help();
var
  iK : Integer;
  iCommand : TGDCommand;
begin
  GDConsole.Write('');
  for ik := 0 to GDConsole.CommandMap.Count - 1 do
  begin
    iCommand := GDConsole.CommandMap.Data[ik];
    GDConsole.Write(iCommand.Command + ' - ' + iCommand.Help);
  end;
  GDConsole.Write('');
end;


constructor TGDConsole.Create();
begin
  FUse          := True;
  FLogText      := TStringList.Create();
  FCommandHistory := TStringList.Create();
  CommandMap    := TGDCommandMap.Create();
  CommandMap.sorted := True;
  FCommandHistory.sorted := True;
  FCursorUpdate := False;
  AddCommand('Help', 'Show help', CT_FUNCTION, @Help);
  Write('Log started at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  FRow := FLogText.Count-1;
  FShow := false;
  FCommand := '';
  FLastTime := GDTiming.GetTime()+500;
end;


destructor  TGDConsole.Destroy();
begin
  Write('Log ended at ' + DateToStr(Date()) + ', ' + TimeToStr(Time()));
  FreeAndNil(CommandMap);
  FreeAndNil(FLogText);
  FreeAndNil(FCommandHistory);
end;


procedure TGDConsole.Render();
var
  iI,iJ,iX : Integer;
  iHalf : Integer;
begin
  If Not(FShow) then
  begin
    FRow := FLogText.Count-1;
    exit;
  end;

  //calculate cursor timing
  if SDL_TICKS_PASSED(GDTiming.GetTime(), FLastTime) then
  begin
    FLastTime := GDTiming.GetTime()+500;
    FCursorUpdate := Not(FCursorUpdate);
  end;

  iHalf := round(R_HUD_HEIGHT/2);
  RenderFlatQuad(0, iHalf-7, R_HUD_WIDTH, iHalf+7, true, false);
  RenderFlatQuad(-5, iHalf-7, R_HUD_WIDTH+5, 32, false, true);

  GDRenderer.RenderState(RS_TEXTS);
  iJ := 0;
  For iI := FRow downto FRow-C_MAX_LINES do
  begin
    If  ((iI) >= 0) then
    begin
      If copy(Uppercase(FLogText.Strings[iI]), 0, 5) = 'ERROR' then
        GDGUI.Font.Color.Red
      else
        GDGUI.Font.Color := GDGUI.FontColor.Copy();
      GDGUI.Font.Render(0, (R_HUD_HEIGHT/2)+28+(iJ*25), 0.40, FLogText.Strings[iI] );
      iJ := iJ + 1;
    end;
  end;

  GDGUI.Font.Color := GDGUI.FontColor.Copy();
  GDGUI.Font.Render(0, (R_HUD_HEIGHT/2)-3, 0.40, FCommand);
  iX := GDGUI.Font.TextWidth(Copy(FCommand, 1, FCursorpos-1), 0.40);
  if FCursorUpdate then
     GDGUI.Font.Render(iX, (R_HUD_HEIGHT/2)-3, 0.40, '_' );

  glDisable(GL_BLEND);
end;


procedure TGDConsole.AddChar( aChar : Char );
begin
  If Not(FShow) then Exit;
  If Not(((Ord(aChar) >= 32) and (Ord(aChar) <= 126))) then Exit;
  If aChar = '`' then Exit;
  Insert(aChar, FCommand, FCursorPos);
  FCursorPos := FCursorPos + 1;
end;


procedure TGDConsole.Control( aKey : Integer );
begin
  if aKey = SDLK_BACKQUOTE then
  begin
    FShow := not(FShow);
    If Not(FShow) then
    begin
      GDWindow.SetMouse();
      SDL_StopTextInput();
      Exit;
    end
    else
    begin
      FCursorPos := length(FCommand)+1;
      SDL_StartTextInput()
    end;
  end;

  case aKey of
    SDLK_PAGEUP : begin
                    If FLogText.Count = 0 then exit;
                    FRow := FRow - 1;
                    If FRow < 0 then FRow := 0;
                  end;
    SDLK_PAGEDOWN : begin
                      If FLogText.Count = 0 then exit;
                      FRow := FRow + 1;
                      If FRow > FLogText.Count-1 then FRow := FLogText.Count-1;
                    end;
    SDLK_UP : begin
                If FCommandHistory.Count = 0 then exit;
                FCommandRow := FCommandRow - 1;
                If FCommandRow < 0 then
                  FCommandRow := FCommandHistory.Count-1;
                FCommand :=  FCommandHistory.Strings[FCommandRow];
                FCursorPos := length(FCommand)+1;
              end;
    SDLK_DOWN : begin
                  If FCommandHistory.Count = 0 then exit;
                  FCommandRow := FCommandRow + 1;
                  If FCommandRow > FCommandHistory.Count-1 then
                    FCommandRow := 0;
                  FCommand :=  FCommandHistory.Strings[FCommandRow];
                  FCursorPos := length(FCommand)+1;
                end;
    SDLK_BACKSPACE : begin
                       if FCursorPos = 1 then exit;
                       Delete(FCommand, FCursorPos-1, 1);
                       FCursorPos := FCursorPos - 1;
                     end;
    SDLK_LEFT : begin
                  if (FCursorPos = 1) then exit;
                  FCursorPos := FCursorPos - 1
                end;
    SDLK_RIGHT : begin
                   if (FCursorPos = (length(FCommand) + 1)) then exit;
                   FCursorPos := FCursorPos + 1
                 end;
    SDLK_RETURN : ExecuteCommand(FCommand);
  end;
end;


procedure TGDConsole.Write(aString : String; aNewLine : boolean = true);
begin
  If FUse = False then exit;
  if aNewLine then
    FLogText.Add(aString)
  else
    FLogText.Strings[FLogText.Count-1] := FLogText.Strings[FLogText.Count-1] + aString;
  FLogText.SaveToFile(ENGINE_LOG);
end;


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


procedure TGDConsole.AddCommand(aCommand, aHelp : String; aType : TGDCommandType; aPointer : Pointer );
var
  iCommand : TGDCommand;
begin
  iCommand.Command      := lowercase(aCommand);
  iCommand.Help         := aHelp;
  iCommand.CommandType  := aType;
  case iCommand.CommandType of
    CT_BOOLEAN  : iCommand.Bool  := aPointer;
    CT_INTEGER  : iCommand.Int   := aPointer;
    CT_FLOAT    : iCommand.Float := aPointer;
    CT_FUNCTION : iCommand.Func  := PFunction(aPointer);
  end;
  CommandMap.Add(iCommand.Command,iCommand);
  CommandMap.Sort;
end;


procedure TGDConsole.ExecuteCommand(aCommand : String);
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
  if aCommand = '' then exit;

  //add command string
  FCommand := aCommand;
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
