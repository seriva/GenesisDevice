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
unit GDInput;

{$MODE Delphi}

{******************************************************************************}
{* This unit holds the input classes of the engine. The input is based on     *}
{* DirectInput 9 and 10 and supports keyboard and mouse. It supports several  *}
{* types of input actions:                                                    *}
{* - Direct (done every frame)                                                *}
{* - Single (done once till the key is pressed again)                         *}
{* - Up     (done as a keyup event)                                           *}
{* - Down   (done as a keydown event)                                         *}
{* These actions can be added to your own needs. Most of them are handled by  *}
{* an internal thread.                                                        *}
{*                                                                            *}
{* Currently only mouse buttons are not supported, but will be in feature     *}
{* versions.                                                                  *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  Classes,
  Windows,
  DXInput,
  GDConstants,
  GDConsole,
  GDSettings,
  GDCamera,
  GDLog,
  GDObjectList;

type

  {******************************************************************************}
  {* Inputaction class                                                          *}
  {******************************************************************************}

  TGDInputAction = class(TObject)
  private
    FKeyString : String;
    FKey       : integer;
    FAction    : TGDProcEngineCallback;
    FConsoleDisabled : boolean;
  public
    property KeyString : String read FKeyString;
    property Key : integer read FKey;

    Constructor Create(aKeyString : String; aKey: integer; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean);
    Destructor  Destroy(); override;

    procedure Execute();
  end;

{******************************************************************************}
{* Input class                                                                *}
{******************************************************************************}
  TGDInput = class;
  TGDInputThreadData = record
    Input : TGDInput;
  end;

  TGDInput = class
  private
    FInitialized     : boolean;
    FDone            : boolean;
    FDirectInput     : IDirectInput8 ;
    FKeyBoard        : IDirectInputDevice8;
    FMouse           : IDirectInputDevice8;
    FKeyBuffer       : array[0..255] of Byte;
    FDIMouseEvent    : THandle;
    FDIKeyBoardEvent : THandle;
    DIMButSwapped    : boolean;
    FEnableInput     : Boolean;
    FDirect          : TGDObjectList;
    FSingle          : TGDObjectList;
    FUp              : TGDObjectList;
    FDown            : TGDObjectList;
    FMouseLook       : boolean;
    FMousePosStart   : TPoint;
    FMousePosCurrent : TPoint;
    FInputThread     : Handle;
    FInputThreadData : TGDInputThreadData;
  public
    property Initialized     : boolean read FInitialized;
    property Done            : boolean read FDone;
    property KeyBoardEvent   : THandle read FDIKeyBoardEvent;
    property MouseEvent      : THandle read FDIMouseEvent;
    property EnableInput     : boolean read FEnableInput write FEnableInput;
    property MouseLook       : boolean read FMouseLook write FMouseLook;
    property MousePosCurrent : TPoint read FMousePosCurrent write FMousePosCurrent;
    property SingleInput     : TGDObjectList read FSingle;

    Constructor Create();
    Destructor  Destroy();override;

    function KeyboardControl( aAcquire: boolean ): boolean;
    function KeyboardState(): Boolean;
    function KeyDown(aKey: byte): boolean;
    function StringToKey( aString : String ) : Byte;
    function KeyToString(aKey : Byte) : String;
    function DetectCurrentKey() : byte;
    function DetectCurrentKeyString() : String;

    function MouseControl(aAcquire: boolean): boolean;
    function GetMousePosition(var aX, aY: LongInt) : boolean;

    procedure ExecuteDirectInput();
    procedure ExecuteMouseMove();
    procedure ExecuteCharInput(aChar : Char);
    procedure CalculateMousePosStart();

    procedure RegisterInputAction(aType : TGDInputTypes; aKeyString : String; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean );
    procedure ClearInputActions();
  end;

var
  Input : TGDInput;

const
  DIMBufSize = 4;

var
  FKeyNames : array[1..256] of String =
  ('ESCAPE','1','2','3','4','5','6','7','8','9','0','','=','BACKSPACE','TAB','Q','W',
   'E','R','T','Y','U','I','O','P','[',']','ENTER','LEFTCONTROL','A','S','D','F','G',
   'H','J','K','L',';','''','`','LEFTSHIFT','\','Z','X','C','V','B','N','M',',','.','/',
   'RIGHTSHIFT','*','LEFTALT','SPACE','CAPSLOCK','F1','F2','F3','F4','F5','F6','F7','F8',
   'F9','F10','NUMLOCK','SCROLLLOCK','NUMPAD7','NUMPAD8','NUMPAD9','-','NUMPAD4','NUMPAD5',
   'NUMPAD6','+','NUMPAD1','NUMPAD2','NUMPAD3','NUMPAD0','.','','','','F11','F12','','',
   '','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',
   '','','','','','','RIGHTCONTROL','','','','','','','','','','','','','','','','','','','',
   '','','','','/','PRINTSCREEN','RIGHTALT','','','','','','','','','','','','','','PAUSE',
   '','HOME','UP','PAGEUP','','LEFT','','RIGHT','','END','DOWN','PAGEDOWN','INSERT','DELETE',
   '','','','','','','','','','','','','','','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','');

implementation

{******************************************************************************}
{* Create inputaction class                                                   *}
{******************************************************************************}

Constructor TGDInputAction.Create(aKeyString : String; aKey: integer; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean);
begin
  FKeyString := aKeyString;
  FKey := aKey;
  FAction := aAction;
  FConsoleDisabled := aConsoleDisabled;
end;

{******************************************************************************}
{* Destroy inputaction class                                                  *}
{******************************************************************************}

Destructor  TGDInputAction.Destroy();
begin
  inherited;
  FKeyString := '';
  FKey := 0;
  FAction := nil;
  FConsoleDisabled := false;
end;

{******************************************************************************}
{* Execute the inputaction                                                    *}
{******************************************************************************}

procedure TGDInputAction.Execute();
begin
  if Assigned(FAction) then
      FAction();
end;

{******************************************************************************}
{* Input thread function                                                      *}
{******************************************************************************}

ThreadVar
pData : ^TGDInputThreadData;

function InputThreadExecute(Parameter : Pointer) : Integer;
var
 iResult : DWORD;
 iTempAction : TGDInputAction;
 iI : Integer;
 iInput : TGDInput;
begin
 Result := 0;
 pData := Parameter;
 iInput:= pData.Input;
 while Not(iInput.Done)  do
 begin
     iResult := WaitForSingleObject(iInput.KeyBoardEvent, 50);
     If Not(iInput.EnableInput) then continue;
     case iResult of
       WAIT_OBJECT_0 : begin
                         If iInput.KeyboardState then
                         begin
                           If iInput.KeyDown(DIK_GRAVE) and (Console <> nil) then
                           begin
                             Console.Show := Not(Console.Show);
                           end;

                           If Console.Show and (Console <> nil) then
                           begin
                             If iInput.KeyDown(DIK_UP) then   Console.MoveInputUp();
                             If iInput.KeyDown(DIK_DOWN) then Console.MoveInputDown();
                             If iInput.KeyDown(DIK_BACK) then Console.RemoveChar();
                             If iInput.KeyDown(DIK_RETURN) then
                             begin
                               if Not(Console.CommandString = '') then
                               begin
                                 Console.ExecuteCommand := true;
                               end;
                             end;
                           end;

                           For iI := 0 to iInput.SingleInput.Count-1 do
                           begin
                             iTempAction := TGDInputAction(iInput.SingleInput.GetObjectI(iI));
                             if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
                             begin
                               If iInput.KeyDown( iTempAction.Key ) Then iTempAction.Execute();
                             end;
                           end;

                         end;
                       end;
       WAIT_TIMEOUT  : Begin
                         If iInput.KeyboardState  and (Console <> nil) then
                         begin
                           If Console.Show then
                           begin
                             If iInput.KeyDown(DIK_NEXT) then  Console.MoveDown();
                             If iInput.KeyDown(DIK_PRIOR) then Console.MoveUp();
                           end;
                         end;
                      End;
     end;
   end;
   EndThread(0);
end;

{******************************************************************************}
{* Create the input class                                                     *}
{******************************************************************************}

Constructor TGDInput.Create();
var
  iError : string;
  iProp  : TDIPropDWord;
  tID    : LongWord;
begin
  Log.Write('Initializing input...');
  try
    FInitialized := true;
    FEnableInput := false;
    DirectInput8Create(GetModuleHandle(nil), DIRECTINPUT_VERSION, IID_IDirectInput8, FDirectInput, NIL);
    if FDirectInput = nil then Raise Exception.Create('Error initializing Direct Input!');

    //Keyboard
    if failed(FDirectInput.CreateDevice(GUID_SysKeyboard, FKeyBoard, NIL)) then
      Raise Exception.Create('Unable to create a keyboard device!');

    if failed(FKeyBoard.SetDataFormat(@c_dfDIKeyboard)) then
      Raise Exception.Create('Unable to set the keyboard data format!');

    FDIKeyBoardEvent := CreateEvent(NIL, false, false, NIL);
    if FDIKeyBoardEvent = 0 then
      Raise Exception.Create('Unable to set the keyboard event!');

    if failed(FKeyBoard.SetEventNotification(FDIKeyBoardEvent)) then
      Raise Exception.Create('Unable to set the keyboard event notification!');

    If Not(KeyboardControl( true )) then
      Raise Exception.Create('Unable to get the keyboard control!');

    //Mouse
    DIMButSwapped := GetSystemMetrics(SM_SWAPBUTTON) <> 0;
    if failed(FDirectInput.CreateDevice(GUID_SysMouse, FMouse, NIL)) then
      Raise Exception.Create('Unable to create a mouse device!');

    if failed(FMouse.SetDataFormat(@c_dfDIMouse)) then
      Raise Exception.Create('Unable to set the mouse data format!');

    FDIMouseEvent := CreateEvent(NIL, false, false, NIL);
    if FDIMouseEvent = 0 then Raise Exception.Create('Unable to set the mouse event!');
    if failed(FMouse.SetEventNotification(FDIMouseEvent)) then
      Raise Exception.Create('Unable to set the mouse event notification!');
    with iProp do begin
      diph.dwSize       := SizeOf(TDIPropDWord);
      diph.dwHeaderSize := SizeOf(TDIPropHeader);
      diph.dwObj        := 0;
      diph.dwHow        := DIPH_DEVICE;
      dwData            := DIMBufSize;
    end;
    if failed(FMouse.SetProperty(DIPROP_BUFFERSIZE, iProp.diph)) then
      Raise Exception.Create('Unable to set the mouse buffersize!');
    If Not(MouseControl( true )) then
      Raise Exception.Create('Unable to get the mouse control!');

    FMouseLook := False;
    FDirect := TGDObjectList.Create();
    FSingle := TGDObjectList.Create();
    FUp     := TGDObjectList.Create();
    FDown   := TGDObjectList.Create();
    CalculateMousePosStart();

    FDone := false;
    FInputThreadData.Input := self;
    FInputThread := BeginThread(nil,
                           0,
                           Addr(InputThreadExecute),
                           Addr(FInputThreadData),
                           0,
                           tID);
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
    end;
  end;

  Log.WriteOkFail(FInitialized, iError);
end;

{******************************************************************************}
{* Destroy the input class                                                    *}
{******************************************************************************}

Destructor  TGDInput.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  Log.Write('Shutting down input...');
  try
    iResult := true;
    FDone := true;
    TerminateThread(FInputThread, 0);
    CloseHandle(FInputThread);

    if Assigned(FDirectInput) then
    begin
      if Assigned(FKeyBoard) then
      begin
        FKeyBoard.Unacquire;
        FKeyBoard := NIL;
      end;
      if Assigned(FMouse) then
      begin
        FMouse.Unacquire;
        FMouse := NIL;
      end;
      FDirectInput := NIL;
    end;

    FreeAndNil(FDirect);
    FreeAndNil(FSingle);
    FreeAndNil(FUp);
    FreeAndNil(FDown);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Log.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Set the keyboard control                                                   *}
{******************************************************************************}

function TGDInput.KeyboardControl( aAcquire: boolean ): boolean;
var
  iHr : hResult;
begin
  if aAcquire = false
    then iHr := FKeyBoard.Unacquire
      else iHr := FKeyBoard.Acquire;
  Result := not failed(iHr);
end;

{******************************************************************************}
{* Get the keyboard state                                                     *}
{******************************************************************************}

function TGDInput.KeyboardState(): Boolean;
var
  iHr : hResult;
begin
  if FKeyBoard = nil then exit;
  iHr := FKeyBoard.GetDeviceState(SizeOf(FKeyBuffer), @FKeyBuffer);
  if iHr = DIERR_INPUTLOST then
  begin
      iHr := FKeyBoard.Acquire;
      if not failed(iHr)
        then iHr := FKeyBoard.GetDeviceState(SizeOf(FKeyBuffer), @FKeyBuffer);
   end;
  Result := not failed(iHr);
end;

{******************************************************************************}
{* Detect if a key is down                                                    *}
{******************************************************************************}

function TGDInput.KeyDown( aKey: byte ): boolean;
begin
  Result := (FKeyBuffer[aKey] and $80 = $80);
end;

{******************************************************************************}
{* Detect the current key pressed                                             *}
{******************************************************************************}

function TGDInput.DetectCurrentKey() : byte;
var
  iI : integer;
  iFound : boolean;
begin
  KeyboardState();
  iI := 1;
  iFound := false;
  while ((iI <= 255) and Not(iFound)) do
  begin
    If KeyDown( iI ) then
    begin
      iFound := true;
      result := iI;
    end;
    iI := iI + 1;
  end;
  If (iFound = False) then
    result := 0;
end;

{******************************************************************************}
{* Detect the current key pressed string                                      *}
{******************************************************************************}

function TGDInput.DetectCurrentKeyString() : String;
var
  iI : integer;
  iFound : boolean;
begin
  KeyboardState();
  iI := 1;
  iFound := false;
  while ((iI <= 256) and Not(iFound)) do
  begin
    If KeyDown( iI ) then
    begin
      iFound := true;
      result := FKeyNames[iI]
    end;
    iI := iI + 1;
  end;
  If (iFound = False) or  (result = '') then
    result := 'Unassigned'
end;

{******************************************************************************}
{* Key stringname to byte                                                     *}
{******************************************************************************}

function TGDInput.StringToKey( aString : String ) : Byte;
var
  iI : integer;
  iFound : boolean;
begin
  iI := 1;
  result := $00;
  iFound := false;
  while ((iI <= 256) and Not(iFound)) do
  begin
    If FKeyNames[iI] = aString then
    begin
      iFound := true;
      result := iI;
    end;
    iI := iI + 1;
  end;
  If iFound = False then
    result := $00;
end;

{******************************************************************************}
{* Key byte to stringname                                                     *}
{******************************************************************************}

function TGDInput.KeyToString(aKey : Byte) : String;
begin
  result := FKeyNames[aKey];
  If result = '' then
     result := 'Unassigned';
end;

{******************************************************************************}
{* Get the mouse inputcontrol                                                 *}
{******************************************************************************}

function  TGDInput.MouseControl(aAcquire: boolean): boolean;
var
  iHr : hResult;
begin
  if aAcquire = false
    then iHr := FMouse.Unacquire
      else iHr := FMouse.Acquire;
  Result := not failed(iHr);
end;

{******************************************************************************}
{* Get the mouse position                                                     *}
{******************************************************************************}

function  TGDInput.GetMousePosition(var aX, aY: longint) : boolean;
var
  iHr : hResult;
  iOd : TDIDeviceObjectData;
  iElements : dWord;
begin
  Result := false;
  repeat
    iElements := 1;
    iHr := FMouse.GetDeviceData(SizeOf(TDIDeviceObjectData), @iOd, iElements, 0);
    if iHr = DIERR_INPUTLOST then
      begin
        iHr := FMouse.Acquire;
        if not failed(iHr)
          then iHr := FMouse.GetDeviceData(SizeOf(TDIDeviceObjectData),@iOd, iElements, 0);
      end;
    if (failed(iHr)) then exit;
    Result := true;
    if (iElements = 0) then exit;

    case iOd.dwOfs of
      DIMOFS_X : aX := aX + longint(iOd.dwData);
      DIMOFS_Y : aY := aY + longint(iOd.dwData);
    end;
  until iElements = 0;
end;

{******************************************************************************}
{* Execute directinput events                                                 *}
{******************************************************************************}

procedure TGDInput.ExecuteDirectInput();
var
  iTempAction : TGDInputAction;
  iI : Integer;
begin
  if Input.EnableInput = false then exit;

  For iI := 0 to FDirect.Count-1 do
  begin
    iTempAction := TGDInputAction(FDirect.GetObjectI(iI));
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Input.KeyDown( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FDown.Count-1 do
  begin
    iTempAction := TGDInputAction(FDown.GetObjectI(iI));
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Input.KeyDown( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FUp.Count-1 do
  begin
    iTempAction := TGDInputAction(FUp.GetObjectI(iI));
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Not(Input.KeyDown( iTempAction.Key )) Then iTempAction.Execute();
  end;
end;

{******************************************************************************}
{* Execute mouse move                                                         *}
{******************************************************************************}

procedure TGDInput.ExecuteMouseMove();
begin
  if Input.EnableInput = false then exit;
  if Console.Show then exit;

  if Input.GetMousePosition(FMousePosCurrent.X, FMousePosCurrent.Y) then
  begin
    if FMouseLook then
    begin
      begin
        Camera.MouseLook(FMousePosStart.X, FMousePosStart.Y, FMousePosCurrent.X,
                         FMousePosCurrent.Y,Settings.MouseSensitivity,
                         Settings.InvertMouse);
        SetCursorPos(FMousePosStart.X, FMousePosStart.Y);
        FMousePosCurrent.X := FMousePosStart.x;
        FMousePosCurrent.Y := FMousePosStart.Y;
      end;
    end;
  end;
end;

{******************************************************************************}
{* Execute char input                                                         *}
{******************************************************************************}

procedure TGDInput.ExecuteCharInput(aChar : Char);
begin
  if Input.EnableInput = false then exit;

  if Console.Show then
    Console.AddChar(aChar);
  ;
end;

{******************************************************************************}
{* Calculate the mousestart position                                          *}
{******************************************************************************}

procedure TGDInput.CalculateMousePosStart();
begin
  FMousePosStart.x := Round( Settings.Left + (Settings.Width / 2) );
  FMousePosStart.Y := Round( Settings.Top + (Settings.Height / 2) );
  FMousePosCurrent.X := FMousePosStart.x;
  FMousePosCurrent.Y := FMousePosStart.Y;
end;

{******************************************************************************}
{* Register an input action                                                   *}
{******************************************************************************}

procedure TGDInput.RegisterInputAction(aType : TGDInputTypes; aKeyString : String; aAction : TGDProcEngineCallback; aConsoleDisabled : boolean );
var
  iTempAction : TGDInputAction;
begin
  iTempAction := TGDInputAction.Create(aKeyString, Input.StringToKey(aKeyString),aAction, aConsoleDisabled);
  case aType of
     IT_DIRECT : FDirect.AddObjectP(iTempAction);
     IT_SINGLE : FSingle.AddObjectP(iTempAction);
     IT_DOWN   : FDown.AddObjectP(iTempAction);
     IT_UP     : FUp.AddObjectP(iTempAction);
  end;
end;

{******************************************************************************}
{* Clear the inputmanager                                                     *}
{******************************************************************************}

procedure TGDInput.ClearInputActions();
begin
  FDirect.Clear();
  FSingle.Clear();
  FDown.Clear();
  FUp.Clear();
  CalculateMousePosStart();
end;

end.
