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
unit GDInput;

{$MODE Delphi}

{******************************************************************************}
{* This unit holds the input classes of the engine. types of input actions:   *}
{* - Direct (done every frame)                                                *}
{* - Single (done once till the key is pressed again)                         *}
{* - Up     (done as a keyup event)                                           *}
{* - Down   (done as a keydown event)                                         *}
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
  GDConstants,
  GDSettings,
  GDCamera,
  Contnrs;

type

{******************************************************************************}
{* Inputaction class                                                          *}
{******************************************************************************}

  TGDInputAction = class(TObject)
  private
    FKey             : integer;
    FAction          : TGDProcEngineCallback;
    FConsoleDisabled : boolean;
    FCanExecute      : boolean;
  public
    property Key : integer read FKey;
    property CanExecute : boolean read FCanExecute write FCanExecute;

    Constructor Create(aKey: integer; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean);
    Destructor  Destroy(); override;

    procedure Execute();
  end;

{******************************************************************************}
{* Input class                                                                *}
{******************************************************************************}

  TGDInput = class
  private
    FInitialized     : boolean;
    FEnableInput     : Boolean;
    FMouseLook       : boolean;
    FKeyBuffer       : array[0..255] of Boolean;
    FMousePosStart   : TPoint;
    FMousePosCurrent : TPoint;

    FSingle          : TObjectList;
    FDirect          : TObjectList;
    FUp              : TObjectList;
    FDown            : TObjectList;
  public
    property Initialized     : boolean read FInitialized;
    property EnableInput     : boolean read FEnableInput write FEnableInput;
    property MouseLook       : boolean read FMouseLook write FMouseLook;

    Constructor Create();
    Destructor  Destroy();override;

    procedure KeyboardState();
    function  KeyDown(aKey: byte): boolean;

    procedure ExecuteInput();

    procedure CalculateMousePosStart();
    procedure SetMouseStartPos();

    procedure RegisterInputAction(aType : TGDInputTypes; aKey : integer; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean );
    procedure ClearInputActions();
  end;

var
  Input : TGDInput;

implementation

uses
  GDConsole;

{******************************************************************************}
{* Create inputaction class                                                   *}
{******************************************************************************}

Constructor TGDInputAction.Create(aKey: integer; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean);
begin
  FKey := aKey;
  FAction := aAction;
  FCanExecute := true;
  FConsoleDisabled := aConsoleDisabled;
end;

{******************************************************************************}
{* Destroy inputaction class                                                  *}
{******************************************************************************}

Destructor  TGDInputAction.Destroy();
begin
  inherited;
  FKey := 0;
  FAction := nil;
  FConsoleDisabled := false;
end;

{******************************************************************************}
{* Execute the inputaction                                                    *}
{******************************************************************************}

procedure TGDInputAction.Execute();
begin
  if Assigned(FAction) and FCanExecute then
      FAction();
end;

{******************************************************************************}
{* Create the input class                                                     *}
{******************************************************************************}

Constructor TGDInput.Create();
var
  iError : string;
begin
  Console.Write('Initializing input...');
  try
    FInitialized := true;
    FEnableInput := false;

    FMouseLook := False;
    FDirect := TObjectList.Create();
    FSingle := TObjectList.Create();
    FUp     := TObjectList.Create();
    FDown   := TObjectList.Create();
    CalculateMousePosStart();
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
    end;
  end;

  Console.WriteOkFail(FInitialized, iError);
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
  Console.Write('Shutting down input...');
  try
    iResult := true;
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
  Console.WriteOkFail(iResult, iError);
end;


{******************************************************************************}
{* Get the keyboard state                                                     *}
{******************************************************************************}

procedure TGDInput.KeyboardState();
var
  iI : integer;
begin
  For iI := 0 to 255 do
    FKeyBuffer[iI] := GetAsyncKeyState(iI) and $8000 <> 0;
end;

{******************************************************************************}
{* Detect if a key is down                                                    *}
{******************************************************************************}

function TGDInput.KeyDown( aKey: byte ): boolean;
begin
  Result := FKeyBuffer[aKey];
end;

{******************************************************************************}
{* Execute direct events                                                      *}
{******************************************************************************}

procedure TGDInput.ExecuteInput();
var
  iTempAction : TGDInputAction;
  iI : Integer;
begin
  if Input.EnableInput = false then exit;

  //Keyboard
  KeyboardState();
  For iI := 0 to FSingle.Count-1 do
  begin
    iTempAction := TGDInputAction(FSingle.Items[iI]);
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Not(Input.KeyDown( iTempAction.Key )) Then
      begin
        iTempAction.CanExecute := true;
      end;
  end;
  For iI := 0 to FSingle.Count-1 do
  begin
    iTempAction := TGDInputAction(FSingle.Items[iI]);
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Input.KeyDown( iTempAction.Key ) Then
      begin
        iTempAction.Execute();
        iTempAction.CanExecute := false;
      end;
  end;

  For iI := 0 to FDirect.Count-1 do
  begin
    iTempAction := TGDInputAction(FDirect.Items[iI]);
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Input.KeyDown( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FDown.Count-1 do
  begin
    iTempAction := TGDInputAction(FDown.Items[iI]);
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Input.KeyDown( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FUp.Count-1 do
  begin
    iTempAction := TGDInputAction(FUp.Items[iI]);
    if (iTempAction.FConsoleDisabled and Not(Console.Show)) or not(iTempAction.FConsoleDisabled) then
      If Not(Input.KeyDown( iTempAction.Key )) Then iTempAction.Execute();
  end;

  //Mouse
  if FMouseLook then
  begin
    if Console.Show then exit;
    GetCursorPos(FMousePosCurrent);
    SetMouseStartPos();
    Camera.MouseLook(FMousePosStart.X, FMousePosStart.Y, FMousePosCurrent.X,
                     FMousePosCurrent.Y,Settings.MouseSensitivity,
                     Settings.InvertMouse);
    FMousePosCurrent.X := FMousePosStart.x;
    FMousePosCurrent.Y := FMousePosStart.Y;
  end;
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
{* Set mouse at the mousestart position                                       *}
{******************************************************************************}

procedure TGDInput.SetMouseStartPos();
begin
  SetCursorPos(FMousePosStart.X, FMousePosStart.Y);
end;

{******************************************************************************}
{* Register an input action                                                   *}
{******************************************************************************}

procedure TGDInput.RegisterInputAction(aType : TGDInputTypes; aKey : integer; aAction : TGDProcEngineCallback; aConsoleDisabled : boolean );
var
  iTempAction : TGDInputAction;
begin
  iTempAction := TGDInputAction.Create(aKey,aAction, aConsoleDisabled);
  case aType of
     IT_DIRECT : FDirect.Add(iTempAction);
     IT_SINGLE : FSingle.Add(iTempAction);
     IT_DOWN   : FDown.Add(iTempAction);
     IT_UP     : FUp.Add(iTempAction);
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
