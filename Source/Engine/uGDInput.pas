{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine                              *
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
unit uGDInput;

{$MODE objfpc}

{******************************************************************************}
{* This unit holds the input classes of the GD types of input actions:   *}
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
  FGL,
  SDL2,
  SysUtils,
  Classes,
  uGDConstants;

type
  PKeyStateArr = ^TKeyStateArr;
  TKeyStateArr = array[0..65000] of UInt8;

{******************************************************************************}
{* Inputaction class                                                          *}
{******************************************************************************}

  TGDInputAction = class
  private
    FKey             : integer;
    FAction          : TGDCallback;
    FConsoleDisabled : boolean;
    FCanExecute      : boolean;
  public
    property Key : integer read FKey;
    property CanExecute : boolean read FCanExecute write FCanExecute;

    Constructor Create(aKey: integer; aAction : TGDCallback;  aConsoleDisabled : boolean);
    Destructor  Destroy(); override;

    procedure Execute();
  end;
  TGDInputActionList = specialize TFPGObjectList<TGDInputAction>;

{******************************************************************************}
{* Input class                                                                *}
{******************************************************************************}

  TGDInput = class
  private
    FInitialized     : boolean;
    FEnableInput     : Boolean;
    FMouseLook       : boolean;
    FKeyBuffer       : PKeyStateArr;
    FSingle          : TGDInputActionList;
    FDirect          : TGDInputActionList;
    FUp              : TGDInputActionList;
    FDown            : TGDInputActionList;

    function KeyState(aKey : Byte): boolean;
  public
    property Initialized     : boolean read FInitialized;
    property EnableInput     : boolean read FEnableInput write FEnableInput;
    property MouseLook       : boolean read FMouseLook write FMouseLook;

    Constructor Create();
    Destructor  Destroy();override;

    procedure Update();

    procedure AddAction(aType : TGDInputTypes; aKey : integer; aAction : TGDCallback;  aConsoleDisabled : boolean );
    procedure Clear();
  end;

implementation

uses
  uGDEngine;

{******************************************************************************}
{* Create inputaction class                                                   *}
{******************************************************************************}

Constructor TGDInputAction.Create(aKey: integer; aAction : TGDCallback;  aConsoleDisabled : boolean);
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
  GDConsole.Write('Initializing input...');
  try
    FInitialized := true;
    FEnableInput := false;

    FMouseLook := False;
    FDirect := TGDInputActionList.Create();
    FSingle := TGDInputActionList.Create();
    FUp     := TGDInputActionList.Create();
    FDown   := TGDInputActionList.Create();
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
    end;
  end;

  GDConsole.WriteOkFail(FInitialized, iError);
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
  GDConsole.Write('Shutting down input...');
  try
    Clear();
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
  GDConsole.WriteOkFail(iResult, iError);
end;


{******************************************************************************}
{* Set key state                                                              *}
{******************************************************************************}

function TGDInput.KeyState(aKey : Byte): boolean;
begin
  result := false;
  if aKey < 255 then
    result := FKeyBuffer^[aKey] <> 0;
end;

{******************************************************************************}
{* Execute direct events                                                      *}
{******************************************************************************}

procedure TGDInput.Update();
var
  iTempAction : TGDInputAction;
  iI : Integer;
  iMousePosCurrent : TPoint;
begin
  if EnableInput = false then exit;

  //get the keyboard state
  SDL_Pumpevents();
  FKeyBuffer:= PKeyStateArr(SDL_Getkeyboardstate(nil));

  //Keyboard
  For iI := 0 to FSingle.Count-1 do
  begin
    iTempAction := FSingle.Items[iI];
    if (iTempAction.FConsoleDisabled and Not(GDConsole.Show)) or not(iTempAction.FConsoleDisabled) then
      If Not(KeyState( iTempAction.Key )) Then
      begin
        iTempAction.CanExecute := true;
      end;
  end;
  For iI := 0 to FSingle.Count-1 do
  begin
    iTempAction := FSingle.Items[iI];
    if (iTempAction.FConsoleDisabled and Not(GDConsole.Show)) or not(iTempAction.FConsoleDisabled) then
      If KeyState( iTempAction.Key ) Then
      begin
        iTempAction.Execute();
        iTempAction.CanExecute := false;
      end;
  end;

  For iI := 0 to FDirect.Count-1 do
  begin
    iTempAction := FDirect.Items[iI];
    if (iTempAction.FConsoleDisabled and Not(GDConsole.Show)) or not(iTempAction.FConsoleDisabled) then
      If KeyState( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FDown.Count-1 do
  begin
    iTempAction := FDown.Items[iI];
    if (iTempAction.FConsoleDisabled and Not(GDConsole.Show)) or not(iTempAction.FConsoleDisabled) then
      If KeyState( iTempAction.Key ) Then iTempAction.Execute();
  end;

  For iI := 0 to FUp.Count-1 do
  begin
    iTempAction := FUp.Items[iI];
    if (iTempAction.FConsoleDisabled and Not(GDConsole.Show)) or not(iTempAction.FConsoleDisabled) then
      If Not(KeyState( iTempAction.Key )) Then iTempAction.Execute();
  end;

  //Mouse
  if FMouseLook then
  begin
    if GDConsole.Show then exit;
    SDL_GetMouseState(@iMousePosCurrent.x, @iMousePosCurrent.y);
    GDWindow.SetMouse();
    GDCamera.MouseLook(GDWindow.Width() div 2, GDWindow.Height() div 2,
                            iMousePosCurrent.X, iMousePosCurrent.Y,GDSettings.MouseSensitivity,
                            GDSettings.InvertMouse);
  end;
end;

{******************************************************************************}
{* Register an input action                                                   *}
{******************************************************************************}

procedure TGDInput.AddAction(aType : TGDInputTypes; aKey : integer; aAction : TGDCallback; aConsoleDisabled : boolean );
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

procedure TGDInput.Clear();
begin
  FDirect.Clear();
  FSingle.Clear();
  FDown.Clear();
  FUp.Clear();
end;

end.
