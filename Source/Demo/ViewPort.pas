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
unit ViewPort;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the viewportform of the demoapplication                   *}
{******************************************************************************}

interface

uses
  Windows,
  multimon,
  LCLIntf,
  LCLType,
  Controls,
  Forms,
  Classes,
  Messages,
  SysUtils,
  dglOpenGL,
  Main,
  GDConstants,
  GDEngine,
  ExtCtrls;

type
  TViewPortForm = class(TForm)
    ApplicationEvents: TApplicationProperties;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure WMMove(var Message: TMessage) ; message WM_MOVE;
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  public
  end;

implementation

{$R *.lfm}

uses
  Configuration;

{******************************************************************************}
{* Form create                                                                *}
{******************************************************************************}

procedure TViewPortForm.FormCreate(Sender: TObject);
var
  iDMScreenSettings : DEVMODE;
begin
  self.Caption := 'Demo';

  //initialize the renderer with the current settings
  If Not(Engine.Renderer.InitViewPort( self.Handle )) then
  begin
    MessageBox(0, 'Error initializing viewport. See Console.txt for details.', 'Error', MB_OK or MB_ICONERROR);
    Application.Terminate();
  end;

  //if fullscreen remove the border
  If Engine.Settings.FullScreen then
  begin
    ZeroMemory(@iDMScreenSettings, SizeOf(iDMScreenSettings));
    with iDMScreenSettings do begin
            dmSize       := SizeOf(iDMScreenSettings);
            dmPelsWidth  := Engine.Settings.Width;
            dmPelsHeight := Engine.Settings.Height;
            dmBitsPerPel := 32;
            dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    end;

    if (ChangeDisplaySettingsEx( @ConfigurationForm.FMonitorInfos[ConfigurationForm.MonitorComboBox.ItemIndex].szDevice[0], &iDMScreenSettings, 0, CDS_FULLSCREEN, nil) <> DISP_CHANGE_SUCCESSFUL) then
      Raise Exception.Create('Unable to make window fullscreen');

    SetWindowLong(self.Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
    SetWindowPos(self.Handle, HWND_TOP, 0,
                                        Screen.Monitors[ConfigurationForm.MonitorComboBox.ItemIndex].Left,
                                        ConfigurationForm.FAvailableModi[ConfigurationForm.ResolutionsComboBox.ItemIndex].Width,
                                        ConfigurationForm.FAvailableModi[ConfigurationForm.ResolutionsComboBox.ItemIndex].Height,
                                        SWP_FRAMECHANGED);

    //remove the border, place and size window.
    SetWindowLong(self.Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
    self.Top    := 0;
    self.Left   := Screen.Monitors[ConfigurationForm.MonitorComboBox.ItemIndex].Left;
    WindowState:=wsMaximized;
  end
  else
  begin
    //remove the border, place and size window.
    self.Top    := 0;
    self.Left   := Screen.Monitors[ConfigurationForm.MonitorComboBox.ItemIndex].Left;
    self.Width  := ConfigurationForm.FAvailableModi[ConfigurationForm.ResolutionsComboBox.ItemIndex].Width;
    self.Height := ConfigurationForm.FAvailableModi[ConfigurationForm.ResolutionsComboBox.ItemIndex].Height;
  end;

  //get the engine settings and set the right windowsettings
  Engine.Renderer.ResizeViewPort(self.Top, self.Left, self.Width, self.Height);

  //show the form
  self.Show();
end;

{******************************************************************************}
{* Form destroy                                                               *}
{******************************************************************************}

procedure TViewPortForm.FormDestroy(Sender: TObject);
begin
  //exit fullscreen
  ChangeDisplaySettings(devmode(nil^), 0);

  //Set settings changed via the console to the configuration interface
  ConfigurationForm.SettingsToInterface();

  //clear the gameresources
  ClearGame();

  //shutdown the renderer
  Engine.Renderer.ShutDownViewPort();

  //back to configuration
  ConfigurationForm.Visible := true;
  ConfigurationForm.Repaint();
end;

{******************************************************************************}
{* Call resize when the window is resized                                     *}
{******************************************************************************}

procedure TViewPortForm.FormResize(Sender: TObject);
begin
  //resize the window and render the window
  Engine.Renderer.ResizeViewPort(self.Top, self.Left, self.Width, self.Height);
end;

{******************************************************************************}
{* Call resize when the window is moved                                       *}
{******************************************************************************}

procedure TViewPortForm.WMMove(var Message: TMessage);
begin
  //resize the window and render the window
  Engine.Renderer.ResizeViewPort(self.Top, self.Left, self.Width, self.Height);
end;

{******************************************************************************}
{* Mainloop                                                                   *}
{******************************************************************************}

procedure TViewPortForm.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  //set done to false to get the most cpu power
  Done := false;

  //do the mainloop
  Engine.Loop( @GameLoop );
end;

{******************************************************************************}
{* Keypress events send to the console                                        *}
{******************************************************************************}

procedure TViewPortForm.FormKeyPress(Sender: TObject; var Key : Char);
begin
  Engine.Console.AddChar(Key);
end;

procedure TViewPortForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Engine.Console.Control(Key);
end;

{******************************************************************************}
{* Form close event                                                           *}
{******************************************************************************}

procedure TViewPortForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

end.
