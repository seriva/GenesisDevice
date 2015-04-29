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
unit GDCallBack;

{$MODE Delphi}

{******************************************************************************}
{* Holds the main callback procedures                                         *}
{******************************************************************************}

interface

{******************************************************************************}
{* Holds the main modes vars of the engine                                    *}
{******************************************************************************}

uses
  LCLIntf, LCLType, LMessages,
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDModes;

type

{******************************************************************************}
{* Callbacks class                                                            *}
{******************************************************************************}

  TGDCallBack  = Class
  private
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure RenderInterface();
    procedure BeforeRender();
    procedure AfterRender();
  end;

var
  CallBack                : TGDCallBack;
  RenderInterfaceCallBack : TGDProcEngineCallback = nil;
  BeforeRenderCallBack    : TGDProcEngineCallback = nil;
  AfterRenderCallBack     : TGDProcEngineCallback = nil;

implementation

{******************************************************************************}
{* Create callback class                                                      *}
{******************************************************************************}

constructor TGDCallBack.Create();
begin
  RenderInterfaceCallBack := nil;
  BeforeRenderCallBack := nil;
  AfterRenderCallBack := nil;
end;

{******************************************************************************}
{* Destroy callback class                                                     *}
{******************************************************************************}

destructor  TGDCallBack.Destroy();
begin
  RenderInterfaceCallBack := nil;
  BeforeRenderCallBack := nil;
  AfterRenderCallBack := nil;
end;

{******************************************************************************}
{* Interface rendering callback                                               *}
{******************************************************************************}

procedure TGDCallBack.RenderInterface();
begin
  if (Assigned(RenderInterfaceCallBack) and Modes.RenderGUI) then
      RenderInterfaceCallBack();
end;

{******************************************************************************}
{* Before rendering callback                                                  *}
{******************************************************************************}

procedure TGDCallBack.BeforeRender();
begin
  if Assigned(BeforeRenderCallBack) then
      BeforeRenderCallBack();
end;

{******************************************************************************}
{* After rendering callback                                                   *}
{******************************************************************************}

procedure TGDCallBack.AfterRender();
begin
  if Assigned(AfterRenderCallBack) then
      AfterRenderCallBack();
end;

end.
