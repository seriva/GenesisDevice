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
unit GDLighting;

{$MODE Delphi}

{******************************************************************************}
{* Holds the lighting classes                                                 *}
{******************************************************************************}

interface

Uses
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDTypes;

Type

{******************************************************************************}
{* DirectionalLight input record                                              *}
{******************************************************************************}

  TGDDirectionalLightInput = record
    DirX  : Double;
    DirY  : Double;
    DirZ  : Double;
    AmbR  : Double;
    AmbG  : Double;
    AmbB  : Double;
    DifR  : Double;
    DifG  : Double;
    DifB  : Double;
  end;

{******************************************************************************}
{* DirectionalLight class                                                     *}
{******************************************************************************}

  TGDDirectionalLight = class (TObject)
  private
  public
    Direction : TGDVector;
    Ambient   : TGDColor;
    Diffuse   : TGDColor;

    constructor Create();
    destructor  Destroy(); override;

    procedure   InitDirectionalLight( aInput : TGDDirectionalLightInput );
    procedure   Clear();
  end;

var
  DirectionalLight : TGDDirectionalLight;

implementation

{******************************************************************************}
{* Create DirectionalLight                                                    *}
{******************************************************************************}

constructor TGDDirectionalLight.Create();
begin
  Direction := TGDVector.Create(-1,-1,-1);
  Ambient   := TGDColor.Create(1.0, 1.0, 1.0, 1.0);
  Diffuse   := TGDColor.Create(1.0, 1.0, 1.0, 1.0);
end;

{******************************************************************************}
{* Destroy DirectionalLight                                                   *}
{******************************************************************************}

destructor TGDDirectionalLight.Destroy();
begin
  FreeAndNil(Direction);
  FreeAndNil(Ambient);
  FreeAndNil(Diffuse);
  inherited;
end;

{******************************************************************************}
{* Init DirectionalLight                                                      *}
{******************************************************************************}

procedure   TGDDirectionalLight.InitDirectionalLight( aInput : TGDDirectionalLightInput );
begin
  Direction.Reset( aInput.DirX, aInput.DirY, aInput.DirZ);
  Ambient.Reset( aInput.AmbR,aInput.AmbG, aInput.AmbB, 1);
  Diffuse.Reset( aInput.DifR, aInput.DifG, aInput.DifB, 1);
end;

{******************************************************************************}
{* Clear DirectionalLight                                                     *}
{******************************************************************************}

procedure   TGDDirectionalLight.Clear();
begin
  Direction.Reset(-1,-1,-1);
  Ambient.Reset(1, 1, 1, 1);
  Diffuse.Reset(1, 1, 1, 1);
end;

end.
