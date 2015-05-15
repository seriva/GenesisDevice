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
unit GDMaterial;

{$MODE Delphi}

{******************************************************************************}
{* Holds the material classes                                                 *}
{******************************************************************************}

interface

Uses
  Classes,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDConstants,
  FileUtil,
  GDResource,
  GDTiming,
  GDStringParsing;

Type

{******************************************************************************}
{* Material class                                                             *}
{******************************************************************************}

  TGDMaterial = class (TGDResource)
  private
    FName    : String;
    FTexture : TGDTexture;
    FHasAlpha : Boolean;
    FAlphaFunc : double;
    FDoBloom : boolean;
    FDoTreeAnim : boolean;
  public
    property Name : String read FName write FName;
    property Texture : TGDTexture read FTexture write FTexture;
    property HasAlpha : Boolean read FHasAlpha write FHasAlpha;
    property AlphaFunc : double read FAlphaFunc write FAlphaFunc;
    property DoBloom : Boolean read FDoBloom write FDoBloom;
    property DoTreeAnim : Boolean read FDoTreeAnim write FDoTreeAnim;

    constructor Create();
    destructor  Destroy(); override;

    procedure   ApplyMaterial();
    procedure   DisableMaterial();
  end;
  
implementation

uses
  GDMap,
  GDRenderer,
  GDResources;

{******************************************************************************}
{* Create material                                                            *}
{******************************************************************************}

constructor TGDMaterial.Create();
begin
end;

{******************************************************************************}
{* Destroy material                                                           *}
{******************************************************************************}

destructor TGDMaterial.Destroy();
begin
  Resources.RemoveResource(TGDResource(FTexture));
  FHasAlpha := false;
  FAlphaFunc := 1.0;
  FDoBloom := false;
  FDoTreeAnim := false;
  inherited;
end;

{******************************************************************************}
{* Apply material                                                             *}
{******************************************************************************}

procedure   TGDMaterial.ApplyMaterial();
begin
  Renderer.MeshShader.Enable();
  Renderer.SetJoinedParams(Renderer.MeshShader);
  Renderer.MeshShader.SetInt('T_COLORMAP', 0);
  if DoTreeAnim then
    Renderer.MeshShader.SetInt('I_DO_TREE_ANIM', 1)
  else
    Renderer.MeshShader.SetInt('I_DO_TREE_ANIM', 0);
  Renderer.MeshShader.SetFloat('F_ANIMATION_SPEED', Timing.ElapsedTime / Map.Foliage.TreeAnimationSpeed);
  Renderer.MeshShader.SetFloat('F_ANIMATION_STRENGTH', Map.Foliage.TreeAnimationStrength);

  If Map.Water.UnderWater() then
   Renderer.MeshShader.SetInt('I_UNDER_WATER', 1)
  else
   Renderer.MeshShader.SetInt('I_UNDER_WATER', 0);

  FTexture.BindTexture( GL_TEXTURE0 );
  if FHasAlpha then
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, FAlphaFunc);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
  end;
end;

{******************************************************************************}
{* Disable material                                                           *}
{******************************************************************************}

procedure TGDMaterial.DisableMaterial();
begin
  if FHasAlpha then
  begin
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_BLEND);
  end;
end;

end.
