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
  GDStringParsing;

Type

{******************************************************************************}
{* Material class                                                             *}
{******************************************************************************}

  TGDMaterial = class (TGDResource)
  private
    FTexture : TGDTexture;
    FDetail  : TGDTexture;
    FHasAlpha : Boolean;
    FAlphaFunc : double;
    FDoTreeAnim : boolean;
    FDetailUVMult : Integer;
    FDetailMult   : double;
  public
    property Texture : TGDTexture read FTexture write FTexture;
    property Detail : TGDTexture read FDetail write FDetail;
    property HasAlpha : Boolean read FHasAlpha write FHasAlpha;
    property AlphaFunc : double read FAlphaFunc write FAlphaFunc;
    property DoTreeAnim : Boolean read FDoTreeAnim write FDoTreeAnim;
    property DetailUVMult : integer read FDetailUVMult write FDetailUVMult;
    property DetailMult : Double read FDetailMult write FDetailMult;

    constructor Create();
    destructor  Destroy(); override;

    procedure   ApplyMaterial();
    procedure   DisableMaterial();
  end;
  
implementation

uses
  GDEngine;

{******************************************************************************}
{* Create material                                                            *}
{******************************************************************************}

constructor TGDMaterial.Create();
begin
  FTexture  := nil;
  FDetail   := nil;
  FHasAlpha := false;
  FAlphaFunc   := 1.0;
  DetailUVMult := 1;
  DetailMult   := 0.5;
  FDoTreeAnim  := false;
end;

{******************************************************************************}
{* Destroy material                                                           *}
{******************************************************************************}

destructor TGDMaterial.Destroy();
begin
  Engine.Resources.RemoveResource(TGDResource(FTexture));
  Engine.Resources.RemoveResource(TGDResource(FDetail));
  inherited;
end;

{******************************************************************************}
{* Apply material                                                             *}
{******************************************************************************}

procedure   TGDMaterial.ApplyMaterial();
begin
  with Engine.Renderer do
  begin
    MeshShader.SetInt('T_COLORMAP', 0);
    if DoTreeAnim then
      MeshShader.SetInt('I_DO_TREE_ANIM', 1)
    else
      MeshShader.SetInt('I_DO_TREE_ANIM', 0);
    MeshShader.SetFloat('F_ANIMATION_SPEED', Engine.Timing.ElapsedTime / Engine.Map.Foliage.TreeAnimationSpeed);
    MeshShader.SetFloat('F_ANIMATION_STRENGTH', Engine.Map.Foliage.TreeAnimationStrength);
    if assigned(FTexture) then FTexture.BindTexture( GL_TEXTURE0 );
    if assigned(FDetail) and Engine.Settings.UseDetail then
    begin
      FDetail.BindTexture(GL_TEXTURE7);
      MeshShader.SetInt('I_DETAIL_UV_MULT', FDetailUVMult);
      MeshShader.SetFloat('F_DETAIL_MULT', FDetailMult);
      MeshShader.SetInt('I_DO_DETAIL', 1);
    end
    else
      MeshShader.SetInt('I_DO_DETAIL', 0);
    if FHasAlpha then
    begin
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GREATER, FAlphaFunc);
    end;
  end;
end;

{******************************************************************************}
{* Disable material                                                           *}
{******************************************************************************}

procedure TGDMaterial.DisableMaterial();
begin
  if FHasAlpha then
    glDisable(GL_ALPHA_TEST);
end;

end.
