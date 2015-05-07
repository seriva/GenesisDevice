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
unit GDMaterials;

{$MODE Delphi}

{******************************************************************************}
{* Holds the material classes                                                 *}
{******************************************************************************}

interface

Uses
  Classes,
  SysUtils,
  dglOpenGL,
  GDRenderer,
  GDTexture,
  GDConstants,
  GDSettings,
  GDFog,
  Contnrs,
  GDLighting,
  FileUtil,
  GDResources,
  GDResource,
  GDTiming,
  GDStringParsing;

Type

{******************************************************************************}
{* Material class                                                             *}
{******************************************************************************}

  TGDMaterial = class (TObject)
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

    procedure   Clear();
    procedure   ApplyMaterial();
    procedure   DisableMaterial();
    procedure   BindMaterialTextures();
  end;

{******************************************************************************}
{* Materialmanager class                                                      *}
{******************************************************************************}

  TGDMaterialList = class (TObjectList)
  private
  public
    function LoadMaterials( aFileName : String ) : boolean;
    function GetMaterial(aName : String): TGDMaterial;
  end;

var
  MaterialList : TGDMaterialList;
  
implementation

uses
  GDFoliage;

{******************************************************************************}
{* Create material                                                            *}
{******************************************************************************}

constructor TGDMaterial.Create();
begin
  clear();
end;

{******************************************************************************}
{* Destroy material                                                           *}
{******************************************************************************}

destructor TGDMaterial.Destroy();
begin
  clear();
  inherited;
end;


{******************************************************************************}
{* Clear material                                                             *}
{******************************************************************************}

procedure TGDMaterial.Clear();
begin
  FName := 'NONE';
  Resources.RemoveResource(TGDResource(FTexture));
  FHasAlpha := false;
  FAlphaFunc := 1.0;
  FDoBloom := false;
  FDoTreeAnim := false;
end;

{******************************************************************************}
{* Apply material                                                             *}
{******************************************************************************}

procedure   TGDMaterial.ApplyMaterial();
begin
  Renderer.MeshShader.Enable();
  Renderer.MeshShader.SetFloat3('V_LIGHT_DIR', DirectionalLight.Direction.X,
                                               DirectionalLight.Direction.Y,
                                               DirectionalLight.Direction.Z);
  Renderer.MeshShader.SetFloat4('V_LIGHT_AMB', DirectionalLight.Ambient.R,
                                               DirectionalLight.Ambient.G,
                                               DirectionalLight.Ambient.B,
                                               DirectionalLight.Ambient.A);
  Renderer.MeshShader.SetFloat4('V_LIGHT_DIFF', DirectionalLight.Diffuse.R,
                                                DirectionalLight.Diffuse.G,
                                                DirectionalLight.Diffuse.B,
                                                DirectionalLight.Diffuse.A);
  Renderer.MeshShader.SetFloat('F_MIN_VIEW_DISTANCE', FogManager.FogShader.MinDistance);
  Renderer.MeshShader.SetFloat('F_MAX_VIEW_DISTANCE', FogManager.FogShader.MaxDistance);
  Renderer.MeshShader.SetFloat4('V_FOG_COLOR', FogManager.FogShader.Color.R,
                                               FogManager.FogShader.Color.G, FogManager.FogShader.Color.B,
                                               FogManager.FogShader.Color.A);
  Renderer.MeshShader.SetInt('T_COLORMAP', 0);

  if DoTreeAnim then
    Renderer.MeshShader.SetInt('I_DO_TREE_ANIM', 1)
  else
    Renderer.MeshShader.SetInt('I_DO_TREE_ANIM', 0);
  Renderer.MeshShader.SetFloat('F_ANIMATION_SPEED', Timing.ElapsedTime / Foliage.TreeAnimationSpeed);
  Renderer.MeshShader.SetFloat('F_ANIMATION_STRENGTH', Foliage.TreeAnimationStrength);

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

{******************************************************************************}
{* Bind the material textures                                                 *}
{******************************************************************************}

procedure   TGDMaterial.BindMaterialTextures();
begin
  FTexture.BindTexture( GL_TEXTURE0 );
end;

{******************************************************************************}
{* Load materials from a file                                                 *}
{******************************************************************************}

function TGDMaterialList.LoadMaterials( aFileName : String ) : boolean;
var
  iFile : TMemoryStream;
  iMat : TGDMaterial;
  iStr : String;
begin
  try
    result := true;
    If Not(FileExistsUTF8(aFileName) ) then
      Raise Exception.Create('');

    iFile := TMemoryStream.Create();
    iFile.LoadFromFile(aFileName);
    CommentString := '#';

    while (iFile.Position < iFile.Size) do
    begin
      iStr := GetNextToken(iFile);
      if iStr = 'newmtl' then //read the material name
      begin
        iStr := GetNextToken(iFile);
        iMat := TGDMaterial.Create();
        iMat.Name := iStr;
        self.Add(iMat);
        continue;
      end
      else if iStr = 'colormap' then //load the material texture
      begin
        if iMat = nil then
           raise Exception.Create('');
        iStr := GetNextToken(iFile);
        iMat.Texture := Resources.LoadTexture(ExtractFilePath(aFileName) + iStr ,Settings.TextureDetail,Settings.TextureFilter);
        continue;
      end
      else if iStr = 'has_alpha' then //read alpha
      begin
        if iMat = nil then
           raise Exception.Create('');
        iStr := GetNextToken(iFile);
        iMat.HasAlpha:= iStr = 'true';
        continue;
      end
      else if iStr = 'do_bloom' then //read bloom
      begin
        if iMat = nil then
           raise Exception.Create('');
        iStr := GetNextToken(iFile);
        iMat.DoBloom:= iStr = 'true';
        continue;
      end
      else if iStr = 'do_treeanim' then //read bloom
      begin
        if iMat = nil then
           raise Exception.Create('');
        iStr := GetNextToken(iFile);
        iMat.DoTreeAnim:= iStr = 'true';
        continue;
      end
      else if iStr = 'alpha_func' then //read bloom
      begin
        if iMat = nil then
          raise Exception.Create('');
        iStr := GetNextToken(iFile);
        iMat.AlphaFunc := StrToFloat(iStr);
        continue;
      end;
    end;
    FreeAndNil(iFile);
  except
    on E: Exception do
    begin
      result := false;
    end;
  end;
end;

{******************************************************************************}
{* Add material                                                               *}
{******************************************************************************}

function TGDMaterialList.GetMaterial( aName : String ) : TGDMaterial;
var
  iMat    :  TGDMaterial;
  iI       : Integer;
begin
  result := nil;
  iI := 0;
  while ((iI < self.Count) and (result = nil )) do
  begin
    iMat := TGDMaterial(self.Items[iI]);
    If UpperCase(iMat.Name) = UpperCase(aName) then
      result := iMat;

    iI := iI + 1;
  end;
end;

end.
