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
unit GDResources;

{$mode objfpc}

interface

uses
  GDSettings,
  GDStringParsing,
  Classes,
  SysUtils,
  GDResource,
  GDTexture,
  GDMesh,
  FileUtil,
  GDMaterial,
  GDConsole,
  GDConstants,
  GDSound,
  FGL;

type

 {******************************************************************************}
 {* Recourses class                                                            *}
 {******************************************************************************}

   TResources = specialize TFPGMap<String,TGDResource>;
   TGDResources = class (TResources)
   private
     procedure AddResource(aName : String; aResource : TGDResource);
     function  GetResource(aIndex : integer): TGDResource;
   public
     function  LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;
     function  LoadMesh(aFileName : String): TGDMesh;
     function  LoadSound(aFileName : String; aType : TGDSoundTypes): TGDSoundFile;
     procedure LoadMaterials(aFileName : String);

     procedure RemoveResource(var aResource : TGDResource);
     procedure Clear();
   end;

var
  Resources : TGDResources;

implementation

{******************************************************************************}
{* Load a texture resource                                                    *}
{******************************************************************************}

function TGDResources.LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;
var
  iIdx : Integer;
  iResource : TGDResource;
begin
  if Find(aFileName, iIdx) then
    result := GetResource(iIdx) as TGDTexture
  else
  begin
    result := TGDTexture.Create(aFileName, aDetail, aTextureFilter);
    AddResource(aFileName, result);
  end;
end;

{******************************************************************************}
{* Load a mesh resource                                                       *}
{******************************************************************************}

function TGDResources.Loadmesh(aFileName : String): TGDMesh;
var
  iIdx : Integer;
  iResource : TGDResource;
begin
  if Find(aFileName, iIdx) then
    result := GetResource(iIdx) as TGDMesh
  else
  begin
    result := TGDMesh.Create(aFileName);
    AddResource(aFileName, result);
  end;
end;

{******************************************************************************}
{* Load a sound resource                                                      *}
{******************************************************************************}

function TGDResources.LoadSound(aFileName : String; aType : TGDSoundTypes): TGDSoundFile;
var
  iIdx : Integer;
  iResource : TGDResource;
begin
  if Find(aFileName, iIdx) then
    result := GetResource(iIdx) as TGDSoundFile
  else
  begin
    result := TGDSoundFile.Create(aFileName, aType);
    AddResource(aFileName, result);
  end;
end;

{******************************************************************************}
{* Load materials                                                             *}
{******************************************************************************}

procedure TGDResources.LoadMaterials(aFileName : String);
var
  iFile : TMemoryStream;
  iMat  : TGDMaterial;
  iStr  : String;
  iIdx : Integer;
  iResource : TGDResource;
begin
  try
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
        if Find(iStr, iIdx) then
        begin
          iMat:= nil;
          continue;
        end;
        iMat := TGDMaterial.Create();
        AddResource(iStr, iMat);
        continue;
      end
      else if iStr = 'colormap' then //load the material texture
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.Texture := Resources.LoadTexture(ExtractFilePath(aFileName) + iStr ,Settings.TextureDetail,Settings.TextureFilter);
        continue;
      end
      else if iStr = 'has_alpha' then //read alpha
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.HasAlpha:= iStr = 'true';
        continue;
      end
      else if iStr = 'do_bloom' then //read bloom
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.DoBloom:= iStr = 'true';
        continue;
      end
      else if iStr = 'do_treeanim' then //read bloom
      begin
        if iMat = nil then
           continue;
        iStr := GetNextToken(iFile);
        iMat.DoTreeAnim:= iStr = 'true';
        continue;
      end
      else if iStr = 'alpha_func' then //read bloom
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.AlphaFunc := StrToFloat(iStr);
        continue;
      end;
    end;
    FreeAndNil(iFile);
  except
    on E: Exception do
    begin
    end;
  end;
end;

{******************************************************************************}
{* Add a resources                                                            *}
{******************************************************************************}

procedure TGDResources.AddResource(aName : String; aResource : TGDResource);
begin
  aResource.Name := aName;
  aResource.RefCount := 1;
  Add(aName, aResource);
  Sort();
end;

{******************************************************************************}
{* Add a resources                                                            *}
{******************************************************************************}

function  TGDResources.GetResource(aIndex : integer): TGDResource;
begin
  result := Data[aIndex];
  result.RefCount := result.RefCount-1;
end;

{******************************************************************************}
{* Remove a resources                                                         *}
{******************************************************************************}

procedure TGDResources.RemoveResource(var aResource : TGDResource);
var
  iIdx : Integer;
begin
  if aResource = nil then exit;
  if Find(aResource.Name, iIdx) then
  begin
    aResource.RefCount := aResource.RefCount-1;
    if aResource.RefCount <= 0 then
    begin
      self.Delete(iIdx);
      self.Remove(aResource.Name);
      FreeAndNil(aResource);
      aResource := nil;
    end;
  end
end;

{******************************************************************************}
{* Clear all resources                                                        *}
{******************************************************************************}

procedure TGDResources.Clear();
var
  iK : Integer;
begin
  for ik := Count - 1 downto 0 do
    Delete(ik);
end;

end.

