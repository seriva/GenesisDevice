unit uGDResources;

{$mode objfpc}

interface

uses
  Classes,
  SysUtils,
  JsonTools,
  uGDResource,
  uGDTexture,
  uGDMesh,
  uGDModel,
  LazFileUtils,
  uGDMaterial,
  uGDConstants,
  uGDSound,
  FGL;

type
   TResources = specialize TFPGMap<String,TGDResource>;
   TGDResources = class (TResources)
   private
     procedure AddResource(aName : String; aResource : TGDResource);
     function  GetResource(aIndex : integer): TGDResource;
   public
     function  LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;
     function  LoadMesh(aFileName : String): TGDMesh;
     function  LoadModel(aFileName : String): TGDModel;
     function  LoadSoundBuffer(aFileName : String): TGDSoundBuffer;
     function  LoadSoundStream(aFileName : String): TGDSoundStream;
     procedure LoadMaterials(aFileName : String);

     procedure RemoveResource(var aResource : TGDResource);
     procedure Clear();
   end;

implementation

uses
  uGDEngine;

function TGDResources.LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;
var
  iIdx : Integer;
begin
  iIdx := IndexOf(aFileName);
  if iIdx >= 0 then
    result := GetResource(iIdx) as TGDTexture
  else
  begin
    result := TGDTexture.Create(aFileName, aDetail, aTextureFilter);
    AddResource(aFileName, result);
  end;
end;


function TGDResources.Loadmesh(aFileName : String): TGDMesh;
var
  iIdx : Integer;
begin
  if aFileName = '' then
  begin
    result := nil;
    exit
  end;

  iIdx := IndexOf(aFileName);
  if iIdx >= 0 then
    result := GetResource(iIdx) as TGDMesh
  else
  begin
    result := TGDMesh.Create(aFileName);
    AddResource(aFileName, result);
  end;
end;


function TGDResources.LoadModel(aFileName : String): TGDModel;
var
  iIdx : Integer;
begin
  if aFileName = '' then
  begin
    result := nil;
    exit
  end;

  iIdx := IndexOf(aFileName);
  if iIdx >= 0 then
    result := GetResource(iIdx) as TGDModel
  else
  begin
    result := TGDModel.Create(aFileName);
    AddResource(aFileName, result);
  end;
end;


function TGDResources.LoadSoundBuffer(aFileName : String): TGDSoundBuffer;
var
  iIdx : Integer;
begin
  iIdx := IndexOf(aFileName);
  if iIdx >= 0 then
    result := GetResource(iIdx) as TGDSoundBuffer
  else
  begin
    result := TGDSoundBuffer.Create(aFileName);
    AddResource(aFileName, result);
  end;
end;


function TGDResources.LoadSoundStream(aFileName : String): TGDSoundStream;
var
  iIdx : Integer;
begin
  iIdx := IndexOf(aFileName);
  if iIdx >= 0 then
    result := GetResource(iIdx) as TGDSoundStream
  else
  begin
    result := TGDSoundStream.Create(aFileName);
    AddResource(aFileName, result);
  end;
end;


procedure TGDResources.LoadMaterials(aFileName : String);
var
  iMaterials, iMaterial : TJsonNode;
  iMat  : TGDMaterial;
  iName, iStr : String;
  iIdx, iI  : Integer;
begin
  try
    If Not(FileExists(aFileName) ) then
      Raise Exception.Create('');
    iMaterials := TJsonNode.Create();
    iMaterials.LoadFromFile(aFileName);

    for iI := 0 to iMaterials.Count-1 do
    begin
      iMaterial := iMaterials.Child(iI);

      iName := iMaterial.find('Name').AsString;
      iIdx := IndexOf(iName);
      if iIdx >= 0 then
        continue;
      iMat := TGDMaterial.Create();
      iMat.Texture := GDResources.LoadTexture(iMaterial.find('ColorMap').AsString , GDSettings.TextureDetail,GDSettings.TextureFilter);
      iStr := iMaterial.find('DetailMap').AsString;
      if iStr <> '' then
        iMat.Detail := GDResources.LoadTexture(iStr , GDSettings.TextureDetail,GDSettings.TextureFilter);
      iMat.HasAlpha     := iMaterial.Find('HasAlpha').AsBoolean; 
      iMat.DoTreeAnim   := iMaterial.Find('DoTreeAnim').AsBoolean;
      iMat.AlphaFunc    := iMaterial.Find('AlphaFunc').AsNumber;
      iMat.DetailUVMult := Trunc(iMaterial.Find('DetailUVMult').AsNumber);
      iMat.DetailMult   := iMaterial.Find('DetailMult').AsNumber;

      AddResource(iName, iMat);
    end;

    FreeAndNil(iMaterials);
  except
    on E: Exception do
    begin
    end;
  end;
end;


procedure TGDResources.AddResource(aName : String; aResource : TGDResource);
begin
  aResource.Name := aName;
  aResource.RefCount := 1;
  Add(aName, aResource);
  Sort();
end;


function  TGDResources.GetResource(aIndex : integer): TGDResource;
begin
  result := Data[aIndex];
  result.RefCount := result.RefCount-1;
end;


procedure TGDResources.RemoveResource(var aResource : TGDResource);
var
  iIdx : Integer;
begin
  if aResource = nil then exit;
  iIdx := IndexOf(aResource.Name);
  if iIdx >= 0 then
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


procedure TGDResources.Clear();
var
  iK : Integer;
begin
  for ik := Count - 1 downto 0 do
    Delete(ik);
end;

end.

