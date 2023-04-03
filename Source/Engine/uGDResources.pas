unit uGDResources;

{$mode objfpc}

interface

uses
  uGDStringParsing,
  Classes,
  SysUtils,
  uGDResource,
  uGDTexture,
  uGDMesh,
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
  iFile : TMemoryStream;
  iMat  : TGDMaterial;
  iStr  : String;
  iIdx : Integer;
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
        iIdx := IndexOf(iStr);
        if iIdx >= 0 then
        begin
          iMat:= nil;
          continue;
        end;
        iMat := TGDMaterial.Create();
        AddResource(iStr, iMat);
        continue;
      end else if iStr = 'colormap' then //load the material texture
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.Texture := GDResources.LoadTexture(iStr , GDSettings.TextureDetail,GDSettings.TextureFilter);
        continue;
      end else if iStr = 'detailmap' then //load the material detail
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.Detail := GDResources.LoadTexture(iStr , GDSettings.TextureDetail,GDSettings.TextureFilter);
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
      else if iStr = 'do_treeanim' then //read tree animation
      begin
        if iMat = nil then
           continue;
        iStr := GetNextToken(iFile);
        iMat.DoTreeAnim:= iStr = 'true';
        continue;
      end
      else if iStr = 'alpha_func' then //read alpha function
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.AlphaFunc := StrToFloat(iStr);
        continue;
      end else if iStr = 'detail_uv_mult' then //read detail uv mult
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.DetailUVMult := StrToInt(iStr);
        continue;
      end else if iStr = 'detail_mult' then //read detail mult
      begin
        if iMat = nil then
          continue;
        iStr := GetNextToken(iFile);
        iMat.DetailMult := StrToFloat(iStr);
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

