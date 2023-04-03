unit uGDMaterial;

{$MODE Delphi}

interface

Uses
  Classes,
  SysUtils,
  dglOpenGL,
  uGDTexture,
  uGDConstants,
  FileUtil,
  uGDResource,
  uGDGLWrappers,
  uGDStringParsing;

Type
  TGDMaterial = class (TGDResource)
  private
    FTexture      : TGDTexture;
    FDetail       : TGDTexture;
    FHasAlpha     : Boolean;
    FAlphaFunc    : double;
    FDoTreeAnim   : boolean;
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

    procedure   ApplyMaterial(aRenderFor : TGDRenderFor );
    procedure   DisableMaterial();
  end;
  
implementation

uses
  uGDEngine;

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


destructor TGDMaterial.Destroy();
begin
  GDResources.RemoveResource(TGDResource(FTexture));
  GDResources.RemoveResource(TGDResource(FDetail));
  inherited;
end;


procedure   TGDMaterial.ApplyMaterial();
begin
  with GDRenderer do
  begin
    MeshShader.SetInt('T_COLORMAP', 0);
    if DoTreeAnim and not(aRenderFor = RF_SHADOW) then
      MeshShader.SetInt('I_DO_TREE_ANIM', 1)
    else
      MeshShader.SetInt('I_DO_TREE_ANIM', 0);

    if assigned(FTexture) then FTexture.BindTexture( GL_TEXTURE0 );
    if assigned(FDetail) and GDSettings.UseDetail then
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


procedure TGDMaterial.DisableMaterial();
begin
  if FHasAlpha then
    glDisable(GL_ALPHA_TEST);
end;

end.
