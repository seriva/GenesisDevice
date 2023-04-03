unit uGDGLWrappers;

{$MODE Delphi}

interface

uses
  SysUtils,
  Classes,
  FileUtil,
  fgl,
  dglOpenGL,
  uGDConstants,
  uGDTypes,
  uGDTexture;

Type
  TGDShaderType = (ST_GEOM, ST_VERT, ST_FRAG);

  TGDGLShader = class
  private
    FGeometryShader : GLhandleARB;
    FVertexShader   : GLhandleARB;
    FFragmentShader : GLhandleARB;
    FProgramObject  : GLhandleARB;

    function LoadShader( aSrc: String; atype: GLenum): GLhandleARB;
    function GetInfoLog(aObject : GLhandleARB): String;
  public
    constructor Create(aFileName : string);
    destructor  Destroy(); override;

    procedure Bind();
    procedure Unbind();
    procedure SetInt(aVariable : String;  aV : integer);
    procedure SetFloat(aVariable : String; aV : Double);
    procedure SetFloat2(aVariable : String; aV0, aV1 : Double);
    procedure SetFloat3(aVariable : String; aV0, aV1, aV2 : Double);
    procedure SetFloat4(aVariable : String; aV0, aV1, aV2, aV3 : Double);
    procedure SetMatrix(aVariable : String; aMatrix : TGDMatrix);
  end;


  TGDGLRenderBuffer = class
  private
    FBufferID : GLuint;
  public
    constructor Create(aSizeW, aSizeH : Integer; aFormat  : cardinal);
    destructor  Destroy(); override;

    procedure Bind();
    procedure Unbind();
  end;


  TGDGLFrameBuffer = class
  private
    FBufferID : GLuint;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure Bind();
    procedure Unbind();
    procedure AttachTexture( aTexture : TGDTexture; aAttachement, aTexTarget : cardinal);
    procedure AttachRenderBuffer(aRenderBuffer : TGDGLRenderBuffer; aAttachement : cardinal);
    procedure Status();
  end;


  TGDGLDisplayList = class
  private
    FDisplayList : GLuint;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure StartList();
    procedure EndList();
    procedure CallList();
  end;


  TGDGLVertexBuffer = class
  private
    FBufferID : GLuint;
    FCount    : Integer;
    FItemSize : Integer;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure Bind(aLayout : TGDVertexLayout);
    procedure Unbind();
    procedure Update(aData : TFPSList; aDrawType  : cardinal);
    procedure Render(aPrimitive : cardinal);
  end;


  TGDGLIndexBuffer = class
  private
    FBufferID : GLuint;
    FCount    : Integer;
    FItemSize : Integer;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure Bind();
    procedure Unbind();
    procedure Update(aData : TFPSList; aDrawType  : cardinal);
    procedure Render(aPrimitive : cardinal);
  end;


  TGDGLOcclusionQuery = class
  private
    FOQID  : GLuint;
    FSTATE : TGDOcclusionState;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure Bind();
    procedure Unbind();
  end;

implementation

uses
  uGDEngine;

constructor TGDGLShader.Create(aFileName : string);
var
  iOk  : boolean;
  iCur : TGDShaderType;
  iI   : Integer;
  iSource : TStringList;
  iInc    : TStringList;
  iSplit  : TStringList;
  iLine, iGeom, ifrag, iVert, iError : String;
label
  IncludeLines;
begin
  GDConsole.Write('  Loading shader ' + ExtractFileName(aFileName) + '...');

  try
    iOk := true;
    FProgramObject := glCreateProgramObjectARB();
    iSource := TStringList.Create;
    iInc    := TStringList.Create;
    iSplit  := TStringList.Create;
    iSource.LoadFromFile(aFileName);

    iGeom := '';
    iVert := '';
    ifrag := '';

    for iI := 0 to iSource.Count-1 do
    begin
      iLine := iSource.Strings[iI];
      if Pos('#GEOMETRY', UpperCase(iLine)) > 0 then
       iCur := ST_GEOM
      else if Pos('#VERTEX', UpperCase(iLine)) > 0 then
        iCur := ST_VERT
      else if Pos('#FRAGMENT', UpperCase(iLine)) > 0 then
        iCur := ST_FRAG
      else if Pos('#INCLUDE', UpperCase(iLine)) > 0 then
      begin
        iSplit.Text := StringReplace(iLine, #32, #13#10, [rfReplaceAll]);
        iInc.LoadFromFile( PATH_SHADERS + Trim(iSplit.Strings[ iSplit.Count-1 ]) );
        iLine := iInc.Text;
        goto IncludeLines;
      end
      else
      begin
        IncludeLines:
        case iCur of
          ST_GEOM : iGeom := iGeom + #13#10 + iLine;
          ST_VERT : iVert := iVert + #13#10 + iLine;
          ST_FRAG : ifrag := ifrag + #13#10 + iLine;
        end;
      end;
    end;

    if iGeom <> '' then FGeometryShader := LoadShader(iGeom, GL_GEOMETRY_SHADER_ARB);
    if iVert <> '' then FVertexShader   := LoadShader(iVert, GL_VERTEX_SHADER_ARB);
    if ifrag <> '' then FFragmentShader := LoadShader(ifrag, GL_FRAGMENT_SHADER_ARB);

    glLinkProgramARB(FProgramObject);
  except
    on E: Exception do
    begin
      iOk    := false;
      iError := E.Message;
    end;
  end;

  FreeAndNil(iSplit);
  FreeAndNil(iInc);
  FreeAndNil(iSource);
  GDConsole.WriteOkFail(iOk, iError);
end;


destructor  TGDGLShader.Destroy();

procedure DestroyShader(aShader : GLhandleARB);
begin
	if (aShader > 0) then
	begin
		glDetachObjectARB(FProgramObject, aShader);
		glDeleteObjectARB(aShader);
		aShader := 0;
	end;
end;

begin
  DestroyShader(FGeometryShader);
  DestroyShader(FVertexShader);
  DestroyShader(FFragmentShader);
  glDeleteObjectARB(FProgramObject);
end;


function TGDGLShader.GetInfoLog(aObject : GLhandleARB): String;
var
  iBLen, iSLen: Integer;
  iInfoLog : PGLCharARB;
begin
  glGetObjectParameterivARB(aObject, GL_OBJECT_INFO_LOG_LENGTH_ARB , @iBLen);
  if iBLen > 1 then
  begin
    GetMem(iInfoLog, iBLen*SizeOf(GLCharARB));
    glGetInfoLogARB(aObject, iBLen, iSLen, iInfoLog);
    Result := String(iInfoLog);
    Dispose(iInfoLog);
  end;
end;


function TGDGLShader.LoadShader( aSrc: String; atype: GLenum): GLhandleARB;
var
  iSource: AnsiString;
  iCompiled, iLen: Integer;
  iLog: String;
  iShader : GLhandleARB;
begin
  iSource := AnsiString(aSrc);
  iLen := Length(aSrc);
  iShader := glCreateShaderObjectARB(atype);
  glShaderSourceARB(iShader, 1, @iSource, @iLen);
  glCompileShaderARB(iShader);
  glGetObjectParameterivARB(iShader, GL_OBJECT_COMPILE_STATUS_ARB, @iCompiled);
  iLog := GetInfoLog(iShader);
  if iCompiled <> 1 then
     raise Exception.Create(iLog);
  glAttachObjectARB(FProgramObject,iShader);
  result := iShader;
end;


procedure TGDGLShader.Bind();
begin
  glUseProgramObjectARB(FProgramObject);
end;


procedure TGDGLShader.UnBind();
begin
  glUseProgramObjectARB(0);
end;


procedure TGDGLShader.SetInt(aVariable : String;  aV : integer);
begin
   glUniform1iARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV);
end;


procedure TGDGLShader.SetFloat(aVariable : String; aV : Double);
begin
  glUniform1fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV);
end;


procedure TGDGLShader.SetFloat2(aVariable : String; aV0, aV1 : Double);
begin
  glUniform2fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV0, aV1);
end;


procedure TGDGLShader.SetFloat3(aVariable : String; aV0, aV1, aV2 : Double);
begin
  glUniform3fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV0, aV1, aV2);
end;


procedure TGDGLShader.SetFloat4(aVariable : String; aV0, aV1, aV2, aV3 : Double);
begin
  glUniform4fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]),aV0, aV1, aV2, aV3);
end;


procedure TGDGLShader.SetMatrix(aVariable : String; aMatrix : TGDMatrix);
begin
  glUniformMatrix4fv( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), 1, false, @aMatrix.data[0]);
end;


constructor TGDGLFrameBuffer.Create();
begin
  glGenFrameBuffersEXT(1, @FBufferID);
end;


destructor TGDGLFrameBuffer.Destroy();
begin
  inherited;
  glDeleteFrameBuffers(1, @FBufferID);
end;


procedure TGDGLFrameBuffer.Bind();
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FBufferID);
end;


procedure TGDGLFrameBuffer.Unbind();
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;


procedure TGDGLFrameBuffer.AttachTexture( aTexture : TGDTexture; aAttachement, aTexTarget : cardinal);
begin
  glFramebufferTexture2D(GL_FRAMEBUFFER, aAttachement, aTexTarget, aTexture.Texture, 0);
end;


procedure TGDGLFrameBuffer.AttachRenderBuffer(aRenderBuffer : TGDGLRenderBuffer; aAttachement : cardinal);
begin
  glFramebufferRenderbuffer(GL_FRAMEBUFFER,aAttachement,GL_RENDERBUFFER,aRenderBuffer.FBufferID);
end;


procedure TGDGLFrameBuffer.Status();
var
  iM: GLenum;
begin
  iM := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  case iM of
    GL_FRAMEBUFFER_COMPLETE:
      Exit;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
      GDConsole.Write('Error messages: FBO : Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
      GDConsole.Write('Error messages: FBO : Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT:
      GDConsole.Write('Error messages: FBO : Duplicate attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      GDConsole.Write('Error messages: FBO : Incomplete dimensions');
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      GDConsole.Write('Error messages: FBO : Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      GDConsole.Write('Error messages: FBO : Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      GDConsole.Write('Error messages: FBO : Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      GDConsole.Write('Error messages: FBO : Framebuffer unsupported');
    else
      GDConsole.Write('Error messages: FBO : Framebuffer unsupported');
  end;
end;


constructor TGDGLRenderBuffer.Create(aSizeW, aSizeH : Integer; aFormat  : cardinal);
begin
  glGenRenderBuffers(1, @FBufferID);
  glBindRenderbuffer(GL_RENDERBUFFER, FBufferID);
  glRenderbufferStorage(GL_RENDERBUFFER, aFormat,aSizeW, aSizeH);
end;


destructor  TGDGLRenderBuffer.Destroy();
begin
  inherited;
  glDeleteRenderBuffers(1, @FBufferID);
end;


procedure TGDGLRenderBuffer.Bind();
begin
  glBindRenderBuffer(GL_RENDERBUFFER, FBufferID);
end;


procedure TGDGLRenderBuffer.Unbind();
begin
  glBindRenderBuffer(GL_RENDERBUFFER, 0);
end;


constructor TGDGLDisplayList.Create();
begin
  FDisplayList := glGenLists(1);
end;


destructor  TGDGLDisplayList.Destroy();
begin
  glDeleteLists(FDisplayList,1);
  inherited;
end;


procedure TGDGLDisplayList.StartList();
begin
  glNewList(FDisplayList,GL_COMPILE);
end;


procedure TGDGLDisplayList.EndList();
begin
  glEndList();
end;


procedure TGDGLDisplayList.CallList();
begin
  glCallList(FDisplayList);
end;


constructor TGDGLVertexBuffer.Create();
begin
  glGenBuffers(1, @FBufferID);
  FCount    := 0;
  FItemSize := 0;
end;


destructor TGDGLVertexBuffer.Destroy();
begin
  glDeleteBuffers(1, @FBufferID);
  inherited;
end;


procedure TGDGLVertexBuffer.Bind(aLayout : TGDVertexLayout);
begin
  glBindBuffer(GL_ARRAY_BUFFER, FBufferID);
  case aLayout of
  VL_V :
  begin
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, 0, nil);
  end;
  VL_V_UV:
  begin
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, sizeof(TGDVertex_V_UV), GLvoid(0));
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, sizeof(TGDVertex_V_UV), GLvoid(12));
  end;
  VL_V_UV_N:
  begin
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, sizeof(TGDVertex_V_UV_N), GLvoid(0));
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, sizeof(TGDVertex_V_UV_N), GLvoid(12));
    glEnableClientState(GL_NORMAL_ARRAY);
    glNormalPointer(GL_FLOAT, sizeof(TGDVertex_V_UV_N), GLvoid(20));
  end;
  VL_V_UV_N_C:
  begin
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, sizeof(TGDVertex_V_UV_N_C), GLvoid(0));
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, sizeof(TGDVertex_V_UV_N_C), GLvoid(12));
    glEnableClientState(GL_NORMAL_ARRAY);
    glNormalPointer(GL_FLOAT, sizeof(TGDVertex_V_UV_N_C), GLvoid(20));
    glEnableClientState(GL_COLOR_ARRAY);
    glColorPointer(4, GL_FLOAT, sizeof(TGDVertex_V_UV_N_C), GLvoid(32));
  end;
  end;
end;


procedure TGDGLVertexBuffer.UnBind();
begin
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;


procedure TGDGLVertexBuffer.Update(aData : TFPSList; aDrawType  : cardinal);
begin
  FCount    := aData.Count;
  FItemSize := aData.ItemSize;
  glBufferData(GL_ARRAY_BUFFER, FItemSize*FCount, aData.List, aDrawType);
end;


procedure TGDGLVertexBuffer.Render(aPrimitive : cardinal);
begin
  glDrawArrays(aPrimitive, 0, FCount);
end;


constructor TGDGLIndexBuffer.Create();
begin
  glGenBuffers(1, @FBufferID);
  FCount    := 0;
  FItemSize := 0;
end;


destructor TGDGLIndexBuffer.Destroy();
begin
  glDeleteBuffers(1, @FBufferID);
  inherited;
end;


procedure TGDGLIndexBuffer.Bind();
begin
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferID);
end;


procedure TGDGLIndexBuffer.UnBind();
begin
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;


procedure TGDGLIndexBuffer.Update(aData : TFPSList; aDrawType  : cardinal);
begin
  FCount    := aData.Count;
  FItemSize := aData.ItemSize;
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, FItemSize*FCount, aData.List, aDrawType);
end;


procedure TGDGLIndexBuffer.Render(aPrimitive : cardinal);
begin
  glDrawElements(aPrimitive, FCount, GL_UNSIGNED_INT, nil);
end;


constructor TGDGLOcclusionQuery.Create();
begin
  glGenQueries(1, @FOQID);
  FState := OS_HIDDEN;
end;


destructor TGDGLOcclusionQuery.Destroy();
begin
  inherited;
  glDeleteQueries(1, @FOQID);
end;


procedure TGDGLOcclusionQuery.Bind();
begin
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);
  glBeginQuery(GL_SAMPLES_PASSED, FOQID);
end;


procedure TGDGLOcclusionQuery.UnBind();
begin
  glEndQuery(GL_SAMPLES_PASSED);
  glDepthMask(GL_TRUE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
end;

end.
