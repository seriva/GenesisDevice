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
unit GDGLObjects;

{$MODE Delphi}

interface

{******************************************************************************}
{* This unit holds wrappers arround some of the latest opengl 2.1             *}
{* specification to make use of them easier. Included are:                    *}
{* - Fragment and vertex Shaders (GLSL)                                       *}
{* - Frame buffers and Render buffers                                         *}
{* - Displaylists                                                             *}
{******************************************************************************}

uses
  SysUtils,
  Classes,
  FileUtil,
  dglOpenGL,
  GDConsole,
  GDConstants,
  GDTexture;

Type

{******************************************************************************}
{* Shader class                                                               *}
{******************************************************************************}

  TGDGLShader = class
  private
    FVertexShader    :   GLhandleARB;
    FFragmentShader  :   GLhandleARB;
    FProgramObject   :   GLhandleARB;
    FLoadedOk        :   Boolean;
    FMessage         :   String;

    function LoadShaderFromFile( aFileName: String;  atype: GLenum): GLhandleARB;
    function LoadShader( aSrc: String; atype: GLenum): GLhandleARB;
    function GetInfoLog(aObject : GLhandleARB): String;
  public
    constructor Create(aPath : string);
    destructor  Destroy(); override;

    procedure Enable();
    procedure Disable();
    procedure SetInt(aVariable : String;  aV : integer);
    procedure SetFloat(aVariable : String; aV : Double);
    procedure SetFloat2(aVariable : String; aV0, aV1 : Double);
    procedure SetFloat3(aVariable : String; aV0, aV1, aV2 : Double);
    procedure SetFloat4(aVariable : String; aV0, aV1, aV2, aV3 : Double);
  end;

{******************************************************************************}
{* Render buffer class                                                        *}
{******************************************************************************}

  TGDGLRenderBufferObject = class
  private
    FRenderBufferObject : GLuint;
  public
    property RenderBufferObject : GLuint read FRenderBufferObject;

    constructor Create();
    destructor  Destroy(); override;
    procedure InitRenderBuffer(aSizeW, aSizeH : Integer; aFormat  : cardinal);
    procedure Clear();
    procedure Bind();
    procedure Unbind();
  end;

{******************************************************************************}
{* Frame buffer class                                                         *}
{******************************************************************************}

  TGDGLFrameBufferObject = class
  private
    FFrameBufferObject : GLuint;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitFrameBuffer();
    procedure Clear();
    procedure Bind();
    procedure Unbind();
    procedure AttachTexture( aTexture : TGDTexture; aAttachement, aTexTarget : cardinal);
    procedure AttachRenderBufferObject(aRenderBuffer : TGDGLRenderBufferObject; aAttachement : cardinal);
    procedure Status();
  end;

{******************************************************************************}
{* Displaylist class                                                          *}
{******************************************************************************}

  TGDGLDisplayList = class
  private
    FDisplayList : GLuint;
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitDisplayList();
    procedure Clear();
    procedure StartList();
    procedure EndList();
    procedure CallList();
  end;

implementation

{******************************************************************************}
{* Create shader class                                                        *}
{******************************************************************************}

constructor TGDGLShader.Create(aPath : string);
var
  iVert, ifrag, iError : String;
begin
  Console.Write('Loading shader ' + ExtractFileName(aPath) + '...');
  FLoadedOk := True;

  try
    iVert := aPath + SHADER_VERT_EXT;
    ifrag := aPath + SHADER_FRAG_EXT;
    FVertexShader := glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
    FFragmentShader := glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);

    If FileExistsUTF8(iVert ) and FileExistsUTF8(ifrag ) then
    begin
      FVertexShader := LoadShaderFromFile(iVert, GL_VERTEX_SHADER_ARB);
      FFragmentShader := LoadShaderFromFile(ifrag, GL_FRAGMENT_SHADER_ARB);
    end
    else
      raise Exception.Create('Shader files not present');

    FProgramObject := glCreateProgramObjectARB();
    glAttachObjectARB(FProgramObject,FVertexShader);
    glAttachObjectARB(FProgramObject,FFragmentShader);
    glLinkProgramARB(FProgramObject);
  except
    on E: Exception do
    begin
      iError := E.Message;
      FLoadedOk := False;
    end;
  end;

  Console.WriteOkFail(FLoadedOk, iError);
end;

{******************************************************************************}
{* Destroy shader class                                                       *}
{******************************************************************************}

destructor  TGDGLShader.Destroy();
begin
	if (FVertexShader > 0) then
	begin
		glDetachObjectARB(FProgramObject, FVertexShader);
		glDeleteObjectARB(FVertexShader);
		FVertexShader := 0;
	end;

	if (FFragmentShader > 0) then
	begin
		glDetachObjectARB(FProgramObject, FFragmentShader);
		glDeleteObjectARB(FFragmentShader);
		FFragmentShader := 0;
	end;

	if (FProgramObject > 0)  then
	begin
		glDeleteObjectARB(FProgramObject);
		FProgramObject := 0;
	end;
end;

{******************************************************************************}
{* Get the infolog after the shader is compiled                               *}
{******************************************************************************}

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

{******************************************************************************}
{* Load and compile a shader shader                                           *}
{******************************************************************************}

function TGDGLShader.LoadShader( aSrc: String; atype: GLenum): GLhandleARB;
var
  iSource: AnsiString;
  iCompiled, iLen: Integer;
  iLog: String;
begin

  iSource := AnsiString(aSrc);
  iLen := Length(aSrc);

  Result := glCreateShaderObjectARB(atype);

  glShaderSourceARB(Result, 1, @iSource, @iLen);
  glCompileShaderARB(Result);
  glGetObjectParameterivARB(Result, GL_OBJECT_COMPILE_STATUS_ARB, @iCompiled);
  iLog := GetInfoLog(Result);

  if iCompiled <> GL_TRUE then
  begin
    FLoadedOk        :=   false;;
    FMessage         :=   iLog;
  end;
end;

{******************************************************************************}
{* Load the and compile both the fragment and the vertex shader from a file   *}
{******************************************************************************}

function TGDGLShader.LoadShaderFromFile( aFileName: String;  atype: GLenum): GLhandleARB;
var
  iTXT: TStringList;
begin

  iTXT := TStringList.Create;
  iTXT.LoadFromFile(aFileName);

  try
    Result := LoadShader(iTXT.Text, atype);
  except
    on E: Exception do
    begin
      FLoadedOk        :=   false;
      FMessage         :=   e.Message;
    end;
  end;

  iTXT.Free;
end;

{******************************************************************************}
{* Enable the shader                                                          *}
{******************************************************************************}

procedure TGDGLShader.Enable();
begin
  glUseProgramObjectARB(FProgramObject);
end;

{******************************************************************************}
{* Disable the shader                                                         *}
{******************************************************************************}

procedure TGDGLShader.Disable();
begin
  glUseProgramObjectARB(0);
end;

{******************************************************************************}
{* Pass an int to the compiled shader shader program                          *}
{******************************************************************************}

procedure TGDGLShader.SetInt(aVariable : String;  aV : integer);
begin
   glUniform1iARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV);
end;

{******************************************************************************}
{* Pass a float to the compiled shader program                                *}
{******************************************************************************}

procedure TGDGLShader.SetFloat(aVariable : String; aV : Double);
begin
  glUniform1fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV);
end;

{******************************************************************************}
{*  Pass 2 floats to the compiled shader program                              *}
{******************************************************************************}

procedure TGDGLShader.SetFloat2(aVariable : String; aV0, aV1 : Double);
begin
  glUniform2fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV0, aV1);
end;

{******************************************************************************}
{* Pass 3 floats to the compiled shader program                               *}
{******************************************************************************}

procedure TGDGLShader.SetFloat3(aVariable : String; aV0, aV1, aV2 : Double);
begin
  glUniform3fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]), aV0, aV1, aV2);
end;

{******************************************************************************}
{* Pass 4 floats to the compiled shader program                               *}
{******************************************************************************}

procedure TGDGLShader.SetFloat4(aVariable : String; aV0, aV1, aV2, aV3 : Double);
begin
  glUniform4fARB( glGetUniformLocationARB(FProgramObject, @PAnsiChar(AnsiString(aVariable))[0]),aV0, aV1, aV2, aV3);
end;

{******************************************************************************}
{* Create the framebuffer class                                               *}
{******************************************************************************}

constructor TGDGLFrameBufferObject.Create();
begin
  glGenFrameBuffersEXT(1, @FFrameBufferObject);
end;

{******************************************************************************}
{* Destroy the framebuffer class                                              *}
{******************************************************************************}

destructor TGDGLFrameBufferObject.Destroy();
begin
  inherited;
  Clear();
end;

{******************************************************************************}
{* Init the frame buffer                                                      *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.InitFrameBuffer();
begin
  Clear();
  glGenFrameBuffersEXT(1, @FFrameBufferObject);
end;

{******************************************************************************}
{* Clear the framebuffer                                                      *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.Clear();
begin
  glDeleteFrameBuffersEXT(1, @FFrameBufferObject);
end;

{******************************************************************************}
{* Bind the framebuffer                                                       *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.Bind();
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBufferObject);
end;

{******************************************************************************}
{* Unbind the framebuffer                                                     *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.Unbind();
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

{******************************************************************************}
{* Attach a texture to the frame buffer                                       *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.AttachTexture( aTexture : TGDTexture; aAttachement, aTexTarget : cardinal);
begin
  glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, aAttachement, aTexTarget, aTexture.Texture, 0);
end;

{******************************************************************************}
{* Attach renderbuffer to the framebuffer                                     *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.AttachRenderBufferObject(aRenderBuffer : TGDGLRenderBufferObject; aAttachement : cardinal);
begin
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT,aAttachement,GL_RENDERBUFFER_EXT,aRenderBuffer.RenderBufferObject);
end;

{******************************************************************************}
{* Get the status of the framebuffer                                          *}
{******************************************************************************}

procedure TGDGLFrameBufferObject.Status();
var
  iM: GLenum;
begin
  iM := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
  case iM of
    GL_FRAMEBUFFER_COMPLETE_EXT:
      Exit;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
      Console.Write('Error messages: FBO : Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
      Console.Write('Error messages: FBO : Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT:
      Console.Write('Error messages: FBO : Duplicate attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      Console.Write('Error messages: FBO : Incomplete dimensions');
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      Console.Write('Error messages: FBO : Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      Console.Write('Error messages: FBO : Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      Console.Write('Error messages: FBO : Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      Console.Write('Error messages: FBO : Framebuffer unsupported');
    else
      Console.Write('Error messages: FBO : Framebuffer unsupported');
  end;
end;

{******************************************************************************}
{* Create the renderbuffer class                                              *}
{******************************************************************************}

constructor TGDGLRenderBufferObject.Create();
begin
  FRenderBufferObject := 0;
end;

{******************************************************************************}
{* Destroy the renderbuffer class                                             *}
{******************************************************************************}

destructor  TGDGLRenderBufferObject.Destroy();
begin
  inherited;
  Clear();
end;

{******************************************************************************}
{* Init the renderbuffer                                                      *}
{******************************************************************************}

procedure TGDGLRenderBufferObject.InitRenderBuffer(aSizeW, aSizeH  : integer;  aFormat : cardinal);
begin
  Clear();
  glGenRenderBuffersEXT(1, @FRenderBufferObject);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FRenderBufferObject);
  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, aFormat,aSizeW, aSizeH);
end;

{******************************************************************************}
{* Clear the renderbuffer                                                     *}
{******************************************************************************}

Procedure TGDGLRenderBufferObject.Clear();
begin
  glDeleteRenderBuffersEXT(1, @FRenderBufferObject);
end;

{******************************************************************************}
{* Bind the renderbuffer                                                      *}
{******************************************************************************}

procedure TGDGLRenderBufferObject.Bind();
begin
  glBindRenderBufferEXT(GL_RENDERBUFFER_EXT, FRenderBufferObject);
end;

{******************************************************************************}
{* Unbind the renderbuffer                                                    *}
{******************************************************************************}

procedure TGDGLRenderBufferObject.Unbind();
begin
  glBindRenderBufferEXT(GL_RENDERBUFFER_EXT, 0);
end;

{******************************************************************************}
{* Create the displaylist class                                               *}
{******************************************************************************}

constructor TGDGLDisplayList.Create();
begin
  FDisplayList := 0;
end;

{******************************************************************************}
{* Destroy the displaylist class                                              *}
{******************************************************************************}

destructor  TGDGLDisplayList.Destroy();
begin
  Clear();
  inherited;
end;

{******************************************************************************}
{* Init the displaylist                                                       *}
{******************************************************************************}

procedure TGDGLDisplayList.InitDisplayList();
begin
  FDisplayList := glGenLists(1);
end;

{******************************************************************************}
{* Clear displaylist                                                          *}
{******************************************************************************}

procedure TGDGLDisplayList.Clear();
begin
  glDeleteLists(FDisplayList,1);
end;

{******************************************************************************}
{* Start the creation of the list                                             *}
{******************************************************************************}

procedure TGDGLDisplayList.StartList();
begin
  glNewList(FDisplayList,GL_COMPILE);
end;

{******************************************************************************}
{* End the creation of the list                                               *}
{******************************************************************************}

procedure TGDGLDisplayList.EndList();
begin
  glEndList();
end;

{******************************************************************************}
{* Call the list                                                              *}
{******************************************************************************}

procedure TGDGLDisplayList.CallList();
begin
  glCallList(FDisplayList);
end;

end.
