unit GDWindow;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  SDL2,
  GDSettings;

type

{******************************************************************************}
{* Window class                                                               *}
{******************************************************************************}

  TGDWindow = class
  private
    FWindow      : PSDL_Window;
    FGLContext   : TSDL_GLContext;
    FInitialized : boolean;
  public
    property    Initialized : boolean read FInitialized;

    Constructor Create();
    Destructor  Destroy(); override;

    procedure Show();
    procedure Hide();
    procedure MakeCurrent();
    procedure Swap();
    procedure Update();
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create window class                                                        *}
{******************************************************************************}

Constructor TGDWindow.Create();
var
  iError : String;
begin
  inherited;

  try
    FInitialized := false;
    Engine.Console.Write('Initializing window...');

  	//create window
  	FWindow := SDL_CreateWindow('', 0, 0, 100, 75, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE or SDL_WINDOW_HIDDEN);
    if FWindow = nil then
      Engine.Console.Write('Failed to initialize SDL window: ' + SDL_GetError());

    //create context
    SDL_GL_SetAttribute(SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1);
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
    FGLContext := SDL_GL_CreateContext(FWindow);
    if FGLContext = nil then
      Engine.Console.Write('Failed to initialize SDL gl context: ' + SDL_GetError());
    SDL_HideWindow(FWindow);

    FInitialized := true;
  except
    on E: Exception do
    begin
      iError := E.Message;
    end;
  end;

  Engine.Console.WriteOkFail(FInitialized, iError);
end;

{******************************************************************************}
{* Destroy window class                                                       *}
{******************************************************************************}

Destructor TGDWindow.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  Engine.Console.Write('Shutting down window...');
  try
    SDL_GL_DeleteContext(FGLContext);
    SDL_DestroyWindow(FWindow);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Engine.Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Show                                                                     *}
{******************************************************************************}

procedure TGDWindow.Show();
begin
  SDL_ShowWindow(FWindow);
  SDL_SetWindowSize(FWindow, Engine.Settings.Width, Engine.Settings.Height);
  //SDL_SetWindowTitle(FWindow, 'test');
  MakeCurrent();
end;

{******************************************************************************}
{* Hide                                                                       *}
{******************************************************************************}

procedure TGDWindow.Hide();
begin
  SDL_HideWindow(FWindow);
end;

{******************************************************************************}
{* Make rendercontext current                                                 *}
{******************************************************************************}

procedure TGDWindow.MakeCurrent();
begin
  SDL_GL_MakeCurrent(FWindow, FGLContext)
end;

{******************************************************************************}
{* Swap the backbuffer                                                        *}
{******************************************************************************}

procedure TGDWindow.Swap();
begin
  SDL_GL_SwapWindow(FWindow);
end;

{******************************************************************************}
{* Update                                                                     *}
{******************************************************************************}

procedure TGDWindow.Update();
begin

end;

end.

