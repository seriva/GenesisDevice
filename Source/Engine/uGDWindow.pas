unit uGDWindow;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  SDL2,
  uGDSettings;

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
    procedure SetMouse();
    procedure SetTitle(aTitle : String);
  end;

implementation

uses
  uGDEngine;

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
    GDConsole.Write('Initializing window...');

  	//create window
  	FWindow := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 480, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE or SDL_WINDOW_HIDDEN);
    if FWindow = nil then
      GDConsole.Write('Failed to initialize SDL window: ' + SDL_GetError());

    //create context
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
    FGLContext := SDL_GL_CreateContext(FWindow);
    if FGLContext = nil then
      GDConsole.Write('Failed to initialize SDL gl context: ' + SDL_GetError());
    SDL_HideWindow(FWindow);

    MakeCurrent();

    FInitialized := true;
  except
    on E: Exception do
    begin
      iError := E.Message;
    end;
  end;

  GDConsole.WriteOkFail(FInitialized, iError);
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
  GDConsole.Write('Shutting down window...');
  try
    iResult := true;
    SDL_GL_DeleteContext(FGLContext);
    SDL_DestroyWindow(FWindow);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  GDConsole.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Show                                                                     *}
{******************************************************************************}

procedure TGDWindow.Show();
var
  iRec : TSDL_Rect;
begin
  SDL_SetWindowSize(FWindow, GDSettings.Width, GDSettings.Height);
  if GDSettings.VerticalSync then
    SDL_GL_SetSwapInterval(1)
  else
    SDL_GL_SetSwapInterval(0);

  SDL_GetDisplayBounds(GDSettings.Display, @iRec);

  if GDSettings.FullScreen then
  begin
    SDL_SetWindowPosition(FWindow ,iRec.x, iRec.y);
    SDL_SetWindowFullscreen(FWindow, SDL_WINDOW_FULLSCREEN);
  end
  else
    SDL_SetWindowPosition(FWindow ,iRec.x+5, iRec.y+40);

  SDL_ShowWindow(FWindow);
  MakeCurrent();
end;

{******************************************************************************}
{* Hide                                                                       *}
{******************************************************************************}

procedure TGDWindow.Hide();
begin
  if GDSettings.FullScreen then
    SDL_SetWindowFullscreen(FWindow, 0);
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
var
  event : TSDL_Event;
begin;
  while ( SDL_PollEvent( @event ) = 1 ) do
  begin
    case event.type_ of
      SDL_QUITEV          : GDEngine.Done := True;
      SDL_KEYDOWN         : GDConsole.Control(event.key.keysym.sym);
      SDL_TEXTINPUT       : GDConsole.AddChar(event.text.text[0]);
      SDL_WINDOWEVENT     : begin
                              case event.window.event of
                                SDL_WINDOWEVENT_RESIZED:
                                  GDRenderer.ResizeViewPort(event.window.data1, event.window.data2);
                              end;
      											end;
    end;
  end;
end;

{******************************************************************************}
{* Set the mouse in the center of the windows                                 *}
{******************************************************************************}

procedure TGDWindow.SetMouse();
begin
  SDL_WarpMouseInWindow(FWindow, GDSettings.Width div 2, GDSettings.Height div 2);
end;

{******************************************************************************}
{* Set the title of the window 							                                  *}
{******************************************************************************}

procedure TGDWindow.SetTitle(aTitle : String);
begin
  SDL_SetWindowTitle(FWindow, PChar(aTitle));
end;

end.

