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
    procedure SetMouse();
    procedure SetTitle(aTitle : String);
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
  	FWindow := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 480, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE or SDL_WINDOW_HIDDEN);
    if FWindow = nil then
      Engine.Console.Write('Failed to initialize SDL window: ' + SDL_GetError());

    //create context
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
    FGLContext := SDL_GL_CreateContext(FWindow);
    if FGLContext = nil then
      Engine.Console.Write('Failed to initialize SDL gl context: ' + SDL_GetError());
    SDL_HideWindow(FWindow);

    MakeCurrent();

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
var
  iRec : TSDL_Rect;
begin
  SDL_SetWindowSize(FWindow, Engine.Settings.Width, Engine.Settings.Height);
  if Engine.Settings.VerticalSync then
    SDL_GL_SetSwapInterval(1)
  else
    SDL_GL_SetSwapInterval(0);

  SDL_GetDisplayBounds(Engine.Settings.Display, @iRec);

  if Engine.Settings.FullScreen then
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
  if Engine.Settings.FullScreen then
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
      SDL_QUITEV          : Engine.Done := True;
      SDL_KEYDOWN         : Engine.Console.Control(event.key.keysym.sym);
      SDL_TEXTINPUT       : Engine.Console.AddChar(event.text.text[0]);
      SDL_WINDOWEVENT     : begin
                              case event.window.event of
                                SDL_WINDOWEVENT_RESIZED:
                                  Engine.Renderer.ResizeViewPort(event.window.data1, event.window.data2);
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
  SDL_WarpMouseInWindow(FWindow, Engine.Settings.Width div 2, Engine.Settings.Height div 2);
end;

{******************************************************************************}
{* Set the title of the window 							                                  *}
{******************************************************************************}

procedure TGDWindow.SetTitle(aTitle : String);
begin
  SDL_SetWindowTitle(FWindow, PChar(aTitle));
end;

end.

