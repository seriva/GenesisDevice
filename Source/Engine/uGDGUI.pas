unit uGDGUI;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fgl,
  sdl2,
  Classes,
  JsonTools,
  SysUtils,
  dglOpenGL,
  uGDTexture,
  uGDConstants,
  uGDWindow,
  uGDResource,
  uGDTypes;

type
  TGDCharacterRect = Record
    X1,Y1,X2,Y2, W, H : single;
    class operator =(aC1, aC2: TGDCharacterRect) B: Boolean;
  end;
  TGDCharacterRectList = specialize TFPGList<TGDCharacterRect>;
  TGDFont = class (TGDResource)
  private
    FSpaceWidth : Integer;
    FCharacters : TGDCharacterRectList;
    FTexture    : TGDTexture;
    FColor      : TGDColor;
  public
    property Color : TGDColor read FColor write FColor;

    constructor Create(aFileName : string);
    destructor  Destroy(); override;
    procedure   Render( aLeft, aTop, aScale : Double; aString : string);
    function    TextWidth(const aString : String; const aScale : Single = 1): Integer;
  end;


  TGDComponent = class
  private
    FDepth : Integer;
    FX : Integer;
    FY : Integer;
  public
    property Depth : Integer read FDepth write FDepth;
    property X : Integer read FX write FX;
    property Y : Integer read FY write FY;

    procedure Render(); virtual;
  end;
  TGDComponentList = specialize TFPGObjectList<TGDComponent>;


  TGDPanel = class (TGDComponent)
  private
    FWidth : Integer;
    FHeight : Integer;
    FTexture : TGDTexture;
  public
    constructor Create(aPanel : TJsonNode);
    destructor  Destroy(); override;
    procedure   Render();  override;
  end;


  TGDLabel = class (TGDComponent)
  private
    FFont: TGDFont;
    FScale : Single;
    FText : String;
    FColor : TGDColor;
  public
    constructor Create(aLabel : TJsonNode);
    destructor  Destroy(); override;
    procedure   Render();  override;
  end;


  TGDScreen = class
  private
    FVisible : Boolean;
    FComponents : TGDComponentList;
  public
    property Visible : Boolean read FVisible write FVisible;

    constructor Create(aFileName : String);
    destructor  Destroy(); override;
    procedure   Render();
  end;
  TGDScreenList = specialize TFPGObjectList<TGDScreen>;


  TGDMouseCursor = class
  private
    FCursorTexture : TGDTexture;
    FVisible       : Boolean;
    FPosition      : TPoint;
    FCursorSize    : Integer;
  public
    property Visible : boolean read FVisible write FVisible;
    property Position : TPoint read FPosition write FPosition;

    constructor Create(aFileName: String; aCursorSize : Integer );
    destructor  Destroy(); override;
    procedure Render();
  end;


  TGDLoadingScreen = class
  private
    FUse             : boolean;
    FX, FY           : Integer;
    FMax             : integer;
    FPosition        : integer;
    FBarColor        : TGDColor;
    FProcesName      : String;
    FBarOnly         : boolean;

    Procedure   Render();
  public
    property Max : integer read FMax write FMax;
    property Position : integer read FPosition write FPosition;
    property Use : boolean read FUse write FUse;

    constructor Create(aSettings : TJsonNode);
    destructor  Destroy();override;

    procedure   Start( aProcesName : String; aMax : integer );
    procedure   Update();
  end;


  TGDGUI = class
  private
    //Base components
    FDefaultFont   : TGDFont;
    FMouseCursor   : TGDMouseCursor;
    FLoadingScreen : TGDLoadingScreen;

    //Base colors
    FFontColor     : TGDColor;
    FOutlineColor  : TGDColor;
    FFillColor     : TGDColor;

    //Screens
    FScreens : TGDScreenList;
  public
    property DefaultFont : TGDFont read FDefaultFont;
    property MouseCursor : TGDMouseCursor read FMouseCursor;
    property LoadingScreen : TGDLoadingScreen read FLoadingScreen;

    property FontColor : TGDColor read FFontColor;
    property OutlineColor : TGDColor read FOutlineColor;
    property FillColor : TGDColor read FFillColor;

    constructor Create();
    destructor  Destroy(); override;

    function  InitScreen(aFileName : String): TGDScreen;
    procedure ClearScreens();
    function  ScreenGetVisible(aScreen : TGDScreen): Boolean;
    procedure ScreenSetVisible(aScreen : TGDScreen; aVisible : Boolean);
    procedure RenderScreens();
  end;

procedure RenderTexturedQuad(x, y, width, height : Integer;
                             u1 : Single=1; u2 : Single=0;
                             v1 : Single=0; v2 : Single=1);
procedure RenderFlatQuad(x, y, width, height : Integer; inside : boolean = true; outline : boolean = true);

implementation

uses
  uGDEngine;


procedure RenderTexturedQuad(x, y, width, height : Integer; u1 : Single=1; u2 : Single=0; v1 : Single=0; v2 : Single=1);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(u1, v2); glVertex2f( x,       y);
    glTexCoord2f(u2, v2); glVertex2f( x+width, y);
    glTexCoord2f(u2, v1); glVertex2f( x+width, y+height);
    glTexCoord2f(u1, v1); glVertex2f( x,       y+height);
  glEnd;
end;


procedure RenderFlatQuad(x, y, width, height : Integer; inside : boolean = true; outline : boolean = true);

procedure SendQuad(mode: GLenum);
begin
  glBegin(mode);
    glVertex2f( x,       y);
    glVertex2f( x+width, y);
    glVertex2f( x+width, y+height);
    glVertex2f( x,       y+height);
  glEnd;
end;

begin
  GDRenderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  if inside then
  begin
    GDRenderer.SetColor(GDGUI.FillColor);
    SendQuad(GL_QUADS);
  end;
  if outline then
  begin
    GDRenderer.SetColor(GDGUI.OutlineColor);
    SendQuad(GL_LINE_LOOP);
  end;
end;


procedure TGDComponent.Render();
begin
  //Do nothing
end;

constructor TGDPanel.Create(aPanel : TJsonNode);
var
  iStr : String;
begin
  Depth   := Trunc(aPanel.Find('Depth').AsNumber); 
  X       := Trunc(aPanel.Find('X').AsNumber);
  Y       := Trunc(aPanel.Find('Y').AsNumber);
  FWidth  := Trunc(aPanel.Find('Width').AsNumber);
  FHeight := Trunc(aPanel.Find('Height').AsNumber); 
  iStr    := aPanel.Find('Texture').AsString;
  if iStr <> '' then
    FTexture := GDResources.LoadTexture(iStr, TD_HIGH, TF_TRILINEAR)
  else
    FTexture := nil;
end;

destructor  TGDPanel.Destroy();
begin
   inherited;
   if FTexture <> nil then
     GDResources.RemoveResource(TGDResource(FTexture));
end;

procedure   TGDPanel.Render();
begin
  if FTexture = nil then
    RenderFlatQuad(X, Y, FWidth, FHeight)
  else
  begin
    GDRenderer.RenderState(RS_TEXTURE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    FTexture.BindTexture( GL_TEXTURE0 );
    GDRenderer.SetColor(1,1,1,1);
    RenderTexturedQuad(X, Y, FWidth, FHeight);
    glDisable(GL_BLEND);
  end;
end;

constructor TGDLabel.Create(aLabel : TJsonNode);
begin
  Depth  := Trunc(aLabel.Find('Depth').AsNumber); 
  FScale := aLabel.Find('Scale').AsNumber;
  FText  := aLabel.Find('Text').AsString; 
  X      := Trunc(aLabel.Find('X').AsNumber);
  Y      := Trunc(aLabel.Find('Y').AsNumber); 
  FFont  := GDResources.LoadFont(aLabel.Find('Font').AsString);
  FColor.Reset(aLabel.Find('Color'));
end;

destructor  TGDLabel.Destroy();
begin
  GDResources.RemoveResource(TGDResource(FFont));
   inherited;
end;

procedure   TGDLabel.Render();
begin
  GDRenderer.RenderState( RS_TEXTS );
  FFont.Color := FColor.Copy();
  FFont.Render(X,Y,FScale,FText);
end;

constructor TGDScreen.Create(aFileName : String);
var
  iScreen, iElement : TJsonNode;
  iI       : Integer;
begin
  FComponents := TGDComponentList.create();
  FVisible := false;
  iScreen := TJsonNode.Create();
  iScreen.LoadFromFile(PATH_GUI_SCREENS + aFileName);

  //Panels
  iElement := iScreen.Find('Panels');
  for iI := 0 to iElement.Count-1 do
    FComponents.Add( TGDPanel.Create(iElement.Child(iI)) );

  //Labels
  iElement := iScreen.Find('Labels');
  for iI := 0 to iElement.Count-1 do
    FComponents.Add( TGDLabel.Create(iElement.Child(iI)) );

  FreeAndNil(iScreen);
end;

destructor  TGDScreen.Destroy();
begin
  inherited;
  FreeAndNil(FComponents);
end;

procedure TGDScreen.Render();
var
  iI : Integer;
begin
  for iI := 0 to FComponents.Count - 1 do
    FComponents.Items[iI].Render();
end;


class operator TGDCharacterRect.=(aC1, aC2: TGDCharacterRect)B: Boolean;
begin
  B := false
end;


constructor TGDFont.Create(aFileName : string);
var
  iFont, iCharacters, iChar : TJsonNode;
  iI : Integer;
  iC : TGDCharacterRect;
begin
  FCharacters := TGDCharacterRectList.create();
  iFont := TJsonNode.Create();
  iFont.LoadFromFile(aFileName);

  FTexture    := GDResources.LoadTexture(iFont.Find('Texture').AsString, TD_HIGH, TF_TRILINEAR);
  FSpaceWidth := Trunc(iFont.Find('SpaceWidth').AsNumber);
  FColor.White();

  //Characters
  iCharacters := iFont.Find('Characters');
  iI := iCharacters.Count;
  for iI := 0 to iCharacters.Count-1 do
  begin
    iChar := iCharacters.Child(iI);

    iC.X1 := iChar.Find('X1').AsNumber;
    iC.Y1 := iChar.Find('Y1').AsNumber;
    iC.X2 := iChar.Find('X2').AsNumber;
    iC.Y2 := iChar.Find('Y2').AsNumber;
    iC.W  := iChar.Find('Width').AsNumber;
    iC.H  := iChar.Find('Height').AsNumber;

    FCharacters.Add(iC);
  end;
  FreeAndNil(iFont);
end;


destructor TGDFont.Destroy();
begin
  FreeAndNil(FCharacters);
  GDResources.RemoveResource(TGDResource(FTexture));
  inherited;
end;


function TGDFont.TextWidth(const aString : String; const aScale : Single = 1): Integer;
var
  i, x, c, inwidth : integer;
begin
  x := 0;
  for i := 1 to length(aString) do
  begin
    if(aString[i] = ' ') then 
    begin 
      x := x + round(FSpaceWidth * aScale); 
      continue; 
    end;
    c := Ord(aString[i])-33;
    if(c < 0) or (c >= 95) then continue;
    inwidth  := Round(FCharacters[c].W * aScale);
    x := x + inwidth + 1;
  end;
  result := x;
end;


Procedure TGDFont.Render( aLeft, aTop, aScale : Double; aString : string);
var
  i, x, y, c, inwidth, inheight : integer;
begin
  FTexture.BindTexture(GL_TEXTURE0);
  GDRenderer.SetColor(FColor);
  x := Round(aLeft);
  y := Round(aTop);
  for i := 1 to length(aString) do
  begin
    if(aString[i] = ' ') then 
    begin 
      x := x + round(FSpaceWidth * aScale); 
      continue; 
    end;
    c := Ord(aString[i])-33;
    if(c < 0) or (c >= 95) then continue;
    inwidth  := Round(FCharacters[c].W * aScale);
    inheight := Round(FCharacters[c].H * aScale);
    RenderTexturedQuad(x,y,inwidth,inheight,FCharacters[c].X1, FCharacters[c].X2, FCharacters[c].Y1, FCharacters[c].Y2);
    x := x + inwidth + 1;
  end;
end;


constructor TGDMouseCursor.Create(aFileName: String; aCursorSize : Integer );
begin
  FVisible := false;
  FCursorSize := aCursorSize;
  FCursorTexture := GDResources.LoadTexture(aFileName, TD_HIGH, TF_TRILINEAR);
end;


destructor  TGDMouseCursor.Destroy();
begin
  inherited;
  GDResources.RemoveResource(TGDResource(FCursorTexture));
  FVisible := false;
end;


procedure TGDMouseCursor.Render();
var
 iCurPos     : TPoint;

procedure CalculateScreenPosition( aX, aY : Integer);
var
  iViewPort   : TGLVectori4;
  iModelView  : TGLMatrixd4;
  iProjection : TGLMatrixd4;
  iOglPoint   : TGLVectord3;
  iWinZ       : Single;
begin
  glGetDoublev( GL_MODELVIEW_MATRIX, @iModelView );
  glGetDoublev( GL_PROJECTION_MATRIX, @iProjection );
  glGetIntegerv( GL_VIEWPORT, @iViewPort );
  if( aY = 0 )then aY := 1;
  glReadPixels(	aX, -aY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @iWinZ );
  gluUnProject(	aX, iViewPort[3]-aY, iWinZ,
		iModelView, iProjection, iViewPort,
		@iOglPoint[0], @iOglPoint[1], @iOglPoint[2]);
  FPosition.X := round(iOglPoint[0]);
  FPosition.Y := round(iOglPoint[1]);
end;

begin
  GDRenderer.RenderState( RS_TEXTS );
  SDL_GetMouseState(@iCurPos.x, @iCurPos.y);
  CalculateScreenPosition(iCurPos.X, iCurPos.Y);
  glDisable(GL_BLEND);

  If FVisible or GDConsole.Show then
  begin
    GDRenderer.RenderState(RS_TEXTURE);
    FCursorTexture.BindTexture( GL_TEXTURE0 );
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    GDRenderer.SetColor(1,1,1,1);
    RenderTexturedQuad(FPosition.x,FPosition.y-FCursorSize,FCursorSize,FCursorSize);
    glDisable(GL_BLEND);
  end;
end;


constructor TGDLoadingScreen.Create(aSettings : TJsonNode);
begin
  FUse := true;
  FMax  := 100;
  FPosition := 0;
  FX := Trunc(aSettings.Find('Loading/X').AsNumber);
  FY := Trunc(aSettings.Find('Loading/Y').AsNumber);
  FBarOnly := false;
  FBarColor.Reset(aSettings.Find('Loading/Bar'));
end;


destructor TGDLoadingScreen.Destroy();
begin
  inherited;
end;


procedure TGDLoadingScreen.Update();
begin
  If FUse = false then exit;
  FPosition := FPosition + 1;
  Render();
end;


procedure TGDLoadingScreen.Start( aProcesName : String; aMax : integer );
begin
  If FUse = false then exit;
  FMax := aMax;
  FProcesName := aProcesName;
  FPosition := 0;
  Render();
end;


Procedure TGDLoadingScreen.Render();
var
  iProgress : Double;
  iPercent : Double;
begin
  glClearColor(0.0, 0.0, 0.0, 1.0);
  GDWindow.MakeCurrent();
  GDRenderer.ClearFrame();
  GDRenderer.SwitchToOrtho(R_HUD_WIDTH, R_HUD_HEIGHT);
  glClear(GL_COLOR_BUFFER_BIT);

  GDRenderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glPushMatrix();
  glTranslatef(FX, FY, 0);

  If Not(FBarOnly) then
    RenderFlatQuad(0, 0, 600, 100);

  iProgress := 580/FMax;
  iProgress := iProgress * FPosition;
  GDRenderer.SetColor(FBarColor);
  glBegin(GL_QUADS);
    glVertex2f(10 + iProgress ,10 );
    glVertex2f(10 + iProgress ,60 );
    glVertex2f(10 ,60 );
    glVertex2f(10 ,10 );
  glEnd;
  RenderFlatQuad(10, 10, 580, 50, false);

  glPopMatrix();
  iPercent := (FPosition * 100) / FMax;

  If Not(FBarOnly) then
  begin
    GDRenderer.RenderState( RS_TEXTS );
    GDGUI.DefaultFont.Color := GDGUI.FontColor.Copy();
    GDGUI.DefaultFont.Render(7+FX,65+FY,0.5,FProcesName);
    GDGUI.DefaultFont.Render(270+FX,16+FY,0.5,IntToStr(round(iPercent)) + '%');
  end;
  glDisable(GL_BLEND);

  GDRenderer.SwitchToPerspective();
  GDWindow.Swap();
end;


constructor TGDGUI.Create();
var
  iSettings : TJsonNode;
begin
  GDConsole.Use:=false;
  iSettings := TJsonNode.Create();
  iSettings.LoadFromFile(GUI_JSON);

  FFontColor.Reset(iSettings.Find('DefaultColors/Font'));
  FOutlineColor.Reset(iSettings.Find('DefaultColors/Outline'));
  FFillColor.Reset(iSettings.Find('DefaultColors/Fill'));
  FDefaultFont   := GDResources.LoadFont(iSettings.Find('DefaultFont').AsString);
  FMouseCursor   := TGDMouseCursor.Create(iSettings.Find('Mouse/Texture').AsString, Trunc(iSettings.Find('Mouse/Size').AsNumber));
  FLoadingScreen := TGDLoadingScreen.Create(iSettings);
  FScreens       := TGDScreenList.Create();

  FreeAndNil(iSettings);
  GDConsole.Use:=true;
end;


destructor  TGDGUI.Destroy();
begin
  GDResources.RemoveResource(TGDResource(FDefaultFont));
  FreeAndNil(FMouseCursor);
  FreeAndNil(FLoadingScreen);
  FreeAndNil(FScreens);
end;


function TGDGUI.InitScreen(aFileName : String): TGDScreen;
begin
  result := TGDScreen.Create(aFileName);
  FScreens.Add(result);
end;


procedure TGDGUI.ClearScreens();
begin
  FScreens.Clear();
end;


function TGDGUI.ScreenGetVisible(aScreen : TGDScreen): Boolean;
begin
  result := aScreen.Visible;
end;


procedure TGDGUI.ScreenSetVisible(aScreen : TGDScreen; aVisible : Boolean);
begin
  aScreen.Visible := aVisible;
end;


procedure TGDGUI.RenderScreens();
var
  iI : Integer;
  iScreen : TGDScreen;
begin
  for iI := 0 to FScreens.Count - 1 do
  begin
    iScreen := FScreens.Items[iI];
    if iScreen.Visible then
      iScreen.Render();
  end;
end;

end.
