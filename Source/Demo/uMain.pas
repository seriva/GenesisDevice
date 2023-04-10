unit uMain;

{$MODE Delphi}

interface

uses
  uGDConstants,
  SysUtils,
  SDL2,
  uPlayer,
  uGDEngine,
  uGDSound,
  uGDGUI;

var
  //main
  Stats      : boolean;
  WireFrame  : boolean;
  TreeNodes  : boolean;
  ObjBoxes   : boolean;
  Clip       : boolean;

  //main classes
  Player     : TPlayer;

  //Sounds
  AmbientBuffer    : TGDSoundResource;
  AmbientSource    : integer;
  UnderWaterBuffer : TGDSoundResource;
  UnderWaterSource : integer;
  MusicBuffer      : TGDSoundResource;
  MusicSource      : integer;

  //Screens
  IntroScreen    : TGDScreen;

procedure InitGame();
procedure ClearGame();
procedure GameLoop();

implementation

procedure ExitCallback();
begin
  GDEngine.Done := true;
end;


procedure PlayerForward();
begin
  if not(IntroScreen.Visible) then
    Player.MoveForward();
end;


procedure PlayerBackward();
begin
  if not(IntroScreen.Visible) then
    Player.MoveBackWard();
end;


procedure PlayerRight();
begin
  if not(IntroScreen.Visible) then
    Player.MoveRight();
end;


procedure PlayerLeft();
begin
  if not(IntroScreen.Visible) then
    Player.MoveLeft();
end;


procedure SetWalk();
begin
  if not(IntroScreen.Visible) then
    Player.Walk();
end;


procedure SetRun();
begin
  if not(IntroScreen.Visible) then
    PLayer.Run();
end;


function BoolToStr(b : boolean): String;
begin
  If b then
    result := '1'
  else
    result := '0';
end;


procedure ToggleStats();
begin
  Stats := not(Stats);
  GDConsole.ExecuteCommand ('RStats ' + BoolToStr(Stats));
end;


procedure ToggleWireFrame();
begin
  WireFrame := not(WireFrame);
  GDConsole.ExecuteCommand ('RTris ' + BoolToStr(WireFrame));
end;


procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  GDConsole.ExecuteCommand ('RNodes ' + BoolToStr(TreeNodes));
end;


procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  GDConsole.ExecuteCommand ('RAABB ' + BoolToStr(ObjBoxes));
end;


procedure ToggleClipping();
begin
  Clip := not(Clip);
end;


procedure ToggleIntroText();
begin
  IntroScreen.Visible := not(IntroScreen.Visible);
  GDInput.MouseLook := not(IntroScreen.Visible);
  GDGUI.MouseCursor.Visible := IntroScreen.Visible;
end;


procedure GameLoop();
begin
  //do soms sound stuff
  if Player.PlayerUnderWater() then
  begin
    GDSound.Resume(UnderWaterSource);
    GDSound.Pause( AmbientSource );
  end
  else
  begin
    GDSound.Pause( UnderWaterSource );
    GDSound.Resume(AmbientSource);
  end;

  //calculate player collision and response
  Player.DoPlayerCollisionAndPhysics();
end;


procedure InitGame();
begin
  //initialize game vars
  Stats      := false;
  WireFrame  := false;
  TreeNodes  := false;
  ObjBoxes   := false;
  Clip       := true;

  //initialize the gamerecources
  GDTiming.Start();
  GDWindow.SetTitle('Demo');
  GDGUI.LoadingScreen.Start('Loading game...', 3);
  GDConsole.Write('.....Initializing game resources');

  //sounds
  AmbientBuffer    := GDSound.Load( 'Sounds/ambient.wav');
  UnderWaterBuffer := GDSound.Load( 'Sounds/underwater.wav');
  MusicBuffer      := GDSound.Load( 'Sounds/music.mp3');
  AmbientSource    := GDSound.Play(AmbientBuffer, true);
  GDSound.Pause(AmbientSource);
  UnderWaterSource := GDSound.Play(UnderWaterBuffer, true);
  GDSound.Pause(UnderWaterSource);
  GDGUI.LoadingScreen.Update();

  //intro
  IntroScreen := GDGUI.InitScreen('Intro.json');
  GDGUI.LoadingScreen.Update();

  //player
  Player  := TPlayer.Create();
  GDGUI.LoadingScreen.Update();
  GDTiming.Stop();
  GDConsole.Write( '.....Done initializing game resources (' +  GDTiming.TimeInSeconds() + ' Sec)' );

  //map
  GDMap.Load( 'Maps/Demo/map.map' );

  //final settings.
  IntroScreen.Visible := true;
  GDInput.EnableInput := true;
  GDInput.MouseLook :=  false;
  GDGUI.MouseCursor.Visible := true;

  //input functions
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_ESCAPE,@ExitCallback, false );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_W,@PlayerForward, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_S,@PlayerBackward, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_A,@PlayerLeft, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_D,@PlayerRight, true );
  GDInput.AddAction(IT_DOWN, SDL_SCANCODE_LSHIFT,@SetRun, true );
  GDInput.AddAction(IT_UP,SDL_SCANCODE_LSHIFT,@SetWalk, true );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F1,@ToggleStats, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F2,@ToggleWireFrame, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F3,@ToggleOctreeNodes, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F4,@ToggleOBJBoxes, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F5,@ToggleClipping, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_P,@ToggleIntroText, true  );

  MusicSource := GDSound.Play( MusicBuffer, true );
end;


procedure ClearGame();
begin
  FreeAndNil(Player);
  GDSound.Stop(AmbientSource);
  GDSound.Remove(AmbientBuffer);
  GDSound.Stop(UnderWaterSource);
  GDSound.Remove(UnderWaterBuffer);
  GDSound.Stop(MusicSource);
  GDSound.Remove(MusicBuffer);
end;

end.
