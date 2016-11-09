{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
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
program Demo;

{$MODE Delphi}

uses
  heaptrc, //For debugging
  LCLIntf, LCLType, LMessages,
  Forms, Interfaces,
  {$IFDEF Win32}
  Configuration in 'Configuration.pas' {ConfigurationForm},
  {$ENDIF}
  {$IFDEF Linux}
  GDEngine,
  {$ENDIF}
  Main in 'Main.pas',
  Player in 'Player.pas';

begin
  //On linux where not running the settings interface because of threading issues
  //we still need to investigate.
  {$IFDEF Linux}
  Engine.Settings.Load();
  Engine.Init(@InitGame);
  While not(Engine.Done) do
  	Engine.Loop(@GameLoop);
  Engine.Clear(@ClearGame);
  {$ENDIF}

  //On windows we have the settings interface.
  {$IFDEF Win32}
  Application.Initialize;
  Application.Title := 'Demo';
  Application.CreateForm(TConfigurationForm, ConfigurationForm);
  Application.Run;
  {$ENDIF}
end.
