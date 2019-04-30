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
  //heaptrc, //For debugging
  LCLIntf, LCLType, LMessages,
  Forms, Interfaces,
  {$IFDEF Win32}
  uConfiguration in 'uConfiguration.pas' {ConfigurationForm},
  {$ENDIF}
  {$IFDEF Linux}
  uGDEngine,
  {$ENDIF}
  uMain in 'Main.pas',
  uPlayer in 'Player.pas';

begin
  //On linux where not running the settings interface because of threading issues
  //we still need to investigate.
  {$IFDEF Linux}
  GDSettings.Load();
  GDEngine.Init(@InitGame);
  While not(GDEngine.Done) do
  	GDEngine.Loop(@GameLoop);
  GDEngine.Clear(@ClearGame);
  {$ENDIF}

  //On windows we have the settings interface.
  {$IFDEF Win32}
  Application.Initialize;
  Application.Title := 'Demo';
  Application.CreateForm(TConfigurationForm, ConfigurationForm);
  Application.Run;
  {$ENDIF}
end.
