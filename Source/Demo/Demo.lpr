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
  // heaptrc,
  SysUtils,
  Forms, Interfaces,
  uConfiguration in 'uConfiguration.pas' {ConfigurationForm},
  uGDEngine,
  uMain in 'Main.pas',
  uPlayer in 'Player.pas';

label start;

begin
  // DeleteFile('leaks.txt');
  // SetHeapTraceOutput('leaks.txt');
  Application.Initialize;
  Application.Scaled:=true;
  start:
  if SettingsExecute() then
  begin
    GDEngine.Init(@InitGame);
    While not(GDEngine.Done) do
      GDEngine.Loop(@GameLoop);
    GDEngine.Clear(@ClearGame);
    goto start;
  end;
end.
