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
unit GDFog;

{$MODE Delphi}

interface

uses
  Classes,
  LCLIntf,
  LCLType,
  SysUtils,
  dglOpenGL,
  GDTypes,
  GDConstants,
  GDSettings;

Type
{******************************************************************************}
{* Fog for distance and water.                                                *}
{******************************************************************************}

  TGDFog = class
  private
    FColor : TGDColor;
    FMinDistance : double;
    FMaxDistance : double;
    FDistanceFog   : Integer;
    FDistanceColor : TGDColor;
    FWaterFog      : Integer;
    FWaterColor    : TGDColor;
  public
    Property Color       : TGDColor read FColor;
    Property MinDistance : Double read FMinDistance;
    Property MaxDistance : Double read FMaxDistance ;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitDistanceFog(aR, aG, aB, aA : Double; aDistance : Integer);
    procedure InitWaterFog(aR, aG, aB, aA : Double; aDistance : Integer);
    procedure Clear();

    procedure UseDistanceFog();
    procedure UseWaterFog();
  end;

implementation

{******************************************************************************}
{* Create fog                                                                 *}
{******************************************************************************}

constructor TGDFog.Create();
begin
  Clear();
end;

{******************************************************************************}
{* Destroy fog                                                                *}
{******************************************************************************}

destructor TGDFog.Destroy();
begin
  inherited
end;

{******************************************************************************}
{* Init the distance fog                                                      *}
{******************************************************************************}

procedure TGDFog.InitDistanceFog(aR, aG, aB, aA : Double; aDistance : Integer);
begin
  FDistanceColor.Reset( aR, aG, aB, aA );
  FDistanceFog := aDistance;
end;

{******************************************************************************}
{* Init the water fog                                                         *}
{******************************************************************************}

procedure TGDFog.InitWaterFog(aR, aG, aB, aA : Double; aDistance : Integer);
begin
  FWaterColor.Reset( aR, aG, aB, aA );
  FWaterFog := aDistance;
end;

{******************************************************************************}
{* Clear fog                                                                  *}
{******************************************************************************}

procedure TGDFog.Clear();
begin
  FColor.Reset(0.5,0.5,0.5,1);
  FMinDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *5);
  FMaxDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *8);
  FWaterColor.Reset( 0.5, 0.5, 0.5, 0.5 );
  FWaterFog := 1;
  FDistanceColor.Reset( 0.5, 0.5, 0.5, 0.5 );
  FDistanceFog := 5;
end;

{******************************************************************************}
{* Use the distance fog                                                       *}
{******************************************************************************}

procedure TGDFog.UseDistanceFog();
begin
  FColor       := FDistanceColor.Copy();
  FMinDistance := (((FDistanceFog * R_VIEW_DISTANCE_STEP) / 10) * 5);
  FMaxDistance := (((FDistanceFog * R_VIEW_DISTANCE_STEP) / 10) * 7.5);
  glClearColor( FColor.R, FColor.G, FColor.B, FColor.A);
end;

{******************************************************************************}
{* Use the water fog                                                          *}
{******************************************************************************}

procedure TGDFog.UseWaterFog();
begin
  FColor       := FWaterColor.Copy();
  FMinDistance := (((FWaterFog * R_WATER_DISTANCE_STEP) / 20) * 5);
  FMaxDistance := (((FWaterFog * R_WATER_DISTANCE_STEP) / 20) * 20);
  glClearColor( FColor.R, FColor.G, FColor.B, FColor.A);
end;

end.
