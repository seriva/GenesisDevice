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
unit GDFog;

{$MODE Delphi}

{******************************************************************************}
{* Holds the classes dealing with distance fog. Normal opengl fog and         *}
{* shader based opengl fog                                                    *}
{******************************************************************************}

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
{* Shader based fog class                                                     *}
{******************************************************************************}

  TGDFogShader = class
  private
    FColor : TGDColor;
    FMinDistance : double;
    FMaxDistance : double;

    procedure SetColor(aColor : TGDColor);
    procedure SetMinDistance( aMinDistance : Double );
    procedure SetMaxDistance( aMaxDistance : Double );
  public
    Property Color : TGDColor read FColor write SetColor;
    Property MinDistance : Double read FMinDistance write SetMinDistance;
    Property MaxDistance : Double read FMaxDistance write SetMaxDistance;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitFog( aR, aG, aB, aA : Double; aMinDistance, aMaxDistance : double );
    procedure Clear();

    procedure ApplyFog();
  end;

{******************************************************************************}
{* Fog manager for distance and water fog.                                    *}
{******************************************************************************}

  TGDFogManager = class
  private
    FFogShader : TGDFogShader;

    FDistanceFog   : Integer;
    FDistanceColor : TGDColor;

    FWaterFog      : Integer;
    FWaterColor    : TGDColor;
  public
    Property FogShader : TGDFogShader read FFogShader;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitDistanceFog(aR, aG, aB, aA : Double; aDistance : Integer);
    procedure ClearDistanceFog();
    procedure UseDistanceFog();

    procedure InitWaterFog(aR, aG, aB, aA : Double; aDistance : Integer);
    procedure ClearWaterFog();
    procedure UseWaterFog();
  end;

var
  FogManager : TGDFogManager;

implementation

{******************************************************************************}
{* Set the fog color                                                          *}
{******************************************************************************}

procedure TGDFogShader.SetColor(aColor : TGDColor);
begin
  FColor.R := aColor.R;
  FColor.B := aColor.B;
  FColor.G := aColor.G;
  FColor.A := aColor.A;
  ApplyFog();
end;

{******************************************************************************}
{* Set the near distance for the fog                                          *}
{******************************************************************************}

procedure TGDFogShader.SetMinDistance( aMinDistance : Double );
begin
  FMinDistance := aMinDistance;
  ApplyFog();
end;

{******************************************************************************}
{* Set the far distance for the fog                                           *}
{******************************************************************************}

procedure TGDFogShader.SetMaxDistance( aMaxDistance : Double );
begin
  FMaxDistance := aMaxDistance;
  ApplyFog();
end;

{******************************************************************************}
{* Create the fogshader class                                                 *}
{******************************************************************************}

constructor TGDFogShader.Create();
begin
  FColor := TGDColor.Create();
  FColor.Reset(0.5,0.5,0.5,1);
  FMinDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *5);
  FMaxDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *8);
end;

{******************************************************************************}
{* Destroy the fogshader class                                                *}
{******************************************************************************}

destructor  TGDFogShader.Destroy();
begin
  FreeAndNil(FColor);
  inherited;
end;

{******************************************************************************}
{* Init the fogshader                                                         *}
{******************************************************************************}

procedure TGDFogShader.InitFog(aR, aG, aB, aA : Double; aMinDistance, aMaxDistance : double  );
begin
  Clear();
  FColor.Reset(aR, aG, aB, aA );
  FMinDistance := aMinDistance;
  FMaxDistance := aMaxDistance;
  ApplyFog();
end;

{******************************************************************************}
{* Clear the fogshader                                                        *}
{******************************************************************************}

procedure TGDFogShader.Clear();
begin
  FColor.Reset(0.5,0.5,0.5,1);
  FMinDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *5);
  FMaxDistance := (((Settings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *7);
end;

{******************************************************************************}
{* Apply the fogshader                                                        *}
{******************************************************************************}

procedure TGDFogShader.ApplyFog();
begin
  glClearColor( FColor.R, FColor.G, FColor.B, FColor.A);
end;

{******************************************************************************}
{* Create fog manager                                                         *}
{******************************************************************************}

constructor TGDFogManager.Create();
begin
  FFogShader := TGDFogShader.Create();

  FDistanceFog   := 5;
  FDistanceColor := TGDColor.Create();

  FWaterFog      := 1;
  FWaterColor    := TGDColor.Create();
end;

{******************************************************************************}
{* Destroy fog manager                                                        *}
{******************************************************************************}

destructor TGDFogManager.Destroy();
begin
  FreeAndNil(FFogShader);
  FreeAndNil(FDistanceColor);
  FreeAndNil(FWaterColor);
end;

{******************************************************************************}
{* Init the distance fog                                                      *}
{******************************************************************************}

procedure TGDFogManager.InitDistanceFog(aR, aG, aB, aA : Double; aDistance : Integer);
begin
  FDistanceColor.Reset( aR, aG, aB, aA );
  FDistanceFog := aDistance;
end;

{******************************************************************************}
{* Clear the distance fog                                                     *}
{******************************************************************************}

procedure TGDFogManager.ClearDistanceFog();
begin
  FDistanceColor.Reset( 0.5, 0.5, 0.5, 0.5 );
  FDistanceFog := 5;
end;

{******************************************************************************}
{* Use the distance fog                                                       *}
{******************************************************************************}

procedure TGDFogManager.UseDistanceFog();
begin
  FFogShader.InitFog( FDistanceColor.R,FDistanceColor.G, FDistanceColor.B, FDistanceColor.A,
                      (((FDistanceFog * R_VIEW_DISTANCE_STEP) / 10) *5),
                      (((FDistanceFog * R_VIEW_DISTANCE_STEP) / 10) *7.5) );
end;

{******************************************************************************}
{* Init the water fog                                                         *}
{******************************************************************************}

procedure TGDFogManager.InitWaterFog(aR, aG, aB, aA : Double; aDistance : Integer);
begin
  FWaterColor.Reset( aR, aG, aB, aA );
  FWaterFog := aDistance;
end;

{******************************************************************************}
{* Clear the water fog                                                        *}
{******************************************************************************}

procedure TGDFogManager.ClearWaterFog();
begin
  FWaterColor.Reset( 0.5, 0.5, 0.5, 0.5 );
  FWaterFog := 1;
end;

{******************************************************************************}
{* Use the water fog                                                          *}
{******************************************************************************}

procedure TGDFogManager.UseWaterFog();
begin
  FFogShader.InitFog( FWaterColor.R,FWaterColor.G, FWaterColor.B, FWaterColor.A,
                      (((FWaterFog * R_WATER_DISTANCE_STEP) / 20) *5),
                      (((FWaterFog * R_WATER_DISTANCE_STEP) / 20) *20) );
end;

end.
