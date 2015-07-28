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
unit GDFoliage;

{$MODE objfpc}

interface

uses
  FGL,
  SysUtils,
  dglOpenGL,
  IniFiles,
  Graphics,
  GDStringParsing,
  GDTexture,
  GDTypes,
  GDConsole,
  GDSettings,
  GDConstants,
  GDMesh,
  GDTiming,
  GDResources,
  GDResource,
  GDModes;

type

{******************************************************************************}
{* Grasstype class                                                            *}
{******************************************************************************}

  TGDGrassType = class
  private
    FTexture : TGDTexture;
    FScale   : TGDVector;
    FRandomScale : TGDVector;
    FCoverOfTotal : Single;
  public
    property Texture : TGDTexture read FTexture;
    property Scale : TGDVector read FScale;
    property RandomScale : TGDVector read FRandomScale;
    property CoverOfTotal : Single read FCoverOfTotal;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
  end;
  TGDGrassTypeList = specialize TFPGObjectList<TGDGrassType>;

{******************************************************************************}
{* Treetype class                                                             *}
{******************************************************************************}

  TGDMeshType = class
  private
    FMesh          : TGDMesh;
    FMeshLOD1      : TGDMesh;
    FMeshLOD2      : TGDMesh;
    FStartRotation : TGDVector;
    FScale         : Single;
    FRandomScale   : Single;
    FCoverOfTotal  : Single;
  public
    property Mesh : TGDMesh read FMesh;
    property MeshLOD1 : TGDMesh read FMeshLOD1;
    property MeshLOD2 : TGDMesh read FMeshLOD2;
    property StartRotation : TGDVector read FStartRotation;
    property Scale : Single read FScale;
    property RandomScale : Single read FRandomScale;
    property CoverOfTotal : Single read FCoverOfTotal;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
  end;
  TGDMeshTypeList = specialize TFPGObjectList<TGDMeshType>;

{******************************************************************************}
{* Foliage class                                                              *}
{******************************************************************************}

  TGDFoliage = class
  private
    FGrassAnimationSpeed    : Single;
    FGrassAnimationStrength : Single;
    FTreeAnimationSpeed     : Single;
    FTreeAnimationStrength  : Single;

    FGrassTypes           : TGDGrassTypeList;

    FTreeTypes            : TGDMeshTypeList;
    FTreeCount            : Integer;
    FTreeLowerLimit       : Integer;
    FTreeUpperLimit       : Integer;

    FRockTypes            : TGDMeshTypeList;
    FRockCount            : Integer;
  public
    GrassMap : array of array of boolean;
    TreeMap : array of array of boolean;
    RockMap : array of array of boolean;

    property GrassAnimationSpeed    : Single read FGrassAnimationSpeed;
    property GrassAnimationStrength : Single read FGrassAnimationStrength;
    property TreeAnimationSpeed     : Single read FTreeAnimationSpeed;
    property TreeAnimationStrength  : Single read FTreeAnimationStrength;

    property GrassTypes : TGDGrassTypeList read FGrassTypes;

    property TreeTypes : TGDMeshTypeList read FTreeTypes;
    property TreeCount : Integer read FTreeCount;
    property TreeLowerLimit : Integer read FTreeLowerLimit;
    property TreeUpperLimit : Integer read FTreeUpperLimit;

    property RockTypes : TGDMeshTypeList read FRockTypes;
    property RockCount : Integer read FRockCount;

    constructor Create();
    destructor  Destroy(); override;

    Function  InitFoliage( aIniFile : TIniFile ) : boolean;
    procedure Clear();

    procedure StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
    procedure EndRenderingGrass();

    Function CheckTreeMap( aX, aY : Integer ) : Boolean;
    Function CheckGrassMap( aX, aY : Integer ) : Boolean;
    Function CheckRockMap( aX, aY : Integer ) : Boolean;
  end;

implementation

uses
  GDRenderer;

{******************************************************************************}
{* Create the grasstype class                                                 *}
{******************************************************************************}

constructor TGDGrassType.Create(aIniFile : TIniFile; aSection : String);
begin
  FTexture      := Resources.LoadTexture(aIniFile.ReadString( aSection, 'Texture', ''),
                                         TD_HIGH,Settings.TextureFilter);
  FScale        := ReadVector(aIniFile, aSection, 'Scale');
  FRandomScale  := ReadVector(aIniFile, aSection, 'RandomScale');
  FCoverOfTotal := aIniFile.ReadFloat( aSection, 'CoverOfTotal', 100 );
end;

{******************************************************************************}
{* Destroy the grasstype                                                      *}
{******************************************************************************}

destructor  TGDGrassType.Destroy();
begin
  Resources.RemoveResource(TGDResource(FTexture));
  inherited
end;

{******************************************************************************}
{* Create the treetype class                                                  *}
{******************************************************************************}

constructor TGDMeshType.Create(aIniFile : TIniFile; aSection : String);
begin
  FMesh          := Resources.Loadmesh(aIniFile.ReadString( aSection, 'Model', ''));
  FMeshLOD1      := Resources.Loadmesh(aIniFile.ReadString( aSection, 'ModelLOD1', ''));
  FMeshLOD2      := Resources.Loadmesh(aIniFile.ReadString( aSection, 'ModelLOD2', ''));
  FStartRotation := ReadVector(aIniFile, aSection, 'StartRotation');
  FScale         := aIniFile.ReadFloat( aSection, 'Scale', 0 );
  FRandomScale   := aIniFile.ReadFloat( aSection, 'RandomScale', 0 );
  FCoverOfTotal  := aIniFile.ReadFloat( aSection, 'CoverOfTotal', 100 );
end;

{******************************************************************************}
{* Destroy the treetype                                                       *}
{******************************************************************************}

destructor  TGDMeshType.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMesh));
  Resources.RemoveResource(TGDResource(FMeshLOD1));
  Resources.RemoveResource(TGDResource(FMeshLOD2));
  inherited
end;

{******************************************************************************}
{* Create the foliage class                                                   *}
{******************************************************************************}

constructor TGDFoliage.Create();
begin
  FGrassTypes := TGDGrassTypeList.Create();
  FTreeTypes  := TGDMeshTypeList.Create();
  FRockTypes  := TGDMeshTypeList.Create();
end;

{******************************************************************************}
{* Destroy the foliage class                                                  *}
{******************************************************************************}

destructor  TGDFoliage.Destroy();
begin
  inherited;
  Clear();
  FreeAndNil(FTreeTypes);
  FreeAndNil(FGrassTypes);
  FreeAndNil(FRockTypes);
end;

{******************************************************************************}
{* Init the foliage                                                           *}
{******************************************************************************}

Function TGDFoliage.InitFoliage( aIniFile : TIniFile ) : boolean;
var
  iTreeMap, iGrassMap, iRockMap : TBitmap;
  iX, iY : integer;
  iError : String;
begin
  Clear();
  Console.Write('Loading foliage...');
  try
    result := true;

    FGrassAnimationSpeed    := aIniFile.ReadFloat( 'Foliage', 'GrassAnimationSpeed', 1000 );
    FGrassAnimationStrength := aIniFile.ReadFloat( 'Foliage', 'GrassAnimationStrength', 5);

    FTreeCount              := aIniFile.ReadInteger( 'Foliage', 'TreeCount', 0 );
    FTreeLowerLimit         := aIniFile.ReadInteger( 'Foliage', 'TreeLowerLimit', 0 );
    FTreeUpperLimit         := aIniFile.ReadInteger( 'Foliage', 'TreeUpperLimit', 0 );
    FTreeAnimationSpeed     := aIniFile.ReadFloat( 'Foliage', 'TreeAnimationSpeed', 1000 );
    FTreeAnimationStrength  := aIniFile.ReadFloat( 'Foliage', 'TreeAnimationStrength', 5 );

    FRockCount              := aIniFile.ReadInteger( 'Foliage', 'RockCount', 0 );

    iTreeMap := TBitmap.Create();
    iTreeMap.pixelformat := pf24bit;
    iTreeMap.LoadFromFile( aIniFile.ReadString( 'Foliage', 'TreeMap', 'treemap.bmp' ));
    iGrassMap := TBitmap.Create();
    iGrassMap.pixelformat := pf24bit;
    iGrassMap.LoadFromFile( aIniFile.ReadString( 'Foliage', 'GrassMap', 'grassmap.bmp' ));
    iRockMap := TBitmap.Create();
    iRockMap.pixelformat := pf24bit;
    iRockMap.LoadFromFile( aIniFile.ReadString( 'Foliage', 'RockMap', 'rockmap.bmp' ));

    if ((iTreeMap.Width mod 2) <> 0) or ((iTreeMap.Height mod 2) <> 0) then
      Raise Exception.Create('Dimensions are incorrect!');

    if (iTreeMap.Width  <> iGrassMap.Width) or
       (iTreeMap.Height <> iGrassMap.Height) or
       (iTreeMap.Width  <> iRockMap.Width) or
       (iTreeMap.Height <> iRockMap.Height) then
      Raise Exception.Create('Dimensions are incorrect!');

    SetLength(TreeMap, iTreeMap.Width);
    SetLength(GrassMap, iTreeMap.Width);
    SetLength(RockMap, iTreeMap.Width);
    for iX := 0 to (iTreeMap.Width-1) do
    begin
      SetLength(TreeMap[iX], iTreeMap.Height);
      SetLength(GrassMap[iX], iTreeMap.Height);
      SetLength(RockMap[iX], iTreeMap.Height);
      for iY := 0 to (iTreeMap.Height-1) do
      begin
          TreeMap[iX,iY]  := iTreeMap.Canvas.Pixels[iX,iY] = clWhite;
          GrassMap[iX,iY] := iGrassMap.Canvas.Pixels[iX,iY] = clWhite;
          RockMap[iX,iY] := iRockMap.Canvas.Pixels[iX,iY] = clWhite;
      end;
    end;
    FreeAndNil(iTreeMap);
    FreeAndNil(iGrassMap);
    FreeAndNil(iRockMap);
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Clear the foliage                                                          *}
{******************************************************************************}

procedure TGDFoliage.Clear();
var
  iX : Integer;
begin
  if (GrassMap <> nil) then
  begin
    for iX := 0 to Length(GrassMap)-1 do
    begin
      SetLength(GrassMap[iX], 0);
      SetLength(TreeMap[iX], 0);
      SetLength(RockMap[iX], 0);
    end;
    SetLength(GrassMap, 0);
    SetLength(TreeMap, 0);
    SetLength(RockMap, 0);
  end;

  FGrassTypes.Clear();
  FTreeTypes.Clear();
  FRockTypes.Clear();
end;

{******************************************************************************}
{* Start the rendering of a grasscell                                         *}
{******************************************************************************}

procedure TGDFoliage.StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Modes.RenderWireframe then
  begin
    Renderer.SetColor(0,0.25,0,1);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.7);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glDisable(GL_CULL_FACE);
    Renderer.GrassShader.Bind();
    Renderer.SetJoinedParams(Renderer.GrassShader);
    Renderer.GrassShader.SetInt('T_GRASSTEX', 0);
    Renderer.GrassShader.SetFloat('F_ANIMATION_SPEED', Timing.ElapsedTime / FGrassAnimationSpeed);
    Renderer.GrassShader.SetFloat('F_ANIMATION_STRENGTH', FGrassAnimationStrength);
  end;
end;

{******************************************************************************}
{* End the rendering of a grasscell                                           *}
{******************************************************************************}

procedure TGDFoliage.EndRenderingGrass();
begin
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_BLEND);
  glEnable(GL_CULL_FACE);
end;

{******************************************************************************}
{* Check on the grass map if grass may grow there                           *}
{******************************************************************************}

Function TGDFoliage.CheckGrassMap( aX, aY : Integer ) : Boolean;
begin
  result := GrassMap[aX, aY];
end;

{******************************************************************************}
{* Check on the tree map if tree may grow there                               *}
{******************************************************************************}

Function TGDFoliage.CheckTreeMap( aX, aY : Integer ) : Boolean;
begin
  result := TreeMap[aX, aY];
end;

{******************************************************************************}
{* Check on the rock map if rock may be there                                 *}
{******************************************************************************}

Function TGDFoliage.CheckRockMap( aX, aY : Integer ) : Boolean;
begin
  result := RockMap[aX, aY];
end;

end.
