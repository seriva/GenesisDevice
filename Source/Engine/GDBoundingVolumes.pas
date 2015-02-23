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
unit GDBoundingVolumes;

{$MODE Delphi}

{******************************************************************************}
{* This unit holds the various bounding volume classes, used for culling and  *}
{* collision                                                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  GDTypes,
  GDObjectList;

type

{******************************************************************************}
{* Axis aligned bounding box class                                            *}
{******************************************************************************}

  TGDBoundingBox = class(TObject)
  private
    FMin : TGDVector;
    FMax : TGDVector;
    FCenter : TGDVector;
    Procedure SetFMin(aV : TGDVector);
    Procedure SetFmax(aV : TGDVector);
    Procedure SetFCenter(aV : TGDVector);
  public
    Property Min : TGDVector read FMin write SetFMin;
    Property Max : TGDVector read FMax write SetFmax;
    Property Center : TGDVector read FCenter write SetFCenter;

    constructor Create();
    destructor  Destroy(); Override;

    procedure RenderWireFrame();
    procedure RenderSolid();

    procedure Generate( aVertexList : TGDObjectList );
    procedure CalculateCenter();
    function  BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
    function  PointInsideBox( aV : TGDVector ) : boolean;
  end;

{******************************************************************************}
{* Bounding sphere class                                                      *}
{******************************************************************************}

  TGDBoundingSphere = class(TObject)
  private
    FCenter : TGDVector;
    FRadius : Double;
    Procedure SetFCenter(aV : TGDVector);
  public
    Property Center : TGDVector read FCenter write SetFCenter;
    Property Radius : Double read FRadius write FRadius;

    constructor Create();
    destructor  Destroy(); Override;

    procedure RenderWireFrame();
    procedure RenderSolid();

    procedure Generate( aVertexList : TGDObjectList );
    function  SphereInsideSphere( aBoundingBox : TGDBoundingSphere ) : boolean;
    function  PointInsideSphere( aV : TGDVector ) : boolean;
  end;

implementation

{******************************************************************************}
{* Set the minimum point for the AABB                                         *}
{******************************************************************************}

Procedure TGDBoundingBox.SetFMin(aV : TGDVector);
begin
  FMin.x := aV.x;
  FMin.y := aV.y;
  FMin.z := aV.z;
end;

{******************************************************************************}
{* Set the maximum point for the AABB                                         *}
{******************************************************************************}

Procedure TGDBoundingBox.SetFmax(aV : TGDVector);
begin
  FMax.x := aV.x;
  FMax.y := aV.y;
  FMax.z := aV.z;
end;

{******************************************************************************}
{* Set the center point for the AABB                                          *}
{******************************************************************************}

Procedure TGDBoundingBox.SetFCenter(aV : TGDVector);
begin
  FCenter.x := aV.x;
  FCenter.y := aV.y;
  FCenter.z := aV.z;
end;

{******************************************************************************}
{* Create the AABB class                                                      *}
{******************************************************************************}

constructor TGDBoundingBox.Create();
begin
  FMin := TGDVector.Create(0,0,0);
  FMax := TGDVector.Create(0,0,0);
  FCenter := TGDVector.Create(0,0,0);
end;

{******************************************************************************}
{* Destroy the AABB class                                                     *}
{******************************************************************************}

destructor  TGDBoundingBox.Destroy();
begin
  FreeAndNil(FMin);
  FreeAndNil(FMax);
  FreeAndNil(FCenter);
  inherited;
end;

{******************************************************************************}
{* Render the AABB wireframe                                                  *}
{******************************************************************************}

procedure TGDBoundingBox.RenderWireFrame();
begin
  glBegin(GL_LINE_LOOP);
    glVertex3f( FMax.x, FMax.y, FMax.Z  );
    glVertex3f( FMin.x, FMax.y, FMax.Z  );
    glVertex3f( FMin.x, FMax.y, FMin.Z  );
    glVertex3f( FMax.x, FMax.y, FMin.Z  );
  glEnd;

  glBegin(GL_LINE_LOOP);
    glVertex3f( FMax.x, FMin.y, FMax.Z  );
    glVertex3f( FMin.x, FMin.y, FMax.Z  );
    glVertex3f( FMin.x, FMin.y, FMin.Z  );
    glVertex3f( FMax.x, FMin.y, FMin.Z  );
  glEnd;

  glBegin(GL_LINES);
    glVertex3f( FMax.x, FMax.y, FMax.Z  );
    glVertex3f( FMax.x, FMin.y, FMax.Z  );

    glVertex3f( FMin.x, FMax.y, FMin.Z  );
    glVertex3f( FMin.x, FMin.y, FMin.Z  );

    glVertex3f( FMin.x, FMax.y, FMax.Z  );
    glVertex3f( FMin.x, FMin.y, FMax.Z  );

    glVertex3f( FMax.x, FMax.y, FMin.Z  );
    glVertex3f( FMax.x, FMin.y, FMin.Z  );
  glEnd;
end;

{******************************************************************************}
{* Render the AABB solid                                                      *}
{******************************************************************************}

procedure TGDBoundingBox.RenderSolid();
begin
  glDisable(GL_CULL_FACE);
  glBegin(GL_QUADS);
    glVertex3f(FMin.x, FMax.y, FMin.z);
    glVertex3f(FMax.x, FMax.y, FMin.z);
    glVertex3f(FMax.x, FMin.y, FMin.z);
    glVertex3f(FMin.x, FMin.y, FMin.z);

    glVertex3f(FMin.x, FMin.y, FMax.z);
    glVertex3f(FMax.x, FMin.y, FMax.z);
    glVertex3f(FMax.x, FMax.y, FMax.z);
    glVertex3f(FMin.x, FMax.y, FMax.z);

    glVertex3f(FMin.x, FMin.y, FMin.z);
    glVertex3f(FMin.x, FMin.y, FMax.z);
    glVertex3f(FMin.x, FMax.y, FMax.z);
    glVertex3f(FMin.x, FMax.y, FMin.z);

    glVertex3f(FMax.x, FMax.y, FMin.z);
    glVertex3f(FMax.x, FMax.y, FMax.z);
    glVertex3f(FMax.x, FMin.y, FMax.z);
    glVertex3f(FMax.x, FMin.y, FMin.z);

    glVertex3f(FMin.x, FMin.y, FMin.z);
    glVertex3f(FMax.x, FMin.y, FMin.z);
    glVertex3f(FMax.x, FMin.y, FMax.z);
    glVertex3f(FMin.x, FMin.y, FMax.z);

    glVertex3f(FMin.x, FMax.y, FMax.z);
    glVertex3f(FMax.x, FMax.y, FMax.z);
    glVertex3f(FMax.x, FMax.y, FMin.z);
    glVertex3f(FMin.x, FMax.y, FMin.z);
  glEnd;

  glEnable(GL_CULL_FACE);
end;

{******************************************************************************}
{* Calculate the boundingbox from a vertexlist                                *}
{******************************************************************************}

procedure TGDBoundingBox.Generate( aVertexList : TGDObjectList );
var
  iI : integer;
  iVector : TGDVector;
  iCenter : TGDVector;
begin
  iCenter := TGDVector.Create();
  for iI := 0 to aVertexList.Count-1 do iCenter.Add( TGDVector(aVertexList.GetObjectI(iI)) );
  iCenter.Devide( aVertexList.Count );
  FMin.Reset(iCenter);
  FMax.Reset(iCenter);

  for iI := 0 to aVertexList.Count-1 do
  begin
    iVector := TGDVector(aVertexList.GetObjectI(iI));

    If (iVector.X <= FMin.x) then
      FMin.x  := iVector.X
    else If (iVector.X >= FMax.x) then
           FMax.x  := iVector.X;

    If (iVector.Y <= FMin.Y) then
      FMin.Y  := iVector.Y
    else If (iVector.Y >= FMax.Y) then
           FMax.Y  := iVector.Y;

    If (iVector.Z <= FMin.Z) then
      FMin.Z  := iVector.Z
    else If (iVector.Z >= FMax.Z) then
           FMax.Z  := iVector.Z;
  end;

  CalculateCenter();
  FreeAndNil(iCenter);
end;

{******************************************************************************}
{* Check if a AABB is inside another AABB                                     *}
{******************************************************************************}

function TGDBoundingBox.BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
begin
  If (FMin.X <= aBoundingBox.FMin.X) and (FMin.Y <= aBoundingBox.FMin.Y) and (FMin.Z <= aBoundingBox.FMin.Z) and
     (FMax.X >= aBoundingBox.FMax.X) and (FMax.Y >= aBoundingBox.FMax.Y) and (FMax.Z >= aBoundingBox.FMax.Z) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Check if a point is inside the AABB                                        *}
{******************************************************************************}

function  TGDBoundingBox.PointInsideBox( aV : TGDVector ) : boolean;
begin
  If (FMin.X <= aV.X) and (FMin.Y <= aV.Y) and (FMin.Z <= aV.Z) and
     (FMax.X >= aV.X) and (FMax.Y >= aV.Y) and (FMax.Z >= aV.Z) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Calculate the center of the AABB using the MIN and the MAX points          *}
{******************************************************************************}

procedure TGDBoundingBox.CalculateCenter();
begin
 FCenter.Reset(Max);
 FCenter.Add(Min);
 FCenter.Devide(2);
end;

{******************************************************************************}
{* Set the center point for the BS                                            *}
{******************************************************************************}

Procedure TGDBoundingSphere.SetFCenter(aV : TGDVector);
begin
  FCenter.x := aV.x;
  FCenter.y := aV.y;
  FCenter.z := aV.z;
end;

{******************************************************************************}
{* Create the BS class                                                        *}
{******************************************************************************}

constructor TGDBoundingSphere.Create();
begin
  FCenter := TGDVector.Create(0,0,0);
  FRadius := 0;
end;

{******************************************************************************}
{* Destroy the BS class                                                       *}
{******************************************************************************}

destructor TGDBoundingSphere.Destroy();
begin
  FreeAndNil(FCenter);
  inherited;
end;

{******************************************************************************}
{* Render the BS wireframe                                                    *}
{******************************************************************************}

procedure TGDBoundingSphere.RenderWireFrame();
begin
  //
end;

{******************************************************************************}
{* Render the BS solid                                                        *}
{******************************************************************************}

procedure TGDBoundingSphere.RenderSolid();
begin
  //
end;

{******************************************************************************}
{* Calculate the sphere from a vertexlist                                     *}
{******************************************************************************}

procedure TGDBoundingSphere.Generate( aVertexList : TGDObjectList );
begin
end;

{******************************************************************************}
{* Check if a sphere is inside another sphere                                 *}
{******************************************************************************}

function TGDBoundingSphere.SphereInsideSphere( aBoundingBox : TGDBoundingSphere ) : boolean;
begin
  result := false;
end;

{******************************************************************************}
{* Check if a point is inside the sphere                                       *}
{******************************************************************************}

function  TGDBoundingSphere.PointInsideSphere( aV : TGDVector ) : boolean;
begin
  result := false;
end;

end.
