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
unit GDFrustum;

{$MODE Delphi}

{******************************************************************************}
{* Holds the main frustrum class for checking if boundingvolumes are inside   *}
{* the frustrum.                                                              *}
{******************************************************************************}

interface

uses
 dglOpenGL,
 GDBoundingVolumes,
 GDTypes;

type

{******************************************************************************}
{* Frustum matrix                                                             *}
{******************************************************************************}

  FrustumArray = array[0..5, 0..3] of double;

{******************************************************************************}
{* Frustum class                                                              *}
{******************************************************************************}

  TGDFrustum = class
  private
    FFrustum: FrustumArray;
  public
    procedure CalculateFrustum();
    function  PointInFrustum(aPoint : TGDVector): Boolean;
    function  SphereInFrustum(aSphere : TGDBoundingSphere): Boolean;
    function  BoxInFrustum(aBox : TGDBoundingBox): Boolean;
  end;

var
  Frustum : TGDFrustum;

implementation

const
  RIGHT             = 0;
  LEFT              = 1;
  BOTTOM            = 2;
  TOP               = 3;
  BACK              = 4;
  FRONT             = 5;

{******************************************************************************}
{* Normalize a frustumplane                                                   *}
{******************************************************************************}

procedure NormalizePlane(aFrustum: FrustumArray; aSide: Integer);
var iMagnitude       : glFloat;
begin
  iMagnitude := sqrt(aFrustum[aSide][0] * aFrustum[aSide][0] +
                     aFrustum[aSide][1] * aFrustum[aSide][1] +
                     aFrustum[aSide][2] * aFrustum[aSide][2]);
  aFrustum[aSide][0] := aFrustum[aSide][0] / iMagnitude;
  aFrustum[aSide][1] := aFrustum[aSide][1] / iMagnitude;
  aFrustum[aSide][2] := aFrustum[aSide][2] / iMagnitude;
  aFrustum[aSide][3] := aFrustum[aSide][3] / iMagnitude;
end;

{******************************************************************************}
{* Calculate the frustum                                                      *}
{******************************************************************************}

procedure TGDFrustum.CalculateFrustum();
var iProj, iModl, iClip: array[0..15] of glFloat;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @iProj);
  glGetFloatv(GL_MODELVIEW_MATRIX, @iModl);

  iClip[0] := iModl[0] * iProj[0] + iModl[1] * iProj[4] + iModl[2] * iProj[8] + iModl[3] * iProj[12];
  iClip[1] := iModl[0] * iProj[1] + iModl[1] * iProj[5] + iModl[2] * iProj[9] + iModl[3] * iProj[13];
  iClip[2] := iModl[0] * iProj[2] + iModl[1] * iProj[6] + iModl[2] * iProj[10] + iModl[3] * iProj[14];
  iClip[3] := iModl[0] * iProj[3] + iModl[1] * iProj[7] + iModl[2] * iProj[11] + iModl[3] * iProj[15];

  iClip[4] := iModl[4] * iProj[0] + iModl[5] * iProj[4] + iModl[6] * iProj[8] + iModl[7] * iProj[12];
  iClip[5] := iModl[4] * iProj[1] + iModl[5] * iProj[5] + iModl[6] * iProj[9] + iModl[7] * iProj[13];
  iClip[6] := iModl[4] * iProj[2] + iModl[5] * iProj[6] + iModl[6] * iProj[10] + iModl[7] * iProj[14];
  iClip[7] := iModl[4] * iProj[3] + iModl[5] * iProj[7] + iModl[6] * iProj[11] + iModl[7] * iProj[15];

  iClip[8] := iModl[8] * iProj[0] + iModl[9] * iProj[4] + iModl[10] * iProj[8] + iModl[11] * iProj[12];
  iClip[9] := iModl[8] * iProj[1] + iModl[9] * iProj[5] + iModl[10] * iProj[9] + iModl[11] * iProj[13];
  iClip[10] := iModl[8] * iProj[2] + iModl[9] * iProj[6] + iModl[10] * iProj[10] + iModl[11] * iProj[14];
  iClip[11] := iModl[8] * iProj[3] + iModl[9] * iProj[7] + iModl[10] * iProj[11] + iModl[11] * iProj[15];

  iClip[12] := iModl[12] * iProj[0] + iModl[13] * iProj[4] + iModl[14] * iProj[8] + iModl[15] * iProj[12];
  iClip[13] := iModl[12] * iProj[1] + iModl[13] * iProj[5] + iModl[14] * iProj[9] + iModl[15] * iProj[13];
  iClip[14] := iModl[12] * iProj[2] + iModl[13] * iProj[6] + iModl[14] * iProj[10] + iModl[15] * iProj[14];
  iClip[15] := iModl[12] * iProj[3] + iModl[13] * iProj[7] + iModl[14] * iProj[11] + iModl[15] * iProj[15];

  FFrustum[RIGHT][0] := iClip[3] - iClip[0];
  FFrustum[RIGHT][1] := iClip[7] - iClip[4];
  FFrustum[RIGHT][2] := iClip[11] - iClip[8];
  FFrustum[RIGHT][3] := iClip[15] - iClip[12];

  NormalizePlane(FFrustum, RIGHT);

  FFrustum[LEFT][0] := iClip[3] + iClip[0];
  FFrustum[LEFT][1] := iClip[7] + iClip[4];
  FFrustum[LEFT][2] := iClip[11] + iClip[8];
  FFrustum[LEFT][3] := iClip[15] + iClip[12];

  NormalizePlane(FFrustum, LEFT);

  FFrustum[BOTTOM][0] := iClip[3] + iClip[1];
  FFrustum[BOTTOM][1] := iClip[7] + iClip[5];
  FFrustum[BOTTOM][2] := iClip[11] + iClip[9];
  FFrustum[BOTTOM][3] := iClip[15] + iClip[13];

  NormalizePlane(FFrustum, BOTTOM);

  FFrustum[TOP][0] := iClip[3] - iClip[1];
  FFrustum[TOP][1] := iClip[7] - iClip[5];
  FFrustum[TOP][2] := iClip[11] - iClip[9];
  FFrustum[TOP][3] := iClip[15] - iClip[13];

  NormalizePlane(FFrustum, TOP);

  FFrustum[BACK][0] := iClip[3] - iClip[2];
  FFrustum[BACK][1] := iClip[7] - iClip[6];
  FFrustum[BACK][2] := iClip[11] - iClip[10];
  FFrustum[BACK][3] := iClip[15] - iClip[14];

  NormalizePlane(FFrustum, BACK);

  FFrustum[FRONT][0] := iClip[3] + iClip[2];
  FFrustum[FRONT][1] := iClip[7] + iClip[6];
  FFrustum[FRONT][2] := iClip[11] + iClip[10];
  FFrustum[FRONT][3] := iClip[15] + iClip[14];

  NormalizePlane(FFrustum, FRONT);
end;

{******************************************************************************}
{* Check if a point is inside the frustum                                     *}
{******************************************************************************}

function TGDFrustum.PointInFrustum(aPoint : TGDVector): Boolean;
var
  iI : Integer;
begin
  result := false;
  for iI := 0 to 5 do
    if FFrustum[iI][0] * aPoint.X + FFrustum[iI][1] * aPoint.Y + FFrustum[iI][2] * aPoint.Z + FFrustum[iI][3] <= 0 then
      exit;
  result := true;
end;

{******************************************************************************}
{* Check if a BS is inside the frustum                                        *}
{******************************************************************************}

function TGDFrustum.SphereInFrustum(aSphere : TGDBoundingSphere): Boolean;
var iI               : Integer;
begin
  result := false;
  for iI := 0 to 5 do
    if FFrustum[iI][0] * aSphere.Center.X + FFrustum[iI][1] * aSphere.Center.Y + FFrustum[iI][2] * aSphere.Center.Z + FFrustum[iI][3] <= -aSphere.Radius then
      exit;
  result := true;
end;

{******************************************************************************}
{* Check if an AABB is inside the frustum                                     *}
{******************************************************************************}

function TGDFrustum.BoxInFrustum(aBox : TGDBoundingBox): Boolean;
var iI : Integer;
begin
  result := false;
  with aBox do
  begin
    for iI := 0 to 5 do
    begin
      if FFrustum[iI][0] * Min.X + FFrustum[iI][1] * Min.Y + FFrustum[iI][2] * Min.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Max.X + FFrustum[iI][1] * Min.Y + FFrustum[iI][2] * Min.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Min.X + FFrustum[iI][1] * Max.Y + FFrustum[iI][2] * Min.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Max.X + FFrustum[iI][1] * Max.Y + FFrustum[iI][2] * Min.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Min.X + FFrustum[iI][1] * Min.Y + FFrustum[iI][2] * Max.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Max.X + FFrustum[iI][1] * Min.Y + FFrustum[iI][2] * Max.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Min.X + FFrustum[iI][1] * Max.Y + FFrustum[iI][2] * Max.Z + FFrustum[iI][3] > 0 then continue;
      if FFrustum[iI][0] * Max.X + FFrustum[iI][1] * Max.Y + FFrustum[iI][2] * Max.Z + FFrustum[iI][3] > 0 then continue;
      exit;
    end;
  end;
  result := true;
end;

end.
