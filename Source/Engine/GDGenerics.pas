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
unit GDGenerics;

{$mode objfpc}

{******************************************************************************}
{* Holds the generic types of the engine. We put them in a seperate file      *}
{* since we need to use objfpc compiler mode to make them work properly.      *}
{******************************************************************************}

interface

uses
  GDTypes,
  FGL;

type
  TGDUVCoordList = specialize TFPGList<TGDUVCoord>;
  TVectorList = specialize TFPGList<TGDVector>;
  TGDVectorList = class (TVectorList)
  public
    function GenerateBoundingBox(): TGDBoundingBox;
  end;

implementation

function TGDVectorList.GenerateBoundingBox(): TGDBoundingBox;
var
  iI : integer;
  iVector : TGDVector;
  iCenter : TGDVector;
begin
  for iI := 0 to Count-1 do iCenter.Add( Items[iI] );
  iCenter.Devide( Count );
  result.Min := iCenter.Copy();
  result.Max := iCenter.Copy();

  for iI := 0 to Count-1 do
  begin
    iVector := Items[iI].Copy();

    If (iVector.X <=  result.Min.x) then
       result.Min.x  := iVector.X
    else If (iVector.X >=  result.Max.x) then
            result.Max.x  := iVector.X;

    If (iVector.Y <=  result.Min.Y) then
       result.Min.Y  := iVector.Y
    else If (iVector.Y >=  result.Max.Y) then
            result.Max.Y  := iVector.Y;

    If (iVector.Z <=  result.Min.Z) then
       result.Min.Z  := iVector.Z
    else If (iVector.Z >=  result.Max.Z) then
            result.Max.Z  := iVector.Z;
  end;

  result.CalculateCenter();
end;

end.

