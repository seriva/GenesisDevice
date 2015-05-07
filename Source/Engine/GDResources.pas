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
unit GDResources;

{$mode objfpc}

interface

uses
  GDResource,
  GDTexture,
  GDConstants,
  FGL;

type

 {******************************************************************************}
 {* Recourses class                                                            *}
 {******************************************************************************}

   TResources = specialize TFPGMap<String,TGDResource>;
   TGDResources = class (TResources)
   private
   public
     function LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;

     procedure RemoveResource(var aResource : TGDResource);
     procedure Clear();
   end;

var
  Resources : TGDResources;

implementation

{******************************************************************************}
{* Load a texture resource                                                    *}
{******************************************************************************}

function TGDResources.LoadTexture(aFileName : String; aDetail : TGDTextureDetail; aTextureFilter : TGDTextureFilter): TGDTexture;
var
  iIdx : Integer;
  iResource : TGDResource;
begin
  if Find(aFileName, iIdx) then
  begin
    iResource := Data[iIdx];
    result := iResource as TGDTexture;
  end
  else
  begin
    result := TGDTexture.Create();
    result.InitTexture(aFileName, aDetail, aTextureFilter);
    Add(aFileName, result);
  end;
end;

{******************************************************************************}
{* Clear all resources                                                        *}
{******************************************************************************}

procedure TGDResources.RemoveResource(var aResource : TGDResource);
begin

end;

{******************************************************************************}
{* Clear all resources                                                        *}
{******************************************************************************}

procedure TGDResources.Clear();
begin
end;

end.

