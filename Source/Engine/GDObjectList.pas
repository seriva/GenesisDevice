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
unit GDObjectList;

{$MODE Delphi}

{******************************************************************************}
{* Holds the objectlist class                                                 *}
{******************************************************************************}

interface

uses
  Contnrs;

type
  TGDObjectList = class(TObjectList)
  private
  public
    constructor Create;
    destructor  Destroy; override;

    function  AddObjectP( aObject : TObject ) : Pointer;
    function  AddObjectI( aObject : TObject ) : Integer;
    function  GetObjectP( aObject : Pointer ) : TObject;
    function  GetObjectI( aIndex  : Integer ) : TObject;
    procedure RemoveObjectP( aObject : Pointer );
    procedure RemoveObjectI( aIndex  : Integer );
  end;

implementation

{******************************************************************************}
{* Creat the objectlist class                                                 *}
{******************************************************************************}

constructor TGDObjectList.Create();
begin
  OwnsObjects := true;
  inherited;
end;

{******************************************************************************}
{* Destroy the objectlist class                                               *}
{******************************************************************************}

destructor  TGDObjectList.Destroy;
begin
 inherited;
end;

{******************************************************************************}
{* Add an object                                                              *}
{******************************************************************************}

function TGDObjectList.AddObjectP( aObject : TObject ) : pointer;
begin
  result := GetItem( Add(aObject) );
end;

function TGDObjectList.AddObjectI( aObject : TObject ) : integer;
begin
  result := Add(aObject);
end;

{******************************************************************************}
{* Get an object                                                              *}
{******************************************************************************}

function TGDObjectList.GetObjectP( aObject : Pointer ) : TObject;
var
  iI : integer;
begin
  iI := IndexOf( aObject );
  if (iI>=0) and (iI<Count) then
    Result := Items[iI]
  else
    Result := nil;
end;

function TGDObjectList.GetObjectI( aIndex  : Integer ) : TObject;
begin
  if (aIndex>=0) and (aIndex<Count) then
    Result := Items[aIndex]
  else
    Result := nil;
end;

{******************************************************************************}
{* Remove an object                                                           *}
{******************************************************************************}

procedure TGDObjectList.RemoveObjectP( aObject : Pointer );
begin
  Remove(aObject);
  aObject := nil;
end;

procedure TGDObjectList.RemoveObjectI( aIndex  : Integer );
var
 iObject : TObject;
begin
  iObject := GetObjectI(aIndex);
  if iObject <> nil then
  begin
    Remove(iObject);
  end;
end;

end.
