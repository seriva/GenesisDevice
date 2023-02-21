{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine                              *
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
unit uGDTypesGenerics;

{$mode objfpc}

{******************************************************************************}
{* Holds the generic types of the GD We put them in a seperate file      *}
{* since we need to use objfpc compiler mode to make them work properly and   *}
{*   the types in GDTypes need delphi compiler mode                           *}
{******************************************************************************}

interface

uses
  uGDTypes,
  FGL;

type
  TGDIndexList = specialize TFPGList<Integer>;
  TGDVertex_V_List  = specialize TFPGList<TGDVector>;
  TGDVertex_UV_List  = specialize TFPGList<TGDUVCoord>;
  TGDVertex_V_UV_List = specialize TFPGList<TGDVertex_V_UV>;
  TGDVertex_V_UV_N_List = specialize TFPGList<TGDVertex_V_UV_N>;
  TGDVertex_V_UV_N_C_List = specialize TFPGList<TGDVertex_V_UV_N_C>;

implementation

end.

