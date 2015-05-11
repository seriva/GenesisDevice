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
unit GDTypes;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the following types that are used in the engine:          *}
{* - Vector                                                                   *}
{* - UV                                                                       *}
{* - Color                                                                    *}
{* - Matrix                                                                   *}
{* - Triangle                                                                 *}
{* - Quad                                                                     *}
{* - BoundingBox                                                              *}
{******************************************************************************}

interface

uses
  Math,
  SysUtils,
  dglOpenGL;

type

{******************************************************************************}
{* Vector                                                                     *}
{******************************************************************************}

  TGDVector = record
    procedure   Reset(aX,aY,aZ: Single);
    procedure   SetX(aX : Single);
    procedure   SetY(aY : Single);
    procedure   SetZ(aZ : Single);
    function    Copy(): TGDVector;

    procedure   Add(aX,aY,aZ: Single);overload;
    procedure   Add(aD : Single);overload;
    procedure   Add(const aVector : TGDVector);overload;
    procedure   Substract(aX,aY,aZ: Single);overload;
    procedure   Substract(aD : Single);overload;
    procedure   Substract(const aVector : TGDVector);overload;
    procedure   Multiply(aX,aY,aZ: Single);overload;
    procedure   Multiply(aD : Single);overload;
    procedure   Multiply(const aVector : TGDVector);overload;
    procedure   Devide(aX,aY,aZ: Single);overload;
    procedure   Devide(aD : Single);overload;
    procedure   Devide(const aVector : TGDVector);overload;

    function    DotProduct(const aVector : TGDVector) : Single; overload;
    procedure   CrossProduct(const aVector1, aVector2: TGDVector);overload;
    function    Angle(const aVector : TGDVector ) : Single;
    procedure   Normalize();
    function    Magnitude(): Single;

    function    ArrayPointer() : PGLfloat;
    class operator Equal (v1, v2: TGDVector) B: Boolean;

    case Boolean of
      TRUE: ( x, y, z : Single; );
      FALSE: ( xyz: array [0..1] of Single; );
  end;

{******************************************************************************}
{* UV                                                                         *}
{******************************************************************************}

  TGDUVCoord  = record
    procedure   Reset(aU,aV : Single);overload;
    function    Copy(): TGDUVCoord;

    function    ArrayPointer() : PGLfloat;
    class operator Equal (uv1, uv2: TGDUVCoord) B: Boolean;

    case Boolean of
      TRUE: ( u, v : Single; );
      FALSE: ( uv: array [0..1] of Single; );
  end;

{******************************************************************************}
{* Color                                                                      *}
{******************************************************************************}

  TGDColor = record
    procedure   Reset(aR,aG,aB,aA : Single);
    function    Copy() : TGDColor;

    procedure   Red();
    procedure   Green();
    procedure   Blue();
    procedure   White();
    procedure   Black();

    function    ArrayPointer() : PGLFloat;

    case Boolean of
      TRUE: ( r, g, b, a : Single; );
      FALSE: ( rgba: array [0..3] of Single; );
  end;

{******************************************************************************}
{* Matrix                                                                     *}
{******************************************************************************}

  TGDMatrix = record
    data : array[0..3, 0..3] of Single;

    procedure   EmptyMatrix();
    procedure   IdentityMatrix();
    procedure   Invert();
    procedure   Multiply(aM1, aM2: TGDMatrix);

    procedure   CreateRotation(const aV : TGDVector );
    procedure   CreateRotationX(aRX : Single);
    procedure   CreateRotationY(aRY : Single);
    procedure   CreateRotationZ(aRZ : Single);
    procedure   ApplyToVector(var aV : TGDVector );overload;

    function    Copy(): TGDMatrix;

    function    ArrayPointer() : PGLfloat;
  end;

{******************************************************************************}
{* Triangle class                                                             *}
{******************************************************************************}

  TGDTriangle = record
    Normal   : TGDVector;
    V1, V2, V3 : TGDVector;

    procedure   Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
    procedure   Move( aMove : TGDVector ); overload;
    procedure   Rotate(  aRotation : TGDVector ); overload;
    procedure   Scale(  aScale : TGDVector ); overload;
    function    Copy(): TGDTriangle;
    procedure   CalculateNormal();
    Function    PointInTraingle( aV : TGDVector ) : boolean;
  end;

{******************************************************************************}
{* Quad class                                                                 *}
{******************************************************************************}

  TGDQuad = record
    Normal   : TGDVector;
    V1, V2, V3, V4 : TGDVector;

    procedure   Reset(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);
    procedure   Move( aMove : TGDVector );
    procedure   Rotate( aM : TGDMatrix );
    procedure   Scale(  aScale : TGDVector );
    procedure   CalculateNormal();
    Function    PointInQuad( aV : TGDVector ) : boolean;

    procedure   Render();
  end;

{******************************************************************************}
{* Axis aligned bounding box class                                            *}
{******************************************************************************}

    TGDBoundingBox = record
      Min : TGDVector;
      Max : TGDVector;
      Center : TGDVector;

      procedure CalculateCenter();

      function  BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
      function  PointInsideBox( aV : TGDVector ) : boolean;

      procedure RenderWireFrame();
    end;

implementation

function SameSide( aP1, aP2, aA, aB : TGDVector) : boolean;
var
  iCP1, iCP2, iBA, iP1A, iP2A : TGDVector;
begin
  iBA := aB.Copy();
  iBA.Substract( aA );
  iP1A := aP1.Copy();
  iP1A.Substract(aA);
  iP2A := aP2.Copy();
  iP2A.Substract(aA);
  iCP1.CrossProduct( iBA, iP1A );
  iCP2.CrossProduct( iBA, iP2A );
  if iCP1.DotProduct(iCP2) >= 0 then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Reset the vector                                                           *}
{******************************************************************************}

procedure TGDVector.Reset(aX,aY,aZ: Single);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;

{******************************************************************************}
{* set the vector element                                                     *}
{******************************************************************************}

procedure   TGDVector.SetX(aX : Single);
begin
  X := aX;
end;

procedure   TGDVector.SetY(aY : Single);
begin
  Y := aY;
end;

procedure   TGDVector.SetZ(aZ : Single);
begin
  Z := aZ;
end;

{******************************************************************************}
{* Add a vector                                                               *}
{******************************************************************************}

procedure TGDVector.Add(aX,aY,aZ: Single);
begin
  X := X + aX;
  Y := Y + aY;
  Z := Z + aZ;
end;

procedure TGDVector.Add(aD : Single);
begin
  X := X + aD;
  Y := Y + aD;
  Z := Z + aD;
end;

procedure TGDVector.Add(const aVector : TGDVector);
begin
  X := X + aVector.X;
  Y := Y + aVector.Y;
  Z := Z + aVector.Z;
end;

{******************************************************************************}
{* Substract a vector                                                         *}
{******************************************************************************}

procedure TGDVector.Substract(aX,aY,aZ: Single);
begin
  X := X - aX;
  Y := Y - aY;
  Z := Z - aZ;
end;

procedure TGDVector.Substract( aD : Single);
begin
  X := X - aD;
  Y := Y - aD;
  Z := Z - aD;
end;

procedure TGDVector.Substract(const aVector : TGDVector);
begin
  X := X - aVector.X;
  Y := Y - aVector.Y;
  Z := Z - aVector.Z;
end;

{******************************************************************************}
{* Multiply the vector                                                        *}
{******************************************************************************}

procedure TGDVector.Multiply(aX,aY,aZ: Single);
begin
  X := X * aX;
  Y := Y * aY;
  Z := Z * aZ;
end;

procedure TGDVector.Multiply(aD : Single);
begin
  X := X * aD;
  Y := Y * aD;
  Z := Z * aD;
end;

procedure TGDVector.Multiply(const aVector : TGDVector);
begin
  X := X * aVector.X;
  Y := Y * aVector.Y;
  Z := Z * aVector.Z;
end;

{******************************************************************************}
{* Devide the vector                                                          *}
{******************************************************************************}

procedure TGDVector.Devide(aX,aY,aZ: Single);
begin
  X := X / aX;
  Y := Y / aY;
  Z := Z / aZ;
end;

procedure TGDVector.Devide(aD : Single);
begin
  X := X / aD;
  Y := Y / aD;
  Z := Z / aD;
end;

procedure TGDVector.Devide(const aVector : TGDVector);
begin
  X := X / aVector.X;
  Y := Y / aVector.Y;
  Z := Z / aVector.Z;
end;

{******************************************************************************}
{* Copy the vector                                                            *}
{******************************************************************************}

function TGDVector.Copy(): TGDVector;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

{******************************************************************************}
{* Calculate the vector magnitude                                             *}
{******************************************************************************}

function TGDVector.Magnitude(): Single;
begin
  Result := sqrt((X * X) + (Y * Y) + (Z * Z));
end;

{******************************************************************************}
{* Normalize the vector                                                       *}
{******************************************************************************}

procedure TGDVector.Normalize();
var
  iMag : Single;
begin
  iMag := Magnitude();
  X := X / iMag;
  Y := Y / iMag;
  Z := Z / iMag;
end;

{******************************************************************************}
{* Dotproduct of the vector                                                   *}
{******************************************************************************}

function TGDVector.DotProduct( const aVector : TGDVector) : Single;
begin
  Result :=  ( (X * aVector.x) + (Y * aVector.y) + (Z * aVector.z) );
end;

{******************************************************************************}
{* Crossproduct of the vector                                                 *}
{******************************************************************************}

procedure TGDVector.CrossProduct(const aVector1, aVector2: TGDVector);
begin
	X := ((aVector1.y * aVector2.z) - (aVector1.z * aVector2.y));
	Y := ((aVector1.z * aVector2.x) - (aVector1.x * aVector2.z));
	Z := ((aVector1.x * aVector2.y) - (aVector1.y * aVector2.x));
end;

{******************************************************************************}
{* Angle between 2 vectors                                                    *}
{******************************************************************************}

function TGDVector.Angle(const aVector : TGDVector ) : Single;
var
  iDotProduct : Single;
  iVectorsMagnitude : Single;
  iAngle : real;
begin
  iDotProduct := self.DotProduct(aVector);
  iVectorsMagnitude := self.Magnitude() * aVector.Magnitude();
	iAngle := arccos( iDotProduct / iVectorsMagnitude );
	if(isnan(iAngle)) then
  begin
		result := 0;
    exit;
  end;
	result :=  iAngle;
end;

{******************************************************************************}
{* Vector equals                                                              *}
{******************************************************************************}

class operator TGDVector.Equal (v1, v2: TGDVector) B: Boolean;
begin
  B:=(v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

{******************************************************************************}
{* Get the array pointer                                                      *}
{******************************************************************************}

function TGDVector.ArrayPointer() : PGLfloat;
begin
  result := @xyz;
end;


{******************************************************************************}
{* Reset the UV                                                               *}
{******************************************************************************}

procedure TGDUVCoord.Reset(aU,aV : Single);
begin
  U := aU;
  V := aV;
end;

{******************************************************************************}
{* Copy the UV                                                                *}
{******************************************************************************}

function TGDUVCoord.Copy(): TGDUVCoord;
begin
  result.u := u;
  result.v := v;
end;

{******************************************************************************}
{* UV equals                                                                  *}
{******************************************************************************}

class operator TGDUVCoord.Equal (uv1, uv2: TGDUVCoord)B: Boolean;
begin
  B:=(uv1.u=uv2.u) and (uv1.v=uv2.v);
end;

{******************************************************************************}
{* Get the UV array pointer                                                   *}
{******************************************************************************}

function TGDUVCoord.ArrayPointer() : PGLfloat;
begin
  result := @uv;
end;

{******************************************************************************}
{* Reset the color                                                            *}
{******************************************************************************}

procedure TGDColor.Reset(aR,aG,aB,aA : Single);
begin
  r := aR;
  g := aG;
  b := aB;
  a := aA;
end;

function TGDColor.Copy(): TGDColor;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;


{******************************************************************************}
{* Set the color to red                                                       *}
{******************************************************************************}

procedure TGDColor.Red();
begin
  r := 1;
  g := 0;
  b := 0;
  a := 1;
end;

{******************************************************************************}
{* Set the color to green                                                     *}
{******************************************************************************}

procedure TGDColor.Green();
begin
  r := 0;
  g := 1;
  b := 0;
  a := 1;
end;

{******************************************************************************}
{* Set the color to blue
{******************************************************************************}

procedure TGDColor.Blue();
begin
  r := 0;
  g := 0;
  b := 1;
  a := 1;
end;

{******************************************************************************}
{* Set the color to white
{******************************************************************************}

procedure TGDColor.White();
begin
  r := 1;
  g := 1;
  b := 1;
  a := 1;
end;

{******************************************************************************}
{* Set the color to black
{******************************************************************************}

procedure TGDColor.Black();
begin
  r := 0;
  g := 0;
  b := 0;
  a := 1;
end;

{******************************************************************************}
{* Get the color array pointer                                                *}
{******************************************************************************}

function TGDColor.ArrayPointer() : PGLFloat;
begin
  result := @rgba;
end;

{******************************************************************************}
{* Empty the matrix                                                           *}
{******************************************************************************}

procedure TGDMatrix.EmptyMatrix();
begin
  Data[0,0] := 0;
  Data[1,0] := 0;
  Data[2,0] := 0;
  Data[3,0] := 0;

  Data[0,1] := 0;
  Data[1,1] := 0;
  Data[2,1] := 0;
  Data[3,1] := 0;

  Data[0,2] := 0;
  Data[1,2] := 0;
  Data[2,2] := 0;
  Data[3,2] := 0;

  Data[0,3] := 0;
  Data[1,3] := 0;
  Data[2,3] := 0;
  Data[3,3] := 0;
end;

{******************************************************************************}
{* Fill with identity matrix                                                  *}
{******************************************************************************}

procedure TGDMatrix.IdentityMatrix();
begin
  Data[0,0] := 1;
  Data[1,0] := 0;
  Data[2,0] := 0;
  Data[3,0] := 0;

  Data[0,1] := 0;
  Data[1,1] := 1;
  Data[2,1] := 0;
  Data[3,1] := 0;

  Data[0,2] := 0;
  Data[1,2] := 0;
  Data[2,2] := 1;
  Data[3,2] := 0;

  Data[0,3] := 0;
  Data[1,3] := 0;
  Data[2,3] := 1;
  Data[3,3] := 0;
end;

{******************************************************************************}
{* invert matrix                                                              *}
{******************************************************************************}

procedure TGDMatrix.Invert();
Var
  iR,iC: integer;
begin
  for iC := 0 to 3 do
    for iR := 0 to 3 do
      Data[iC,iR] := -Data[iC,iR];
end;

{******************************************************************************}
{* Create X rotation matrix                                                   *}
{******************************************************************************}

procedure TGDMatrix.CreateRotationX(aRX : Single);
begin
  aRX := DegToRad(aRX);
  IdentityMatrix();
  Data[1,1] := cos(aRX);
  Data[2,1] := sin(aRX);
  Data[1,2] := -sin(aRX);
  Data[2,2] := cos(aRX);
end;

{******************************************************************************}
{* Create Y rotation matrix                                                   *}
{******************************************************************************}

procedure TGDMatrix.CreateRotationY(aRY : Single);
begin
  aRY := DegToRad(aRY);
  IdentityMatrix();
  Data[0,0] := cos(aRY);
  Data[0,2] := sin(aRY);
  Data[2,0] := -sin(aRY);
  Data[2,2] := cos(aRY);
end;

{******************************************************************************}
{* Create Z rotation matrix                                                   *}
{******************************************************************************}

procedure TGDMatrix.CreateRotationZ(aRZ : Single);
begin
  aRZ := DegToRad(aRZ);
  IdentityMatrix();
  Data[0,0] := cos(aRZ);
  Data[1,0] := sin(aRZ);
  Data[0,1] := -sin(aRZ);
  Data[1,1] := cos(aRZ);
end;

{******************************************************************************}
{* Multiply the matrix                                                        *}
{******************************************************************************}

procedure TGDMatrix.Multiply(aM1, aM2: TGDMatrix);
begin
  Data[0,0]:=aM1.Data[0,0]*aM2.Data[0,0]+aM1.Data[0,1]*aM2.Data[1,0]+aM1.Data[0,2]*aM2.Data[2,0]+aM1.Data[0,3]*aM2.Data[3,0];
  Data[0,1]:=aM1.Data[0,0]*aM2.Data[0,1]+aM1.Data[0,1]*aM2.Data[1,1]+aM1.Data[0,2]*aM2.Data[2,1]+aM1.Data[0,3]*aM2.Data[3,1];
  Data[0,2]:=aM1.Data[0,0]*aM2.Data[0,2]+aM1.Data[0,1]*aM2.Data[1,2]+aM1.Data[0,2]*aM2.Data[2,2]+aM1.Data[0,3]*aM2.Data[3,2];
  Data[0,3]:=aM1.Data[0,0]*aM2.Data[0,3]+aM1.Data[0,1]*aM2.Data[1,3]+aM1.Data[0,2]*aM2.Data[2,3]+aM1.Data[0,3]*aM2.Data[3,3];
  Data[1,0]:=aM1.Data[1,0]*aM2.Data[0,0]+aM1.Data[1,1]*aM2.Data[1,0]+aM1.Data[1,2]*aM2.Data[2,0]+aM1.Data[1,3]*aM2.Data[3,0];
  Data[1,1]:=aM1.Data[1,0]*aM2.Data[0,1]+aM1.Data[1,1]*aM2.Data[1,1]+aM1.Data[1,2]*aM2.Data[2,1]+aM1.Data[1,3]*aM2.Data[3,1];
  Data[1,2]:=aM1.Data[1,0]*aM2.Data[0,2]+aM1.Data[1,1]*aM2.Data[1,2]+aM1.Data[1,2]*aM2.Data[2,2]+aM1.Data[1,3]*aM2.Data[3,2];
  Data[1,3]:=aM1.Data[1,0]*aM2.Data[0,3]+aM1.Data[1,1]*aM2.Data[1,3]+aM1.Data[1,2]*aM2.Data[2,3]+aM1.Data[1,3]*aM2.Data[3,3];
  Data[2,0]:=aM1.Data[2,0]*aM2.Data[0,0]+aM1.Data[2,1]*aM2.Data[1,0]+aM1.Data[2,2]*aM2.Data[2,0]+aM1.Data[2,3]*aM2.Data[3,0];
  Data[2,1]:=aM1.Data[2,0]*aM2.Data[0,1]+aM1.Data[2,1]*aM2.Data[1,1]+aM1.Data[2,2]*aM2.Data[2,1]+aM1.Data[2,3]*aM2.Data[3,1];
  Data[2,2]:=aM1.Data[2,0]*aM2.Data[0,2]+aM1.Data[2,1]*aM2.Data[1,2]+aM1.Data[2,2]*aM2.Data[2,2]+aM1.Data[2,3]*aM2.Data[3,2];
  Data[2,3]:=aM1.Data[2,0]*aM2.Data[0,3]+aM1.Data[2,1]*aM2.Data[1,3]+aM1.Data[2,2]*aM2.Data[2,3]+aM1.Data[2,3]*aM2.Data[3,3];
  Data[3,0]:=aM1.Data[3,0]*aM2.Data[0,0]+aM1.Data[3,1]*aM2.Data[1,0]+aM1.Data[3,2]*aM2.Data[2,0]+aM1.Data[3,3]*aM2.Data[3,0];
  Data[3,1]:=aM1.Data[3,0]*aM2.Data[0,1]+aM1.Data[3,1]*aM2.Data[1,1]+aM1.Data[3,2]*aM2.Data[2,1]+aM1.Data[3,3]*aM2.Data[3,1];
  Data[3,2]:=aM1.Data[3,0]*aM2.Data[0,2]+aM1.Data[3,1]*aM2.Data[1,2]+aM1.Data[3,2]*aM2.Data[2,2]+aM1.Data[3,3]*aM2.Data[3,2];
  Data[3,3]:=aM1.Data[3,0]*aM2.Data[0,3]+aM1.Data[3,1]*aM2.Data[1,3]+aM1.Data[3,2]*aM2.Data[2,3]+aM1.Data[3,3]*aM2.Data[3,3];
end;

{******************************************************************************}
{* Create a rotation matrix                                                   *}
{******************************************************************************}

procedure TGDMatrix.CreateRotation(const aV : TGDVector );
var
  iM, iMX, iMY, iMZ : TGDMatrix;
begin
  IdentityMatrix();

  iMX.CreateRotationX(aV.x);
  iMY.CreateRotationY(aV.y);
  iMZ.CreateRotationZ(aV.z);

  iM.Multiply(iMZ,iMY);
  Multiply(iMX,iM);
end;

{******************************************************************************}
{* Apply the matrix to a vector                                               *}
{******************************************************************************}

procedure TGDMatrix.ApplyToVector(var aV : TGDVector );
var
  iV : TGDVector;
begin
  iV := aV.Copy();
  aV.x := iV.x * Data[0,0] + iV.y * Data[1,0] + iV.z * Data[2,0] + Data[3,0];
  aV.y := iV.x * Data[0,1] + iV.y * Data[1,1] + iV.z * Data[2,1] + Data[3,1];
  aV.z := iV.x * Data[0,2] + iV.y * Data[1,2] + iV.z * Data[2,2] + Data[3,2];
end;

{******************************************************************************}
{* Copy the matrix                                                            *}
{******************************************************************************}

function TGDMatrix.Copy() : TGDMatrix;
Var
  iR,iC: integer;
begin
  for iC := 0 to 3 do
    for iR := 0 to 3 do
      result.Data[iC,iR] := Data[iC,iR];
end;

{******************************************************************************}
{* Get the matrix array pointer                                               *}
{******************************************************************************}

function TGDMatrix.ArrayPointer() : PGLfloat;
begin
  result := @Data;
end;

{******************************************************************************}
{* Reset the triangle                                                         *}
{******************************************************************************}

procedure TGDTriangle.Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
begin
  V1.Reset(aX1,aY1,aZ1);
  V2.Reset(aX2,aY2,aZ2);
  V3.Reset(aX3,aY3,aZ3);
end;

{******************************************************************************}
{* Move the traingle                                                          *}
{******************************************************************************}

procedure TGDTriangle.Move( aMove : TGDVector );
begin
  V1.Add( aMove );
  V2.Add( aMove );
  V3.Add( aMove );
end;

{******************************************************************************}
{* Rotate the triangle                                                        *}
{******************************************************************************}

procedure TGDTriangle.Rotate( aRotation : TGDVector );
var
  iM : TGDMatrix;
begin
  iM.CreateRotation( aRotation );
  iM.ApplyToVector( V1 );
  iM.ApplyToVector( V2 );
  iM.ApplyToVector( V3 );
end;

{******************************************************************************}
{* Scale the triangle                                                         *}
{******************************************************************************}

procedure   TGDTriangle.Scale( aScale : TGDVector );
begin
  V1.Multiply(aScale);
  V1.Devide(100);
  V2.Multiply(aScale);
  V2.Devide(100);
  V3.Multiply(aScale);
  V3.Devide(100);
end;

{******************************************************************************}
{* Calculate the normal of the triangle                                       *}
{******************************************************************************}

procedure TGDTriangle.CalculateNormal();
var
  iVVector1 : TGDVector;
  iVVector2 : TGDVector;
begin
  iVVector1.Reset( V3.x, V3.Y, V3.Z);
  iVVector1.Substract( V1 );
  iVVector2.Reset(V2.x, V2.Y, V2.Z);
  iVVector2.Substract( V1 );
  Normal.CrossProduct( iVVector1, iVVector2 );
  Normal.Normalize();
end;

{******************************************************************************}
{* Copy the triangle                                                          *}
{******************************************************************************}

function TGDTriangle.Copy(): TGDTriangle;
begin
  result.Normal := Normal.Copy();
  result.V1 := V1.Copy();
  result.V2 := V2.Copy();
  result.V3 := V3.Copy();
end;

{******************************************************************************}
{* Check if the point is in the triangle
{******************************************************************************}

Function TGDTriangle.PointInTraingle( aV : TGDVector ) : boolean;
begin
  if SameSide(aV,V1,V2,V3) and
     SameSide(aV,V2,V1,V3) and
     SameSide(aV,V3,V1,V2) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Reset the quad                                                             *}
{******************************************************************************}

procedure   TGDQuad.Reset(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);
begin
  V1.Reset(aX1,aY1,aZ1);
  V2.Reset(aX1,aY2,aZ1);
  V3.Reset(aX2,aY2,aZ2);
  V4.Reset(aX2,aY1,aZ2);
end;

{******************************************************************************}
{* Move the quad                                                              *}
{******************************************************************************}

procedure TGDQuad.Move( aMove : TGDVector );
begin
  V1.Add( aMove );
  V2.Add( aMove );
  V3.Add( aMove );
  V4.Add( aMove );
end;

{******************************************************************************}
{* Rotate the quad                                                            *}
{******************************************************************************}

procedure TGDQuad.Rotate( aM : TGDMatrix );
begin
  aM.ApplyToVector( V1 );
  aM.ApplyToVector( V2 );
  aM.ApplyToVector( V3 );
  aM.ApplyToVector( V4 );
end;

{******************************************************************************}
{* Scale the quad                                                             *}
{******************************************************************************}

procedure TGDQuad.Scale( aScale : TGDVector );
begin
  V1.Multiply(aScale);
  V1.Devide(100);
  V2.Multiply(aScale);
  V2.Devide(100);
  V3.Multiply(aScale);
  V3.Devide(100);
  V4.Multiply(aScale);
  V4.Devide(100);
end;

{******************************************************************************}
{* Calculate the normal of the quad                                           *}
{******************************************************************************}

procedure TGDQuad.CalculateNormal();
var
  iVVector1 : TGDVector;
  iVVector2 : TGDVector;
begin
  iVVector1.Reset( V3.x, V3.Y, V3.Z);
  iVVector1.Substract( V1 );
  iVVector2.Reset(V2.x, V2.Y, V2.Z);
  iVVector2.Substract( V1 );
  Normal.CrossProduct( iVVector1, iVVector2 );
  Normal.Normalize();
end;

{******************************************************************************}
{* Check if the point is in the quad
{******************************************************************************}

Function TGDQuad.PointInQuad( aV : TGDVector ) : boolean;
begin
  if ((SameSide(aV,V1,V2,V3) and SameSide(aV,V2,V1,V3) and SameSide(aV,V3,V1,V2)) or
      (SameSide(aV,V1,V3,V4) and SameSide(aV,V3,V1,V4) and SameSide(aV,V4,V1,V3))) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Render the quad                                                            *}
{******************************************************************************}

procedure TGDQuad.Render();
begin
  glBegin(GL_TRIANGLE_STRIP);
    glNormal3fv(Normal.ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.99, 0.99);
    glVertex3fv(V1.ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.99, 0.00);
    glVertex3fv(V2.ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.0, 0.99);
    glVertex3fv(V4.ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.0, 0.0);
    glVertex3fv(V3.ArrayPointer);
  glEnd();
end;

{******************************************************************************}
{* Check if a AABB is inside another AABB                                     *}
{******************************************************************************}

function TGDBoundingBox.BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
begin
  If (Min.X <= aBoundingBox.Min.X) and (Min.Y <= aBoundingBox.Min.Y) and (Min.Z <= aBoundingBox.Min.Z) and
     (Max.X >= aBoundingBox.Max.X) and (Max.Y >= aBoundingBox.Max.Y) and (Max.Z >= aBoundingBox.Max.Z) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Check if a point is inside the AABB                                        *}
{******************************************************************************}

function  TGDBoundingBox.PointInsideBox( aV : TGDVector ) : boolean;
begin
  If (Min.X <= aV.X) and (Min.Y <= aV.Y) and (Min.Z <= aV.Z) and
     (Max.X >= aV.X) and (Max.Y >= aV.Y) and (Max.Z >= aV.Z) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Calculate the center of the AABB using the MIN and the MAX points          *}
{******************************************************************************}

procedure TGDBoundingBox.CalculateCenter();
begin
 Center := Max.Copy;
 Center.Add(Min);
 Center.Devide(2);
end;

{******************************************************************************}
{* Render the AABB wireframe                                                  *}
{******************************************************************************}

procedure TGDBoundingBox.RenderWireFrame();
begin
  glBegin(GL_LINE_LOOP);
    glVertex3f( Max.x, Max.y, Max.Z  );
    glVertex3f( Min.x, Max.y, Max.Z  );
    glVertex3f( Min.x, Max.y, Min.Z  );
    glVertex3f( Max.x, Max.y, Min.Z  );
  glEnd;

  glBegin(GL_LINE_LOOP);
    glVertex3f( Max.x, Min.y, Max.Z  );
    glVertex3f( Min.x, Min.y, Max.Z  );
    glVertex3f( Min.x, Min.y, Min.Z  );
    glVertex3f( Max.x, Min.y, Min.Z  );
  glEnd;

  glBegin(GL_LINES);
    glVertex3f( Max.x, Max.y, Max.Z  );
    glVertex3f( Max.x, Min.y, Max.Z  );

    glVertex3f( Min.x, Max.y, Min.Z  );
    glVertex3f( Min.x, Min.y, Min.Z  );

    glVertex3f( Min.x, Max.y, Max.Z  );
    glVertex3f( Min.x, Min.y, Max.Z  );

    glVertex3f( Max.x, Max.y, Min.Z  );
    glVertex3f( Max.x, Min.y, Min.Z  );
  glEnd;
end;

end.
