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

  TGDVector = class(TObject)
  private
    FArray : array[0..2] of Single;
  public
    Property X : Single read FArray[0] write FArray[0];
    Property Y : Single read FArray[1] write FArray[1];
    Property Z : Single read FArray[2] write FArray[2];

    constructor Create();overload;
    constructor Create(aX,aY,aZ: Single);overload;
    destructor  Destroy();override;

    procedure   Reset(aX,aY,aZ: Single);overload;
    procedure   Reset(aD : Single);overload;
    procedure   Reset(aVector : TGDVector);overload;
    procedure   Add(aX,aY,aZ: Single);overload;
    procedure   Add(aD : Single);overload;
    procedure   Add(aVector    : TGDVector);overload;
    procedure   Substract(aX,aY,aZ: Single);overload;
    procedure   Substract(aD : Single);overload;
    procedure   Substract(aVector : TGDVector);overload;
    procedure   Multiply(aX,aY,aZ: Single);overload;
    procedure   Multiply(aD : Single);overload;
    procedure   Multiply(aVector : TGDVector);overload;
    procedure   Devide(aX,aY,aZ: Single);overload;
    procedure   Devide(aD : Single);overload;
    procedure   Devide(aVector : TGDVector);overload;
    function    Copy(): TGDVector;
    procedure   Snap(aD : Single); overload;
    procedure   Invert();

    function    DotProduct(aVector : TGDVector) : Single; overload;
    procedure   CrossProduct(aVector1, aVector2: TGDVector);overload;
    function    Angle( aVector : TGDVector ) : Single;
    procedure   Normalize();
    function    Magnitude(): Single;

    function    ArrayPointer() : PGLfloat;
  end;

{******************************************************************************}
{* UV                                                                         *}
{******************************************************************************}

  TGDUVCoord  = record
    procedure   Reset(aU,aV : Single);overload;
    function    Copy(): TGDUVCoord;

    procedure   Add(aU,aV : Single);overload;
    procedure   Add(aUVCoord : TGDUVCoord);overload;
    procedure   Substract(aU,aV : Single);overload;
    procedure   Substract(aUVCoord : TGDUVCoord);overload;
    procedure   Multiply(aU,aV : Single);overload;
    procedure   Multiply(aUVCoord : TGDUVCoord);overload;
    procedure   Devide(aU,aV : Single);overload;
    procedure   Devide(aUVCoord : TGDUVCoord);overload;

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

    procedure   CreateRotation( aV : TGDVector );
    procedure   CreateRotationX(aRX : Single);
    procedure   CreateRotationY(aRY : Single);
    procedure   CreateRotationZ(aRZ : Single);
    procedure   ApplyToVector( aV : TGDVector );overload;

    function    Copy(): TGDMatrix;

    function    ArrayPointer() : PGLfloat;
  end;

{******************************************************************************}
{* Triangle class                                                             *}
{******************************************************************************}

  TGDTriangle = class
  private
    FNormal   : TGDVector;
  public
    Vertices : array[0..2] of TGDVector;
    Property V1 : TGDVector read Vertices[0] write Vertices[0];
    Property V2 : TGDVector read Vertices[1] write Vertices[1];
    Property V3 : TGDVector read Vertices[2] write Vertices[2];
    Property Normal : TGDVector read FNormal write FNormal;

    constructor Create();overload;
    constructor Create(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);overload;
    destructor  Destroy(); override;

    procedure   Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
    procedure   Move( aMove : TGDVector ); overload;
    procedure   Rotate(  aRotation : TGDVector ); overload;
    procedure   Scale(  aScale : TGDVector ); overload;
    function    Copy(): TGDTriangle;
    procedure   CalculateNormal();
    Function    PointInTraingle( aV : TGDVector ) : boolean;

    procedure   Render();
    procedure   RenderSolid();
    procedure   RenderWireFrame();
  end;

{******************************************************************************}
{* Quad class                                                                 *}
{******************************************************************************}

  TGDQuad = class(TObject)
  private
    FNormal   : TGDVector;
  public
    Vertices : array[0..3] of TGDVector;
    Property V1 : TGDVector read Vertices[0] write Vertices[0];
    Property V2 : TGDVector read Vertices[1] write Vertices[1];
    Property V3 : TGDVector read Vertices[2] write Vertices[2];
    Property V4 : TGDVector read Vertices[3] write Vertices[3];
    Property Normal : TGDVector read FNormal write FNormal;

    constructor Create();overload;
    constructor Create(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);overload;
    destructor  Destroy(); override;

    procedure   Reset(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);overload;
    procedure   Move( aMove : TGDVector );overload;
    procedure   Rotate(  aRotation : TGDVector );overload;
    procedure   Scale(  aScale : TGDVector );overload;
    procedure   CalculateNormal();
    Function    PointInQuad( aV : TGDVector ) : boolean;

    procedure   Render(aNormal : TGDVector);
    procedure   RenderSolid();
    procedure   RenderWireFrame();
  end;

implementation

function SameSide( aP1, aP2, aA, aB : TGDVector) : boolean;
var
  iCP1, iCP2, iBA, iP1A, iP2A : TGDVector;
begin
  iCP1 := TGDVector.Create();
  iCP2 := TGDVector.Create();
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
  FreeAndNil(iCP1);
  FreeAndNil(iCP2);
  FreeAndNil(iBA);
  FreeAndNil(iP1A);
  FreeAndNil(iP2A);
end;

{******************************************************************************}
{* Create the vector class                                                    *}
{******************************************************************************}

constructor TGDVector.Create();
begin
  X := 0;
  Y := 0;
  Z := 0;
end;

constructor TGDVector.Create(aX,aY,aZ: Single);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;

{******************************************************************************}
{* Destroy the vector class                                                   *}
{******************************************************************************}

destructor TGDVector.Destroy();
begin
  inherited;
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

procedure TGDVector.Reset(aD : Single);
begin
  X := aD;
  Y := aD;
  Z := aD;
end;

procedure TGDVector.Reset(aVector : TGDVector);
begin
  X := aVector.x;
  Y := aVector.Y;
  Z := aVector.Z;
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

procedure TGDVector.Add(aVector : TGDVector);
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

procedure TGDVector.Substract(aD : Single);
begin
  X := X - aD;
  Y := Y - aD;
  Z := Z - aD;
end;

procedure TGDVector.Substract(aVector : TGDVector);
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

procedure TGDVector.Multiply(aVector : TGDVector);
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

procedure TGDVector.Devide(aVector : TGDVector);
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
  result := TGDVector.Create(X,Y,Z);
end;

{******************************************************************************}
{* Snap the vector                                                            *}
{******************************************************************************}

procedure TGDVector.Snap(aD : Single);
begin
  X := round( X / aD ) * aD;
  Y := round( Y / aD ) * aD;
  Z := round( Z / aD ) * aD;
end;

{******************************************************************************}
{* Invert the vector                                                          *}
{******************************************************************************}

procedure TGDVector.Invert();
begin
  X := -X;
  Y := -Y;
  Z := -Z;
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

function TGDVector.DotProduct(aVector : TGDVector) : Single;
begin
  Result :=  ( (X * aVector.x) + (Y * aVector.y) + (Z * aVector.z) );
end;

{******************************************************************************}
{* Crossproduct of the vector                                                 *}
{******************************************************************************}

procedure TGDVector.CrossProduct(aVector1, aVector2: TGDVector);
begin
	X := ((aVector1.y * aVector2.z) - (aVector1.z * aVector2.y));
	Y := ((aVector1.z * aVector2.x) - (aVector1.x * aVector2.z));
	Z := ((aVector1.x * aVector2.y) - (aVector1.y * aVector2.x));
end;

{******************************************************************************}
{* Angle between 2 vectors                                                    *}
{******************************************************************************}

function TGDVector.Angle( aVector : TGDVector ) : Single;
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
{* Get the array pointer                                                      *}
{******************************************************************************}

function TGDVector.ArrayPointer() : PGLfloat;
begin
  result := @FArray;
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
{* Add a UV                                                                   *}
{******************************************************************************}

procedure TGDUVCoord.Add(aU,aV : Single);
begin
  U := U + aU;
  V := V + aV;
end;

procedure TGDUVCoord.Add(aUVCoord : TGDUVCoord);
begin
  U := U + aUVCoord.U;
  V := V + aUVCoord.V;
end;

{******************************************************************************}
{* Substract a UV                                                             *}
{******************************************************************************}

procedure TGDUVCoord.Substract(aU,aV : Single);
begin
  U := U - aU;
  V := V - aV;
end;

procedure TGDUVCoord.Substract(aUVCoord : TGDUVCoord);
begin
  U := U - aUVCoord.U;
  V := V - aUVCoord.V;
end;

{******************************************************************************}
{* Multiply the UV                                                            *}
{******************************************************************************}

procedure TGDUVCoord.Multiply(aU,aV : Single);
begin
  U := U * aU;
  V := V * aV;
end;

procedure TGDUVCoord.Multiply(aUVCoord : TGDUVCoord);
begin
  U := U * aUVCoord.U;
  V := V * aUVCoord.V;
end;

{******************************************************************************}
{* Devide the UV                                                              *}
{******************************************************************************}

procedure TGDUVCoord.Devide(aU,aV : Single);
begin
  U := U / aU;
  V := V / aV;
end;

procedure TGDUVCoord.Devide(aUVCoord : TGDUVCoord);
begin
  U := U / aUVCoord.U;
  V := V / aUVCoord.V;
end;

{******************************************************************************}
{* Copy the UV                                                                *}
{******************************************************************************}

function TGDUVCoord.Copy(): TGDUVCoord;
begin
  result.u := u;
  result.v := v;
end;

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

procedure TGDMatrix.CreateRotation( aV : TGDVector );
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

procedure TGDMatrix.ApplyToVector( aV : TGDVector );
var
  iV : TGDVector;
begin
  iV := aV.Copy();
  aV.x := iV.x * Data[0,0] + iV.y * Data[1,0] + iV.z * Data[2,0] + Data[3,0];
  aV.y := iV.x * Data[0,1] + iV.y * Data[1,1] + iV.z * Data[2,1] + Data[3,1];
  aV.z := iV.x * Data[0,2] + iV.y * Data[1,2] + iV.z * Data[2,2] + Data[3,2];
  FreeAndNil(iV)
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
{* Create the triangle class                                                  *}
{******************************************************************************}

constructor TGDTriangle.Create();
begin
  Vertices[0] := TGDVector.Create();
  Vertices[1] := TGDVector.Create();
  Vertices[2] := TGDVector.Create();
  FNormal     := TGDVector.Create();
end;

constructor TGDTriangle.Create(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
begin
  Vertices[0] := TGDVector.Create(aX1,aY1,aZ1);
  Vertices[1] := TGDVector.Create(aX2,aY2,aZ2);
  Vertices[2] := TGDVector.Create(aX3,aY3,aZ3);
  FNormal     := TGDVector.Create();
end;

{******************************************************************************}
{* Destroy the triangle class                                                 *}
{******************************************************************************}

destructor  TGDTriangle.Destroy();
begin
  inherited;
  FreeAndNil(Vertices[0]);
  FreeAndNil(Vertices[1]);
  FreeAndNil(Vertices[2]);
  FreeAndNil(FNormal);
end;

{******************************************************************************}
{* Reset the triangle                                                         *}
{******************************************************************************}

procedure TGDTriangle.Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
begin
  Vertices[0].Reset(aX1,aY1,aZ1);
  Vertices[1].Reset(aX2,aY2,aZ2);
  Vertices[2].Reset(aX3,aY3,aZ3);
end;

{******************************************************************************}
{* Move the traingle                                                          *}
{******************************************************************************}

procedure TGDTriangle.Move( aMove : TGDVector );
begin
  Vertices[0].Add( aMove );
  Vertices[1].Add( aMove );
  Vertices[2].Add( aMove );
end;

{******************************************************************************}
{* Rotate the triangle                                                        *}
{******************************************************************************}

procedure TGDTriangle.Rotate( aRotation : TGDVector );
var
  iM : TGDMatrix;
begin
  iM.CreateRotation( aRotation );
  iM.ApplyToVector( Vertices[0] );
  iM.ApplyToVector( Vertices[1] );
  iM.ApplyToVector( Vertices[2] );
end;

{******************************************************************************}
{* Scale the triangle                                                         *}
{******************************************************************************}

procedure   TGDTriangle.Scale( aScale : TGDVector );
begin
  Vertices[0].Multiply(aScale);
  Vertices[0].Devide(100);
  Vertices[1].Multiply(aScale);
  Vertices[1].Devide(100);
  Vertices[2].Multiply(aScale);
  Vertices[2].Devide(100);
end;

{******************************************************************************}
{* Calculate the normal of the triangle                                           *}
{******************************************************************************}

procedure TGDTriangle.CalculateNormal();
var
  iVVector1 : TGDVector;
  iVVector2 : TGDVector;
begin
  iVVector1 := TGDVector.Create();
  iVVector2 := TGDVector.Create();

  iVVector1.Reset( V3.x, V3.Y, V3.Z);
  iVVector1.Substract( V1 );
  iVVector2.Reset(V2.x, V2.Y, V2.Z);
  iVVector2.Substract( V1 );
  FNormal.CrossProduct( iVVector1, iVVector2 );
  FNormal.Normalize();

  FreeAndNil(iVVector1);
  FreeAndNil(iVVector2);
end;

{******************************************************************************}
{* Copy the triangle                                                          *}
{******************************************************************************}

function TGDTriangle.Copy(): TGDTriangle;
begin
  result := TGDTriangle.Create( V1.X, V1.Y, V1.Z, V2.X, V2.Y, V2.Z, V3.X, V3.Y, V3.Z );
end;

{******************************************************************************}
{* Check if the point is in the triangle
{******************************************************************************}

Function TGDTriangle.PointInTraingle( aV : TGDVector ) : boolean;
begin
  if SameSide(aV,Vertices[0],Vertices[1],Vertices[2]) and
     SameSide(aV,Vertices[1],Vertices[0],Vertices[2]) and
     SameSide(aV,Vertices[2],Vertices[0],Vertices[1]) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Render the triangle                                                        *}
{******************************************************************************}

procedure TGDTriangle.Render();
begin
  glBegin(GL_TRIANGLES);
    glVertex3fv(Vertices[0].ArrayPointer);
    glVertex3fv(Vertices[1].ArrayPointer);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

procedure TGDTriangle.RenderSolid();
begin
  glBegin(GL_TRIANGLES);
    glVertex3fv(Vertices[0].ArrayPointer);
    glVertex3fv(Vertices[1].ArrayPointer);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

procedure TGDTriangle.RenderWireFrame();
begin
  glBegin(GL_LINES);
    glVertex3fv(Vertices[0].ArrayPointer);
    glVertex3fv(Vertices[1].ArrayPointer);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

{******************************************************************************}
{* Create the quad class                                                      *}
{******************************************************************************}

constructor TGDQuad.Create();
begin
  Vertices[0] := TGDVector.Create();
  Vertices[1] := TGDVector.Create();
  Vertices[2] := TGDVector.Create();
  Vertices[3] := TGDVector.Create();
  Normal      := TGDVector.Create();
end;

constructor TGDQuad.Create(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);
begin
  Vertices[0] := TGDVector.Create(aX1,aY1,aZ1);
  Vertices[1] := TGDVector.Create(aX1,aY2,aZ1);
  Vertices[2] := TGDVector.Create(aX2,aY2,aZ2);
  Vertices[3] := TGDVector.Create(aX2,aY1,aZ2);
  FNormal     := TGDVector.Create();
end;

{******************************************************************************}
{* Destroy the quad class                                                     *}
{******************************************************************************}

destructor  TGDQuad.Destroy();
begin
  inherited;
  FreeAndNil(Vertices[0]);
  FreeAndNil(Vertices[1]);
  FreeAndNil(Vertices[2]);
  FreeAndNil(Vertices[3]);
  FreeAndNil(FNormal);
end;

{******************************************************************************}
{* Reset the quad                                                             *}
{******************************************************************************}

procedure   TGDQuad.Reset(aX1,aY1,aZ1,aX2,aY2,aZ2 : Single);
begin
  Vertices[0].Reset(aX1,aY1,aZ1);
  Vertices[1].Reset(aX1,aY2,aZ1);
  Vertices[2].Reset(aX2,aY2,aZ2);
  Vertices[3].Reset(aX2,aY1,aZ2);
end;

{******************************************************************************}
{* Move the quad                                                              *}
{******************************************************************************}

procedure TGDQuad.Move( aMove : TGDVector );
begin
  Vertices[0].Add( aMove );
  Vertices[1].Add( aMove );
  Vertices[2].Add( aMove );
  Vertices[3].Add( aMove );
end;

{******************************************************************************}
{* Rotate the quad                                                            *}
{******************************************************************************}

procedure TGDQuad.Rotate( aRotation : TGDVector );
var
  iM : TGDMatrix;
begin
  iM.CreateRotation( aRotation );
  iM.ApplyToVector( Vertices[0] );
  iM.ApplyToVector( Vertices[1] );
  iM.ApplyToVector( Vertices[2] );
  iM.ApplyToVector( Vertices[3] );
end;

{******************************************************************************}
{* Scale the quad                                                             *}
{******************************************************************************}

procedure TGDQuad.Scale( aScale : TGDVector );
begin
  Vertices[0].Multiply(aScale);
  Vertices[0].Devide(100);
  Vertices[1].Multiply(aScale);
  Vertices[1].Devide(100);
  Vertices[2].Multiply(aScale);
  Vertices[2].Devide(100);
  Vertices[3].Multiply(aScale);
  Vertices[3].Devide(100);
end;

{******************************************************************************}
{* Calculate the normal of the quad                                           *}
{******************************************************************************}

procedure TGDQuad.CalculateNormal();
var
  iVVector1 : TGDVector;
  iVVector2 : TGDVector;
begin
  iVVector1 := TGDVector.Create();
  iVVector2 := TGDVector.Create();

  iVVector1.Reset( V3.x, V3.Y, V3.Z);
  iVVector1.Substract( V1 );
  iVVector2.Reset(V2.x, V2.Y, V2.Z);
  iVVector2.Substract( V1 );
  Normal.CrossProduct( iVVector1, iVVector2 );
  Normal.Normalize();

  FreeAndNil(iVVector1);
  FreeAndNil(iVVector2);
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

procedure TGDQuad.Render(aNormal : TGDVector);
begin
  glBegin(GL_TRIANGLE_STRIP);
    if aNormal <> nil then
       glNormal3fv(aNormal.ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.99, 0.99);
    glVertex3fv(Vertices[0].ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.99, 0.00);
    glVertex3fv(Vertices[1].ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.0, 0.99);
    glVertex3fv(Vertices[3].ArrayPointer);
    glMultiTexCoord2f(GL_TEXTURE0, 0.0, 0.0);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

procedure   TGDQuad.RenderSolid();
begin
  glBegin(GL_TRIANGLE_STRIP);
    glVertex3fv(Vertices[0].ArrayPointer);
    glVertex3fv(Vertices[1].ArrayPointer);
    glVertex3fv(Vertices[3].ArrayPointer);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

procedure TGDQuad.RenderWireFrame();
begin
  glBegin(GL_LINE_LOOP);
    glVertex3fv(Vertices[1].ArrayPointer);
    glVertex3fv(Vertices[0].ArrayPointer);
    glVertex3fv(Vertices[3].ArrayPointer);
    glVertex3fv(Vertices[2].ArrayPointer);
  glEnd();
end;

end.
