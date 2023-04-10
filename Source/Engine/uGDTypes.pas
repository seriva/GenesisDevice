
unit uGDTypes;

{$MODE Delphi}

interface

uses
  Classes,
  Math,
  JsonTools,
  SysUtils,
  dglOpenGL;

type
  TGDVector = record
    procedure   Reset(aX,aY,aZ: Single); overload;
    procedure   Reset(aArray: TJsonNode); overload;
    procedure   SetX(aX : Single);
    procedure   SetY(aY : Single);
    procedure   SetZ(aZ : Single);
    function    Copy(): TGDVector;
    function    DotProduct(const aVector : TGDVector) : Single; overload;
    procedure   CrossProduct(const aVector1, aVector2: TGDVector);overload;
    function    Angle(const aVector : TGDVector ) : Single;
    procedure   Normalize();
    function    Magnitude(): Single;
    function    Inverse(): TGDVector;
    function    ArrayPointer() : PGLfloat;

    class operator Add(const aV1, aV2: TGDVector): TGDVector; overload;
    class operator Add(const aV: TGDVector; aD: Single): TGDVector; overload;
    class operator Subtract(const aV1, aV2: TGDVector): TGDVector; overload;
    class operator Subtract(const aV: TGDVector; aD: Single): TGDVector; overload;
    class operator Multiply(const aV1, aV2: TGDVector): TGDVector; overload;
    class operator Multiply(const aV: TGDVector; aD: Single): TGDVector; overload;
    class operator Divide(const aV1, aV2: TGDVector): TGDVector; overload;
    class operator Divide(const aV: TGDVector; aD: Single): TGDVector; overload;
    class operator Equal(aV1, aV2: TGDVector) B: Boolean;

    case Boolean of
      TRUE: ( x, y, z : Single; );
      FALSE: ( xyz: array [0..2] of Single; );
  end;


  TGDUVCoord  = record
    procedure   Reset(aU,aV : Single);
    function    Copy(): TGDUVCoord;

    function    ArrayPointer() : PGLfloat;
    class operator Equal (uv1, uv2: TGDUVCoord) B: Boolean;

    case Boolean of
      TRUE: ( u, v : Single; );
      FALSE: ( uv: array [0..1] of Single; );
  end;


  TGDColor = record
    procedure   Reset(aR,aG,aB,aA : Single); overload;
    procedure   Reset(aArray: TJsonNode); overload;
    function    Copy() : TGDColor;

    procedure   Red();
    procedure   Green();
    procedure   Blue();
    procedure   White();
    procedure   Black();

    function    ArrayPointer() : PGLFloat;
    class operator Equal (c1, c2: TGDColor) B: Boolean;

    case Boolean of
      TRUE: ( r, g, b, a : Single; );
      FALSE: ( rgba: array [0..3] of Single; );
  end;


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


  TGDTriangle = record
    Normal   : TGDVector;
    V1, V2, V3 : TGDVector;

    procedure   Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
    procedure   Move( aMove : TGDVector ); overload;
    procedure   Rotate(  aRotation : TGDVector ); overload;
    procedure   Scale(  aScale : TGDVector ); overload;
    function    Copy(): TGDTriangle;
    procedure   CalculateNormal();
    function    PointInTraingle( aV : TGDVector ) : boolean;
  end;


  TGDBoundingBox = record
    Min : TGDVector;
    Max : TGDVector;
    Center : TGDVector;

    procedure CalculateCenter();
    function  BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
    function  PointInsideBox( aV : TGDVector ) : boolean;

    procedure RenderWireFrame();
  end;


  TGDIdxVertex = record
    Vertex : Integer;
    UV     : Integer;
    Normal : Integer;
  end;

  TGDVertex_V_UV = record
    Vertex : TGDVector;
    UV     : TGDUVCoord;

    class operator Equal(v1, v2: TGDVertex_V_UV) B: Boolean;
  end;

  TGDVertex_V_UV_N = record
    Vertex : TGDVector;
    UV     : TGDUVCoord;
    Normal : TGDVector;

    class operator Equal(v1, v2: TGDVertex_V_UV_N) B: Boolean;
  end;

  TGDVertex_V_UV_N_C = record
    Vertex : TGDVector;
    UV     : TGDUVCoord;
    Normal : TGDVector;
    Color  : TGDColor;

    class operator Equal(v1, v2: TGDVertex_V_UV_N_C) B: Boolean;
  end;


function Vector(aX,aY,aZ: Single) : TGDVector;
function UVCoord(aU,aV : Single) : TGDUVCoord;
function Color(aR,aG,aB,aA : Single) : TGDColor;

const
  EPSILON   = 0.0001;

implementation

uses
  uGDEngine;

function SameSide( aP1, aP2, aA, aB : TGDVector) : boolean;
var
  iCP1, iCP2, iBA, iP1A, iP2A : TGDVector;
begin
  iBA := aB - aA;
  iP1A := aP1 - aA;
  iP2A := aP2 - aA;
  iCP1.CrossProduct( iBA, iP1A );
  iCP2.CrossProduct( iBA, iP2A );
  result := (iCP1.DotProduct(iCP2) >= 0)
end;


function Vector(aX,aY,aZ: Single) : TGDVector;
begin
  result.reset(aX,aY,aZ);
end;

function UVCoord(aU,aV : Single) : TGDUVCoord;
begin
  result.reset(aU,aV)
end;

function Color(aR,aG,aB,aA : Single) : TGDColor;
begin
  result.reset(aR,aG,aB,aA)
end;


procedure TGDVector.Reset(aX,aY,aZ: Single);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;

procedure TGDVector.Reset(aArray: TJsonNode);
begin
  X := aArray.Child(0).AsNumber;
  Y := aArray.Child(1).AsNumber;
  Z := aArray.Child(2).AsNumber;
end;


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


class operator TGDVector.Add(const aV1, aV2: TGDVector): TGDVector;
begin
  Result.x := aV1.x + aV2.x;
  Result.y := aV1.y + aV2.y;
  Result.z := aV1.z + aV2.z;
end;

class operator TGDVector.Add(const aV: TGDVector; aD: Single): TGDVector;
begin
  Result.x := aV.x + aD;
  Result.y := aV.y + aD;
  Result.z := aV.z + aD;
end;


class operator TGDVector.Subtract(const aV1, aV2: TGDVector): TGDVector;
begin
  Result.x := aV1.x - aV2.x;
  Result.y := aV1.y - aV2.y;
  Result.z := aV1.z - aV2.z;
end;

class operator TGDVector.Subtract(const aV: TGDVector; aD: Single): TGDVector;
begin
  Result.x := aV.x - aD;
  Result.y := aV.y - aD;
  Result.z := aV.z - aD;
end;


class operator TGDVector.Multiply(const aV1, aV2: TGDVector): TGDVector;
begin
  Result.x := aV1.x * aV2.x;
  Result.y := aV1.y * aV2.y;
  Result.z := aV1.z * aV2.z;
end;

class operator TGDVector.Multiply(const aV: TGDVector; aD: Single): TGDVector;
begin
  Result.x := aV.x * aD;
  Result.y := aV.y * aD;
  Result.z := aV.z * aD;
end;


class operator TGDVector.Divide(const aV1, aV2: TGDVector): TGDVector;
begin
  Result.x := aV1.x / aV2.x;
  Result.y := aV1.y / aV2.y;
  Result.z := aV1.z / aV2.z;
end;

class operator TGDVector.Divide(const aV: TGDVector; aD: Single): TGDVector;
begin
  Result.x := aV.x / aD;
  Result.y := aV.y / aD;
  Result.z := aV.z / aD;
end;


function TGDVector.Copy(): TGDVector;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;


function TGDVector.Magnitude(): Single;
begin
  Result := sqrt((X * X) + (Y * Y) + (Z * Z));
end;


procedure TGDVector.Normalize();
var
  iMag : Single;
begin
  iMag := Magnitude();
  X := X / iMag;
  Y := Y / iMag;
  Z := Z / iMag;
end;


function TGDVector.DotProduct( const aVector : TGDVector) : Single;
begin
  Result :=  ( (X * aVector.x) + (Y * aVector.y) + (Z * aVector.z) );
end;


procedure TGDVector.CrossProduct(const aVector1, aVector2: TGDVector);
begin
	X := ((aVector1.y * aVector2.z) - (aVector1.z * aVector2.y));
	Y := ((aVector1.z * aVector2.x) - (aVector1.x * aVector2.z));
	Z := ((aVector1.x * aVector2.y) - (aVector1.y * aVector2.x));
end;


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


function TGDVector.Inverse(): TGDVector;
begin
  result.x := -self.x;
  result.y := -self.y;
  result.z := -self.z;
end;


class operator TGDVector.Equal(aV1, aV2: TGDVector) B: Boolean;
begin
  B := (Abs(aV1.x - aV2.x) < EPSILON) and
       (Abs(aV1.y - aV2.y) < EPSILON) and
       (Abs(aV1.z - aV2.z) < EPSILON);
end;


function TGDVector.ArrayPointer() : PGLfloat;
begin
  result := @xyz;
end;


procedure TGDUVCoord.Reset(aU,aV : Single);
begin
  U := aU;
  V := aV;
end;


function TGDUVCoord.Copy(): TGDUVCoord;
begin
  result.u := u;
  result.v := v;
end;


class operator TGDUVCoord.Equal (uv1, uv2: TGDUVCoord)B: Boolean;
begin
  B := (Abs(uv1.u - uv2.u) < EPSILON) and
       (Abs(uv1.v - uv2.v) < EPSILON);
end;


function TGDUVCoord.ArrayPointer() : PGLfloat;
begin
  result := @uv;
end;


procedure TGDColor.Reset(aR,aG,aB,aA : Single);
begin
  r := aR;
  g := aG;
  b := aB;
  a := aA;
end;

procedure TGDColor.Reset(aArray: TJsonNode);
begin
  r := aArray.Child(0).AsNumber;
  g := aArray.Child(1).AsNumber;
  b := aArray.Child(2).AsNumber;
  a := aArray.Child(3).AsNumber;
end;


function TGDColor.Copy(): TGDColor;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;


procedure TGDColor.Red();
begin
  r := 1;
  g := 0;
  b := 0;
  a := 1;
end;


procedure TGDColor.Green();
begin
  r := 0;
  g := 1;
  b := 0;
  a := 1;
end;


procedure TGDColor.Blue();
begin
  r := 0;
  g := 0;
  b := 1;
  a := 1;
end;


procedure TGDColor.White();
begin
  r := 1;
  g := 1;
  b := 1;
  a := 1;
end;


procedure TGDColor.Black();
begin
  r := 0;
  g := 0;
  b := 0;
  a := 1;
end;


function TGDColor.ArrayPointer() : PGLFloat;
begin
  result := @rgba;
end;


class operator TGDColor.Equal (c1, c2: TGDColor) B: Boolean;
begin
  B := (Abs(c1.r - c2.r) < EPSILON) and
       (Abs(c1.g - c2.g) < EPSILON) and
       (Abs(c1.b - c2.b) < EPSILON) and
       (Abs(c1.a - c2.a) < EPSILON);
end;


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


procedure TGDMatrix.Invert();
Var
  iR,iC: integer;
begin
  for iC := 0 to 3 do
    for iR := 0 to 3 do
      Data[iC,iR] := -Data[iC,iR];
end;


procedure TGDMatrix.CreateRotationX(aRX : Single);
begin
  aRX := DegToRad(aRX);
  IdentityMatrix();
  Data[1,1] := cos(aRX);
  Data[2,1] := sin(aRX);
  Data[1,2] := -sin(aRX);
  Data[2,2] := cos(aRX);
end;


procedure TGDMatrix.CreateRotationY(aRY : Single);
begin
  aRY := DegToRad(aRY);
  IdentityMatrix();
  Data[0,0] := cos(aRY);
  Data[0,2] := sin(aRY);
  Data[2,0] := -sin(aRY);
  Data[2,2] := cos(aRY);
end;


procedure TGDMatrix.CreateRotationZ(aRZ : Single);
begin
  aRZ := DegToRad(aRZ);
  IdentityMatrix();
  Data[0,0] := cos(aRZ);
  Data[1,0] := sin(aRZ);
  Data[0,1] := -sin(aRZ);
  Data[1,1] := cos(aRZ);
end;


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


procedure TGDMatrix.ApplyToVector(var aV : TGDVector );
var
  iV : TGDVector;
begin
  iV := aV.Copy();
  aV.x := iV.x * Data[0,0] + iV.y * Data[1,0] + iV.z * Data[2,0] + Data[3,0];
  aV.y := iV.x * Data[0,1] + iV.y * Data[1,1] + iV.z * Data[2,1] + Data[3,1];
  aV.z := iV.x * Data[0,2] + iV.y * Data[1,2] + iV.z * Data[2,2] + Data[3,2];
end;


function TGDMatrix.Copy() : TGDMatrix;
Var
  iR,iC: integer;
begin
  for iC := 0 to 3 do
    for iR := 0 to 3 do
      result.Data[iC,iR] := Data[iC,iR];
end;


function TGDMatrix.ArrayPointer() : PGLfloat;
begin
  result := @Data;
end;


procedure TGDTriangle.Reset(aX1,aY1,aZ1,aX2,aY2,aZ2,aX3,aY3,aZ3 : Single);
begin
  V1.Reset(aX1,aY1,aZ1);
  V2.Reset(aX2,aY2,aZ2);
  V3.Reset(aX3,aY3,aZ3);
end;


procedure TGDTriangle.Move( aMove : TGDVector );
begin
  V1 += aMove;
  V2 += aMove;
  V3 += aMove;
end;


procedure TGDTriangle.Rotate( aRotation : TGDVector );
var
  iM : TGDMatrix;
begin
  iM.CreateRotation( aRotation );
  iM.ApplyToVector( V1 );
  iM.ApplyToVector( V2 );
  iM.ApplyToVector( V3 );
end;


procedure   TGDTriangle.Scale( aScale : TGDVector );
begin
  V1 := (V1 * aScale) / 100;
  V2 := (V2 * aScale) / 100;
  V2 := (V3 * aScale) / 100;
end;


procedure TGDTriangle.CalculateNormal();
var
  iVVector1 : TGDVector;
  iVVector2 : TGDVector;
begin
  iVVector1.Reset( V3.x, V3.Y, V3.Z);
  iVVector1 -= V1;
  iVVector2.Reset(V2.x, V2.Y, V2.Z);
  iVVector2 -= V1;
  Normal.CrossProduct( iVVector1, iVVector2 );
  Normal.Normalize();
end;


function TGDTriangle.Copy(): TGDTriangle;
begin
  result.Normal := Normal.Copy();
  result.V1 := V1.Copy();
  result.V2 := V2.Copy();
  result.V3 := V3.Copy();
end;


Function TGDTriangle.PointInTraingle( aV : TGDVector ) : boolean;
begin
  if SameSide(aV,V1,V2,V3) and
     SameSide(aV,V2,V1,V3) and
     SameSide(aV,V3,V1,V2) then
    result := true
  else
    result := false;
end;


function TGDBoundingBox.BoxInsideBox( aBoundingBox : TGDBoundingBox ) : boolean;
begin
  If (Min.X <= aBoundingBox.Min.X) and (Min.Y <= aBoundingBox.Min.Y) and (Min.Z <= aBoundingBox.Min.Z) and
     (Max.X >= aBoundingBox.Max.X) and (Max.Y >= aBoundingBox.Max.Y) and (Max.Z >= aBoundingBox.Max.Z) then
    result := true
  else
    result := false;
end;


function  TGDBoundingBox.PointInsideBox( aV : TGDVector ) : boolean;
begin
  If (Min.X <= aV.X) and (Min.Y <= aV.Y) and (Min.Z <= aV.Z) and
     (Max.X >= aV.X) and (Max.Y >= aV.Y) and (Max.Z >= aV.Z) then
    result := true
  else
    result := false;
end;


procedure TGDBoundingBox.CalculateCenter();
begin
 Center := Max.Copy;
 Center := (Center + Min) / 2;
end;


procedure TGDBoundingBox.RenderWireFrame();
begin
  GDRenderer.AddLine( Vector(Max.x, Max.y, Max.Z), Vector(Min.x, Max.y, Max.Z));
  GDRenderer.AddLine( Vector(Min.x, Max.y, Max.Z), Vector(Min.x, Max.y, Min.Z));
  GDRenderer.AddLine( Vector(Min.x, Max.y, Min.Z), Vector(Max.x, Max.y, Min.Z));
  GDRenderer.AddLine( Vector(Max.x, Max.y, Min.Z), Vector(Max.x, Max.y, Max.Z));

  GDRenderer.AddLine( Vector(Max.x, Min.y, Max.Z), Vector(Min.x, Min.y, Max.Z));
  GDRenderer.AddLine( Vector(Min.x, Min.y, Max.Z), Vector(Min.x, Min.y, Min.Z));
  GDRenderer.AddLine( Vector(Min.x, Min.y, Min.Z), Vector(Max.x, Min.y, Min.Z));
  GDRenderer.AddLine( Vector(Max.x, Min.y, Min.Z), Vector(Max.x, Min.y, Max.Z));

  GDRenderer.AddLine( Vector(Max.x, Max.y, Max.Z), Vector(Max.x, Min.y, Max.Z ));
  GDRenderer.AddLine( Vector(Min.x, Max.y, Min.Z), Vector(Min.x, Min.y, Min.Z ));
  GDRenderer.AddLine( Vector(Min.x, Max.y, Max.Z), Vector(Min.x, Min.y, Max.Z ));
  GDRenderer.AddLine( Vector(Max.x, Max.y, Min.Z), Vector(Max.x, Min.y, Min.Z ));
end;


class operator TGDVertex_V_UV.Equal(v1, v2: TGDVertex_V_UV) B: Boolean;
begin
  B := (v1.Vertex = v2.Vertex) and (v1.UV = v2.UV);
end;

class operator TGDVertex_V_UV_N.Equal(v1, v2: TGDVertex_V_UV_N) B: Boolean;
begin
  B := (v1.Vertex = v2.Vertex) and (v1.UV = v2.UV) and (v1.Normal = v2.Normal);
end;

class operator TGDVertex_V_UV_N_C.Equal(v1, v2: TGDVertex_V_UV_N_C) B: Boolean;
begin
  B := (v1.Vertex = v2.Vertex) and (v1.UV = v2.UV) and (v1.Normal = v2.Normal);
end;

end.
