unit uGDCamera;

{$mode objfpc}

interface

uses
  dglOpenGL,
  uGDTypes;

type

  TCameraFrustum = array[0..5, 0..3] of double;

  TGDCamera = class
  private
    FFrustum   : TCameraFrustum;
    FDirection : TGDVector;
    FUpvector  : TGDVector;
    FPosition  : TGDVector;
    FRotation  : TGDVector;

    Procedure SetDirection(aV : TGDVector);
    Procedure SetUpvector(aV : TGDVector);
    Procedure SetPosition(aV : TGDVector);
    Procedure SetRotation(aV : TGDVector);
  public
    Property Direction : TGDVector read FDirection write SetDirection;
    Property Upvector : TGDVector read FUpvector write SetUpvector;
    Property Position : TGDVector read FPosition write SetPosition;
    Property Rotation : TGDVector read FRotation write SetRotation;

    Constructor Create();
    Destructor  Destroy(); override;

    Procedure   Move(aStep : Double);
    Procedure   Strafe(aStep : Double);
    procedure   MouseLook(aOldX, aOldY, aNewX, aNewY  : Integer; aSensitivity : Double; aInvertMouse : Boolean);

    Procedure   Translate();
    Procedure   CalculateFrustum();

    function    BoxInView(aBox : TGDBoundingBox): Boolean;
  end;

implementation

procedure NormalizePlane(aFrustum : TCameraFrustum; aSide: Integer);
var
  iMagnitude       : glFloat;
begin
  iMagnitude := sqrt(aFrustum[aSide][0] * aFrustum[aSide][0] +
                     aFrustum[aSide][1] * aFrustum[aSide][1] +
                     aFrustum[aSide][2] * aFrustum[aSide][2]);
  aFrustum[aSide][0] := aFrustum[aSide][0] / iMagnitude;
  aFrustum[aSide][1] := aFrustum[aSide][1] / iMagnitude;
  aFrustum[aSide][2] := aFrustum[aSide][2] / iMagnitude;
  aFrustum[aSide][3] := aFrustum[aSide][3] / iMagnitude;
end;


Constructor TGDCamera.Create();
Begin
  FPosition.Reset(0,0,0);
  FUpvector.Reset(0,1,0);
  FDirection.Reset(-1,0,0);
  FRotation.Reset(0,0,0);
end;


Destructor  TGDCamera.Destroy();
begin
  inherited
end;


Procedure TGDCamera.SetDirection(aV : TGDVector);
Begin
  FDirection.x := aV.x;
  FDirection.y := aV.y;
  FDirection.z := aV.z;
end;


Procedure TGDCamera.SetUpvector(aV : TGDVector);
Begin
  FUpvector.x := aV.x;
  FUpvector.y := aV.y;
  FUpvector.z := aV.z;
end;


Procedure TGDCamera.SetPosition(aV : TGDVector);
Begin
  FPosition.x := aV.x;
  FPosition.y := aV.y;
  FPosition.z := aV.z;
end;


Procedure TGDCamera.SetRotation(aV : TGDVector);
Begin
  FRotation.x := aV.x;
  FRotation.y := aV.y;
  FRotation.z := aV.z;
end;


procedure TGDCamera.Strafe(aStep : Double);
var
  iM : TGDMatrix;
  iV,iV1,iV2 : TGDVector;
begin
  iV1 := FRotation.Copy();

  iV1.X := 0;
  iV1.Y := iV1.Y + 90;

  iM.CreateRotation( iV1 );
  iV2.Reset(-1,0,0);
  iM.ApplyToVector(iV2);

  iV.x := -iV2.x * aStep;
  iV.y := 0;
  iV.z := -iV2.z * aStep;

  FPosition += iV;
end;


procedure TGDCamera.Move(aStep : Double);
var
  iM : TGDMatrix;
  iV : TGDVector;
begin
  iM.CreateRotation( FRotation );
  Direction.Reset(-1,0,0);
  iM.ApplyToVector(FDirection);

  iV := FDirection * aStep;
  FPosition += iV;
end;


procedure  TGDCamera.MouseLook(aOldX, aOldY, aNewX, aNewY  : Integer; aSensitivity : Double; aInvertMouse : Boolean);
var dDeltaX, dDeltaY : double;
    iM : TGDMatrix;
begin
  if ((aOldX = aNewX) and (aOldY = aNewY)) then exit;

  dDeltaX := aOldX-aNewX;
  dDeltaY := aOldY-aNewY;

  FRotation.y := FRotation.y - dDeltaX / (10-aSensitivity);

  If aInvertMouse then
    FRotation.z := FRotation.z - dDeltaY / (10-aSensitivity)
  else
    FRotation.z := FRotation.z + dDeltaY / (10-aSensitivity);

  if FRotation.z < -89 then FRotation.z := -89;
  if FRotation.z > 89 then FRotation.z :=   89;

  iM.CreateRotation( FRotation );
  FDirection.Reset(-1,0,0);
  iM.ApplyToVector(FDirection);
end;


procedure TGDCamera.Translate();

Begin
  //translate camera
  gluLookAt(FPosition.x, FPosition.y, FPosition.z,
            FPosition.x+FDirection.x, FPosition.y+FDirection.y, FPosition.z+fDirection.z,
            FUpvector.x, FUpvector.y, FUpvector.z);

  //calculate frustum
  CalculateFrustum();
end;


Procedure TGDCamera.CalculateFrustum();
var
  iProj, iModl, iClip: array[0..15] of glFloat;
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

  FFrustum[0][0] := iClip[3] - iClip[0];
  FFrustum[0][1] := iClip[7] - iClip[4];
  FFrustum[0][2] := iClip[11] - iClip[8];
  FFrustum[0][3] := iClip[15] - iClip[12];
  NormalizePlane(FFrustum, 0);

  FFrustum[1][0] := iClip[3] + iClip[0];
  FFrustum[1][1] := iClip[7] + iClip[4];
  FFrustum[1][2] := iClip[11] + iClip[8];
  FFrustum[1][3] := iClip[15] + iClip[12];
  NormalizePlane(FFrustum, 1);

  FFrustum[2][0] := iClip[3] + iClip[1];
  FFrustum[2][1] := iClip[7] + iClip[5];
  FFrustum[2][2] := iClip[11] + iClip[9];
  FFrustum[2][3] := iClip[15] + iClip[13];
  NormalizePlane(FFrustum, 2);

  FFrustum[3][0] := iClip[3] - iClip[1];
  FFrustum[3][1] := iClip[7] - iClip[5];
  FFrustum[3][2] := iClip[11] - iClip[9];
  FFrustum[3][3] := iClip[15] - iClip[13];
  NormalizePlane(FFrustum, 3);

  FFrustum[4][0] := iClip[3] - iClip[2];
  FFrustum[4][1] := iClip[7] - iClip[6];
  FFrustum[4][2] := iClip[11] - iClip[10];
  FFrustum[4][3] := iClip[15] - iClip[14];
  NormalizePlane(FFrustum, 4);

  FFrustum[5][0] := iClip[3] + iClip[2];
  FFrustum[5][1] := iClip[7] + iClip[6];
  FFrustum[5][2] := iClip[11] + iClip[10];
  FFrustum[5][3] := iClip[15] + iClip[14];
  NormalizePlane(FFrustum, 5);
end;


function TGDCamera.BoxInView(aBox : TGDBoundingBox): Boolean;
var
  iI : Integer;
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
