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
unit GDCamera;

{$MODE Delphi}

{******************************************************************************}
{* Holds the FPS camera class                                                 *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  dglOpenGL,
  GDTypes;

type

{******************************************************************************}
{* Camera class                                                               *}
{******************************************************************************}

  TGDCamera = class
  private
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

    procedure   InitCamera(aPositionX, aPositionY, aPositionZ : Double);

    Procedure   Move(aStep : Double);
    Procedure   Strafe(aStep : Double);

    procedure   MouseLook(aOldX, aOldY, aNewX, aNewY  : Integer; aSensitivity : Double; aInvertMouse : Boolean);

    Procedure   Translate();
  end;

var
 Camera : TGDCamera;

implementation

{******************************************************************************}
{* Create the camera class                                                    *}
{******************************************************************************}

Constructor TGDCamera.Create();
Begin
  FPosition := TGDVector.Create(0,0,0);
  FUpvector  := TGDVector.Create(0,1,0);
  FDirection  := TGDVector.Create(0,0,-1);
  FRotation  := TGDVector.Create(0,0,0);
end;

{******************************************************************************}
{* Destroy the camera class                                                   *}
{******************************************************************************}

Destructor  TGDCamera.Destroy();
begin
  FreeAndNil(FDirection);
  FreeAndNil(FUpvector);
  FreeAndNil(FPosition);
  FreeAndNil(FRotation);
  inherited
end;

{******************************************************************************}
{* Init the camera class                                                      *}
{******************************************************************************}

procedure TGDCamera.InitCamera(aPositionX, aPositionY, aPositionZ : Double);
begin
  FPosition.Reset(aPositionX,aPositionY,aPositionZ);
  FUpvector.Reset(0,1,0);
  FDirection.Reset(0,0,-1);
  FRotation.Reset(0,0,0);
end;

{******************************************************************************}
{* Translate the camera matrix to opengl                                      *}
{******************************************************************************}

procedure TGDCamera.Translate();
Begin
  gluLookAt(FPosition.x, FPosition.y, FPosition.z,
            FPosition.x+FDirection.x, FPosition.y+FDirection.y, FPosition.z+fDirection.z,
            FUpvector.x, FUpvector.y, FUpvector.z);
end;

{******************************************************************************}
{* Set the direction vector                                                   *}
{******************************************************************************}

Procedure TGDCamera.SetDirection(aV : TGDVector);
Begin
  FDirection.x := aV.x;
  FDirection.y := aV.y;
  FDirection.z := aV.z;
end;

{******************************************************************************}
{* Set the uo vector                                                          *}
{******************************************************************************}

Procedure TGDCamera.SetUpvector(aV : TGDVector);
Begin
  FUpvector.x := aV.x;
  FUpvector.y := aV.y;
  FUpvector.z := aV.z;
end;

{******************************************************************************}
{* Set the position point                                                     *}
{******************************************************************************}

Procedure TGDCamera.SetPosition(aV : TGDVector);
Begin
  FPosition.x := aV.x;
  FPosition.y := aV.y;
  FPosition.z := aV.z;
end;

{******************************************************************************}
{* Set the rotation vector                                                    *}
{******************************************************************************}

Procedure TGDCamera.SetRotation(aV : TGDVector);
Begin
  FRotation.x := aV.x;
  FRotation.y := aV.y;
  FRotation.z := aV.z;
end;

{******************************************************************************}
{* Strafe the camera left or right                                            *}
{******************************************************************************}

procedure TGDCamera.Strafe(aStep : Double);
var
  iM : TGDMatrix;
  iV,iV1,iV2 : TGDVector;
begin
  iM := TGDMatrix.Create();
  iV := TGDVector.Create();
  iV2 := TGDVector.Create();

  iV1 := FRotation.Copy();

  iV1.X := 0;
  iV1.Y := iV1.Y + 90;

  iM.CreateRotation( iV1 );
  iV2.Reset(0,0,-1);
  iM.ApplyToVector(iV2);

  iV.x := -iV2.x * aStep;
  iV.y := 0;
  iV.z := -iV2.z * aStep;

  FPosition.Add( iV );

  FreeAndNil(iM);
  FreeAndNil(iV);
  FreeAndNil(iV1);
  FreeAndNil(iV2);
end;

{******************************************************************************}
{* move the camera forward or backward                                        *}
{******************************************************************************}

procedure TGDCamera.Move(aStep : Double);
var
  iM : TGDMatrix;
  iV : TGDVector;
begin
  iM := TGDMatrix.Create();
  iV := TGDVector.Create();
  iM.CreateRotation( FRotation );
  FDirection.Reset(0,0,-1);
  iM.ApplyToVector(FDirection);

  iV.x := FDirection.x * aStep;
  iV.y := FDirection.y * aStep;
  iV.z := FDirection.z * aStep;

  FPosition.Add( iV );

  FreeAndNil(iM);
  FreeAndNil(iV);
end;

{******************************************************************************}
{* Use the mouse to look arround 6 degrees                                    *}
{******************************************************************************}

procedure  TGDCamera.MouseLook(aOldX, aOldY, aNewX, aNewY  : Integer; aSensitivity : Double; aInvertMouse : Boolean);
var dDeltaX, dDeltaY : double;
    iM : TGDMatrix;
begin
  if ((aOldX = aNewX) and (aOldY = aNewY)) then exit;

  iM := TGDMatrix.Create();

  dDeltaX := aOldX-aNewX;
  dDeltaY := aOldY-aNewY;

  FRotation.y := FRotation.y - dDeltaX / (10-aSensitivity);

  If aInvertMouse then
    FRotation.x := FRotation.x + dDeltaY / (10-aSensitivity)
  else
    FRotation.x := FRotation.x - dDeltaY / (10-aSensitivity);

  if FRotation.x < -89.99 then FRotation.x := -89.999999;
  if FRotation.x > 89.99 then FRotation.x :=   89.999999;

  iM.CreateRotation( FRotation );
  FDirection.Reset(0,0,-1);
  iM.ApplyToVector(FDirection);

  FreeAndNil(iM);
end;

end.
