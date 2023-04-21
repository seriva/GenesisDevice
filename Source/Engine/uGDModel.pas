unit uGDModel;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

Uses
  fgl,
  SysUtils,
  Classes,
  JsonTools,
  dglOpenGL,
  LazFileUtils,
  uGDTypes,
  uGDConstants,
  uGDMaterial,
  uGDGLWrappers,
  uGDTypesGenerics,
  uGDResource,
  uGDMesh;

type
  TGDWeight = record
    Joint : Integer;
    W	    : Single;
    Pos   : TGDVector;

    class operator = (aW1, aW2: TGDWeight): Boolean;
  end;
  TGDWeightList = specialize TFPGList<TGDWeight>;


  TGDModelMesh = record
    Mesh       : TGDMesh;
    Weights	   : TGDWeightList;
    WeightData : array of array[0..1] of Integer;

    procedure Create();
    procedure Clear();

    class operator = (aM1, aM2: TGDModelMesh): Boolean;
  end;
  TGDModelMeshList = specialize TFPGList<TGDModelMesh>;


  TGDJoint = record
    Pos      : TGDVector;
    Quat     : TGDQuaternion;
    Parent   : Integer;

    class operator = (aJ1, aJ2: TGDJoint): Boolean;
  end;
  TGDJointList  = specialize TFPGList<TGDJoint>;


  TGDFrame = Record
    AABB    : TGDBoundingBox;
    AniComp : array of Single;
    Joints  : TGDJointList;

    procedure Create();
    procedure Clear();

    class operator = (aF1, aF2: TGDFrame): Boolean;
  end;
  TGDFrameList  = specialize TFPGList<TGDFrame>;


  TGDAnimation = Record
    NumAniComp : Integer;
    NumFrames  : Integer;
    FrameRate  : Integer;
    Frames 	   : TGDFrameList;
    BaseJoints : TGDJointList;
    JointInfo  : array of array[0..2] of Integer;

    procedure Create();
    procedure Clear();

    class operator = (aA1, aA2: TGDAnimation): Boolean;
  end;
  TGDAnimationMap = specialize TFPGMap<String,TGDAnimation>;


  TGDModel = class (TGDResource)
  private
    FMeshes     : TGDModelMeshList;
    FJoints     : TGDJointList;
    FAnimations : TGDAnimationMap;
  public
    constructor Create(aFileName : String);
    destructor  Destroy(); override;
  end;

implementation

uses
  uGDEngine;

class operator TGDWeight.=(aW1, aW2: TGDWeight): Boolean;
begin
  Result := false;
end;


procedure TGDModelMesh.Create();
begin
  Weights	:= TGDWeightList.Create();
end;


procedure TGDModelMesh.Clear();
begin
  FreeAndNil(Weights);
end;


class operator TGDModelMesh.=(aM1, aM2: TGDModelMesh): Boolean;
begin
  Result := false;
end;


class operator TGDJoint.= (aJ1, aJ2: TGDJoint): Boolean;
begin
  Result := false;
end;

procedure TGDFrame.Create();
begin
  Joints := TGDJointList.Create();
end;


procedure TGDFrame.Clear();
begin
  FreeAndNil(Joints);
end;


class operator TGDFrame.= (aF1, aF2: TGDFrame): Boolean;
begin
  result := false;
end;


procedure TGDAnimation.Create();
begin
  Frames 	   := TGDFrameList.Create();
  BaseJoints := TGDJointList.Create();
end;


procedure TGDAnimation.Clear();
begin
  FreeAndNil(Frames);
  FreeAndNil(BaseJoints);
end;


class operator TGDAnimation.= (aA1, aA2: TGDAnimation): Boolean;
begin
  result := false;
end;


constructor TGDModel.Create( aFileName : String );
var
  iResult : boolean;
  iError : String;
begin
  GDConsole.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    FMeshes     := TGDModelMeshList.Create();
    FJoints     := TGDJointList.Create();
    FAnimations := TGDAnimationMap.create();

    If Not(FileExists(aFileName) ) then
      Raise Exception.Create('Mesh ' + aFileName + ' doesn`t excist!');





  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;

  GDConsole.Use:=true;
  GDConsole.WriteOkFail(iResult, iError);
end;


destructor  TGDModel.Destroy();
begin
  FreeAndNil(FMeshes);
  FreeAndNil(FJoints);
  FreeAndNil(FAnimations);
end;

end.
