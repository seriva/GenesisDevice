unit newton;

interface

  {$IFDEF WINDOWS}
    uses
      Windows;
  {$ENDIF}

  {$IFDEF LINUX}
    uses
      X,
      XLib;
  {$ENDIF}

  {$IFDEF DARWIN}
    uses
      X,
      XLib,
      CocoaAll;
  {$ENDIF}

  const
    {$IFDEF WINDOWS}
      NEWTON_API = 'newton.dll';
    {$ENDIF}
    {$IFDEF UNIX}
      {$IFDEF DARWIN}
        NEWTON_API = 'libnewton.dylib';
      {$ELSE}
        NEWTON_API = 'libNewton.so';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF MACOS}
      NEWTON_API = 'libnewton';
    {$ENDIF}
    NEWTON_MAJOR_VERSION = 3;
    NEWTON_MINOR_VERSION = 14;
    NEWTON_DYNAMIC_BODY = 0;
    NEWTON_KINEMATIC_BODY = 1;
    NEWTON_DEFORMABLE_BODY = 2;
    SERIALIZE_ID_SPHERE = 0;
    SERIALIZE_ID_CAPSULE = 1;
    SERIALIZE_ID_CHAMFERCYLINDER = 2;
    SERIALIZE_ID_TAPEREDCAPSULE = 3;
    SERIALIZE_ID_CYLINDER = 4;
    SERIALIZE_ID_TAPEREDCYLINDER = 5;
    SERIALIZE_ID_BOX = 6;
    SERIALIZE_ID_CONE = 7;
    SERIALIZE_ID_CONVEXHULL = 8;
    SERIALIZE_ID_NULL = 9;
    SERIALIZE_ID_COMPOUND = 10;
    SERIALIZE_ID_TREE = 11;
    SERIALIZE_ID_HEIGHTFIELD = 12;
    SERIALIZE_ID_CLOTH_PATCH = 13;
    SERIALIZE_ID_DEFORMABLE_SOLID = 14;
    SERIALIZE_ID_USERMESH = 15;
    SERIALIZE_ID_SCENE = 16;
    SERIALIZE_ID_FRACTURED_COMPOUND = 17;

  type
    dFloat = single;
    dFloat64 = double;
    dLong = int64;
    PdFloat = ^dFloat;
    PdFloat64 = ^dFloat64;
    PdLong = ^dLong;
    NewtonMesh = Pointer;
    NewtonBody = Pointer;
    NewtonWorld = Pointer;
    NewtonJoint = Pointer;
    NewtonMaterial = Pointer;
    NewtonCollision = Pointer;
    NewtonSkeletonContainer = Pointer;
    NewtonDeformableMeshSegment = Pointer;
    NewtonFracturedCompoundMeshPart = Pointer;
    NewtonSerializeHandle = Pointer;
    NewtonMeshHandle = Pointer;
    NewtonMeshVertex = Pointer;
    NewtonMeshPoint = Pointer;
    NewtonMeshEdge = Pointer;
    NewtonMeshFace = Pointer;
    NewtonListener = Pointer;

    TNewtonBoxParam = packed record
      m_x, m_y, m_z: dFloat;
    end;

    TNewtonSphereParam = packed record
      m_radio: dFloat;
    end;

    TNewtonCylinderParam = packed record
      m_radio, m_height: dFloat;
    end;

    TNewtonCapsuleParam = packed record
      m_radio, m_height: dFloat;
    end;

    TNewtonConeParam = packed record
      m_radio, m_height: dFloat;
    end;

    TNewtonTaperedCapsuleParam = packed record
      m_radio0, m_radio1, m_height: dFloat;
    end;

    TNewtonTaperedCylinderParam = packed record
      m_radio0, m_radio1, m_height: dFloat;
    end;

    TNewtonChamferCylinderParam = packed record
      m_radio, m_height: dFloat;
    end;

    TNewtonConvexHullParam = packed record
      m_vertexCount, m_vertexStrideInBytes, m_faceCount: integer;
      m_vertex: PdFloat;
    end;

    TNewtonCompoundCollisionParam = packed record
      m_chidrenCount: integer;
    end;

    TNewtonCollisionTreeParam = packed record
      m_vertexCount,
      m_indexCount: integer;
    end;

    TNewtonDeformableMeshParam = packed record
      m_vertexCount, m_triangleCount, m_vertexStrideInBytes: integer;
      m_indexList: PWord;
      m_vertexList: PdFloat;
    end;

    TNewtonHeightFieldCollisionParam = packed record
      m_width, m_height, m_gridsDiagonals, m_elevationDataType: integer;
      m_horizonalScale, m_verticalScale: dFloat;
      m_horizonalDisplacementScale: dFloat;
      m_vertialElevation: Pointer;
      m_horizotalDisplacement: PSmallInt;
      m_atributes: PShortInt;
    end;

    TNewtonSceneCollisionParam = packed record
      m_childrenProxyCount: integer;
    end;
    TNewtonCollisionNullParam = packed record
    end;
    PNewtonCollisionInfoRecord = ^TNewtonCollisionInfoRecord;

    TNewtonCollisionInfoRecord = packed record
      m_offsetMatrix: array[0..3, 0..3] of dFloat;
      m_collisionType, m_collisionUserID: integer;
      case integer of
        SERIALIZE_ID_SPHERE: (sdSphere: TNewtonSphereParam);
        SERIALIZE_ID_CAPSULE: (sdCapsule: TNewtonCapsuleParam);
        SERIALIZE_ID_CHAMFERCYLINDER: (sdChamferCylinder:
          TNewtonChamferCylinderParam);
        SERIALIZE_ID_TAPEREDCAPSULE: (sdTaperedCapsule: TNewtonTaperedCapsuleParam);
        SERIALIZE_ID_CYLINDER: (sdCylinder: TNewtonCylinderParam);
        SERIALIZE_ID_TAPEREDCYLINDER: (sdTaperedCylinder: TNewtonTaperedCylinderParam);
        SERIALIZE_ID_BOX: (sdBox: TNewtonBoxParam);
        SERIALIZE_ID_CONE: (sdCone: TNewtonConeParam);
        SERIALIZE_ID_CONVEXHULL: (sdConvexHull: TNewtonConvexHullParam);
        SERIALIZE_ID_NULL: (sdNull: TNewtonCollisionNullParam);
        SERIALIZE_ID_COMPOUND: (sdCompound: TNewtonCompoundCollisionParam);
        SERIALIZE_ID_TREE: (sdTree: TNewtonCollisionTreeParam);
        SERIALIZE_ID_HEIGHTFIELD: (sdHeightField: TNewtonHeightFieldCollisionParam);
        SERIALIZE_ID_DEFORMABLE_SOLID: (sdDeformableMesh: TNewtonDeformableMeshParam);
        SERIALIZE_ID_USERMESH: (sdUserMesh:
          array[0..63] of dFloat);
        SERIALIZE_ID_SCENE: (sdSceneCollision: TNewtonSceneCollisionParam);
    end;
    PNewtonJointRecord = ^TNewtonJointRecord;

    TNewtonJointRecord = packed record
      m_attachmenMatrix_0: array[0..3, 0..3] of dFloat;
      m_attachmenMatrix_1: array[0..3, 0..3] of dFloat;
      m_minLinearDof: array[0..2] of dFloat;
      m_maxLinearDof: array[0..2] of dFloat;
      m_minAngularDof: array[0
      ..2] of dFloat;
      m_maxAngularDof: array[0..2] of dFloat;
      m_attachBody_0: NewtonBody;
      m_attachBody_1: NewtonBody;
      m_extraParameters: array[0..63] of dFloat;
      m_bodiesCollisionOn: integer;
      m_descriptionType: array[0..127] of shortint;
    end;
    PNewtonUserMeshCollisionCollideDesc = ^TNewtonUserMeshCollisionCollideDesc;

    TNewtonUserMeshCollisionCollideDesc = packed record
      m_boxP0, m_boxP1, m_m_boxDistanceTravel: array[0..3] of dFloat;
      m_threadNumber, m_faceCount, m_vertexStrideInBytes: integer;
      m_skinThickness: dFloat;
      m_userData: Pointer;
      m_objBody, m_polySoupBody: NewtonBody;
      m_objCollision, m_polySoupCollision: NewtonCollision;
      m_vertex: PdFloat;
      m_faceIndexCount, m_faceVertexIndex: PInteger;
    end;
    PNewtonWorldConvexCastReturnInfo = ^TNewtonWorldConvexCastReturnInfo;

    TNewtonWorldConvexCastReturnInfo = packed record
      m_point, m_normal: array[0..3] of dFloat;
      m_contactID: integer;
      m_hitBody: NewtonBody;
      m_penetration: dFloat;
    end;
    PNewtonUserMeshCollisionRayHitDesc = ^TNewtonUserMeshCollisionRayHitDesc;

    TNewtonUserMeshCollisionRayHitDesc = packed record
      m_p0, m_p1, m_normalOut: array[0..
      3] of dFloat;
      m_userIdOut: dLong;
      m_userData: Pointer;
    end;
    PNewtonHingeSliderUpdateDesc = ^TNewtonHingeSliderUpdateDesc;

    TNewtonHingeSliderUpdateDesc = packed record
      m_accel, m_minFriction, m_maxFriction, m_timestep: dFloat;
    end;
    PNewtonClothPatchMaterial = ^TNewtonClothPatchMaterial;

    TNewtonClothPatchMaterial = packed record
      m_damper, m_stiffness: dFloat;
    end;
    PNewtonSkeletonBoneJacobian = ^TNewtonSkeletonBoneJacobian;

    TNewtonSkeletonBoneJacobian = packed record
      m_linear,
      m_angular: array[0..3] of dFloat;
    end;
    PNewtonSkeletonBoneJacobianPair = ^TNewtonSkeletonBoneJacobianPair;

    TNewtonSkeletonBoneJacobianPair = packed record
      m_j01, m_j10: TNewtonSkeletonBoneJacobian;
    end;
    PNewtonUserContactPoint = ^TNewtonUserContactPoint;

    TNewtonUserContactPoint = packed record
      m_point, m_normal: array[0..3] of dFloat;
      m_shapeId0: dLong;
      m_shapeId1: dLong;
      m_penetration: dFloat;
      m_unused: array[0..2] of integer;
    end;
    PNewtonAllocMemory = ^TNewtonAllocMemory;
    TNewtonAllocMemory = function(sizeInBytes: integer): Pointer;
    PNewtonFreeMemory = ^TNewtonFreeMemory;
    TNewtonFreeMemory = procedure(ptr: Pointer; sizeInBytes: integer);
    PNewtonWorldDestructorCallback = ^TNewtonWorldDestructorCallback;
    TNewtonWorldDestructorCallback = procedure(const World: NewtonWorld);
    PNewtonWorldListenerBodyDestroyCallback = ^TNewtonWorldListenerBodyDestroyCallback;
    TNewtonWorldListenerBodyDestroyCallback = procedure(const World: NewtonWorld; listenerUserData: Pointer; body: NewtonBody);
    PNewtonWorldUpdateListenerCallback = ^TNewtonWorldUpdateListenerCallback;
    TNewtonWorldUpdateListenerCallback = procedure(const World: NewtonWorld; listenerUserData: Pointer; timestep: dFloat);
    PNewtonWorldDestroyListenerCallback = ^TNewtonWorldDestroyListenerCallback;
    TNewtonWorldDestroyListenerCallback = procedure(const World: NewtonWorld; listenerUserData: Pointer);
    PNewtonGetTicksCountCallback = ^TNewtonGetTicksCountCallback;
    TNewtonGetTicksCountCallback = function(): cardinal;
    PNewtonSerializeCallback = ^TNewtonSerializeCallback;
    TNewtonSerializeCallback = procedure(Handle: NewtonSerializeHandle; const buffer: Pointer; size: integer);
    PNewtonDeserializeCallback = ^TNewtonDeserializeCallback;
    TNewtonDeserializeCallback = procedure(Handle: NewtonSerializeHandle; buffer: Pointer; size: integer);
    PNewtonOnBodySerializationCallback = ^TNewtonOnBodySerializationCallback;
    TNewtonOnBodySerializationCallback = procedure(body: NewtonBody; userData: Pointer; Callback: PNewtonSerializeCallback;
      Handle: NewtonSerializeHandle);
    PNewtonOnBodyDeserializationCallback = ^TNewtonOnBodyDeserializationCallback;
    TNewtonOnBodyDeserializationCallback = procedure(body: NewtonBody; userData: Pointer; Callback: PNewtonDeserializeCallback;
      Handle: NewtonSerializeHandle);
    PNewtonOnJointSerializationCallback = ^TNewtonOnJointSerializationCallback;
    TNewtonOnJointSerializationCallback = procedure(joint: NewtonJoint; funct: PNewtonSerializeCallback; serializeHandle: NewtonSerializeHandle);
    PNewtonOnJointDeserializationCallback = ^TNewtonOnJointDeserializationCallback;
    TNewtonOnJointDeserializationCallback = procedure(body0: NewtonBody; body1: NewtonBody; funct: PNewtonDeserializeCallback;
      serializeHandle: NewtonSerializeHandle);
    PNewtonOnUserCollisionSerializationCallback = ^TNewtonOnUserCollisionSerializationCallback;
    TNewtonOnUserCollisionSerializationCallback = procedure(userData: Pointer; Callback: PNewtonSerializeCallback; Handle: NewtonSerializeHandle);
    PNewtonUserMeshCollisionDestroyCallback = ^TNewtonUserMeshCollisionDestroyCallback;
    TNewtonUserMeshCollisionDestroyCallback = procedure(userData: Pointer);
    PNewtonUserMeshCollisionRayHitCallback = ^TNewtonUserMeshCollisionRayHitCallback;
    TNewtonUserMeshCollisionRayHitCallback = function(lineDescData: PNewtonUserMeshCollisionRayHitDesc): dFloat;
    PNewtonUserMeshCollisionGetCollisionInfo = ^TNewtonUserMeshCollisionGetCollisionInfo;
    TNewtonUserMeshCollisionGetCollisionInfo = procedure(userData: Pointer; infoRecord: PNewtonCollisionInfoRecord);
    PNewtonUserMeshCollisionAABBTest = ^TNewtonUserMeshCollisionAABBTest;
    TNewtonUserMeshCollisionAABBTest = function(userData: Pointer; const boxP0, boxP1: PdFloat): integer;
    PNewtonUserMeshCollisionGetFacesInAABB = ^TNewtonUserMeshCollisionGetFacesInAABB;
    TNewtonUserMeshCollisionGetFacesInAABB = function(userData: Pointer; const p0: PdFloat; const p1: PdFloat;
      const vertexArray: PdFloat; vertexCount: PInteger; vertexStrideInBytes: PInteger; const indexList: PInteger;
      maxIndexCount: integer; const userDataList: PInteger): integer;
    PNewtonUserMeshCollisionCollideCallback = ^TNewtonUserMeshCollisionCollideCallback;
    TNewtonUserMeshCollisionCollideCallback = procedure(collideDescData: PNewtonUserMeshCollisionCollideDesc; continueCollisionHandle: Pointer);
    PNewtonTreeCollisionFaceCallback = ^TNewtonTreeCollisionFaceCallback;
    TNewtonTreeCollisionFaceCallback = function(context: Pointer; polygon: PdFloat; strideInBytes: integer;
      indexArray: PInteger; indexCount: integer): integer;
    PNewtonCollisionTreeRayCastCallback = ^TNewtonCollisionTreeRayCastCallback;
    TNewtonCollisionTreeRayCastCallback = function(const body: NewtonBody; const treeCollision: NewtonCollision;
      interception: dFloat; normal: PdFloat; faceID: integer; userData: Pointer): dFloat;
    PNewtonHeightFieldRayCastCallback = ^TNewtonHeightFieldRayCastCallback;
    TNewtonHeightFieldRayCastCallback = function(const body: NewtonBody; const heightFieldCollision: NewtonCollision;
      interception: dFloat; row, col: integer; normal: PdFloat; faceID: integer; userData: Pointer): dFloat;
    PNewtonCollisionCopyConstructionCallback = ^TNewtonCollisionCopyConstructionCallback;
    TNewtonCollisionCopyConstructionCallback = procedure(newtonWorld: NewtonWorld; collision: NewtonCollision; sourceCollision: NewtonCollision);
    PNewtonCollisionDestructorCallback = ^TNewtonCollisionDestructorCallback;
    TNewtonCollisionDestructorCallback = procedure(newtonWorld: NewtonWorld; collision: NewtonCollision);
    PNewtonTreeCollisionCallback = ^TNewtonTreeCollisionCallback;
    TNewtonTreeCollisionCallback = procedure(const bodyWithTreeCollision: NewtonBody; const body: NewtonBody;
      faceID: integer; vertexCount: integer; const vertexArray: PdFloat; vertexStrideInBytes: integer);
    PNewtonBodyDestructor = ^TNewtonBodyDestructor;
    TNewtonBodyDestructor = procedure(const body: NewtonBody);
    PNewtonApplyForceAndTorque = ^TNewtonApplyForceAndTorque;
    TNewtonApplyForceAndTorque = procedure(const body: NewtonBody; timestep: dFloat; threadIndex: integer);
    PNewtonSetTransform = ^TNewtonSetTransform;
    TNewtonSetTransform = procedure(const body: NewtonBody; const matrix: PdFloat; threadIndex: integer);
    PNewtonIslandUpdate = ^TNewtonIslandUpdate;
    TNewtonIslandUpdate = function(const world: NewtonWorld; islandHandle: Pointer; bodyCount: integer): integer;
    PNewtonFractureCompoundCollisionOnEmitCompoundFractured = ^TNewtonFractureCompoundCollisionOnEmitCompoundFractured;
    TNewtonFractureCompoundCollisionOnEmitCompoundFractured = procedure(fracturedBody: NewtonBody);
    PNewtonFractureCompoundCollisionOnEmitChunk = ^TNewtonFractureCompoundCollisionOnEmitChunk;
    TNewtonFractureCompoundCollisionOnEmitChunk = procedure(chunkBody: NewtonBody; fracturexChunkMesh: NewtonFracturedCompoundMeshPart;
      fracturedCompountCollision: NewtonCollision);
    PNewtonFractureCompoundCollisionReconstructMainMeshCallBack = ^TNewtonFractureCompoundCollisionReconstructMainMeshCallBack;
    TNewtonFractureCompoundCollisionReconstructMainMeshCallBack = procedure(body: NewtonBody; mainMesh: NewtonFracturedCompoundMeshPart;
      fracturedCompountCollision: NewtonCollision);
    PNewtonWorldRayPrefilterCallback = ^TNewtonWorldRayPrefilterCallback;
    TNewtonWorldRayPrefilterCallback = function(const body: NewtonBody; collision: NewtonCollision; userData: Pointer): cardinal;
    PNewtonWorldRayFilterCallback = ^TNewtonWorldRayFilterCallback;
    TNewtonWorldRayFilterCallback = function(const body: NewtonBody; const shapeHit: NewtonCollision; const hitContact: PDFloat;
      const hitNormal: PdFloat; collisionID: dLong; userData: Pointer; intersectParam: dFloat): dFloat;
    PNewtonContactsProcess = ^TNewtonContactsProcess;
    TNewtonContactsProcess = procedure(const contact: NewtonJoint; timestep: dFloat; threadIndex: integer);
    PNewtonOnAABBOverlap = ^TNewtonOnAABBOverlap;
    TNewtonOnAABBOverlap = function(const material: NewtonMaterial; const body0: NewtonBody; const body1: NewtonBody; threadIndex: integer): integer;
    PNewtonOnCompoundSubCollisionAABBOverlap = ^TNewtonOnCompoundSubCollisionAABBOverlap;
    TNewtonOnCompoundSubCollisionAABBOverlap = function(const material: NewtonMaterial; body0: NewtonBody;
      collisionNode0: Pointer; body1: NewtonBody; collisionNode1: Pointer; threadIndex: integer): integer;
    PNewtonOnContactGeneration = ^TNewtonOnContactGeneration;
    TNewtonOnContactGeneration = function(const material: NewtonMaterial; body0: NewtonBody; collision0: Pointer;
      body1: NewtonBody; collision1: Pointer; contactBuffer: PNewtonUserContactPoint; maxCount: integer; threadIndex: integer): integer;
    PNewtonBodyIterator = ^TNewtonBodyIterator;
    TNewtonBodyIterator = function(const body: NewtonBody; userData: Pointer): integer;
    PNewtonJointIterator = ^TNewtonJointIterator;
    TNewtonJointIterator = procedure(const joint: NewtonJoint; userData: Pointer);
    PNewtonCollisionIterator = ^TNewtonCollisionIterator;
    TNewtonCollisionIterator = procedure(userData: Pointer; vertexCount: integer; const faceArray: PdFloat; faceId: integer);
    PNewtonBallCallback = ^TNewtonBallCallback;
    TNewtonBallCallback = procedure(const ball: NewtonJoint; timestep: dFloat);
    PNewtonHingeCallback = ^TNewtonHingeCallback;
    TNewtonHingeCallback = function(const hinge: NewtonJoint; desc: PNewtonHingeSliderUpdateDesc): cardinal;
    PNewtonSliderCallback = ^TNewtonSliderCallback;
    TNewtonSliderCallback = function(const slider: NewtonJoint; desc: PNewtonHingeSliderUpdateDesc): cardinal;
    PNewtonUniversalCallback = ^TNewtonUniversalCallback;
    TNewtonUniversalCallback = function(const universal: NewtonJoint; desc: PNewtonHingeSliderUpdateDesc): cardinal;
    PNewtonCorkscrewCallback = ^TNewtonCorkscrewCallback;
    TNewtonCorkscrewCallback = function(const corkscrew: NewtonJoint; desc: PNewtonHingeSliderUpdateDesc): cardinal;
    PNewtonUserBilateralCallback = ^TNewtonUserBilateralCallback;
    TNewtonUserBilateralCallback = procedure(const userJoint: NewtonJoint; timestep: dFloat; threadIndex: integer);
    PNewtonUserBilateralGetInfoCallback = ^TNewtonUserBilateralGetInfoCallback;
    TNewtonUserBilateralGetInfoCallback = procedure(const userJoint: NewtonJoint; info: PNewtonJointRecord);
    PNewtonConstraintDestructor = ^TNewtonConstraintDestructor;
    TNewtonConstraintDestructor = procedure(const me: NewtonJoint);
    PNewtonSkeletontDestructor = ^TNewtonSkeletontDestructor;
    TNewtonSkeletontDestructor = procedure(const me: NewtonSkeletonContainer);
    PNewtonJobTask = ^TNewtonJobTask;
    TNewtonJobTask = procedure(userData: Pointer; threadIndex: integer);
    PNewtonReportProgress = ^TNewtonReportProgress;
    TNewtonReportProgress = procedure(progressNormalzedPercent: dFloat);

  function NewtonWorldGetVersion(): integer; cdecl; external NEWTON_API;
  function NewtonWorldFloatSize(): integer; cdecl; external NEWTON_API;
  function NewtonGetMemoryUsed(): integer; cdecl; external NEWTON_API;
  procedure NewtonSetMemorySystem(malloc: PNewtonAllocMemory; Free: PNewtonFreeMemory); cdecl; external NEWTON_API;
  function NewtonCreate: NewtonWorld; cdecl; external NEWTON_API;
  procedure NewtonDestroy(const world: NewtonWorld); cdecl; external NEWTON_API;
  procedure NewtonDestroyAllBodies(const world: NewtonWorld); cdecl; external NEWTON_API;
  function NewtonAlloc(sizeInBytes: integer): Pointer; cdecl; external NEWTON_API;
  procedure NewtonFree(ptr: Pointer); cdecl; external NEWTON_API;
  function NewtonEnumerateDevices(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  function NewtonGetCurrentDevice(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  procedure NewtonSetCurrentDevice(const world: NewtonWorld; deviceIndex: integer); cdecl; external NEWTON_API;
  procedure NewtonGetDeviceString(const world: NewtonWorld; deviceIndex: integer; vendorString: PChar; maxSize: integer); cdecl; external NEWTON_API;
  function NewtonGetGlobalScale(const world: NewtonWorld): dFloat; cdecl; external NEWTON_API;
  procedure NewtonSetGlobalScale(const world: NewtonWorld; scale: dFloat); cdecl; external NEWTON_API;
  function NewtonGetContactMergeTolerance(const world: NewtonWorld): dFloat; cdecl; external NEWTON_API;
  procedure NewtonSetContactMergeTolerance(const world: NewtonWorld; tolerance: dFloat); cdecl; external NEWTON_API;
  procedure NewtonInvalidateCache(const world: NewtonWorld); cdecl; external NEWTON_API;
  procedure NewtonSetSolverModel(const world: NewtonWorld; model: integer); cdecl; external NEWTON_API;
  procedure NewtonSetSolverConvergenceQuality(const world: NewtonWorld; lowOrHigh: integer); cdecl; external NEWTON_API;
  procedure NewtonSetMultiThreadSolverOnSingleIsland(const world: NewtonWorld; mode: integer); cdecl; external NEWTON_API;
  function NewtonGetMultiThreadSolverOnSingleIsland(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  function NewtonGetBroadphaseAlgorithm(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  procedure NewtonSelectBroadphaseAlgorithm(const world: NewtonWorld; algorithmType: integer); cdecl; external NEWTON_API;
  procedure NewtonUpdate(const world: NewtonWorld; timestep: dFloat); cdecl; external NEWTON_API;
  procedure NewtonUpdateAsync(const world: NewtonWorld; timestep: dFloat); cdecl; external NEWTON_API;
  procedure NewtonWaitForUpdateToFinish(const world: NewtonWorld); cdecl; external NEWTON_API;
  procedure NewtonSerializeToFile(const world: NewtonWorld; const filename: PChar; bodyCallback: PNewtonOnBodySerializationCallback;
    bodyUserData: Pointer); cdecl; external NEWTON_API;
  procedure NewtonDeserializeFromFile(const world: NewtonWorld; const filename: PChar; bodyCallback: PNewtonOnBodyDeserializationCallback;
    bodyUserData: Pointer); cdecl; external
    NEWTON_API;
  procedure NewtonSetJointSerializationCallbacks(const world: NewtonWorld; serializeJoint: PNewtonOnJointSerializationCallback;
    deserializeJoint: PNewtonOnJointDeserializationCallback); cdecl; external NEWTON_API;
  procedure NewtonGetJointSerializationCallbacks(const world: NewtonWorld; serializeJoint: PNewtonOnJointSerializationCallback;
    deserializeJoint: PNewtonOnJointDeserializationCallback); cdecl; external NEWTON_API;
  procedure NewtonWorldCriticalSectionLock(const world: NewtonWorld; threadIndex: integer); cdecl; external NEWTON_API;
  procedure NewtonWorldCriticalSectionUnlock(const world: NewtonWorld); cdecl; external NEWTON_API;
  procedure NewtonSetThreadsCount(const world: NewtonWorld; threads: integer); cdecl; external
    NEWTON_API;
  function NewtonGetThreadsCount(const world: NewtonWorld): integer; cdecl;
    external NEWTON_API;
  function NewtonGetMaxThreadsCount(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  procedure NewtonDispachThreadJob(const world: NewtonWorld; task: PNewtonJobTask; userData: Pointer); cdecl; external NEWTON_API;
  procedure NewtonSyncThreadJobs(const world: NewtonWorld); cdecl; external NEWTON_API;
  function NewtonAtomicAdd(ptr: PInteger; Value: integer): integer; cdecl; external NEWTON_API;
  function NewtonAtomicSwap(ptr: PInteger; Value: integer): integer; cdecl; external NEWTON_API;
  procedure NewtonYield(); cdecl; external NEWTON_API;
  procedure NewtonSetFrictionModel(const world: NewtonWorld; model: integer); cdecl; external NEWTON_API;
  procedure NewtonSetMinimumFrameRate(const world: NewtonWorld; frameRate: dFloat); cdecl; external NEWTON_API;
  procedure NewtonSetIslandUpdateEvent(const world: NewtonWorld; islandUpdate: PNewtonIslandUpdate); cdecl; external NEWTON_API;
  procedure NewtonWorldForEachJointDo(const world: NewtonWorld; callback: PNewtonJointIterator; userData: Pointer); cdecl; external NEWTON_API;
  procedure NewtonWorldForEachBodyInAABBDo(const world: NewtonWorld; const p0: PdFloat; const p1: PdFloat;
    callback: PNewtonBodyIterator; userData: Pointer); cdecl; external NEWTON_API;
  procedure NewtonWorldSetUserData(const world: NewtonWorld; userData: Pointer); cdecl; external NEWTON_API;
  function NewtonWorldGetUserData(const world: NewtonWorld): Pointer; cdecl; external NEWTON_API;
  function NewtonWorldGetListenerUserData(const world: NewtonWorld; Listener: NewtonListener): Pointer; cdecl; external NEWTON_API;
  function NewtonWorldListenerGetBodyDestroyCallback(const world: NewtonWorld; Listener: NewtonListener): PNewtonWorldListenerBodyDestroyCallback;
    cdecl; external NEWTON_API;
  procedure NewtonWorldListenerSetBodyDestroyCallback(const world: NewtonWorld; Listener: NewtonListener;
    bodyDestroyCallback: PNewtonWorldListenerBodyDestroyCallback); cdecl; external NEWTON_API;
  function NewtonWorldGetPreListener(const world: NewtonWorld; nameID: PChar): NewtonListener; cdecl; external NEWTON_API;
  function NewtonWorldAddPreListener(const world: NewtonWorld; nameID: PChar; listenerUserData: Pointer;
    Update: PNewtonWorldUpdateListenerCallback; Destroy: PNewtonWorldDestroyListenerCallback): NewtonListener; cdecl; external NEWTON_API;
  function NewtonWorldGetPostListener(const world: NewtonWorld; nameID: PChar): NewtonListener; cdecl; external NEWTON_API;
  function NewtonWorldAddPostListener(const world: NewtonWorld; nameID: PChar; listenerUserData: Pointer;
    Update: PNewtonWorldUpdateListenerCallback; Destroy: PNewtonWorldDestroyListenerCallback): NewtonListener; cdecl; external NEWTON_API;
  procedure NewtonWorldSetDestructorCallback(const world: NewtonWorld; callback: PNewtonWorldDestructorCallback); cdecl; external NEWTON_API;
  function NewtonWorldGetDestructorCallback(const world: NewtonWorld): PNewtonWorldDestructorCallback; cdecl; external NEWTON_API;
  procedure NewtonWorldSetCollisionConstructorDestructorCallback(const world: NewtonWorld; construct: PNewtonCollisionCopyConstructionCallback;
    destruct: PNewtonCollisionDestructorCallback); cdecl; external NEWTON_API;
  procedure NewtonWorldRayCast(const world: NewtonWorld; const p0: PdFloat; const p1: PdFloat; filter: PNewtonWorldRayFilterCallback;
    userData: Pointer; prefilter: PNewtonWorldRayPrefilterCallback; threadIndex: integer);
    cdecl; external NEWTON_API;
  procedure NewtonWorldConvexRayCast(const world: NewtonWorld; shape: NewtonCollision; const Matrix: PdFloat;
    p1: PdFloat; filter: PNewtonWorldRayFilterCallback; userData: Pointer; prefilter: PNewtonWorldRayPrefilterCallback; threadIndex: integer);
    cdecl; external NEWTON_API;
  function NewtonWorldCollide(const world: NewtonWorld; const Matrix: PdFloat; const shape: NewtonCollision;
    UserData: Pointer; prefilter: PNewtonWorldRayPrefilterCallback; Info: PNewtonWorldConvexCastReturnInfo;
    maxContactsCount: integer; threadIndex: integer): integer; cdecl; external NEWTON_API;
  function NewtonWorldConvexCast(const world: NewtonWorld; const matrix: PdFloat; const target: PdFloat;
    const shape: NewtonCollision; hitParam: PdFloat; userData: Pointer; prefilter: PNewtonWorldRayPrefilterCallback;
    info: PNewtonWorldConvexCastReturnInfo; maxContactsCount: integer; threadIndex: integer): integer; cdecl; external NEWTON_API;
  function NewtonWorldGetBodyCount(const world: NewtonWorld): integer; cdecl; external
    NEWTON_API;
  function NewtonWorldGetConstraintCount(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  function NewtonIslandGetBody(const island: Pointer; bodyIndex: integer): NewtonBody; cdecl; external NEWTON_API;
  procedure NewtonIslandGetBodyAABB(const island: Pointer; bodyIndex: integer; p0: PdFloat; p1: PdFloat); cdecl; external NEWTON_API;
  function NewtonMaterialCreateGroupID(const world: NewtonWorld): integer; cdecl; external
    NEWTON_API;
  function NewtonMaterialGetDefaultGroupID(const world: NewtonWorld): integer; cdecl; external NEWTON_API;
  procedure NewtonMaterialDestroyAllGroupID(const world: NewtonWorld); cdecl; external NEWTON_API;
  function NewtonMaterialGetUserData(const world: NewtonWorld; id0: integer; id1: integer): Pointer; cdecl; external NEWTON_API;
  procedure NewtonMaterialSetSurfaceThickness(const world: NewtonWorld; id0: integer; id1: integer; thickness: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetCallbackUserData(const world: NewtonWorld; id0: integer; id1: integer; userData: Pointer); cdecl; external
    NEWTON_API;
  procedure NewtonMaterialSetContactGenerationCallback(const world: NewtonWorld; id0: integer; id1: integer;
    contactGeneration: PNewtonOnContactGeneration);
    cdecl; external
    NEWTON_API;
  procedure NewtonMaterialSetCompoundCollisionCallback(const world: NewtonWorld; id0: integer; id1: integer;
    compoundAabbOverlap: PNewtonOnCompoundSubCollisionAABBOverlap); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetCollisionCallback(const world: NewtonWorld; id0: integer; id1: integer; aabbOverlap: PNewtonOnAABBOverlap;
    process: PNewtonContactsProcess); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetDefaultSoftness(const world: NewtonWorld; id0: integer; id1: integer; Value: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetDefaultElasticity(const world: NewtonWorld; id0: integer; id1: integer; elasticCoef: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetDefaultCollidable(const world: NewtonWorld; id0: integer; id1: integer; state: integer); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetDefaultFriction(const world: NewtonWorld; id0: integer; id1: integer; staticFriction: dFloat; kineticFriction: dFloat);
    cdecl; external NEWTON_API;
  function NewtonWorldGetFirstMaterial(const world: NewtonWorld): NewtonMaterial; cdecl; external NEWTON_API;
  function NewtonWorldGetNextMaterial(const world: NewtonWorld; const material: NewtonMaterial): NewtonMaterial; cdecl; external NEWTON_API;
  function NewtonWorldGetFirstBody(const world: NewtonWorld): NewtonBody; cdecl; external NEWTON_API;
  function NewtonWorldGetNextBody(const world: NewtonWorld; const curBody: NewtonBody): NewtonBody; cdecl; external NEWTON_API;
  function NewtonMaterialGetMaterialPairUserData(const material: NewtonMaterial): Pointer; cdecl; external NEWTON_API;
  function NewtonMaterialGetContactFaceAttribute(const material: NewtonMaterial): cardinal; cdecl; external NEWTON_API;
  function NewtonMaterialGetBodyCollidingShape(const material: NewtonMaterial; const body: NewtonBody): NewtonCollision; cdecl; external
    NEWTON_API;
  function NewtonMaterialGetContactNormalSpeed(const material: NewtonMaterial): dFloat; cdecl; external NEWTON_API;
  procedure NewtonMaterialGetContactForce(const material: NewtonMaterial; const body: NewtonBody; force: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialGetContactPositionAndNormal(const material: NewtonMaterial; const body: NewtonBody; posit: PdFloat; normal: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonMaterialGetContactTangentDirections(const material: NewtonMaterial; const body: NewtonBody; dir0: PdFloat; dir1: PdFloat);
    cdecl; external NEWTON_API;
  function NewtonMaterialGetContactTangentSpeed(const material: NewtonMaterial; index: integer): dFloat; cdecl; external NEWTON_API;
  function NewtonMaterialGetContactMaxNormalImpact(const material: NewtonMaterial): dFloat; cdecl; external NEWTON_API;
  function NewtonMaterialGetContactMaxTangentImpact(const material: NewtonMaterial; index: integer): dFloat; cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactSoftness(const material: NewtonMaterial; softness: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactElasticity(const material: NewtonMaterial; restitution: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactFrictionState(const material: NewtonMaterial; state: integer; index: integer); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactFrictionCoef(const material: NewtonMaterial; staticFrictionCoef: dFloat;
    kineticFrictionCoef: dFloat; index: integer);
    cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactNormalAcceleration(const material: NewtonMaterial; accel: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactNormalDirection(const material: NewtonMaterial; const directionVector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactPosition(const material: NewtonMaterial; const position: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactTangentFriction(const material: NewtonMaterial; friction: dFloat; index: integer); cdecl; external NEWTON_API;
  procedure NewtonMaterialSetContactTangentAcceleration(const material: NewtonMaterial; accel: dFloat; index: integer); cdecl; external NEWTON_API;
  procedure NewtonMaterialContactRotateTangentDirections(const material: NewtonMaterial; const directionVector: PdFloat); cdecl; external NEWTON_API;
  function NewtonCreateNull(const world: NewtonWorld): NewtonCollision; cdecl; external
    NEWTON_API;
  function NewtonCreateSphere(const world: NewtonWorld; radius: dFloat; shapeID: integer; const offsetMatrix: PdFloat): NewtonCollision;
    cdecl; external NEWTON_API;
  function NewtonCreateBox(const world: NewtonWorld; dx: dFloat; dy: dFloat; dz: dFloat; shapeID: integer;
    const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateCone(const world: NewtonWorld; radius: dFloat; Height: dFloat; shapeID: integer; const offsetMatrix: PdFloat): NewtonCollision;
    cdecl; external NEWTON_API;
  function NewtonCreateCapsule(const world: NewtonWorld; radius: dFloat; Height: dFloat; shapeID: integer;
    const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateCylinder(const world: NewtonWorld; radius: dFloat; Height: dFloat; shapeID: integer;
    const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateTaperedCapsule(const world: NewtonWorld; radio0: dFloat; radio1: dFloat; Height: dFloat;
    shapeID: integer; const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateTaperedCylinder(const world: NewtonWorld; radio0: dFloat; radio1: dFloat; Height: dFloat;
    shapeID: integer; const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateChamferCylinder(const world: NewtonWorld; radius: dFloat; Height: dFloat; shapeID: integer;
    const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateConvexHull(const world: NewtonWorld; Count: integer; const vertexCloud: PdFloat; strideInBytes: integer;
    tolerance: dFloat; shapeID: integer; const offsetMatrix: PdFloat): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateConvexHullFromMesh(const world: NewtonWorld; const mesh: NewtonMesh; tolerance: dFloat;
    shapeID: integer): NewtonCollision; cdecl; external
    NEWTON_API;
  function NewtonCollisionGetMode(const convexCollision: NewtonCollision): integer; cdecl; external NEWTON_API;
  procedure NewtonCollisionSetMode(const convexCollision: NewtonCollision; mode: integer); cdecl; external NEWTON_API;
  function NewtonConvexHullGetFaceIndices(const convexHullCollision: NewtonCollision; face: integer; faceIndices: PInteger): integer;
    cdecl; external NEWTON_API;
  function NewtonConvexHullGetVertexData(const convexHullCollision: NewtonCollision; vertexData: PdFloat; strideInBytes: PInteger): integer;
    cdecl; external
    NEWTON_API;
  function NewtonConvexCollisionCalculateVolume(const convexCollision: NewtonCollision): dFloat; cdecl; external NEWTON_API;
  procedure NewtonConvexCollisionCalculateInertialMatrix(const convexCollision: NewtonCollision; inertia: PdFloat; origin: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonConvexCollisionCalculateBuoyancyAcceleration(const convexCollision: NewtonCollision; matrix: PdFloat;
    shapeOrigin: PdFloat; gravityVector: PdFloat; fluidPlane: PdFloat; fluidDensity: dFloat; fluidViscosity: dFloat; accel: PdFloat; alpha: PdFloat);
    cdecl; external NEWTON_API;
  function NewtonCollisionDataPointer(const convexCollision: NewtonCollision): Pointer; cdecl; external NEWTON_API;
  function NewtonCreateCompoundCollision(const world: NewtonWorld; shapeID: integer): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateCompoundCollisionFromMesh(const world: NewtonWorld; const mesh: NewtonMesh; hullTolerance: dFloat;
    shapeID: integer; subShapeID: integer): NewtonCollision; cdecl; external
    NEWTON_API;
  procedure NewtonCompoundCollisionBeginAddRemove(compoundCollision: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonCompoundCollisionAddSubCollision(const compoundCollision: NewtonCollision; convexCollision: NewtonCollision): Pointer; cdecl;
    external NEWTON_API;
  procedure NewtonCompoundCollisionRemoveSubCollision(const compoundCollision: NewtonCollision; collisionNode: Pointer); cdecl; external NEWTON_API;
  procedure NewtonCompoundCollisionRemoveSubCollisionByIndex(const compoundCollision: NewtonCollision; nodeIndex: integer); cdecl; external NEWTON_API;
  procedure NewtonCompoundCollisionSetSubCollisionMatrix(const compoundCollision: NewtonCollision; node: Pointer; matrix: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonCompoundCollisionEndAddRemove(compoundCollision: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonCompoundCollisionGetFirstNode(compoundCollision: NewtonCollision): Pointer; cdecl; external NEWTON_API;
  function NewtonCompoundCollisionGetNextNode(const compoundCollision: NewtonCollision; node: Pointer): Pointer; cdecl; external NEWTON_API;
  function NewtonCompoundCollisionGetNodeByIndex(const compoundCollision: NewtonCollision; index: integer): Pointer; cdecl; external NEWTON_API;
  function NewtonCompoundCollisionGetNodeIndex(const compoundCollision: NewtonCollision; node: Pointer): integer; cdecl; external NEWTON_API;
  function NewtonCompoundCollisionGetCollisionFromNode(const compoundCollision: NewtonCollision; node: Pointer): NewtonCollision;
    cdecl; external NEWTON_API;
  function NewtonCreateFracturedCompoundCollision(world: NewtonWorld; solidMesh: NewtonMesh; shapeID: integer;
    fracturePhysicsMaterialID: integer; pointcloudCount: integer; const vertexCloud: PdFloat; strideInBytes: integer;
    materialID: integer; const textureMatrix: PdFloat; regenerateMainMeshCallback: PNewtonFractureCompoundCollisionReconstructMainMeshCallBack;
    emitFracturedCompound: PNewtonFractureCompoundCollisionOnEmitCompoundFractured;
    emitFracfuredChunk: PNewtonFractureCompoundCollisionOnEmitChunk): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateSceneCollision(const world: NewtonWorld; shapeID: integer): NewtonCollision; cdecl; external NEWTON_API;
  procedure NewtonSceneCollisionBeginAddRemove(sceneCollision: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonSceneCollisionAddSubCollision(sceneCollision: NewtonCollision; collision: NewtonCollision): Pointer; cdecl; external
    NEWTON_API;
  procedure NewtonSceneCollisionSetSubCollisionMatrix(sceneCollision: NewtonCollision; node: Pointer; matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonSceneCollisionEndAddRemove(sceneCollision: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonSceneCollisionGetFirstNode(sceneCollision: NewtonCollision): Pointer; cdecl; external NEWTON_API;
  function NewtonSceneCollisionGetNextNode(sceneCollision: NewtonCollision; node: Pointer): Pointer; cdecl; external NEWTON_API;
  function NewtonSceneCollisionGetCollisionFromNode(sceneCollision: NewtonCollision; node: Pointer): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateUserMeshCollision(const world: NewtonWorld; const minBox: PdFloat; const maxBox: PdFloat;
    userData: Pointer; collideCallback: PNewtonUserMeshCollisionCollideCallback; rayHitCallback: PNewtonUserMeshCollisionRayHitCallback;
    destroyCallback: PNewtonUserMeshCollisionDestroyCallback; getInfoCallback: PNewtonUserMeshCollisionGetCollisionInfo;
    getLocalAABBCallback: PNewtonUserMeshCollisionAABBTest; facesInAABBCallback: PNewtonUserMeshCollisionGetFacesInAABB;
    serializeCallback: PNewtonOnUserCollisionSerializationCallback; shapeID: integer): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateCollisionFromSerialization(const world: NewtonWorld; deserializeFunction: PNewtonDeserializeCallback;
    Handle: NewtonSerializeHandle): NewtonCollision; cdecl; external NEWTON_API;
  procedure NewtonCollisionSerialize(const world: NewtonWorld; const collision: NewtonCollision;
    serializeFunction: PNewtonSerializeCallback; Handle: NewtonSerializeHandle); cdecl; external NEWTON_API;
  procedure NewtonCollisionGetInfo(const collision: NewtonCollision; collisionInfo: PNewtonCollisionInfoRecord); cdecl;
    external NEWTON_API;
  function NewtonCreateHeightFieldCollision(const world: NewtonWorld; Width: integer; Height: integer; gridsDiagonals: integer;
    const elevationMap: PdFloat; const attributeMap: PShortInt; horizontalScale: dFloat; shapeID: integer): NewtonCollision;
    cdecl; external NEWTON_API;
  procedure NewtonHeightFieldSetUserRayCastCallback(const heightfieldCollision: NewtonCollision; rayHitCallback: PNewtonHeightFieldRayCastCallback);
    cdecl; external
    NEWTON_API;
  function NewtonCreateTreeCollision(const world: NewtonWorld; shapeID: integer): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCreateTreeCollisionFromMesh(const world: NewtonWorld; const mesh: NewtonMesh; shapeID: integer): NewtonCollision;
    cdecl; external NEWTON_API;
  procedure NewtonTreeCollisionSetUserRayCastCallback(const treeCollision: NewtonCollision; rayHitCallback: PNewtonCollisionTreeRayCastCallback);
    cdecl; external
    NEWTON_API;
  procedure NewtonTreeCollisionBeginBuild(const treeCollision: NewtonCollision); cdecl; external NEWTON_API;
  procedure NewtonTreeCollisionAddFace(const treeCollision: NewtonCollision; vertexCount: integer; const vertexPtr: PdFloat;
    strideInBytes: integer; faceAttribute: integer); cdecl; external NEWTON_API;
  procedure NewtonTreeCollisionEndBuild(const treeCollision: NewtonCollision; optimize: integer); cdecl; external NEWTON_API;
  function NewtonTreeCollisionGetFaceAtribute(const treeCollision: NewtonCollision; const faceIndexArray: PInteger; indexCount: integer): integer;
    cdecl; external NEWTON_API;
  procedure NewtonTreeCollisionSetFaceAtribute(const treeCollision: NewtonCollision; const faceIndexArray: PInteger;
    indexCount: integer; attribute: integer); cdecl; external NEWTON_API;
  function NewtonTreeCollisionGetVertexListIndexListInAABB(const treeCollision: NewtonCollision; const p0: PdFloat;
    const p1: PdFloat; const vertexArray: PdFloat; vertexCount: PInteger; vertexStrideInBytes: PInteger;
    const indexList: PInteger; maxIndexCount: integer; const faceAttribute: PInteger): integer; cdecl; external NEWTON_API;
  procedure NewtonStaticCollisionSetDebugCallback(const staticCollision: NewtonCollision; userCallback: PNewtonTreeCollisionCallback);
    cdecl; external NEWTON_API;
  function NewtonCollisionCreateInstance(const collision: NewtonCollision): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonCollisionGetType(const collision: NewtonCollision): integer; cdecl; external NEWTON_API;
  procedure NewtonCollisionSetUserData(const collision: NewtonCollision; userData: Pointer); cdecl; external NEWTON_API;
  function NewtonCollisionGetUserData(const collision: NewtonCollision): Pointer; cdecl; external
    NEWTON_API;
  procedure NewtonCollisionSetUserID(const collision: NewtonCollision; id: cardinal); cdecl; external NEWTON_API;
  function NewtonCollisionGetUserID(const collision: NewtonCollision): cardinal; cdecl; external NEWTON_API;
  procedure NewtonCollisionSetMatrix(const collision: NewtonCollision; const matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonCollisionGetMatrix(const collision: NewtonCollision; matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonCollisionSetScale(const collision: NewtonCollision; scaleX: dFloat; scaleY: dFloat; scaleZ: dFloat); cdecl; external NEWTON_API;
  procedure NewtonCollisionGetScale(const collision: NewtonCollision; scaleX: dFloat; scaleY: dFloat; scaleZ: dFloat); cdecl; external NEWTON_API;
  procedure NewtonDestroyCollision(const collision: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonCollisionPointDistance(const world: NewtonWorld; const point: PdFloat; const collision: NewtonCollision;
    const matrix: PdFloat; contact: PdFloat; normal: PdFloat; threadIndex: integer): integer; cdecl; external NEWTON_API;
  function NewtonCollisionClosestPoint(const world: NewtonWorld; const collisionA: NewtonCollision; const matrixA: PdFloat;
    const collisionB: NewtonCollision; const matrixB: PdFloat; contactA: PdFloat; contactB: PdFloat; normalAB: PdFloat; threadIndex: integer): integer;
    cdecl; external
    NEWTON_API;
  function NewtonCollisionCollide(const world: NewtonWorld; maxSize: integer; const collisionA: NewtonCollision; const matrixA: PdFloat; const collisionB: NewtonCollision; const matrixB: PdFloat; const contacts: PdFloat; const normals: PdFloat; const penetration: PdFloat; const AttributeA: PDLong; const AttributeB: PDLong; threadIndex: integer): integer; cdecl; external NEWTON_API;
  function NewtonCollisionCollideContinue(const world: NewtonWorld; maxSize: integer; const timestep: dFloat;
    const collisionA: NewtonCollision; const matrixA: PdFloat; const velocA: PdFloat; const omegaA: PdFloat;
    const collisionB: NewtonCollision; const matrixB: PdFloat; const velocB: PdFloat; const omegaB: PdFloat;
    timeOfImpact: PdFloat; contacts: PdFloat; normals: PdFloat; penetration: PdFloat; threadIndex: integer): integer;
    cdecl; external NEWTON_API;
  procedure NewtonCollisionSupportVertex(const collision: NewtonCollision; const dir: PdFloat; vertex: PdFloat); cdecl; external
    NEWTON_API;
  function NewtonCollisionRayCast(const collision: NewtonCollision; const p0: PdFloat; const p1: PdFloat; normal: PdFloat;
    attribute: PInteger): dFloat; cdecl; external
    NEWTON_API;
  procedure NewtonCollisionCalculateAABB(const collision: NewtonCollision; const matrix: PdFloat; p0: PdFloat; p1: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonCollisionForEachPolygonDo(const collision: NewtonCollision; const matrix: PdFloat;
    callback: PNewtonCollisionIterator; userData: Pointer); cdecl; external NEWTON_API;
  procedure NewtonGetEulerAngle(const matrix: PdFloat; eulersAngles: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonSetEulerAngle(const eulersAngles: PdFloat; matrix: PdFloat); cdecl; external
    NEWTON_API;
  function NewtonCalculateSpringDamperAcceleration(dt: dFloat; ks: dFloat; x: dFloat; kd: dFloat; s: dFloat): dFloat; cdecl; external NEWTON_API;
  function NewtonCreateDynamicBody(pworld: NewtonWorld; collision: NewtonCollision; matrix: PdFloat): NewtonBody; cdecl; external
    NEWTON_API;
  function NewtonCreateKinematicBody(const world: NewtonWorld; const collision: NewtonCollision; const matrix: PdFloat): NewtonBody;
    cdecl; external NEWTON_API;
  procedure NewtonDestroyBody(const body: NewtonBody); cdecl; external NEWTON_API;
  function NewtonBodyGetType(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodyAddForce(const body: NewtonBody; const force: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyAddTorque(const body: NewtonBody; const torque: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyCalculateInverseDynamicsForce(const body: NewtonBody; timestep: dFloat; const desiredVeloc: PdFloat; forceOut: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonBodySetCentreOfMass(const body: NewtonBody; const com: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetMassMatrix(const body: NewtonBody; mass: dFloat; Ixx: dFloat; Iyy: dFloat; Izz: dFloat); cdecl; external
    NEWTON_API;
  procedure NewtonBodySetMassProperties(const body: NewtonBody; mass: dFloat; const collision: NewtonCollision); cdecl; external NEWTON_API;
  procedure NewtonBodySetMatrix(const body: NewtonBody; const matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetMatrixRecursive(const body: NewtonBody; const matrix: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonBodySetMaterialGroupID(const body: NewtonBody; id: integer); cdecl; external NEWTON_API;
  procedure NewtonBodySetContinuousCollisionMode(const body: NewtonBody; state: cardinal); cdecl; external NEWTON_API;
  procedure NewtonBodySetJointRecursiveCollision(const body: NewtonBody; state: cardinal); cdecl; external NEWTON_API;
  procedure NewtonBodySetOmega(const body: NewtonBody; const omega: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetVelocity(const body: NewtonBody; const velocity: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetForce(const body: NewtonBody; const force: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetTorque(const body: NewtonBody; const torque: PdFloat); cdecl;
    external NEWTON_API;
  procedure NewtonBodySetLinearDamping(const body: NewtonBody; linearDamp: dFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetAngularDamping(const body: NewtonBody; const angularDamp: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodySetCollision(const body: NewtonBody; const collision: NewtonCollision); cdecl; external NEWTON_API;
  procedure NewtonBodySetCollisionScale(const body: NewtonBody; scaleX: dFloat; scaleY: dFloat; scaleZ: dFloat); cdecl; external NEWTON_API;
  function NewtonBodyGetSleepState(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodySetSleepState(const body: NewtonBody; state: integer); cdecl; external NEWTON_API;
  function NewtonBodyGetAutoSleep(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodySetAutoSleep(const body: NewtonBody; state: integer); cdecl; external NEWTON_API;
  function NewtonBodyGetFreezeState(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodySetFreezeState(const body: NewtonBody; state: integer); cdecl; external NEWTON_API;
  procedure NewtonBodySetDestructorCallback(const body: NewtonBody; callback: PNewtonBodyDestructor); cdecl; external NEWTON_API;
  function NewtonBodyGetDestructorCallback(const body: NewtonBody): PNewtonBodyDestructor; cdecl; external NEWTON_API;
  procedure NewtonBodySetTransformCallback(const body: NewtonBody; callback: PNewtonSetTransform); cdecl; external NEWTON_API;
  function NewtonBodyGetTransformCallback(const body: NewtonBody): PNewtonSetTransform; cdecl; external NEWTON_API;
  procedure NewtonBodySetForceAndTorqueCallback(const body: NewtonBody; callback: PNewtonApplyForceAndTorque); cdecl; external NEWTON_API;
  function NewtonBodyGetForceAndTorqueCallback(const body: NewtonBody): PNewtonApplyForceAndTorque; cdecl; external NEWTON_API;
  function NewtonBodyGetID(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodySetUserData(const body: NewtonBody; userData: Pointer); cdecl; external NEWTON_API;
  function NewtonBodyGetUserData(const body: NewtonBody): Pointer; cdecl; external NEWTON_API;
  function NewtonBodyGetWorld(const body: NewtonBody): NewtonWorld; cdecl; external NEWTON_API;
  function NewtonBodyGetCollision(const body: NewtonBody): NewtonCollision; cdecl; external NEWTON_API;
  function NewtonBodyGetMaterialGroupID(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  function NewtonBodyGetContinuousCollisionMode(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  function NewtonBodyGetJointRecursiveCollision(const body: NewtonBody): integer; cdecl; external NEWTON_API;
  procedure NewtonBodyGetMatrix(const body: NewtonBody; matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetRotation(const body: NewtonBody; rotation: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetMassMatrix(const body: NewtonBody; mass: PdFloat; Ixx: PdFloat; Iyy: PdFloat; Izz: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonBodyGetInvMass(const body: NewtonBody; invMass: PdFloat; invIxx: PdFloat; invIyy: PdFloat; invIzz: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonBodyGetInertiaMatrix(const body: NewtonBody; inertiaMatrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetInvInertiaMatrix(const body: NewtonBody; invInertiaMatrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetOmega(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetVelocity(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetForce(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetTorque(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetForceAcc(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetTorqueAcc(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetCentreOfMass(const body: NewtonBody; com: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetPointVelocity(const body: NewtonBody; const point: PdFloat; velocOut: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyAddImpulse(const body: NewtonBody; const pointDeltaVeloc: PdFloat; const pointPosit: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyApplyImpulseArray(const body: NewtonBody; impuleCount: integer; strideinBytes: integer;
    const impulseArray: PdFloat; const pointArray: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyApplyImpulsePair(const body: NewtonBody; linearImpulse: PdFloat; angularImpulse: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonBodyIntegrateVelocity(const body: NewtonBody; timestep: dFloat); cdecl; external NEWTON_API;
  function NewtonBodyGetLinearDamping(const body: NewtonBody): dFloat; cdecl; external NEWTON_API;
  procedure NewtonBodyGetAngularDamping(const body: NewtonBody; vector: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBodyGetAABB(const body: NewtonBody; p0: PdFloat; p1: PdFloat); cdecl; external NEWTON_API;
  function NewtonBodyGetFirstJoint(const body: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  function NewtonBodyGetNextJoint(const body: NewtonBody; const joint: NewtonJoint): NewtonJoint; cdecl; external NEWTON_API;
  function NewtonBodyGetFirstContactJoint(const body: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  function NewtonBodyGetNextContactJoint(const body: NewtonBody; const contactJoint: NewtonJoint): NewtonJoint; cdecl; external NEWTON_API;
  function NewtonContactJointGetFirstContact(const contactJoint: NewtonJoint): Pointer; cdecl; external NEWTON_API;
  function NewtonContactJointGetNextContact(const contactJoint: NewtonJoint; contact: Pointer): Pointer; cdecl; external NEWTON_API;
  function NewtonContactJointGetContactCount(const contactJoint: NewtonJoint): integer; cdecl; external NEWTON_API;
  procedure NewtonContactJointRemoveContact(const contactJoint: NewtonJoint; contact: Pointer); cdecl; external NEWTON_API;
  function NewtonContactGetMaterial(const contact: Pointer): NewtonMaterial; cdecl; external NEWTON_API;
  function NewtonJointGetUserData(const joint: NewtonJoint): Pointer; cdecl; external
    NEWTON_API;
  procedure NewtonJointSetUserData(const joint: NewtonJoint; userData: Pointer); cdecl; external NEWTON_API;
  function NewtonJointGetBody0(const joint: NewtonJoint): NewtonBody; cdecl; external NEWTON_API;
  function NewtonJointGetBody1(const joint: NewtonJoint): NewtonBody; cdecl; external NEWTON_API;
  procedure NewtonJointGetInfo(const joint: NewtonJoint; info: PNewtonJointRecord); cdecl; external NEWTON_API;
  function NewtonJointGetCollisionState(const joint: NewtonJoint): integer; cdecl; external NEWTON_API;
  procedure NewtonJointSetCollisionState(const joint: NewtonJoint; state: integer); cdecl; external NEWTON_API;
  function NewtonJointGetStiffness(const joint: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  procedure NewtonJointSetStiffness(const joint: NewtonJoint; state: dFloat); cdecl; external NEWTON_API;
  procedure NewtonDestroyJoint(const world: NewtonWorld; const joint: NewtonJoint); cdecl; external NEWTON_API;
  procedure NewtonJointSetDestructor(const joint: NewtonJoint; callback: PNewtonConstraintDestructor); cdecl; external NEWTON_API;
  function NewtonCreateDeformableMesh(const world: NewtonWorld; mesh: NewtonMesh; shapeID: integer): NewtonCollision; cdecl; external NEWTON_API;
  procedure NewtonDeformableMeshSetPlasticity(deformableMesh: NewtonCollision; plasticity: dFloat); cdecl; external NEWTON_API;
  procedure NewtonDeformableMeshSetStiffness(deformableMesh: NewtonCollision; stiffness: dFloat); cdecl; external NEWTON_API;
  procedure NewtonDeformableMeshSetSkinThickness(deformableMesh: NewtonCollision; skinThickness: dFloat); cdecl; external NEWTON_API;
  function NewtonCreateDeformableBody(const world: NewtonWorld; const deformableMesh: NewtonCollision; const matrix: PdFloat): NewtonBody;
    cdecl; external NEWTON_API;
  procedure NewtonDeformableMeshUpdateRenderNormals(const deformableMesh: NewtonCollision); cdecl; external NEWTON_API;
  function NewtonDeformableMeshGetVertexCount(const deformableMesh: NewtonCollision): integer; cdecl; external NEWTON_API;
  procedure NewtonDeformableMeshGetVertexStreams(const deformableMesh: NewtonCollision; vertexStrideInBytes: integer;
    vertex: PdFloat; normalStrideInBytes: integer; normal: PdFloat; uvStrideInBytes0: integer; uv0: PdFloat; uvStrideInBytes1: integer; uv1: PdFloat);
    cdecl; external NEWTON_API;
  function NewtonDeformableMeshGetFirstSegment(const deformableMesh: NewtonCollision): NewtonDeformableMeshSegment; cdecl; external NEWTON_API;
  function NewtonDeformableMeshGetNextSegment(const deformableMesh: NewtonCollision;
    const segment: NewtonDeformableMeshSegment): NewtonDeformableMeshSegment; cdecl; external NEWTON_API;
  function NewtonDeformableMeshSegmentGetMaterialID(const deformableMesh: NewtonCollision; const segment: NewtonDeformableMeshSegment): integer;
    cdecl; external NEWTON_API;
  function NewtonDeformableMeshSegmentGetIndexCount(const deformableMesh: NewtonCollision; const segment: NewtonDeformableMeshSegment): integer;
    cdecl; external NEWTON_API;
  function NewtonDeformableMeshSegmentGetIndexList(const deformableMesh: NewtonCollision; const segment: NewtonDeformableMeshSegment): PSmallInt;
    cdecl; external NEWTON_API;
  function NewtonConstraintCreateBall(const world: NewtonWorld; const pivotPoint: PdFloat; const childBody: NewtonBody;
    const parentBody: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  procedure NewtonBallSetUserCallback(const ball: NewtonJoint; callback: PNewtonBallCallback); cdecl; external NEWTON_API;
  procedure NewtonBallGetJointAngle(const ball: NewtonJoint; angle: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBallGetJointOmega(const ball: NewtonJoint; omega: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonBallGetJointForce(const ball: NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonBallSetConeLimits(const ball: NewtonJoint; const pin: PdFloat; maxConeAngle: dFloat; maxTwistAngle: dFloat);
    cdecl; external NEWTON_API;
  function NewtonConstraintCreateHinge(const world: NewtonWorld; const pivotPoint: PdFloat; const pinDir: PdFloat;
    const childBody: NewtonBody; const parentBody: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  procedure NewtonHingeSetUserCallback(const hinge: NewtonJoint; callback: PNewtonHingeCallback); cdecl; external NEWTON_API;
  function NewtonHingeGetJointAngle(const hinge: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonHingeGetJointOmega(const hinge: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  procedure NewtonHingeGetJointForce(const hinge: NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
  function NewtonHingeCalculateStopAlpha(const hinge: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;
    cdecl; external NEWTON_API;
  function NewtonConstraintCreateSlider(const world: NewtonWorld; const pivotPoint: PdFloat; const pinDir: PdFloat;
    const childBody: NewtonBody; const parentBody: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  procedure NewtonSliderSetUserCallback(const slider: NewtonJoint; callback: PNewtonSliderCallback); cdecl; external NEWTON_API;
  function NewtonSliderGetJointPosit(const slider: NewtonJoint): dFloat; cdecl; external
    NEWTON_API;
  function NewtonSliderGetJointVeloc(const slider: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  procedure NewtonSliderGetJointForce(const slider: NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
  function NewtonSliderCalculateStopAccel(const slider: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; position: dFloat): dFloat;
    cdecl; external NEWTON_API;
  function NewtonConstraintCreateCorkscrew(const world: NewtonWorld; const pivotPoint: PdFloat; const pinDir: PdFloat;
    const childBody: NewtonBody; const parentBody: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  procedure NewtonCorkscrewSetUserCallback(const corkscrew: NewtonJoint; callback: PNewtonCorkscrewCallback); cdecl; external NEWTON_API;
  function NewtonCorkscrewGetJointPosit(const corkscrew: NewtonJoint): dFloat; cdecl;
    external NEWTON_API;
  function NewtonCorkscrewGetJointAngle(const corkscrew: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonCorkscrewGetJointVeloc(const corkscrew: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonCorkscrewGetJointOmega(const corkscrew: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  procedure NewtonCorkscrewGetJointForce(const corkscrew: NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
  function NewtonCorkscrewCalculateStopAlpha(const corkscrew: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;
    cdecl; external NEWTON_API;
  function NewtonCorkscrewCalculateStopAccel(const corkscrew: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc;
    position: dFloat): dFloat; cdecl; external NEWTON_API;
  function NewtonConstraintCreateUniversal(const world: NewtonWorld; const pivotPoint: PdFloat; const pinDir0: PdFloat;
    const pinDir1: PdFloat; const childBody: NewtonBody; const parentBody: NewtonBody): NewtonJoint; cdecl; external NEWTON_API;
  procedure NewtonUniversalSetUserCallback(const universal: NewtonJoint; callback: PNewtonUniversalCallback); cdecl; external NEWTON_API;
  function NewtonUniversalGetJointAngle0(const universal: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonUniversalGetJointAngle1(const universal: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonUniversalGetJointOmega0(const universal: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  function NewtonUniversalGetJointOmega1(const universal: NewtonJoint): dFloat; cdecl; external NEWTON_API;
  procedure NewtonUniversalGetJointForce(const universal: NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
  function NewtonUniversalCalculateStopAlpha0(const universal: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;
    cdecl; external NEWTON_API;
  function NewtonUniversalCalculateStopAlpha1(const universal: NewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;
    cdecl; external NEWTON_API;
  function NewtonConstraintCreateUpVector(const world: NewtonWorld; const pinDir: PdFloat; const body: NewtonBody): NewtonJoint;
    cdecl; external NEWTON_API;
  procedure NewtonUpVectorGetPin(const upVector: NewtonJoint; pin: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonUpVectorSetPin(const upVector: NewtonJoint; const pin: PdFloat); cdecl; external NEWTON_API;
  function NewtonConstraintCreateUserJoint(const world: NewtonWorld; maxDOF: integer; callback: PNewtonUserBilateralCallback;
    getInfo: PNewtonUserBilateralGetInfoCallback; const childBody: NewtonBody; const parentBody: NewtonBody): NewtonJoint; cdecl; external
    NEWTON_API;
  procedure NewtonUserJointSetFeedbackCollectorCallback(const joint: NewtonJoint; getFeedback: PNewtonUserBilateralCallback);
    cdecl; external NEWTON_API;
  procedure NewtonUserJointAddLinearRow(const joint: NewtonJoint; const pivot0: PdFloat; const pivot1: PdFloat; const dir: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonUserJointAddAngularRow(const joint: NewtonJoint; relativeAngle: dFloat; const dir: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonUserJointAddGeneralRow(const joint: NewtonJoint; const jacobian0: PdFloat; const jacobian1: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonUserJointSetRowMinimumFriction(const joint: NewtonJoint; friction: dFloat); cdecl; external NEWTON_API;
  procedure NewtonUserJointSetRowMaximumFriction(const joint: NewtonJoint; friction: dFloat); cdecl; external NEWTON_API;
  procedure NewtonUserJointSetRowAcceleration(const joint: NewtonJoint; acceleration: dFloat); cdecl; external NEWTON_API;
  procedure NewtonUserJointSetRowSpringDamperAcceleration(const joint: NewtonJoint; springK: dFloat; springD: dFloat); cdecl; external NEWTON_API;
  procedure NewtonUserJointSetRowStiffness(const joint: NewtonJoint; stiffness: dFloat); cdecl; external NEWTON_API;
  function NewtonUserJointGetRowForce(const joint: NewtonJoint; row: integer): dFloat; cdecl; external NEWTON_API;
  procedure NewtonUserJointSetSolver(const joint: NewtonJoint; solver: integer; maxContactJoints: integer); cdecl; external NEWTON_API;
  function NewtonMeshCreate(const world: NewtonWorld): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateFromMesh(const mesh: NewtonMesh): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateFromCollision(const collision: NewtonCollision): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateConvexHull(const world: NewtonWorld; pointCount: integer; const vertexCloud: PdFloat;
    strideInBytes: integer; tolerance: dFloat): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateDelaunayTetrahedralization(const world: NewtonWorld; pointCount: integer; const vertexCloud: PdFloat;
    strideInBytes: integer; materialID: integer; const textureMatrix: PdFloat): NewtonMesh; cdecl; external
    NEWTON_API;
  function NewtonMeshCreateVoronoiConvexDecomposition(const world: NewtonWorld; pointCount: integer; const vertexCloud: PdFloat;
    strideInBytes: integer; materialID: integer; const textureMatrix: PdFloat; boderConvexSize: dFloat): NewtonMesh; cdecl; external NEWTON_API;
  procedure NewtonMeshDestroy(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  procedure NewtonMeshSaveOFF(const mesh: NewtonMesh; const filename: PChar); cdecl; external NEWTON_API;
  function NewtonMeshLoadOFF(const world: NewtonWorld; const filename: PChar): NewtonMesh; cdecl; external NEWTON_API;
  procedure NewtonMesApplyTransform(const mesh: NewtonMesh; const matrix: PdFloat); cdecl; external NEWTON_API;
  procedure NewtonMeshCalculateOOBB(const mesh: NewtonMesh; matrix: PdFloat; x: PdFloat; y: PdFloat; z: PdFloat); cdecl; external
    NEWTON_API;
  procedure NewtonMeshCalculateVertexNormals(const mesh: NewtonMesh; angleInRadians: dFloat); cdecl; external NEWTON_API;
  procedure NewtonMeshApplySphericalMapping(const mesh: NewtonMesh; material: integer); cdecl; external NEWTON_API;
  procedure NewtonMeshApplyBoxMapping(const mesh: NewtonMesh; front: integer; side: integer; top: integer); cdecl; external NEWTON_API;
  procedure NewtonMeshApplyCylindricalMapping(const mesh: NewtonMesh; cylinderMaterial: integer; capMaterial: integer); cdecl; external NEWTON_API;
  function NewtonMeshIsOpenMesh(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  procedure NewtonMeshFixTJoints(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  procedure NewtonMeshPolygonize(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  procedure NewtonMeshTriangulate(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  function NewtonMeshUnion(const mesh: NewtonMesh; const clipper: NewtonMesh; const clipperMatrix: PdFloat): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshDifference(const mesh: NewtonMesh; const clipper: NewtonMesh; const clipperMatrix: PdFloat): NewtonMesh;
    cdecl; external NEWTON_API;
  function NewtonMeshIntersection(const mesh: NewtonMesh; const clipper: NewtonMesh; const clipperMatrix: PdFloat): NewtonMesh;
    cdecl; external NEWTON_API;
  procedure NewtonMeshClip(const mesh: NewtonMesh; const clipper: NewtonMesh; const clipperMatrix: PdFloat;
    topMesh: NewtonMesh; bottomMesh: NewtonMesh); cdecl; external NEWTON_API;
  function NewtonMeshSimplify(const mesh: NewtonMesh; maxVertexCount: integer; reportPrograssCallback: PNewtonReportProgress): NewtonMesh;
    cdecl; external NEWTON_API;
  function NewtonMeshApproximateConvexDecomposition(const mesh: NewtonMesh; maxConcavity: dFloat; backFaceDistanceFactor: dFloat;
    maxCount: integer; maxVertexPerHull: integer; reportPrograssCallback: PNewtonReportProgress): NewtonMesh; cdecl; external NEWTON_API;
  procedure NewtonRemoveUnusedVertices(const mesh: NewtonMesh; vertexRemapTable: PInteger); cdecl; external NEWTON_API;
  procedure NewtonMeshBeginFace(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  procedure NewtonMeshAddFace(const mesh: NewtonMesh; vertexCount: integer; const vertex: PdFloat; strideInBytes: integer;
    materialIndex: integer); cdecl; external NEWTON_API;
  procedure NewtonMeshEndFace(const mesh: NewtonMesh); cdecl; external NEWTON_API;
  procedure NewtonMeshBuildFromVertexListIndexList(const mesh: NewtonMesh; faceCount: integer; const faceIndexCount: PInteger;
    const faceMaterialIndex: PInteger; const vertex: PdFloat; vertexStrideInBytes: integer; const vertexIndex: PInteger;
    const normal: PdFloat; normalStrideInBytes: integer; const normalIndex: PInteger; const uv0: PdFloat;
    uv0StrideInBytes: integer; const uv0Index: PInteger; const uv1: PdFloat; uv1StrideInBytes: integer; const uv1Index: PInteger);
    cdecl; external NEWTON_API;
  procedure NewtonMeshGetVertexStreams(const mesh: NewtonMesh; vertexStrideInByte: integer; vertex: PdFloat;
    normalStrideInByte: integer; normal: PdFloat; uvStrideInByte0: integer; uv0: PdFloat; uvStrideInByte1: integer; uv1: PdFloat);
    cdecl; external NEWTON_API;
  procedure NewtonMeshGetIndirectVertexStreams(const mesh: NewtonMesh; vertexStrideInByte: integer; vertex: PdFloat;
    vertexIndices: PInteger; vertexCount: PInteger; normalStrideInByte: integer; normal: PdFloat; normalIndices: PInteger;
    normalCount: PInteger; uvStrideInByte0: integer; uv0: PdFloat; uvIndices0: PInteger; uvCount0: PInteger;
    uvStrideInByte1: integer; uv1: PdFloat; uvIndices1: PInteger; uvCount1: PInteger);
    cdecl; external NEWTON_API;
  function NewtonMeshBeginHandle(const mesh: NewtonMesh): NewtonMeshHandle; cdecl; external NEWTON_API;
  procedure NewtonMeshEndHandle(const mesh: NewtonMesh; handle: NewtonMeshHandle); cdecl; external NEWTON_API;
  function NewtonMeshFirstMaterial(const mesh: NewtonMesh; handle: NewtonMeshHandle): integer; cdecl; external NEWTON_API;
  function NewtonMeshNextMaterial(const mesh: NewtonMesh; handle: NewtonMeshHandle; materialId: integer): integer; cdecl; external NEWTON_API;
  function NewtonMeshMaterialGetMaterial(const mesh: NewtonMesh; handle: NewtonMeshHandle; materialId: integer): integer; cdecl;
    external NEWTON_API;
  function NewtonMeshMaterialGetIndexCount(const mesh: NewtonMesh; handle: NewtonMeshHandle; materialId: integer): integer; cdecl; external NEWTON_API;
  procedure NewtonMeshMaterialGetIndexStream(const mesh: NewtonMesh; handle: NewtonMeshHandle; materialId: integer; index: PInteger);
    cdecl; external NEWTON_API;
  procedure NewtonMeshMaterialGetIndexStreamShort(const mesh: NewtonMesh; handle: NewtonMeshHandle; materialId: integer; index: PSmallInt);
    cdecl; external NEWTON_API;
  function NewtonMeshCreateFirstSingleSegment(const mesh: NewtonMesh): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateNextSingleSegment(const mesh: NewtonMesh; const segment: NewtonMesh): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshCreateFirstLayer(const mesh: NewtonMesh): NewtonMesh; cdecl; external
    NEWTON_API;
  function NewtonMeshCreateNextLayer(const mesh: NewtonMesh; const segment: NewtonMesh): NewtonMesh; cdecl; external NEWTON_API;
  function NewtonMeshGetTotalFaceCount(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetTotalIndexCount(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  procedure NewtonMeshGetFaces(const mesh: NewtonMesh; faceIndexCount: PInteger; faceMaterial: PInteger; faceIndices: Pointer);
    cdecl; external NEWTON_API;
  function NewtonMeshGetPointCount(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetPointStrideInByte(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetPointArray(const mesh: NewtonMesh): PdFloat64; cdecl; external NEWTON_API;
  function NewtonMeshGetNormalArray(const mesh: NewtonMesh): PdFloat64; cdecl; external NEWTON_API;
  function NewtonMeshGetUV0Array(const mesh: NewtonMesh): PdFloat64; cdecl; external NEWTON_API;
  function NewtonMeshGetUV1Array(const mesh: NewtonMesh): PdFloat64; cdecl; external NEWTON_API;
  function NewtonMeshGetVertexCount(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetVertexStrideInByte(const mesh: NewtonMesh): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetVertexArray(const mesh: NewtonMesh): PdFloat64; cdecl; external NEWTON_API;
  function NewtonMeshGetFirstVertex(const mesh: NewtonMesh): NewtonMeshVertex; cdecl; external NEWTON_API;
  function NewtonMeshGetNextVertex(const mesh: NewtonMesh; const vertex: NewtonMeshVertex): NewtonMeshVertex; cdecl; external
    NEWTON_API;
  function NewtonMeshGetVertexIndex(const mesh: NewtonMesh; const vertex: NewtonMeshVertex): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetFirstPoint(const mesh: NewtonMesh): NewtonMeshPoint; cdecl; external NEWTON_API;
  function NewtonMeshGetNextPoint(const mesh: NewtonMesh; const point: NewtonMeshPoint): NewtonMeshPoint; cdecl; external NEWTON_API;
  function NewtonMeshGetPointIndex(const mesh: NewtonMesh; const point: NewtonMeshPoint): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetVertexIndexFromPoint(const mesh: NewtonMesh; const point: NewtonMeshPoint): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetFirstEdge(const mesh: NewtonMesh): NewtonMeshEdge; cdecl; external NEWTON_API;
  function NewtonMeshGetNextEdge(const mesh: NewtonMesh; const edge: NewtonMeshEdge): NewtonMeshEdge; cdecl; external NEWTON_API;
  procedure NewtonMeshGetEdgeIndices(const mesh: NewtonMesh; const edge: NewtonMeshEdge; v0: PInteger; v1: PInteger); cdecl; external
    NEWTON_API;
  function NewtonMeshGetFirstFace(const mesh: NewtonMesh): NewtonMeshFace;
    cdecl; external NEWTON_API;
  function NewtonMeshGetNextFace(const mesh: NewtonMesh; const face: NewtonMeshFace): NewtonMeshFace; cdecl; external NEWTON_API;
  function NewtonMeshIsFaceOpen(const mesh: NewtonMesh; const face: NewtonMeshFace): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetFaceMaterial(const mesh: NewtonMesh; const face: NewtonMeshFace): integer; cdecl; external NEWTON_API;
  function NewtonMeshGetFaceIndexCount(const mesh: NewtonMesh; const face: NewtonMeshFace): integer; cdecl; external NEWTON_API;
  procedure NewtonMeshGetFaceIndices(const mesh: NewtonMesh; const face: NewtonMeshFace; indices: PInteger); cdecl; external NEWTON_API;
  procedure NewtonMeshGetFacePointIndices(const mesh: NewtonMesh; const face: NewtonMeshFace; indices: PInteger); cdecl; external NEWTON_API;
  procedure NewtonMeshCalculateFaceNormal(const mesh: NewtonMesh; const face: NewtonMeshFace; normal: PdFloat64); cdecl; external NEWTON_API;
  procedure NewtonMeshSetFaceMaterial(const mesh: NewtonMesh; const face: NewtonMeshFace; matId: integer); cdecl; external NEWTON_API;

implementation
end.
