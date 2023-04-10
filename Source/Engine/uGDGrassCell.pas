unit uGDGrassCell;

{$MODE Delphi}

interface

uses
  SysUtils,
  Types,
  dglOpenGL,
  uGDConstants,
  uGDGLWrappers,
  uGDFoliage,
  uGDTypes,
  uGDTerrain,
  uGDBaseCell;

type
  TGDGrassPartical = record
    Normal    : TGDVector;
    Animation : TGDVector;
    UVs       : array[0..7] of byte;
    Verts     : array[0..7] of TGDVector;

    procedure InitGrassPartical(  aMove, aScale, aRotate : TGDVector  );
    procedure Render();
  end;


  TGDGrassCell = class (TGDBaseCell)
  private
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
    FDisplayList : TGDGLDisplayList;
    FTrisCount   : Integer;

    procedure CalculateBoundingBox(aTerrain : TGDTerrain);
  public
    property TrisCount : Integer Read FTrisCount;

    constructor Create(aTerrain : TGDTerrain; aLayer : TGDLayer; aStartX, aStartY, aEndX, aEndY : Integer);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;
  end;

implementation

uses
  uGDEngine;


procedure TGDGrassPartical.InitGrassPartical( aMove, aScale, aRotate : TGDVector );
var
  iM1, iM2  : TGDMatrix;
  iI : Integer;
begin
  Animation.Reset(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100));
  Normal.reset(0,1,0);

  Verts[0].Reset( -50, -50, 0 ); UVs[0] := 0;
  Verts[1].Reset( -50, 50, 0 );  UVs[1] := 1;
  Verts[2].Reset( 50, 50, 0 );   UVs[2] := 2;
  Verts[3].Reset( 50, -50, 0 );  UVs[3] := 3;
  Verts[4].Reset( 0, -50, -50 ); UVs[4] := 0;
  Verts[5].Reset( 0,  50, -50 ); UVs[5] := 1;
  Verts[6].Reset( 0, 50, 50 );   UVs[6] := 2;
  Verts[7].Reset( 0, -50, 50 );  UVs[7] := 3;

  iM1.CreateRotation( Vector(0,Random(360),0) );
  iM2.CreateRotation( aRotate );
  iM2.ApplyToVector(Normal);

  for iI := 0 to 7 do
  begin
    iM1.ApplyToVector(Verts[iI]);
    iM2.ApplyToVector(Verts[iI]);
    Verts[iI] := ((Verts[iI] * aScale) / 100) + aMove;
  end;
end;


procedure TGDGrassPartical.Render();
var
  iI : Integer;
begin
  glColor3fv(Animation.ArrayPointer());
  glNormal3fv(Normal.ArrayPointer);

  for iI := 0 to 7 do
  begin
    glTexCoord1i(UVs[iI]);
    glVertex3fv(Verts[iI].ArrayPointer);
  end;
end;


constructor TGDGrassCell.Create(aTerrain : TGDTerrain; aLayer : TGDLayer; aStartX, aStartY, aEndX, aEndY : Integer );
var
  iI, iJ : Integer;
  iX, iY : Integer;
  iPos, iScale, iRot : TGDVector;
  iParticalLists : array of array of TGDGrassPartical;
  iParticalCount : array of Integer;
  iHeight, iRandomHeightScale : Double;
  iGrassItem : TGDGrassItem;
begin
  OjectType    := SO_GRASSCELL;
  FTrisCount   := 0;
  FDisplayList := TGDGLDisplayList.Create();

  randomize();

  //set the boundingbox
  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;
  iRot.Reset(0,0,0);
  CalculateBoundingBox(aTerrain);

  //create temp arrays
  SetLength( iParticalCount, aLayer.LayerItems.Count);
  For iI := 0 to Length(iParticalCount)-1 do
    iParticalCount[iI] :=  Round( (GDSettings.FoliageDensity * TGDGrassItem( aLayer.LayerItems.Items[iI] ).CoverOfTotal) / 100 );

  SetLength( iParticalLists, aLayer.LayerItems.Count);
  for iY := (FStartPoint.Y-1) to FEndPoint.Y-2 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-2 do
    begin
      If aLayer.CheckMap(iX,iY) then
      begin
        for iI := 0 to aLayer.LayerItems.Count-1 do
        begin
          iGrassItem := TGDGrassItem(aLayer.LayerItems.Items[iI]);
          for iJ := 0 to iParticalCount[iI]-1 do
          begin
            //position
            iPos.Reset( aTerrain.GetPoint( iX, iY ).Vertex.X + random(aTerrain.TriangleSize) , 0,
                        aTerrain.GetPoint( iX, iY ).Vertex.Z + random(aTerrain.TriangleSize) );
            aTerrain.GetHeight(iPos.X, iPos.Z, iHeight  );

            iRandomHeightScale := iGrassItem.Scale.Y + Random( Round( iGrassItem.RandomScale.Y ) );
            iPos.Y := iHeight + ((50 * iRandomHeightScale) / 100 );

            //scale
            iScale.Reset( iGrassItem.Scale.X + random( Round( iGrassItem.RandomScale.X )),
                          iRandomHeightScale,
                          iGrassItem.Scale.Z + random( Round( iGrassItem.RandomScale.Z )) );

            //rotation
            if iGrassItem.TerrainRotation then
            	aTerrain.GetRotation( iPos.X, iPos.Z, iRot );

            //create partical
            if ((iHeight > aLayer.LowerLimit) and (iHeight < aLayer.UpperLimit)) then
            begin
              SetLength(iParticalLists[iI], Length(iParticalLists[iI]) + 1 );
              iParticalLists[iI,Length(iParticalLists[iI])-1].InitGrassPartical( iPos, iScale, iRot );
            end;

            FTrisCount := FTrisCount + 4;
          end;
        end;
      end;
    end;
  end;

  //create displaylist for this cell
  FDisplayList.StartList();
  For iI := 0 to Length(iParticalLists)-1 do
  begin
    TGDGrassItem(aLayer.LayerItems.Items[iI]).Texture.BindTexture( GL_TEXTURE0 );
    glBegin(GL_QUADS);
    for iJ := 0 to Length( iParticalLists[iI] )-1 do
      iParticalLists[iI,iJ].Render();
    glEnd();
  end;
  FDisplayList.EndList();

  //delete temp arrays
  For iI := 0 to Length(iParticalLists)-1 do
    SetLength( iParticalLists[iI], 0);
  SetLength( iParticalLists, 0);
  SetLength( iParticalCount, 0);
end;


destructor  TGDGrassCell.Destroy();
begin
  FTrisCount := 0;
  FreeAndNil(FDisplayList);
  inherited;
end;


procedure TGDGrassCell.CalculateBoundingBox(aTerrain : TGDTerrain);
var
  iX,iY : Integer;
begin
  BoundingBox.Min.Reset( aTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.X,
                         999999999999999,
                         aTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.Z);

  BoundingBox.Max.Reset( aTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.X,
                         -999999999999999,
                         aTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.Z);

  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If aTerrain.GetPoint( iX,iY ).Vertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.setY( aTerrain.GetPoint( iX,iY ).Vertex.Y);
      If aTerrain.GetPoint( iX,iY ).Vertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.setY( aTerrain.GetPoint( iX,iY ).Vertex.Y);
    end;
  end;
  BoundingBox.Max.SetY( BoundingBox.Max.Y+150 );
  BoundingBox.CalculateCenter();
end;


procedure TGDGrassCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          FDisplayList.CallList();
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : //grass doesn`t have normals
    end;
end;

end.
