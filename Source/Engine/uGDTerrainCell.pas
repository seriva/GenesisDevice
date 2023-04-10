  unit uGDTerrainCell;

{$MODE Delphi}

interface

uses
  SysUtils,
  Types,
  dglOpenGL,
  uGDConstants,
  uGDTerrain,
  uGDTypes,
  uGDGLWrappers,
  uGDTypesGenerics,
  uGDBaseCell;

type
  TGDTerrainCell = class (TGDBaseCell)
  private
    FTerrain     : TGDTerrain;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
    FIndexes     : TGDGLIndexBuffer;
    FTrisCount   : Integer;

    procedure CalculateBoundingBox();
  public
    constructor Create( aTerrain : TGDTerrain; aStartX, aStartY, aEndX, aEndY : Integer);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;
  end;

implementation

uses
  uGDEngine;

constructor TGDTerrainCell.Create( aTerrain : TGDTerrain; aStartX, aStartY, aEndX, aEndY : Integer);
var
  iX, iY: Integer;
  iIdxs : TGDIndexList;
begin
  OjectType    := SO_TERRAINCELL;
  FTerrain     := aTerrain;

  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;
  CalculateBoundingBox();

  iIdxs := TGDIndexList.Create();
  for iY := (FStartPoint.Y-1) to FEndPoint.Y-2 do
  begin
    for iX := (FStartPoint.X-1) to FEndPoint.X-2 do
    begin
      iIdxs.Add((iX * FTerrain.TerrainWidth) + iY);
      iIdxs.Add(((iX+1) * FTerrain.TerrainWidth) + iY+1);
      iIdxs.Add(((iX+1) * FTerrain.TerrainWidth) + iY);

      iIdxs.Add((iX * FTerrain.TerrainWidth) + iY);
      iIdxs.Add((iX * FTerrain.TerrainWidth) + iY+1);
      iIdxs.Add(((iX+1) * FTerrain.TerrainWidth) + iY+1);

      FTrisCount := FTrisCount + 2;
    end;
  end;

  FIndexes := TGDGLIndexBuffer.Create();
  FIndexes.Bind();
  FIndexes.Update(iIdxs, GL_STATIC_DRAW);
  FIndexes.Unbind();
  FreeAndNil(iIdxs);
end;


destructor  TGDTerrainCell.Destroy();
begin
  FreeAndNil(FIndexes);
  Inherited;
end;


procedure TGDTerrainCell.CalculateBoundingBox();
var
  iX,iY : Integer;
begin
  BoundingBox.Min.Reset(  FTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.X,
                         999999999999999,
                         FTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.Z);
  BoundingBox.Max.Reset( FTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.X,
                         -999999999999999,
                         FTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.Z);


  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If FTerrain.GetPoint( iX,iY ).Vertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.setY(FTerrain.GetPoint( iX,iY ).Vertex.Y);
      If FTerrain.GetPoint( iX,iY ).Vertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.setY(FTerrain.GetPoint( iX,iY ).Vertex.Y);
    end;
  end;
  BoundingBox.CalculateCenter();
end;


procedure TGDTerrainCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iX,iY : Integer;
  iV1, iV2 : TGDVector;
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          FIndexes.Bind();
                          FIndexes.Render(GL_TRIANGLES);
                          FIndexes.Unbind();
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : begin
                          With FTerrain do
                          begin
                            for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
                            begin
                              iX := (FStartPoint.X-1);
                              for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
                              begin
                                iV1 := GetPoint(iX,iY).Vertex.Copy();
                                iV2 := GetPoint(iX,iY).Normal.Copy();
                                iV2 *= R_NORMAL_LENGTH;
                                iV2 += iV1;
                                GDRenderer.AddLine(iV1, iV2);
                              end;
                            end;
                          end;
                        end;
    end;
end;

end.
