unit uGDWaterCell;

{$MODE Delphi}

interface

uses
  SysUtils,
  dglOpenGL,
  uGDWater,
  uGDConstants,
  uGDGLWrappers,
  uGDTypes,
  uGDBaseCell;

type
  TGDWaterCell = class (TGDBaseCell)
  private
    FIndexes : TGDGLIndexBuffer;
  public
    constructor Create(aWater : TGDWater; aStartX, aStartY, aEndX, aEndY, aStartU, aStartV, aEndU, aEndV : Double);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;
  end;

implementation

constructor TGDWaterCell.Create(aWater : TGDWater; aStartX, aStartY, aEndX, aEndY, aStartU, aStartV, aEndU, aEndV : Double);
var
  iIdxs : TGDIndexList;
  iV : TGDVertex_V_UV;
begin
  OjectType := SO_WATERCELL;
  FIndexes := TGDGLIndexBuffer.Create();
  iIdxs := TGDIndexList.Create();

  BoundingBox.Min.Reset(aStartX, aWater.WaterHeight, aStartY);
  BoundingBox.Max.Reset(aEndX, aWater.WaterHeight, aEndY);
  BoundingBox.CalculateCenter();

  iV.Vertex.Reset(aStartX, aWater.WaterHeight, aStartY);
  iV.UV.Reset(aStartU, aStartV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aStartX, aWater.WaterHeight, aEndY);
  iV.UV.Reset(aStartU, aEndV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aEndX, aWater.WaterHeight, aStartY);
  iV.UV.Reset(aEndU, aStartV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aEndX, aWater.WaterHeight, aEndY);
  iV.UV.Reset(aEndU, aEndV);
  iIdxs.Add(aWater.AddVertex(iV));

  FIndexes.Bind();
  FIndexes.Update(iIdxs, GL_STATIC_DRAW);
  FIndexes.Unbind();
  FreeAndNil(iIdxs);
end;


destructor  TGDWaterCell.Destroy();
begin
  FreeAndNil(FIndexes);
  Inherited;
end;


procedure TGDWaterCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          FIndexes.Bind();
                          FIndexes.Render(GL_TRIANGLE_STRIP);
                          FIndexes.Unbind();
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : //water doesn`t have normals
    end;
end;

end.
