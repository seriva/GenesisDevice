unit uGDBaseCell;

{$MODE objfpc}

interface

uses
  FGL,
  uGDConstants,
  uGDTypes;

type

  TGDBaseCell = class
  private
    FObjectType  : TGDStaticObjectType;
    FDistance    : Double;
    FBoundingBox : TGDBoundingBox;
  public
    property BoundingBox : TGDBoundingBox read FBoundingBox write FBoundingBox;
    property OjectType   : TGDStaticObjectType read FObjectType write FObjectType;
    property Distance    : Double read FDistance write FDistance;

    constructor Create();
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); virtual;
  end;

  TGDBaseCellList = specialize TFPGObjectList<TGDBaseCell>;

implementation

constructor TGDBaseCell.Create();
begin
  FObjectType     := SO_NONE;
  FDistance       := 0;
end;


destructor TGDBaseCell.Destroy();
begin
  inherited;
end;


procedure TGDBaseCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
 //Do nothing.
end;

end.
