unit uGDTypesGenerics;

{$mode objfpc}

interface

uses
  uGDTypes,
  FGL;

type
  TGDIndexList = specialize TFPGList<Integer>;
  TGDVertex_V_List  = specialize TFPGList<TGDVector>;
  TGDVertex_UV_List  = specialize TFPGList<TGDUVCoord>;
  TGDVertex_V_UV_List = specialize TFPGList<TGDVertex_V_UV>;
  TGDVertex_V_UV_N_List = specialize TFPGList<TGDVertex_V_UV_N>;
  TGDVertex_V_UV_N_C_List = specialize TFPGList<TGDVertex_V_UV_N_C>;

implementation

end.

