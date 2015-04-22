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
unit GDFont;

{$MODE Delphi}

interface

Uses
 Classes,
 SysUtils,
 dglOpenGL,
 GDConstants,
 GDTypes,
 GDTexture;

const
  FONT_TEXHEIGHT = 512;
  FONT_HEIGHT    = 64;
  FONT_CHARCOORDS : array[0..93,0..3] of Word = (
      (0,0,25,64),        //!
      (25,0,54,64),       //"
      (54,0,107,64),      //#
      (107,0,148,64),     //$
      (148,0,217,64),     //%
      (217,0,263,64),     //&
      (263,0,280,64),     //'
      (280,0,309,64),     //(
      (309,0,338,64),     //)
      (338,0,379,64),     //*
      (379,0,432,64),     //+
      (432,0,455,64),     //,
      (455,0,484,64),     //-
      (0,64,21,128),      //.
      (23,64,52,128),     ///
      (52,64,93,128),     //0
      (93,64,133,128),    //1
      (133,64,174,128),   //2
      (174,64,215,128),   //3
      (215,64,256,128),   //4
      (256,64,296,128),   //5
      (296,64,337,128),   //6
      (337,64,378,128),   //7
      (378,64,419,128),   //8
      (419,64,459,128),   //9
      (459,64,488,128),   //:
      (0,128,29,192),     //;
      (29,128,81,192),    //<
      (81,128,134,192),   //=
      (134,128,186,192),  //>
      (186,128,221,192),  //?
      (221,128,285,192),  //@
      (285,128,329,192),  //A
      (329,128,373,192),  //B
      (373,128,418,192),  //C
      (418,128,467,192),  //D
      (0,192,40,256),     //E
      (40,192,77,256),    //F
      (77,192,127,256),   //G
      (127,192,175,256),  //H
      (175,192,202,256),  //I
      (202,192,231,256),  //J
      (231,192,275,256),  //K
      (275,192,311,256),  //L
      (311,192,365,256),  //M
      (365,192,413,256),  //N
      (413,192,463,256),  //O
      (1,256,38,320),     //P
      (38,256,89,320),    //Q
      (89,256,133,320),   //R
      (133,256,176,320),  //S
      (177,256,216,320),  //T
      (217,256,263,320),  //U
      (263,256,307,320),  //V
      (307,256,370,320),  //W
      (370,256,414,320),  //X
      (414,256,453,320),  //Y
      (453,256,497,320),  //Z
      (0,320,29,384),     //[
      (29,320,58,384),    //"\"
      (59,320,87,384),    //]
      (87,320,139,384),   //^
      (139,320,180,384),  //_
      (180,320,221,384),  //`
      (221,320,259,384),  //a
      (259,320,299,384),  //b
      (299,320,332,384),  //c
      (332,320,372,384),  //d
      (372,320,411,384),  //e
      (411,320,433,384),  //f
      (435,320,473,384),  //g
      (0,384,40,448),     //h
      (40,384,56,448),    //i
      (58,384,80,448),    //j
      (80,384,118,448),   //k
      (118,384,135,448),  //l
      (135,384,197,448),  //m
      (197,384,238,448),  //n
      (238,384,277,448),  //o
      (277,384,317,448),  //p
      (317,384,356,448),  //q
      (357,384,384,448),  //r
      (385,384,417,448),  //s
      (417,384,442,448),  //t
      (443,384,483,448),  //u
      (0,448,38,512),     //v
      (38,448,90,512),    //w
      (90,448,128,512),   //x
      (128,448,166,512),  //y
      (166,448,200,512),  //z
      (200,448,241,512),  //{
      (241,448,270,512),  //|
      (270,448,310,512),  //}
      (310,448,363,512)   //~
  );

{******************************************************************************}
{* Font class                                                                 *}
{******************************************************************************}

type
  TGDFont = class
  private
    FTexture : TGDTexture;
    FColor   : TGDColor;
  public
    property Color : TGDColor read FColor write FColor;

    constructor Create();
    destructor  Destroy(); override;
    function    InitFont( aTexture : string) : boolean;
    procedure   Clear();
    procedure   Render( aLeft, aTop, aScale : Double; aString : string);
  end;

var
  Font : TGDFont;

implementation

{******************************************************************************}
{* Create the font class                                                      *}
{******************************************************************************}

constructor TGDFont.Create();
begin
  FColor   := TGDColor.Create();
  FColor.White();
  FTexture := TGDTexture.Create();
end;

{******************************************************************************}
{* Destroy the font class                                                     *}
{******************************************************************************}

destructor TGDFont.Destroy();
begin
  Clear();
  FreeAndNil(FColor);
  FreeAndNil(FTexture);
  inherited;
end;


{******************************************************************************}
{* Init the font                                                              *}
{******************************************************************************}

function TGDFont.InitFont( aTexture : string) : boolean;
begin
  result := FTexture.InitTexture(aTexture, TD_HIGH, TF_TRILINEAR);
end;

{******************************************************************************}
{* Clear the font                                                             *}
{******************************************************************************}

Procedure TGDFont.Clear();
begin
  FTexture.Clear();
end;

{******************************************************************************}
{* Render a string                                                            *}
{******************************************************************************}

Procedure TGDFont.Render( aLeft, aTop, aScale : Double; aString : string);
var
  i, x, y, c, inwidth, inheight : integer;
  inleft, intop, inright, inbottom : Single;

procedure RenderTexturedQuad(x,y,width,height: Single;
                               u1 : Single=0; v1 : Single=0;
                               u2 : Single=1; v2 : Single=0;
                               u3 : Single=1; v3 : Single=1;
                               u4 : Single=0; v4 : Single=1);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(u1, v1); glVertex2f( x,       y);
    glTexCoord2f(u2, v2); glVertex2f( x+width, y);
    glTexCoord2f(u3, v3); glVertex2f( x+width, y+height);
    glTexCoord2f(u4, v4); glVertex2f( x,       y+height);
end;

begin
  FTexture.BindTexture(GL_TEXTURE0);
  x := Round(aLeft);
  y := Round(aTop);
  for i := 1 to length(aString) do
  begin
    if(aString[i] = ' ') then begin x := x + round((FONT_HEIGHT/2) * aScale); continue; end;
    c := Ord(aString[i])-33;
    if(c < 0) or (c >= 95) then continue;
    inleft   := FONT_CHARCOORDS[c][0]   / FONT_TEXHEIGHT;
    inright  := FONT_CHARCOORDS[c][2]   / FONT_TEXHEIGHT;
    inwidth  := Round((FONT_CHARCOORDS[c][2] - FONT_CHARCOORDS[c][0]) * aScale);
    inheight := Round((FONT_CHARCOORDS[c][3] - FONT_CHARCOORDS[c][1]) * aScale);
    RenderTexturedQuad(x,y,inwidth,inheight,inleft,intop,inright,intop,inright,inbottom,inleft,inbottom);
    x := x + inwidth + 1;
  end;
end;

end.
