{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
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
unit GDConstants;

{******************************************************************************}
{* Holds the main types, constants of the engine                              *}
{******************************************************************************}

interface

type
  //render types
  TGDRenderState     = (RS_COLOR, RS_WIREFRAME, RS_TEXTS, RS_TEXTURE);
  TGDRenderAttribute = (RA_NORMAL, RA_FRUSTUM_BOXES, RA_NODE_BOXES, RA_NORMALS);
  TGDRenderFor       = (RF_NORMAL, RF_WATER, RF_BLOOM);

  //settings types
  TGDTextureDetail   = (TD_LOW, TD_MEDIUM,TD_HIGH);
  TGDTextureFilter   = (TF_BILINEAR, TF_TRILINEAR, TF_AF2, TF_AF4, TF_AF8, TF_AF16);
  TGDWaterDetail     = (WD_LOW, WD_MEDIUM, WD_HIGH);
  TGDWaterReflection = (WR_ALL, WR_TERRAIN_ONLY);

  //input types
  TGDInputTypes = (IT_SINGLE, IT_DIRECT, IT_DOWN, IT_UP);

  //static object types
  TGDStaticObjectType = (SO_NONE, SO_TERRAINCELL, SO_WATERCELL, SO_MESHCELL, SO_GRASSCELL );

  //polygon type
  TGDPolygonType = (PT_TRIANGLE, PT_QUAD);

  //Mesh LOD type
  TGDMeshLODType = (LT_NONE, LT_FADE_IN, LT_STAGES);

  //base procedure callback
  TGDCallback = procedure();

const
  //settings strings
  TTextureDetail   : array[1..3] of String  = ('Low', 'Medium','High');
  TTextureFilter   : array[1..6] of String  = ('Bilinear', 'Trilinear','Anisotropic 2x','Anisotropic 4x','Anisotropic 8x','Anisotropic 16x');
  TWaterDetail     : array[1..3] of String  = ('Low', 'Medium','High');
  TWaterReflection : array[1..2] of String  = ('Terrain Only', 'All');

  //engine constants
  ENGINE_INFO     = '72 -19 May 2015';

  //renderer constants
  R_HUDWIDTH              = 1600;
  R_HUDHEIGHT             = 1200;
  R_VIEW_DISTANCE_STEP    = 10240;
  R_MIN_VIEW_DISTANCE     = 51200;
  R_MAX_VIEW_DISTANCE     = 102400;
  R_WATER_DISTANCE_STEP   = 512;
  R_MIN_WATER_DISTANCE    = 512;
  R_MAX_WATER_DISTANCE    = 10240;
  R_FOLIAGE_LOD_DISTANCE  = 2000;
  R_FOLIAGE_DISTANCE_STEP = 1024;
  R_CAUSTIC_TIME          = 50;
  R_NORMAL_LENGTH         = 32;
  R_LOD0_DISTANCE         = 15;
  R_LOD1_DISTANCE         = 50;
  R_LOD2_DISTANCE         = 100;

  //sound constants
  S_MAX_SOURCES = 16;

  //minimum required settings constants
  MRS_OPENGL_MAJOR_VERSION  = 2;
  MRS_OPENGL_MINOR_VERSION  = 0;
  MRS_TEXTURE_UNITS         = 8;
  MRS_ANISOTROPIC_FILTERING = 16;
  MRS_TEXTURE_SIZE          = 4096;
  MRS_OPENAL_MAJOR_VERSION  = 1;
  MRS_OPENAL_MINOR_VERSION  = 1;

  //filepaths constants
  FP_MAPS         = 'Maps\';
  FP_SHADERS      = 'Shaders\';
  FP_SOUNDS       = 'Sounds\';
  FP_MODELS       = 'Models\';
  FP_TEXTURES     = 'Textures\';
  FP_INITS        = 'Ini\';

  //file constants
  ENGINE_INI = 'Engine.ini';
  ENGINE_LOG = 'Engine.log';
  GUI_INI    = 'GUI.ini';

  //setting strings
  TGDTextureDetailStrings   : array[1..3] of String  = ('Low', 'Medium','High');
  TGDTextureFilterStrings   : array[1..6] of String  = ('Bilinear', 'Trilinear','Anisotropic 2x','Anisotropic 4x','Anisotropic 8x','Anisotropic 16x');
  TGDWaterDetailStrings     : array[1..3] of String  = ('Low', 'Medium','High');
  TGDWaterReflectionStrings : array[1..2] of String  = ('Terrain Only', 'All');

  //console constants
  C_MAX_LINES    = 22;
  C_CURSOR_TIME  = 500;

  //stats update
  S_UPDATE_TIME  = 1000;

  //shader constants
  SHADER_FRAG_EXT = '.frag';
  SHADER_VERT_EXT = '.vert';
  SHADER_TERRAIN  = FP_SHADERS + 'terrain';
  SHADER_SKY      = FP_SHADERS + 'sky';
  SHADER_WATER    = FP_SHADERS + 'water';
  SHADER_GRASS    = FP_SHADERS + 'grass';
  SHADER_BLUR     = FP_SHADERS + 'blur';
  SHADER_BLOOMMIX = FP_SHADERS + 'bloommix';
  SHADER_COPY     = FP_SHADERS + 'copy';
  SHADER_FINAL    = FP_SHADERS + 'final';
  SHADER_MESH     = FP_SHADERS + 'mesh';
  SHADER_COLOR    = FP_SHADERS + 'color';
  SHADER_TEXTURE  = FP_SHADERS + 'texture';

implementation
end.
