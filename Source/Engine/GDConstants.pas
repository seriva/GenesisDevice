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
  TGDRenderFor       = (RF_NORMAL, RF_WATER, RF_SHADOW);

  //settings types
  TGDTextureDetail   = (TD_LOW=1, TD_MEDIUM=2,TD_HIGH=3);
  TGDTextureFilter   = (TF_BILINEAR=1, TF_TRILINEAR=2, TF_AF2=3, TF_AF4=4, TF_AF8=5, TF_AF16=6);
  TGDWaterDetail     = (WD_LOW=1, WD_MEDIUM=2, WD_HIGH=3);

  //input types
  TGDInputTypes = (IT_SINGLE, IT_DIRECT, IT_DOWN, IT_UP);

  //static object types
  TGDStaticObjectType = (SO_NONE, SO_TERRAINCELL, SO_WATERCELL, SO_MESHCELL, SO_GRASSCELL );

  //Mesh LOD type
  TGDMeshLODType = (LT_NONE, LT_FADE_IN, LT_STAGES);

  //Sound type
  TGDSoundResourceType = (SR_BUFFER, SR_STREAM);

  //Vertex layouts
  TGDVertexLayout = (VL_NONE, VL_V, VL_V_UV, VL_V_UV_N, VL_V_UV_N_C);

  //base procedure callback
  TGDCallback = procedure();

const
  //settings strings
  TTextureDetail   : array[1..3] of String  = ('Low', 'Medium','High');
  TTextureFilter   : array[1..6] of String  = ('Bilinear', 'Trilinear','Anisotropic 2x','Anisotropic 4x','Anisotropic 8x','Anisotropic 16x');
  TWaterDetail     : array[1..3] of String  = ('Low', 'Medium','High');

  //engine constants
  ENGINE_INFO     = '82 - 25 September 2015';

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
  R_SHADOW_SIZE           = 2048;

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
  PATH_MAPS         = 'Maps\';
  PATH_SHADERS      = 'Shaders\';
  PATH_SOUNDS       = 'Sounds\';
  PATH_MODELS       = 'Models\';
  PATH_TEXTURES     = 'Textures\';
  PATH_INITS        = 'Ini\';

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

  //terrain constants
  TERRAIN_CELLSIZE = 16;

  //sky constants
  SKY_COMPLEXITY = 16;

  //grass constants
  GRASS_CELLSIZE = 4;

  //shader constants
  SHADER_TERRAIN  = PATH_SHADERS + 'terrain.shader';
  SHADER_SKY      = PATH_SHADERS + 'sky.shader';
  SHADER_WATER    = PATH_SHADERS + 'water.shader';
  SHADER_GRASS    = PATH_SHADERS + 'grass.shader';
  SHADER_BLUR     = PATH_SHADERS + 'blur.shader';
  SHADER_POST     = PATH_SHADERS + 'postprocess.shader';
  SHADER_MESH     = PATH_SHADERS + 'mesh.shader';
  SHADER_COLOR    = PATH_SHADERS + 'color.shader';
  SHADER_TEXTURE  = PATH_SHADERS + 'texture.shader';
  SHADER_CLEAR    = PATH_SHADERS + 'clear.shader';

implementation
end.
