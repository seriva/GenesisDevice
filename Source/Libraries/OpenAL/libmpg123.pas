(*
	libmpg123: MPEG Audio Decoder library (version 1.25.10)

	copyright 1995-2015 by the mpg123 project
	free software under the terms of the LGPL 2.1
	see COPYING and AUTHORS files in distribution or http://mpg123.org
*)
 
(**
* Conversion to Pascal Copyright 2018 (c) Oleksandr Nazaruk <mail@freehand.com.ua>
*
*)


unit libmpg123;

{$MINENUMSIZE 4}

interface

uses
  SysUtils;

const
  _PU = '';
  {$IF Defined(MSWINDOWS)}
    {$IFDEF WIN64}
      link_libmpg123 = 'libmpg123-0.dll';
    {$ENDIF}
    {$IFDEF WIN32}
      link_libmpg123 = 'libmpg123-0.dll';
    {$ENDIF}
  {$ELSEIF Defined(MACOS)}
    link_libmpg123 = '@executable_path/../Frameworks/libmpg123-0.dylib';
    _PU = '_';
  {$ELSEIF Defined(UNIX)}
    link_libmpg123 = 'libmpg123.so';
  {$IFEND}


type
  (** \defgroup mpg123_init mpg123 library and handle setup
   *
   * Functions to initialise and shutdown the mpg123 library and handles.
   * The parameters of handles have workable defaults, you only have to tune them when you want to tune something;-)
   * Tip: Use a RVA setting...
   *
   * @{
   *)

  (** Opaque structure for the libmpg123 decoder handle. *)
  TMpg123_handle_struct = packed record
    {undefined structure}
  end;

  (** Opaque structure for the libmpg123 decoder handle.
   *  Most functions take a pointer to a mpg123_handle as first argument and operate on its data in an object-oriented manner.
   *)
  pMpg123_handle = ^TMpg123_handle;
  TMpg123_handle = TMpg123_handle_struct;

  (** \defgroup mpg123_error mpg123 error handling
   *
   * Functions to get text version of the error numbers and an enumeration
   * of the error codes returned by libmpg123.
   *
   * Most functions operating on a mpg123_handle simply return MPG123_OK (0)
   * on success and MPG123_ERR (-1) on failure, setting the internal error
   * variable of the handle to the specific error code. If there was not a valid
   * (non-NULL) handle provided to a function operating on one, MPG123_BAD_HANDLE
   * may be returned if this can not be confused with a valid positive return
   * value.
   * Meaning: A function expected to return positive integers on success will
   * always indicate error or a special condition by returning a negative one.
   *
   * Decoding/seek functions may also return message codes MPG123_DONE,
   * MPG123_NEW_FORMAT and MPG123_NEED_MORE (all negative, see below on how to
   * react). Note that calls to those can be nested, so generally watch out
   * for these codes after initial handle setup.
   * Especially any function that needs information about the current stream
   * to work will try to at least parse the beginning if that did not happen
   * yet.
   *
   * On a function that is supposed to return MPG123_OK on success and
   * MPG123_ERR on failure, make sure you check for != MPG123_OK, not
   * == MPG123_ERR, as the error code could get more specific in future,
   * or there is just a special message from a decoding routine as indicated
   * above.
   *
   * @{
   *)

  (** Enumeration of the message and error codes and returned by libmpg123 functions. *)
  TMpg123_errors = (
    MPG123_DONE = -12,	(*< Message: Track ended. Stop decoding. *)
    MPG123_NEW_FORMAT = -11,	(*< Message: Output format will be different on next call. Note that some libmpg123 versions between 1.4.3 and 1.8.0 insist on you calling mpg123_getformat() after getting this message code. Newer verisons behave like advertised: You have the chance to call mpg123_getformat(), but you can also just continue decoding and get your data. *)
    MPG123_NEED_MORE = -10,	(*< Message: For feed reader: "Feed me more!" (call mpg123_feed() or mpg123_decode() with some new input data). *)
    MPG123_ERR = -1,			(*< Generic Error *)
    MPG123_OK = 0, 			(*< Success *)
    MPG123_BAD_OUTFORMAT, 	(*< Unable to set up output format! *)
    MPG123_BAD_CHANNEL,		(*< Invalid channel number specified. *)
    MPG123_BAD_RATE,		(*< Invalid sample rate specified.  *)
    MPG123_ERR_16TO8TABLE,	(*< Unable to allocate memory for 16 to 8 converter table! *)
    MPG123_BAD_PARAM,		(*< Bad parameter id! *)
    MPG123_BAD_BUFFER,		(*< Bad buffer given -- invalid pointer or too small size. *)
    MPG123_OUT_OF_MEM,		(*< Out of memory -- some malloc() failed. *)
    MPG123_NOT_INITIALIZED,	(*< You didn't initialize the library! *)
    MPG123_BAD_DECODER,		(*< Invalid decoder choice. *)
    MPG123_BAD_HANDLE,		(*< Invalid mpg123 handle. *)
    MPG123_NO_BUFFERS,		(*< Unable to initialize frame buffers (out of memory?). *)
    MPG123_BAD_RVA,			(*< Invalid RVA mode. *)
    MPG123_NO_GAPLESS,		(*< This build doesn't support gapless decoding. *)
    MPG123_NO_SPACE,		(*< Not enough buffer space. *)
    MPG123_BAD_TYPES,		(*< Incompatible numeric data types. *)
    MPG123_BAD_BAND,		(*< Bad equalizer band. *)
    MPG123_ERR_NULL,		(*< Null pointer given where valid storage address needed. *)
    MPG123_ERR_READER,		(*< Error reading the stream. *)
    MPG123_NO_SEEK_FROM_END,(*< Cannot seek from end (end is not known). *)
    MPG123_BAD_WHENCE,		(*< Invalid 'whence' for seek function.*)
    MPG123_NO_TIMEOUT,		(*< Build does not support stream timeouts. *)
    MPG123_BAD_FILE,		(*< File access error. *)
    MPG123_NO_SEEK,			(*< Seek not supported by stream. *)
    MPG123_NO_READER,		(*< No stream opened. *)
    MPG123_BAD_PARS,		(*< Bad parameter handle. *)
    MPG123_BAD_INDEX_PAR,	(*< Bad parameters to mpg123_index() and mpg123_set_index() *)
    MPG123_OUT_OF_SYNC,	(*< Lost track in bytestream and did not try to resync. *)
    MPG123_RESYNC_FAIL,	(*< Resync failed to find valid MPEG data. *)
    MPG123_NO_8BIT,	(*< No 8bit encoding possible. *)
    MPG123_BAD_ALIGN,	(*< Stack aligmnent error *)
    MPG123_NULL_BUFFER,	(*< NULL input buffer with non-zero size... *)
    MPG123_NO_RELSEEK,	(*< Relative seek not possible (screwed up file offset) *)
    MPG123_NULL_POINTER, (*< You gave a null pointer somewhere where you shouldn't have. *)
    MPG123_BAD_KEY,	(*< Bad key value given. *)
    MPG123_NO_INDEX,	(*< No frame index in this build. *)
    MPG123_INDEX_FAIL,	(*< Something with frame index went wrong. *)
    MPG123_BAD_DECODER_SETUP,	(*< Something prevents a proper decoder setup *)
    MPG123_MISSING_FEATURE  (*< This feature has not been built into libmpg123. *)
    ,MPG123_BAD_VALUE (*< A bad value has been given, somewhere. *)
    ,MPG123_LSEEK_FAILED (*< Low-level seek failed. *)
    ,MPG123_BAD_CUSTOM_IO (*< Custom I/O not prepared. *)
    ,MPG123_LFS_OVERFLOW (*< Offset value overflow during translation of large file API calls -- your client program cannot handle that large file. *)
    ,MPG123_INT_OVERFLOW (*< Some integer overflow. *)
  );


(** Look up error strings given integer code.
 *  \param errcode integer error code
 *  \return string describing what that error error code means
 *)
function mpg123_plain_strerror(errcode: integer): MarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_plain_strerror';

(** Give string describing what error has occured in the context of handle mh.
 *  When a function operating on an mpg123 handle returns MPG123_ERR, you should check for the actual reason via
 *  char *errmsg = mpg123_strerror(mh)
 *  This function will catch mh == NULL and return the message for MPG123_BAD_HANDLE.
 *  \param mh handle
 *  \return error message
 *)
function mpg123_strerror(mh: pMpg123_handle): MarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_strerror';

(** Return the plain errcode intead of a string.
 *  \param mh handle
 *  \return error code recorded in handle or MPG123_BAD_HANDLE
 *)
function mpg123_errcode(mh: pMpg123_handle): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_errcode';


(** Function to initialise the mpg123 library.
 *	This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library.
 *
 *	\return MPG123_OK if successful, otherwise an error number.
 *)
function mpg123_init(): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_init';

(** Function to close down the mpg123 library.
 *	This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library. *)
procedure mpg123_exit();
 cdecl; external link_libmpg123 name _PU + 'mpg123_exit';

(** Create a handle with optional choice of decoder (named by a string, see mpg123_decoders() or mpg123_supported_decoders()).
 *  and optional retrieval of an error code to feed to mpg123_plain_strerror().
 *  Optional means: Any of or both the parameters may be NULL.
 *
 *  \param decoder optional choice of decoder variant (NULL for default)
 *  \param error optional address to store error codes
 *  \return Non-NULL pointer to fresh handle when successful.
 *)
function mpg123_new(const decoder: MarshaledAString; var error: integer): pMpg123_handle;
 cdecl; external link_libmpg123 name _PU + 'mpg123_new';

(** Delete handle, mh is either a valid mpg123 handle or NULL.
 *  \param mh handle
 *)
procedure mpg123_delete(mh: pMpg123_handle);
 cdecl; external link_libmpg123 name _PU + 'mpg123_delete';

type
  (** Enumeration of the parameters types that it is possible to set/get. *)
  TMpg123_parms = (
    MPG123_VERBOSE = 0,        (**< set verbosity value for enabling messages to stderr, >= 0 makes sense (integer) *)
    MPG123_FLAGS,          (**< set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX (integer) *)
    MPG123_ADD_FLAGS,      (**< add some flags (integer) *)
    MPG123_FORCE_RATE,     (**< when value > 0, force output rate to that value (integer) *)
    MPG123_DOWN_SAMPLE,    (**< 0=native rate, 1=half rate, 2=quarter rate (integer) *)
    MPG123_RVA,            (**< one of the RVA choices above (integer) *)
    MPG123_DOWNSPEED,      (**< play a frame N times (integer) *)
    MPG123_UPSPEED,        (**< play every Nth frame (integer) *)
    MPG123_START_FRAME,    (**< start with this frame (skip frames before that, integer) *)
    MPG123_DECODE_FRAMES,  (**< decode only this number of frames (integer) *)
    MPG123_ICY_INTERVAL,   (**< stream contains ICY metadata with this interval (integer) *)
    MPG123_OUTSCALE,       (**< the scale for output samples (amplitude - integer or float according to mpg123 output format, normally integer) *)
    MPG123_TIMEOUT,        (**< timeout for reading from a stream (not supported on win32, integer) *)
    MPG123_REMOVE_FLAGS,   (**< remove some flags (inverse of MPG123_ADD_FLAGS, integer) *)
    MPG123_RESYNC_LIMIT,   (**< Try resync on frame parsing for that many bytes or until end of stream (<0 ... integer). This can enlarge the limit for skipping junk on beginning, too (but not reduce it).  *)
    MPG123_INDEX_SIZE      (**< Set the frame index size (if supported). Values <0 mean that the index is allowed to grow dynamically in these steps (in positive direction, of course) -- Use this when you really want a full index with every individual frame. *)
    ,MPG123_PREFRAMES (**< Decode/ignore that many frames in advance for layer 3. This is needed to fill bit reservoir after seeking, for example (but also at least one frame in advance is needed to have all "normal" data for layer 3). Give a positive integer value, please.*)
    ,MPG123_FEEDPOOL  (**< For feeder mode, keep that many buffers in a pool to avoid frequent malloc/free. The pool is allocated on mpg123_open_feed(). If you change this parameter afterwards, you can trigger growth and shrinkage during decoding. The default value could change any time. If you care about this, then set it. (integer) *)
    ,MPG123_FEEDBUFFER (**< Minimal size of one internal feeder buffer, again, the default value is subject to change. (integer) *)
  );

  (** Flag bits for MPG123_FLAGS, use the usual binary or to combine. *)
  TMpg123_param_flags = (
     MPG123_FORCE_MONO   = $7  (**<     0111 Force some mono mode: This is a test bitmask for seeing if any mono forcing is active. *)
    ,MPG123_MONO_LEFT    = $1  (**<     0001 Force playback of left channel only.  *)
    ,MPG123_MONO_RIGHT   = $2  (**<     0010 Force playback of right channel only. *)
    ,MPG123_MONO_MIX     = $4  (**<     0100 Force playback of mixed mono.         *)
    ,MPG123_FORCE_STEREO = $8  (**<     1000 Force stereo output.                  *)
    ,MPG123_FORCE_8BIT   = $10 (**< 00010000 Force 8bit formats.                   *)
    ,MPG123_QUIET        = $20 (**< 00100000 Suppress any printouts (overrules verbose).                    *)
    ,MPG123_GAPLESS      = $40 (**< 01000000 Enable gapless decoding (default on if libmpg123 has support). *)
    ,MPG123_NO_RESYNC    = $80 (**< 10000000 Disable resync stream after error.                             *)
    ,MPG123_SEEKBUFFER   = $100 (**< 000100000000 Enable small buffer on non-seekable streams to allow some peek-ahead (for better MPEG sync). *)
    ,MPG123_FUZZY        = $200 (**< 001000000000 Enable fuzzy seeks (guessing byte offsets or using approximate seek points from Xing TOC) *)
    ,MPG123_FORCE_FLOAT  = $400 (**< 010000000000 Force floating point output (32 or 64 bits depends on mpg123 internal precision). *)
    ,MPG123_PLAIN_ID3TEXT = $800 (**< 100000000000 Do not translate ID3 text data to UTF-8. ID3 strings will contain the raw text data, with the first byte containing the ID3 encoding code. *)
    ,MPG123_IGNORE_STREAMLENGTH = $1000 (**< 1000000000000 Ignore any stream length information contained in the stream, which can be contained in a 'TLEN' frame of an ID3v2 tag or a Xing tag *)
    ,MPG123_SKIP_ID3V2 = $2000 (**< 10 0000 0000 0000 Do not parse ID3v2 tags, just skip them. *)
    ,MPG123_IGNORE_INFOFRAME = $4000 (**< 100 0000 0000 0000 Do not parse the LAME/Xing info frame, treat it as normal MPEG data. *)
    ,MPG123_AUTO_RESAMPLE = $8000 (**< 1000 0000 0000 0000 Allow automatic internal resampling of any kind (default on if supported). Especially when going lowlevel with replacing output buffer, you might want to unset this flag. Setting MPG123_DOWNSAMPLE or MPG123_FORCE_RATE will override this. *)
    ,MPG123_PICTURE = $10000 (**< 17th bit: Enable storage of pictures from tags (ID3v2 APIC). *)
    ,MPG123_NO_PEEK_END    = $20000
    (**< 18th bit: Do not seek to the end of
     *  the stream in order to probe
     *  the stream length and search for the id3v1 field. This also means
     *  the file size is unknown unless set using mpg123_set_filesize() and
     *  the stream is assumed as non-seekable unless overridden.
     *)
    ,MPG123_FORCE_SEEKABLE = $40000 (**< 19th bit: Force the stream to be seekable. *)
  );

  (** choices for MPG123_RVA *)
  TMpg123_param_rva = (
     MPG123_RVA_OFF   = 0 (**< RVA disabled (default).   *)
    ,MPG123_RVA_MIX   = 1 (**< Use mix/track/radio gain. *)
    ,MPG123_RVA_ALBUM = 2 (**< Use album/audiophile gain *)
    ,MPG123_RVA_MAX   = MPG123_RVA_ALBUM (**< The maximum RVA code, may increase in future. *)
  );

(** Set a specific parameter, for a specific mpg123_handle, using a parameter
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 *  \return MPG123_OK on success
 *)
function mpg123_param(mh: pMpg123_handle; _type: TMpg123_parms; value: LongInt; fvalue: double): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_param';

(** Get a specific parameter, for a specific mpg123_handle.
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
function mpg123_getparam(mh: pMpg123_handle; _type: TMpg123_parms; var value: LongInt; var fvalue: Double): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_getparam';

type
  (** Feature set available for query with mpg123_feature. *)
  TMpg123_feature_set = (
     MPG123_FEATURE_ABI_UTF8OPEN = 0     (* < mpg123 expects path names to be given in UTF-8 encoding instead of plain native. *)
    ,MPG123_FEATURE_OUTPUT_8BIT          (**< 8bit output   *)
    ,MPG123_FEATURE_OUTPUT_16BIT         (**< 16bit output  *)
    ,MPG123_FEATURE_OUTPUT_32BIT         (**< 32bit output  *)
    ,MPG123_FEATURE_INDEX                (**< support for building a frame index for accurate seeking *)
    ,MPG123_FEATURE_PARSE_ID3V2          (**< id3v2 parsing *)
    ,MPG123_FEATURE_DECODE_LAYER1        (**< mpeg layer-1 decoder enabled *)
    ,MPG123_FEATURE_DECODE_LAYER2        (**< mpeg layer-2 decoder enabled *)
    ,MPG123_FEATURE_DECODE_LAYER3        (**< mpeg layer-3 decoder enabled *)
    ,MPG123_FEATURE_DECODE_ACCURATE      (**< accurate decoder rounding    *)
    ,MPG123_FEATURE_DECODE_DOWNSAMPLE    (**< downsample (sample omit)     *)
    ,MPG123_FEATURE_DECODE_NTOM          (**< flexible rate decoding       *)
    ,MPG123_FEATURE_PARSE_ICY            (**< ICY support                  *)
    ,MPG123_FEATURE_TIMEOUT_READ         (**< Reader with timeout (network). *)
    ,MPG123_FEATURE_EQUALIZER            (**< tunable equalizer *)
  );

(** Query libmpg123 features.
 *  \param key feature selection
 *  \return 1 for success, 0 for unimplemented functions
 *)
function mpg123_feature(const key: TMpg123_feature_set): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_feature';
(* @} *)


(** \defgroup mpg123_decoder mpg123 decoder selection
 *
 * Functions to list and select the available decoders.
 * Perhaps the most prominent feature of mpg123: You have several (optimized) decoders to choose from (on x86 and PPC (MacOS) systems, that is).
 *
 *
 *)

(** Get available decoder list.
 *  \return NULL-terminated array of generally available decoder names (plain 8bit ASCII)
 *)
function mpg123_decoders(): PMarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_decoders';

(** Get supported decoder list.
 *  \return NULL-terminated array of the decoders supported by the CPU (plain 8bit ASCII)
 *)
function mpg123_supported_decoders(): PMarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_supported_decoders';

(** Set the active decoder.
 *  \param mh handle
 *  \param decoder_name name of decoder
 *  \return MPG123_OK on success
 *)
function mpg123_decoder(mh: pMpg123_handle; decoder_name: MarshaledAString): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_decoder';

(** Get the currently active decoder name.
 *  The active decoder engine can vary depening on output constraints,
 *  mostly non-resampling, integer output is accelerated via 3DNow & Co. but for
 *  other modes a fallback engine kicks in.
 *  Note that this can return a decoder that is only active in the hidden and not
 *  available as decoder choice from the outside.
 *  \param mh handle
 *  \return The decoder name or NULL on error.
 *)
function mpg123_current_decoder(mh: pMpg123_handle): MarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_current_decoder';

(*@}*)


(** \defgroup mpg123_output mpg123 output audio format
 *
 * Functions to get and select the format of the decoded audio.
 *
 * Before you dive in, please be warned that you might get confused by this. This seems to happen a lot, therefore I am trying to explain in advance.
 *
 * The mpg123 library decides what output format to use when encountering the first frame in a stream, or actually any frame that is still valid but differs from the frames before in the prompted output format. At such a deciding point, an internal table of allowed encodings, sampling rates and channel setups is consulted. According to this table, an output format is chosen and the decoding engine set up accordingly (including optimized routines for different output formats). This might seem unusual but it just follows from the non-existence of "MPEG audio files" with defined overall properties. There are streams, streams are concatenations of (semi) independent frames. We store streams on disk and call them "MPEG audio files", but that does not change their nature as the decoder is concerned (the LAME/Xing header for gapless decoding makes things interesting again).
 *
 * To get to the point: What you do with mpg123_format() and friends is to fill the internal table of allowed formats before it is used. That includes removing support for some formats or adding your forced sample rate (see MPG123_FORCE_RATE) that will be used with the crude internal resampler. Also keep in mind that the sample encoding is just a question of choice -- the MPEG frames do only indicate their native sampling rate and channel count. If you want to decode to integer or float samples, 8 or 16 bit ... that is your decision. In a "clean" world, libmpg123 would always decode to 32 bit float and let you handle any sample conversion. But there are optimized routines that work faster by directly decoding to the desired encoding / accuracy. We prefer efficiency over conceptual tidyness.
 *
 * People often start out thinking that mpg123_format() should change the actual decoding format on the fly. That is wrong. It only has effect on the next natural change of output format, when libmpg123 will consult its format table again. To make life easier, you might want to call mpg123_format_none() before any thing else and then just allow one desired encoding and a limited set of sample rates / channel choices that you actually intend to deal with. You can force libmpg123 to decode everything to 44100 KHz, stereo, 16 bit integer ... it will duplicate mono channels and even do resampling if needed (unless that feature is disabled in the build, same with some encodings). But I have to stress that the resampling of libmpg123 is very crude and doesn't even contain any kind of "proper" interpolation.
 *
 * In any case, watch out for MPG123_NEW_FORMAT as return message from decoding routines and call mpg123_getformat() to get the currently active output format.
 *
 * @{
 *)

 type
  (** They can be combined into one number (3) to indicate mono and stereo... *)
  TMpg123_channelcount =
  (
     MPG123_MONO   = 1, (**< mono *)
     MPG123_STEREO = 2 (**< stereo *)
  );

(** An array of supported standard sample rates
 *  These are possible native sample rates of MPEG audio files.
 *  You can still force mpg123 to resample to a different one, but by default you will only get audio in one of these samplings.
 *  \param list Store a pointer to the sample rates array there.
 *  \param number Store the number of sample rates there. *)
procedure mpg123_rates(const list: PLongInt; var number: Integer);
  cdecl; external link_libmpg123 name _PU + 'mpg123_rates';

(** An array of supported audio encodings.
 *  An audio encoding is one of the fully qualified members of mpg123_enc_enum (MPG123_ENC_SIGNED_16, not MPG123_SIGNED).
 *  \param list Store a pointer to the encodings array there.
 *  \param number Store the number of encodings there. *)
procedure mpg123_encodings(const list: PInteger; var number: Integer);
  cdecl; external link_libmpg123 name _PU + 'mpg123_encodings';

(** Return the size (in bytes) of one mono sample of the named encoding.
 * \param encoding The encoding value to analyze.
 * \return positive size of encoding in bytes, 0 on invalid encoding. *)
function mpg123_encsize(encoding: integer): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_encsize';

(** Configure a mpg123 handle to accept no output format at all,
 *  use before specifying supported formats with mpg123_format
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_format_none(mh: pMpg123_handle): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_format_none';

(** Configure mpg123 handle to accept all formats
 *  (also any custom rate you may set) -- this is default.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_format_all(mh: pMpg123_handle): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_format_all';

(** Set the audio format support of a mpg123_handle in detail:
 *  \param mh handle
 *  \param rate The sample rate value (in Hertz).
 *  \param channels A combination of MPG123_STEREO and MPG123_MONO.
 *  \param encodings A combination of accepted encodings for rate and channels, p.ex MPG123_ENC_SIGNED16 | MPG123_ENC_ULAW_8 (or 0 for no support). Please note that some encodings may not be supported in the library build and thus will be ignored here.
 *  \return MPG123_OK on success, MPG123_ERR if there was an error. *)
function mpg123_format(mh: pMpg123_handle; rate: LongInt; channels: integer; encodings: integer): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_format';

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_handle.
 *  \param mh handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO,
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
function mpg123_format_support(mh: pMpg123_handle; rate: LongInt; encoding: integer): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_format_support';

(** Get the current output format written to the addresses given.
 *  If the stream is freshly loaded, this will try to parse enough
 *  of it to give you the format to come. This clears the flag that
 *  would otherwise make the first decoding call return
 *  MPG123_NEW_FORMAT.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \return MPG123_OK on success
 *)
function mpg123_getformat(mh: pMpg123_handle; var rate: LongInt; var channels: integer; var encodings: integer): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_getformat';

(** Get the current output format written to the addresses given.
 *  This differs from plain mpg123_getformat() in that you can choose
 *  _not_ to clear the flag that would trigger the next decoding call
 *  to return MPG123_NEW_FORMAT in case of a new format arriving.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \param clear_flag if true, clear internal format flag
 *  \return MPG123_OK on success
 *)
function mpg123_getformat2(mh: pMpg123_handle; var rate: LongInt; var channels: integer; var encodings: integer; clear_flag: integer): integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_getformat2';

(*@}*)


(** \defgroup mpg123_input mpg123 file input and decoding
 *
 * Functions for input bitstream and decoding operations.
 * Decoding/seek functions may also return message codes MPG123_DONE, MPG123_NEW_FORMAT and MPG123_NEED_MORE (please read up on these on how to react!).
 * @{
 *)

(* reading samples / triggering decoding, possible return values: *)
(** Enumeration of the error codes returned by libmpg123 functions. *)

(** Open and prepare to decode the specified file by filesystem path.
 *  This does not open HTTP urls; libmpg123 contains no networking code.
 *  If you want to decode internet streams, use mpg123_open_fd() or mpg123_open_feed().
 *  \param mh handle
 *  \param path filesystem path
 *  \return MPG123_OK on success
 *)
function mpg123_open(mh: pMpg123_handle; const path: MarshaledAString): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_open';

(** Use an already opened file descriptor as the bitstream input
 *  mpg123_close() will _not_ close the file descriptor.
 *  \param mh handle
 *  \param fd file descriptor
 *  \return MPG123_OK on success
 *)
function mpg123_open_fd(mh: pMpg123_handle; fd: integer): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_open_fd';

(** Use an opaque handle as bitstream input. This works only with the
 *  replaced I/O from mpg123_replace_reader_handle()!
 *  mpg123_close() will call the cleanup callback for your handle (if you gave one).
 *  \param mh handle
 *  \param iohandle your handle
 *  \return MPG123_OK on success
 *)
function mpg123_open_handle(mh: pMpg123_handle; iohandle: Pointer): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_open_handle';

(** Open a new bitstream and prepare for direct feeding
 *  This works together with mpg123_decode(); you are responsible for reading and feeding the input bitstream.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_open_feed(mh: pMpg123_handle): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_open_feed';

(** Closes the source, if libmpg123 opened it.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_close(mh: pMpg123_handle): integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_close';

(** Read from stream and decode up to outmemsize bytes.
 *  \param mh handle
 *  \param outmemory address of output buffer to write to
 *  \param outmemsize maximum number of bytes to write
 *  \param done address to store the number of actually decoded bytes to
 *  \return MPG123_OK or error/message code
 *)
function mpg123_read(mh: pMpg123_handle; outmemory: PByte; outmemsize: Longint; done: PCardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_read';

(** Feed data for a stream that has been opened with mpg123_open_feed().
 *  It's give and take: You provide the bytestream, mpg123 gives you the decoded samples.
 *  \param mh handle
 *  \param in input buffer
 *  \param size number of input bytes
 *  \return MPG123_OK or error/message code.
 *)
function mpg123_feed(mh: pMpg123_handle; const _in: PByte; size: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_feed';

(** Decode MPEG Audio from inmemory to outmemory.
 *  This is very close to a drop-in replacement for old mpglib.
 *  When you give zero-sized output buffer the input will be parsed until
 *  decoded data is available. This enables you to get MPG123_NEW_FORMAT (and query it)
 *  without taking decoded data.
 *  Think of this function being the union of mpg123_read() and mpg123_feed() (which it actually is, sort of;-).
 *  You can actually always decide if you want those specialized functions in separate steps or one call this one here.
 *  \param mh handle
 *  \param inmemory input buffer
 *  \param inmemsize number of input bytes
 *  \param outmemory output buffer
 *  \param outmemsize maximum number of output bytes
 *  \param done address to store the number of actually decoded bytes to
 *  \return error/message code (watch out especially for MPG123_NEED_MORE)
 *)
function mpg123_decode(mh: pMpg123_handle; const inmemory: PByte; inmemsize: Cardinal; outmemory: PByte; outmemsize: Cardinal; var done: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_decode';

(** Decode next MPEG frame to internal buffer
 *  or read a frame and return after setting a new format.
 *  \param mh handle
 *  \param num current frame offset gets stored there
 *  \param audio This pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
function mpg123_decode_frame(mh: pMpg123_handle; var num: Longint; var audio: PByte; var bytes: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_decode_frame';

(** Decode current MPEG frame to internal buffer.
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \param num last frame offset gets stored there
 *  \param audio this pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
function mpg123_framebyframe_decode(mh: pMpg123_handle; var num: Longint; var audio: PByte; var bytes: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_framebyframe_decode';

(** Find, read and parse the next mp3 frame
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \return MPG123_OK or error/message code
 *)
function mpg123_framebyframe_next(mh: pMpg123_handle): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_framebyframe_next';

(** Get access to the raw input data for the last parsed frame.
 * This gives you a direct look (and write access) to the frame body data.
 * Together with the raw header, you can reconstruct the whole raw MPEG stream without junk and meta data, or play games by actually modifying the frame body data before decoding this frame (mpg123_framebyframe_decode()).
 * A more sane use would be to use this for CRC checking (see mpg123_info() and MPG123_CRC), the first two bytes of the body make up the CRC16 checksum, if present.
 * You can provide NULL for a parameter pointer when you are not interested in the value.
 *
 * \param mh handle
 * \param header the 4-byte MPEG header
 * \param bodydata pointer to the frame body stored in the handle (without the header)
 * \param bodybytes size of frame body in bytes (without the header)
 * \return MPG123_OK if there was a yet un-decoded frame to get the
 *    data from, MPG123_BAD_HANDLE or MPG123_ERR otherwise (without further
 *    explanation, the error state of the mpg123_handle is not modified by
 *    this function).
 *)
function mpg123_framedata(mh: pMpg123_handle; header: PLongWord; var bodydata: PByte; bodybytes: PCardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_framedata';

(** Get the input position (byte offset in stream) of the last parsed frame.
 *  This can be used for external seek index building, for example.
 *  It just returns the internally stored offset, regardless of validity --
 *  you ensure that a valid frame has been parsed before!
 * \param mh handle
 * \return byte offset in stream
 *)
function mpg123_framepos(mh: pMpg123_handle): Longint
 cdecl; external link_libmpg123 name _PU + 'mpg123_framepos';

(*@}*)


(** \defgroup mpg123_seek mpg123 position and seeking
 *
 * Functions querying and manipulating position in the decoded audio bitstream.
 * The position is measured in decoded audio samples, or MPEG frame offset for the specific functions.
 * If gapless code is in effect, the positions are adjusted to compensate the skipped padding/delay - meaning, you should not care about that at all and just use the position defined for the samples you get out of the decoder;-)
 * The general usage is modelled after stdlib's ftell() and fseek().
 * Especially, the whence parameter for the seek functions has the same meaning as the one for fseek() and needs the same constants from stdlib.h:
 * - SEEK_SET: set position to (or near to) specified offset
 * - SEEK_CUR: change position by offset from now
 * - SEEK_END: set position to offset from end
 *
 * Note that sample-accurate seek only works when gapless support has been enabled at compile time; seek is frame-accurate otherwise.
 * Also, really sample-accurate seeking (meaning that you get the identical sample value after seeking compared to plain decoding up to the position) is only guaranteed when you do not mess with the position code by using MPG123_UPSPEED, MPG123_DOWNSPEED or MPG123_START_FRAME. The first two mainly should cause trouble with NtoM resampling, but in any case with these options in effect, you have to keep in mind that the sample offset is not the same as counting the samples you get from decoding since mpg123 counts the skipped samples, too (or the samples played twice only once)!
 * Short: When you care about the sample position, don't mess with those parameters;-)
 * Also, seeking is not guaranteed to work for all streams (underlying stream may not support it).
 * And yet another caveat: If the stream is concatenated out of differing pieces (Frankenstein stream), seeking may suffer, too.
 *
 * @{
 *)

(** Returns the current position in samples.
 *  On the next successful read, you'd get that sample.
 *  \param mh handle
 *  \return sample offset or MPG123_ERR (null handle)
 *)
function mpg123_tell(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_tell';

(** Returns the frame number that the next read will give you data from.
 *  \param mh handle
 *  \return frame offset or MPG123_ERR (null handle)
 *)
function mpg123_tellframe(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_tellframe';

(** Returns the current byte offset in the input stream.
 *  \param mh handle
 *  \return byte offset or MPG123_ERR (null handle)
 *)
function mpg123_tell_stream(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_tell_stream';

(** Seek to a desired sample offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param sampleoff offset in PCM samples
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code
 *)
function mpg123_seek(mh: pMpg123_handle; sampleoff: Longint; whence: integer): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_seek';

(** Seek to a desired sample offset in data feeding mode.
 *  This just prepares things to be right only if you ensure that the next chunk of input data will be from input_offset byte position.
 *  \param mh handle
 *  \param sampleoff offset in PCM samples
 *  \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 *  \param input_offset The position it expects to be at the
 *                      next time data is fed to mpg123_decode().
 *  \return The resulting offset >= 0 or error/message code *)
function mpg123_feedseek(mh: pMpg123_handle; sampleoff: Longint; whence: integer; input_offset: PLongint): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_feedseek';

(** Seek to a desired MPEG frame offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param frameoff offset in MPEG frames
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code *)
function mpg123_seek_frame(mh: pMpg123_handle; frameoff: Longint; whence: Integer): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_seek_frame';

(** Return a MPEG frame offset corresponding to an offset in seconds.
 *  This assumes that the samples per frame do not change in the file/stream, which is a good assumption for any sane file/stream only.
 *  \return frame offset >= 0 or error/message code *)
function mpg123_timeframe(mh: pMpg123_handle; sec: double): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_timeframe';

(** Give access to the frame index table that is managed for seeking.
 *  You are asked not to modify the values... Use mpg123_set_index to set the
 *  seek index
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step one index byte offset advances this many MPEG frames
 *  \param fill number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
function mpg123_index(mh: pMpg123_handle; var offsets: PLongint; step: PLongint; fill: PCardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_index';

(** Set the frame index table
 *  Setting offsets to NULL and fill > 0 will allocate fill entries. Setting offsets
 *  to NULL and fill to 0 will clear the index and free the allocated memory used by the index.
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step    one index byte offset advances this many MPEG frames
 *  \param fill    number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
function mpg123_set_index(mh: pMpg123_handle; offsets: PLongint; step: Longint; fill: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_set_index';

(** An old crutch to keep old mpg123 binaries happy.
 *  WARNING: This function is there only to avoid runtime linking errors with
 *  standalone mpg123 before version 1.23.0 (if you strangely update the
 *  library but not the end-user program) and actually is broken
 *  for various cases (p.ex. 24 bit output). Do never use. It might eventually
 *  be purged from the library.
 *)
function mpg123_position(mh: pMpg123_handle; frame_offset: Longint; buffered_bytes: Longint; current_frame: PLongint; frames_left: PLongint; current_seconds: PDouble; seconds_left: PDouble): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_position';

(*@}*)


(** \defgroup mpg123_voleq mpg123 volume and equalizer
 *
 * @{
 *)

type
  (** another channel enumeration, for left/right choice *)
  TMpg123_channels =
  (
    MPG123_LEFT=$1,	(**< The Left Channel. *)
    MPG123_RIGHT=$2,	(**< The Right Channel. *)
    MPG123_LR=$3	(**< Both left and right channel; same as MPG123_LEFT|MPG123_RIGHT *)
  );

(** Set the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \param val The (linear) adjustment factor.
 *  \return MPG123_OK on success
 *)
function mpg123_eq( mh: pMpg123_handle; channel: TMpg123_channels; band: integer; val: double): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_eq';

(** Get the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for (arithmetic mean of) both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \return The (linear) adjustment factor (zero for pad parameters) *)
function mpg123_geteq(mh: pMpg123_handle; channel: TMpg123_channels; band: integer; val: Integer): Double;
 cdecl; external link_libmpg123 name _PU + 'mpg123_geteq';

(** Reset the 32 Band Audio Equalizer settings to flat
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_reset_eq(mh: pMpg123_handle): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_reset_eq';

(** Set the absolute output volume including the RVA setting,
 *  vol<0 just applies (a possibly changed) RVA setting.
 *  \param mh handle
 *  \param vol volume value (linear factor)
 *  \return MPG123_OK on success
 *)
function mpg123_volume(mh: pMpg123_handle; vol: double): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_volume';

(** Adjust output volume including the RVA setting by chosen amount
 *  \param mh handle
 *  \param change volume value (linear factor increment)
 *  \return MPG123_OK on success
 *)
function mpg123_volume_change(mh: pMpg123_handle; change: double): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_volume_change';

(** Return current volume setting, the actual value due to RVA, and the RVA
 *  adjustment itself. It's all as double float value to abstract the sample
 *  format. The volume values are linear factors / amplitudes (not percent)
 *  and the RVA value is in decibels.
 *  \param mh handle
 *  \param base return address for base volume (linear factor)
 *  \param really return address for actual volume (linear factor)
 *  \param rva_db return address for RVA value (decibels)
 *  \return MPG123_OK on success
 *)
function mpg123_getvolume(mh: pMpg123_handle; base: PDouble; really: PDouble; rva_db: PDouble): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_getvolume';

(* TODO: Set some preamp in addition / to replace internal RVA handling? *)

(*@}*)

type
  (** \defgroup mpg123_status mpg123 status and information
   *
   * @{
   *)

  (** Enumeration of the mode types of Variable Bitrate *)
  TMpg123_vbr = (
    MPG123_CBR = 0,	(**< Constant Bitrate Mode (default) *)
    MPG123_VBR,		(**< Variable Bitrate Mode *)
    MPG123_ABR		(**< Average Bitrate Mode *)
  );

  (** Enumeration of the MPEG Versions *)
  TMpg123_version = (
    MPG123_1_0=0,	(**< MPEG Version 1.0 *)
    MPG123_2_0,		(**< MPEG Version 2.0 *)
    MPG123_2_5		(**< MPEG Version 2.5 *)
  );


  (** Enumeration of the MPEG Audio mode.
   *  Only the mono mode has 1 channel, the others have 2 channels. *)
  TMpg123_mode = (
    MPG123_M_STEREO=0,	(**< Standard Stereo. *)
    MPG123_M_JOINT,		(**< Joint Stereo. *)
    MPG123_M_DUAL,		(**< Dual Channel. *)
    MPG123_M_MONO		(**< Single Channel. *)
  );


  (** Enumeration of the MPEG Audio flag bits *)
  TMpg123_flags = (
    MPG123_CRC=$1,			(**< The bitstream is error protected using 16-bit CRC. *)
    MPG123_COPYRIGHT=$2,	(**< The bitstream is copyrighted. *)
    MPG123_PRIVATE=$4,		(**< The private bit has been set. *)
    MPG123_ORIGINAL=$8	(**< The bitstream is an original, not a copy. *)
  );

  (** Data structure for storing information about a frame of MPEG Audio *)
 pMpg123_frameinfo = ^TMpg123_frameinfo;
 TMpg123_frameinfo = record
    version: TMpg123_version;	(**< The MPEG version (1.0/2.0/2.5). *)
    layer: Integer;						(**< The MPEG Audio Layer (MP1/MP2/MP3). *)
    rate: LongInt; 						(**< The sampling rate in Hz. *)
    mode: TMpg123_mode;			(**< The audio mode (Mono, Stereo, Joint-stero, Dual Channel). *)
    mode_ext: Integer;					(**< The mode extension bit flag. *)
    framesize: Integer;					(**< The size of the frame (in bytes, including header). *)
    flags: TMpg123_flags;		(**< MPEG Audio flag bits. Just now I realize that it should be declared as int, not enum. It's a bitwise combination of the enum values. *)
    emphasis: Integer;					(**< The emphasis type. *)
    bitrate: Integer;					(**< Bitrate of the frame (kbps). *)
    abr_rate: Integer;					(**< The target average bitrate. *)
    vbr: TMpg123_vbr;			(**< The VBR mode. *)
  end;


(** Get frame information about the MPEG audio bitstream and store it in a mpg123_frameinfo structure.
 *  \param mh handle
 *  \param mi address of existing frameinfo structure to write to
 *  \return MPG123_OK on success
 *)
function mpg123_info(mh: pMpg123_handle; mi: pMpg123_frameinfo): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_info';

(** Get the safe output buffer size for all cases
 *  (when you want to replace the internal buffer)
 *  \return safe buffer size
 *)
function mpg123_safe_buffer(): Cardinal;
 cdecl; external link_libmpg123 name _PU + 'mpg123_safe_buffer';

(** Make a full parsing scan of each frame in the file. ID3 tags are found. An
 *  accurate length value is stored. Seek index will be filled. A seek back to
 *  current position is performed. At all, this function refuses work when
 *  stream is not seekable.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_scan(mh: pMpg123_handle): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_scan';

(** Return, if possible, the full (expected) length of current track in frames.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
function mpg123_framelength(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_framelength';

(** Return, if possible, the full (expected) length of current track in samples.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
function mpg123_length(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_length';

(** Override the value for file size in bytes.
 *  Useful for getting sensible track length values in feed mode or for HTTP streams.
 *  \param mh handle
 *  \param size file size in bytes
 *  \return MPG123_OK on success
 *)
function mpg123_set_filesize(mh: pMpg123_handle; size: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_set_filesize';

(** Get MPEG frame duration in seconds.
 *  \param mh handle
 *  \return frame duration in seconds, <0 on error
 *)
function mpg123_tpf(mh: pMpg123_handle): Double;
 cdecl; external link_libmpg123 name _PU + 'mpg123_tpf';

(** Get MPEG frame duration in samples.
 *  \param mh handle
 *  \return samples per frame for the most recently parsed frame; <0 on errors
 *)
function mpg123_spf(mh: pMpg123_handle): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_spf';

(** Get and reset the clip count.
 *  \param mh handle
 *  \return count of clipped samples
 *)
function mpg123_clip(mh: pMpg123_handle): Longint;
 cdecl; external link_libmpg123 name _PU + 'mpg123_clip';

type
  (** The key values for state information from mpg123_getstate(). *)
  TMpg123_state =
  (
     MPG123_ACCURATE = 1 (**< Query if positons are currently accurate (integer value, 0 if false, 1 if true). *)
    ,MPG123_BUFFERFILL   (**< Get fill of internal (feed) input buffer as integer byte count returned as long and as double. An error is returned on integer overflow while converting to (signed) long, but the returned floating point value shold still be fine. *)
    ,MPG123_FRANKENSTEIN (**< Stream consists of carelessly stitched together files. Seeking may yield unexpected results (also with MPG123_ACCURATE, it may be confused). *)
    ,MPG123_FRESH_DECODER (**< Decoder structure has been updated, possibly indicating changed stream (integer value, 0 if false, 1 if true). Flag is cleared after retrieval. *)
  );

(** Get various current decoder/stream state information.
 *  \param mh handle
 *  \param key the key to identify the information to give.
 *  \param val the address to return (long) integer values to
 *  \param fval the address to return floating point values to
 *  \return MPG123_OK on success
 *)
function mpg123_getstate(mh: pMpg123_handle; key: TMpg123_state; val: PLongInt; fval: PDouble): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_getstate';

(*@}*)


(** \defgroup mpg123_metadata mpg123 metadata handling
 *
 * Functions to retrieve the metadata from MPEG Audio files and streams.
 * Also includes string handling functions.
 *
 * @{
 *)
type
  (** Data structure for storing strings in a safer way than a standard C-String.
   *  Can also hold a number of null-terminated strings. *)
  pMpg123_string = ^TMpg123_string;
  TMpg123_string = record
    p     : MarshaledAString;     (**< pointer to the string data *)
    size  : Cardinal; (**< raw number of bytes allocated *)
    fill  : Cardinal; (**< number of used bytes (including closing zero byte) *)
  end;


(** Create and allocate memory for a new mpg123_string
 *  \param sb string handle (address of existing structure on your side)
 *)
procedure mpg123_init_string(sb: pMpg123_string);
 cdecl; external link_libmpg123 name _PU + 'mpg123_init_string';

(** Free-up mempory for an existing mpg123_string
 *  \param sb string handle
 *)
procedure mpg123_free_string(sb: pMpg123_string);
 cdecl; external link_libmpg123 name _PU + 'mpg123_free_string';

(** Change the size of a mpg123_string
 *  \param sb string handle
 *  \param news new size in bytes
 *  \return 0 on error, 1 on success
 *)
function mpg123_resize_string(sb: pMpg123_string; news: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_resize_string';

(** Increase size of a mpg123_string if necessary (it may stay larger).
 *  Note that the functions for adding and setting in current libmpg123
 *  use this instead of mpg123_resize_string().
 *  That way, you can preallocate memory and safely work afterwards with
 *  pieces.
 *  \param sb string handle
 *  \param news new minimum size
 *  \return 0 on error, 1 on success
 *)
function mpg123_grow_string(sb: pMpg123_string; news: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_grow_string';

(** Copy the contents of one mpg123_string string to another.
 *  Yes the order of arguments is reversed compated to memcpy().
 *  \param from string handle
 *  \param to string handle
 *  \return 0 on error, 1 on success
 *)
function mpg123_copy_string(from: pMpg123_string; _to: pMpg123_string): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_copy_string';

(** Append a C-String to an mpg123_string
 *  \param sb string handle
 *  \param stuff to append
 *  \return 0 on error, 1 on success
 *)
function mpg123_add_string(sb: pMpg123_string; const stuff: MarshaledAString): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_add_string';

(** Append a C-substring to an mpg123 string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
function mpg123_add_substring(sb: pMpg123_string; const stuff: MarshaledAString; from: Cardinal; count: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_add_substring';

(** Set the content of a mpg123_string to a C-string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \return 0 on error, 1 on success
 *)
function mpg123_set_string(sb: pMpg123_string; const stuff: MarshaledAString): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_set_string';

(** Set the content of a mpg123_string to a C-substring
 *  \param sb string handle
 *  \param stuff the future content
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
function mpg123_set_substring(sb: pMpg123_string; const stuff: MarshaledAString; from: Cardinal; count: Cardinal ): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_set_substring';

(** Count characters in a mpg123 string (non-null bytes or UTF-8 characters).
 *  Even with the fill property, the character count is not obvious as there could be multiple trailing null bytes.
 *  \param sb string handle
 *  \param utf8 a flag to tell if the string is in utf8 encoding
 *  \return character count
*)
function mpg123_strlen(sb: pMpg123_string; utf8: Integer): Cardinal;
 cdecl; external link_libmpg123 name _PU + 'mpg123_strlen';

(** Remove trailing \\r and \\n, if present.
 *  \param sb string handle
 *  \return 0 on error, 1 on success
 *)
function mpg123_chomp_string(sb: pMpg123_string): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_chomp_string';

type
  (** The mpg123 text encodings. This contains encodings we encounter in ID3 tags or ICY meta info. *)
  TMpg123_text_encoding =
  (
     mpg123_text_unknown  = 0 (**< Unkown encoding... mpg123_id3_encoding can return that on invalid codes. *)
    ,mpg123_text_utf8     = 1 (**< UTF-8 *)
    ,mpg123_text_latin1   = 2 (**< ISO-8859-1. Note that sometimes latin1 in ID3 is abused for totally different encodings. *)
    ,mpg123_text_icy      = 3 (**< ICY metadata encoding, usually CP-1252 but we take it as UTF-8 if it qualifies as such. *)
    ,mpg123_text_cp1252   = 4 (**< Really CP-1252 without any guessing. *)
    ,mpg123_text_utf16    = 5 (**< Some UTF-16 encoding. The last of a set of leading BOMs (byte order mark) rules.
                               *   When there is no BOM, big endian ordering is used. Note that UCS-2 qualifies as UTF-8 when
                               *   you don't mess with the reserved code points. If you want to decode little endian data
                               *   without BOM you need to prepend $ff $fe yourself. *)
    ,mpg123_text_utf16bom = 6 (**< Just an alias for UTF-16, ID3v2 has this as distinct code. *)
    ,mpg123_text_utf16be  = 7 (**< Another alias for UTF16 from ID3v2. Note, that, because of the mess that is reality,
                               *   BOMs are used if encountered. There really is not much distinction between the UTF16 types for mpg123
                               *   One exception: Since this is seen in ID3v2 tags, leading null bytes are skipped for all other UTF16
                               *   types (we expect a BOM before real data there), not so for utf16be!*)
    ,mpg123_text_max      = 7 (**< Placeholder for the maximum encoding value. *)
  );

  (** The encoding byte values from ID3v2. *)
  TMpg123_id3_enc =
  (
     mpg123_id3_latin1   = 0 (**< Note: This sometimes can mean anything in practice... *)
    ,mpg123_id3_utf16bom = 1 (**< UTF16, UCS-2 ... it's all the same for practical purposes. *)
    ,mpg123_id3_utf16be  = 2 (**< Big-endian UTF-16, BOM see note for mpg123_text_utf16be. *)
    ,mpg123_id3_utf8     = 3 (**< Our lovely overly ASCII-compatible 8 byte encoding for the world. *)
    ,mpg123_id3_enc_max  = 3 (**< Placeholder to check valid range of encoding byte. *)
  );

(** Convert ID3 encoding byte to mpg123 encoding index.
 *  \param id3_enc_byte the ID3 encoding code
 *  \return the mpg123 encoding index
 *)

function mpg123_enc_from_id3(id3_enc_byte: Byte): TMpg123_text_encoding;
 cdecl; external link_libmpg123 name _PU + 'mpg123_enc_from_id3';

(** Store text data in string, after converting to UTF-8 from indicated encoding
 *  A prominent error can be that you provided an unknown encoding value, or this build of libmpg123 lacks support for certain encodings (ID3 or ICY stuff missing).
 *  Also, you might want to take a bit of care with preparing the data; for example, strip leading zeroes (I have seen that).
 *  \param sb  target string
 *  \param enc mpg123 text encoding value
 *  \param source source buffer with plain unsigned bytes (you might need to cast from signed char)
 *  \param source_size number of bytes in the source buffer
 *  \return 0 on error, 1 on success (on error, mpg123_free_string is called on sb)
 *)
function mpg123_store_utf8(sb: pMpg123_string; enc: TMpg123_text_encoding; const source: PByte; source_size: Cardinal): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_store_utf8';

type
  (** Sub data structure for ID3v2, for storing various text fields (including comments).
   *  This is for ID3v2 COMM, TXXX and all the other text fields.
   *  Only COMM and TXXX have a description, only COMM and USLT have a language.
   *  You should consult the ID3v2 specification for the use of the various text fields ("frames" in ID3v2 documentation, I use "fields" here to separate from MPEG frames). *)
  pMpg123_text = ^TMpg123_text;
  TMpg123_text = record
    lang: array[0..2] of AnsiChar; (**< Three-letter language code (not terminated). *)
    id: array[0..3] of AnsiChar;   (**< The ID3v2 text field id, like TALB, TPE2, ... (4 characters, no string termination). *)
    description: TMpg123_string; (**< Empty for the generic comment... *)
    text: TMpg123_string;        (**< ... *)
  end;


  (** The picture type values from ID3v2. *)
  TMpg123_id3_pic_type =
  (
     mpg123_id3_pic_other          =  0 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_icon           =  1 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_other_icon     =  2 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_front_cover    =  3 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_back_cover     =  4 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_leaflet        =  5 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_media          =  6 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_lead           =  7 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_artist         =  8 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_conductor      =  9 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_orchestra      = 10 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_composer       = 11 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_lyricist       = 12 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_location       = 13 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_recording      = 14 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_performance    = 15 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_video          = 16 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_fish           = 17 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_illustration   = 18 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_artist_logo    = 19 (**< see ID3v2 docs *)
    ,mpg123_id3_pic_publisher_logo = 20 (**< see ID3v2 docs *)
  );

  (** Sub data structure for ID3v2, for storing picture data including comment.
   *  This is for the ID3v2 APIC field. You should consult the ID3v2 specification
   *  for the use of the APIC field ("frames" in ID3v2 documentation, I use "fields"
   *  here to separate from MPEG frames). *)
  pMpg123_picture = ^TMpg123_picture;
  TMpg123_picture = record
    _type: AnsiChar;                 (**< mpg123_id3_pic_type value *)
    description : TMpg123_string; (**< description string *)
    mime_type: TMpg123_string;   (**< MIME type *)
    size: Cardinal;               (**< size in bytes *)
    data: PByte;       (**< pointer to the image data *)
  end;

  (** Data structure for storing IDV3v2 tags.
   *  This structure is not a direct binary mapping with the file contents.
   *  The ID3v2 text frames are allowed to contain multiple strings.
   *  So check for null bytes until you reach the mpg123_string fill.
   *  All text is encoded in UTF-8. *)
  pMpg123_id3v2 = ^TMpg123_id3v2;
  TMpg123_id3v2 = record
    version: Byte; (**< 3 or 4 for ID3v2.3 or ID3v2.4. *)
    title: pMpg123_string;   (**< Title string (pointer into text_list). *)
    artist: pMpg123_string;  (**< Artist string (pointer into text_list). *)
    album: pMpg123_string;   (**< Album string (pointer into text_list). *)
    year: pMpg123_string;    (**< The year as a string (pointer into text_list). *)
    genre: pMpg123_string;   (**< Genre String (pointer into text_list). The genre string(s) may very well need postprocessing, esp. for ID3v2.3. *)
    comment: pMpg123_string; (**< Pointer to last encountered comment text with empty description. *)
    (* Encountered ID3v2 fields are appended to these lists.
       There can be multiple occurences, the pointers above always point to the last encountered data. *)
    comment_list: pMpg123_text; (**< Array of comments. *)
    comments: Cardinal;     (**< Number of comments. *)
    text: pMpg123_text;         (**< Array of ID3v2 text fields (including USLT) *)
    texts: Cardinal;        (**< Numer of text fields. *)
    extra: pMpg123_text;        (**< The array of extra (TXXX) fields. *)
    extras: Cardinal;       (**< Number of extra text (TXXX) fields. *)
    picture: pMpg123_picture;     (**< Array of ID3v2 pictures fields (APIC). *)
    pictures: Cardinal;    (**< Number of picture (APIC) fields. *)
  end;

  (** Data structure for ID3v1 tags (the last 128 bytes of a file).
   *  Don't take anything for granted (like string termination)!
   *  Also note the change ID3v1.1 did: comment[28] = 0; comment[29] = track_number
   *  It is your task to support ID3v1 only or ID3v1.1 ...*)
  pMpg123_id3v1 = ^TMpg123_id3v1;
  TMpg123_id3v1 = record
    tag: array[0..2] of AnsiChar;         (**< Always the string "TAG", the classic intro. *)
    title: array[0..29] of AnsiChar;      (**< Title string.  *)
    artist: array[0..29] of AnsiChar;     (**< Artist string. *)
    album: array[0..29] of AnsiChar;      (**< Album string. *)
    year: array[0..3] of AnsiChar;        (**< Year string. *)
    comment: array[0..29] of AnsiChar;    (**< Comment string. *)
    genre: Byte; (**< Genre index. *)
  end;


const
  MPG123_ID3     = $3; (**< 0011 There is some ID3 info. Also matches 0010 or NEW_ID3. *)
  MPG123_NEW_ID3 = $1; (**< 0001 There is ID3 info that changed since last call to mpg123_id3. *)
  MPG123_ICY     = $c; (**< 1100 There is some ICY info. Also matches 0100 or NEW_ICY.*)
  MPG123_NEW_ICY = $4; (**< 0100 There is ICY info that changed since last call to mpg123_icy. *)


(** Query if there is (new) meta info, be it ID3 or ICY (or something new in future).
 *  \param mh handle
 *  \return combination of flags, 0 on error (same as "nothing new")
 *)
function mpg123_meta_check(mh: pMpg123_handle): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_meta_check';

(** Clean up meta data storage (ID3v2 and ICY), freeing memory.
 *  \param mh handle
 *)
procedure mpg123_meta_free(mh: pMpg123_handle);
 cdecl; external link_libmpg123 name _PU + 'mpg123_meta_free';

(** Point v1 and v2 to existing data structures wich may change on any next read/decode function call.
 *  v1 and/or v2 can be set to NULL when there is no corresponding data.
 *  \return MPG123_OK on success
 *)
function mpg123_id3_1(mh: pMpg123_handle;	var v1: pMpg123_id3v1; var v2: pMpg123_id3v2): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_id3';

(** Point icy_meta to existing data structure wich may change on any next read/decode function call.
 *  \param mh handle
 *  \param icy_meta return address for ICY meta string (set to NULL if nothing there)
 *  \return MPG123_OK on success
 *)
function mpg123_icy_1(mh: pMpg123_handle; var icy_meta: MarshaledAString): Integer;
 cdecl; external link_libmpg123 name _PU + 'mpg123_icy';

(** Decode from windows-1252 (the encoding ICY metainfo used) to UTF-8.
 *  Note that this is very similar to mpg123_store_utf8(&sb, mpg123_text_icy, icy_text, strlen(icy_text+1)) .
 *  \param icy_text The input data in ICY encoding
 *  \return pointer to newly allocated buffer with UTF-8 data (You free() it!) *)
function mpg123_icy2utf8(const icy_text: MarshaledAString): MarshaledAString;
 cdecl; external link_libmpg123 name _PU + 'mpg123_icy2utf8';

(* @} *)


(** \defgroup mpg123_advpar mpg123 advanced parameter API
 *
 *  Direct access to a parameter set without full handle around it.
 *	Possible uses:
 *    - Influence behaviour of library _during_ initialization of handle (MPG123_VERBOSE).
 *    - Use one set of parameters for multiple handles.
 *
 *	The functions for handling mpg123_pars (mpg123_par() and mpg123_fmt()
 *  family) directly return a fully qualified mpg123 error code, the ones
 *  operating on full handles normally MPG123_OK or MPG123_ERR, storing the
 *  specific error code itseld inside the handle.
 *
 * @{
 *)
type
  (** Opaque structure for the libmpg123 decoder parameters. *)
  TMpg123_pars_struct = packed record

  end;

  (** Opaque structure for the libmpg123 decoder parameters. *)
  pMpg123_pars = ^TMpg123_pars;
  TMpg123_pars = TMpg123_pars_struct;

(** Create a handle with preset parameters.
 *  \param mp parameter handle
 *  \param decoder decoder choice
 *  \param error error code return address
 *  \return mpg123 handle
 *)
function mpg123_parnew(mp: pMpg123_pars; const decoder: MarshaledAString; error: PInteger): PMpg123_handle;
  cdecl; external link_libmpg123 name _PU + 'mpg123_parnew';

(** Allocate memory for and return a pointer to a new mpg123_pars
 *  \param error error code return address
 *  \return new parameter handle
 *)
function mpg123_new_pars_1(error: PInteger): PMpg123_pars;
  cdecl; external link_libmpg123 name _PU + 'mpg123_new_pars';

(** Delete and free up memory used by a mpg123_pars data structure
 *  \param mp parameter handle
 *)
procedure mpg123_new_pars(mp: pMpg123_pars);
  cdecl; external link_libmpg123 name _PU + 'mpg123_new_pars';
(** Configure mpg123 parameters to accept no output format at all,
 *  use before specifying supported formats with mpg123_format
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
function mpg123_fmt_none(mp: pMpg123_pars): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_fmt_none';

(** Configure mpg123 parameters to accept all formats
 *  (also any custom rate you may set) -- this is default.
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
function mpg123_fmt_all(mp: pMpg123_pars): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_fmt_all';

(** Set the audio format support of a mpg123_pars in detail:
 * \param mp parameter handle
 * \param rate The sample rate value (in Hertz).
 * \param channels A combination of MPG123_STEREO and MPG123_MONO.
 * \param encodings A combination of accepted encodings for rate and channels,
 *                  p.ex MPG123_ENC_SIGNED16|MPG123_ENC_ULAW_8 (or 0 for no
 *                  support).
 * \return MPG123_OK on success
*)
function mpg123_fmt(mp: pMpg123_pars; rate: Longint; channels: Integer; encodings: Integer): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_fmt';

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_pars.
 *  \param mp parameter handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO,
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
function mpg123_fmt_support(mp: pMpg123_pars; rate: Longint; encodings: Integer): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_fmt_support';

(** Set a specific parameter, for a specific mpg123_pars, using a parameter
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 *  \return MPG123_OK on success
 *)
function mpg123_par(mp: pMpg123_pars; _type: TMpg123_parms; value: Longint; fvalue: Double): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_par';

(** Get a specific parameter, for a specific mpg123_pars.
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
function mpg123_getpar(mp: pMpg123_pars; _type: TMpg123_parms; value: PLongint; fvalue: PDouble): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_getpar';

(* @} *)


(** \defgroup mpg123_lowio mpg123 low level I/O
  * You may want to do tricky stuff with I/O that does not work with mpg123's default file access or you want to make it decode into your own pocket...
  *
  * @{ *)

(** Replace default internal buffer with user-supplied buffer.
  * Instead of working on it's own private buffer, mpg123 will directly use the one you provide for storing decoded audio.
  * Note that the required buffer size could be bigger than expected from output
  * encoding if libmpg123 has to convert from primary decoder output (p.ex. 32 bit
  * storage for 24 bit output).
  * \param mh handle
  * \param data pointer to user buffer
  * \param size of buffer in bytes
  * \return MPG123_OK on success
  *)
function mpg123_replace_buffer(mh: pMpg123_handle; data: PByte; size: Cardinal): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_replace_buffer';

(** The max size of one frame's decoded output with current settings.
 *  Use that to determine an appropriate minimum buffer size for decoding one frame.
 *  \param mh handle
 *  \return maximum decoded data size in bytes
 *)
function mpg123_outblock(mh: pMpg123_handle): Cardinal;
  cdecl; external link_libmpg123 name _PU + 'mpg123_outblock';

(** Replace low-level stream access functions; read and lseek as known in POSIX.
 *  You can use this to make any fancy file opening/closing yourself,
 *  using mpg123_open_fd() to set the file descriptor for your read/lseek
 *  (doesn't need to be a "real" file descriptor...).
 *  Setting a function to NULL means that the default internal read is
 *  used (active from next mpg123_open call on).
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this implies mpg123_close().
 * \param mh handle
 * \param r_read callback for reading (behaviour like POSIX read)
 * \param r_lseek callback for seeking (like POSIX lseek)
 * \return MPG123_OK on success
 *)
type
  r_read_cb = function(fildes: Integer; buf: Pointer; nbyte: Cardinal): Cardinal; cdecl;
  r_lseek_cb = function(fildes: Integer; offset: LongInt; whence: Integer): LongInt; cdecl;

function mpg123_replace_reader( mh: pMpg123_handle; r_read: r_read_cb; r_lseek: r_lseek_cb): Integer;
  cdecl; external link_libmpg123 name _PU + 'mpg123_replace_reader';

(** Replace I/O functions with your own ones operating on some kind of
 *  handle instead of integer descriptors.
 *  The handle is a void pointer, so you can pass any data you want...
 *  mpg123_open_handle() is the call you make to use the I/O defined here.
 *  There is no fallback to internal read/seek here.
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this mpg123_close() is implied here.
 *  \param mh handle
 *  \param r_read callback for reading (behaviour like POSIX read)
 *  \param r_lseek callback for seeking (like POSIX lseek)
 *  \param cleanup A callback to clean up an I/O handle on mpg123_close,
 *         can be NULL for none (you take care of cleaning your handles).
 * \return MPG123_OK on success
 *)
//function mpg123_replace_reader_handle( mh: pMpg123_handle
//,	ssize_t (*r_read) (void *, void *, size_t)
//,	off_t (*r_lseek)(void *, off_t, int)
//,	void (*cleanup)(void*) ): Integer;
//  cdecl; external link_libmpg123 name _PU + 'mpg123_replace_reader_handle';

(* @} *)


implementation

end.
