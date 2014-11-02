(*
  delphi/kylix headers for oggvorbis software codec.
  translated from codec.h header
  by andrei borovsky, acs@compiler4.net
  the original c/c++ headers and libraries (c) copyright 1994-2001
  by the xiphophorus company http://www.xiph.org/
*)

{
$Log: vorbiscodec.pas,v $
Revision 1.1  2005/12/29 20:46:00  z0m3ie
fixed some problems with vorbis in lazarus

Revision 1.1  2005/12/19 18:36:56  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/12 22:04:53  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}
{$IFDEF FPC}
  {$IFDEF WIN32}
    {$PACKRECORDS C}
  {$ENDIF WIN32}
{$ENDIF WIN32}

unit vorbiscodec;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses

  ACS_Procs,

  {$IFDEF LINUX}
  baseunix,dl,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  ogg;

type

  PVORBIS_INFO = ^VORBIS_INFO;

  VORBIS_INFO = record
    version: Integer;
    channels: Integer;
    rate: LongInt;

    (* The below bitrate declarations are *hints*.
      Combinations of the three values carry the following implications:
      all three set to the same value:
      implies a fixed rate bitstream
      only nominal set:
      implies a VBR stream that averages the nominal bitrate. No hard
      upper/lower limit
      upper and or lower set:
      implies a VBR bitstream that obeys the bitrate limits. nominal
      may also be set to give a nominal rate.
      none set:
      the coder does not care to speculate. *)

    bitrate_upper: LongInt;
    bitrate_nominal: LongInt;
    bitrate_lower: LongInt;
    bitrate_window: LongInt;
    codec_setup: Pointer;
  end;

(* vorbis_dsp_state buffers the current vorbis audio
  analysis/synthesis state. The DSP state belongs to a specific
  logical bitstream *)

  PVORBIS_DSP_STATE = ^VORBIS_DSP_STATE;

  VORBIS_DSP_STATE = record
    analysisp: Integer;
    vi: PVORBIS_INFO;
    pcm: PPFLOAT;
    pcmret: PPFLOAT;
    pcm_storage: Integer;
    pcm_current: Integer;
    pcm_returned: Integer;
    preextrapolate: Integer;
    eofflag: Integer;
    lW: LongInt;
    W: LongInt;
    nW: LongInt;
    centerW: LongInt;
    granulepos: OGG_INT64_T;
    sequence: OGG_INT64_T;
    glue_bits: OGG_INT64_T;
    time_bits: OGG_INT64_T;
    floor_bits: OGG_INT64_T;
    res_bits: OGG_INT64_T;
    backend_state: Pointer;
  end;

  PALLOC_CHAIN = ^ALLOC_CHAIN;

  ALLOC_CHAIN = record
    ptr: Pointer;
    next: PALLOC_CHAIN;
  end;


  VORBIS_BLOCK = record
    // necessary stream state for linking to the framing abstraction
    pcm: PPFLOAT;
    // this is a pointer into local storage
    opb: OGGPACK_BUFFER;
    lW: LongInt;
    W: LongInt;
    nW: LongInt;
    pcmend: Integer;
    mode: Integer;
    eofflag: Integer;
    granulepos: OGG_INT64_T;
    sequence: OGG_INT64_T;
    vd: PVORBIS_DSP_STATE; // For read-only access of configuration
    (* local storage to avoid remallocing; it's up to the mapping to
    structure it *)
    localstore: Pointer;
    localtop: LongInt;
    localalloc: LongInt;
    totaluse: LongInt;
    reap: PALLOC_CHAIN;
    // bitmetrics for the frame
    glue_bits: LongInt;
    time_bits: LongInt;
    floor_bits: LongInt;
    res_bits: LongInt;
    internal: Pointer;
  end;

(* vorbis_block is a single block of data to be processed as part of
  the analysis/synthesis stream; it belongs to a specific logical
  bitstream, but is independant from other vorbis_blocks belonging to
  that logical bitstream. *)

(* vorbis_info contains all the setup information specific to the
  specific compression/decompression mode in progress (eg,
  psychoacoustic settings, channel setup, options, codebook
  etc). vorbis_info and substructures are in backends.h. *)

(* the comments are not part of vorbis_info so that vorbis_info can be
  static storage *)

  PVORBIS_COMMENT = ^VORBIS_COMMENT;

  VORBIS_COMMENT = record
  (* unlimited user comment fields. libvorbis writes 'libvorbis'
   whatever vendor is set to in encode *)
    user_comments: PPChar;
    comment_lengths: PInteger;
    comments: Integer;
    vendor: PChar;
  end;

 (* libvorbis encodes in two abstraction layers; first we perform DSP
  and produce a packet (see docs/analysis.txt). The packet is then
  coded into a framed OggSquish bitstream by the second layer (see
  docs/framing.txt). Decode is the reverse process; we sync/frame
  the bitstream and extract individual packets, then decode the
  packet back into PCM audio. *)

(* The extra framing/packetizing is used in streaming formats, such as
  files. Over the net (such as with UDP), the framing and
  packetization aren't necessary as they're provided by the transport
  and the streaming layer is not used *)

// Vorbis PRIMITIVES: general


const

{$IFDEF LINUX}
  LibvorbisPath = 'libvorbis.so*';  //'/usr/lib/libvorbis.so';
  {$DEFINE SEARCH_LIBS}
{$ENDIF}

{$IFDEF WIN32}
  LibvorbisPath = 'vorbis.dll';
{$ENDIF}

var
  LibvorbisLoaded : Boolean = False;

type

  vorbis_info_init_t = procedure(var vi: VORBIS_INFO); cdecl;

  vorbis_info_clear_t = procedure(var vi: VORBIS_INFO); cdecl;

  vorbis_info_blocksize_t = function(var vi: VORBIS_INFO;
                               zo: Integer): Integer; cdecl;

  vorbis_comment_init_t = procedure(var vc: VORBIS_COMMENT); cdecl;

  vorbis_comment_add_t = procedure(var vc: VORBIS_COMMENT;
                             comment: PChar); cdecl;

  vorbis_comment_add_tag_t = procedure(var vc: VORBIS_COMMENT;
                                 tag: PChar;
                                 contents: PChar); cdecl;

  vorbis_comment_query_t = function(var vc: VORBIS_COMMENT;
                              tag: PChar;
                              count: Integer): PChar; cdecl;

  vorbis_comment_query_count_t = function(var vc: VORBIS_COMMENT;
                                    tag: PChar): Integer; cdecl;

  vorbis_comment_clear_t = procedure(var vc: VORBIS_COMMENT) cdecl;

  vorbis_block_init_t = function(var v: VORBIS_DSP_STATE;
                           var vb: VORBIS_BLOCK): Integer; cdecl;

  vorbis_block_clear_t = function(var vb: VORBIS_BLOCK): Integer; cdecl;

  vorbis_dsp_clear_t = procedure(var v: VORBIS_DSP_STATE); cdecl;

// Vorbis PRIMITIVES: analysis/DSP layer

  vorbis_analysis_init_t = function(var v: VORBIS_DSP_STATE;
                              var vi: VORBIS_INFO): Integer; cdecl;

  vorbis_commentheader_out_t = function(var vc: VORBIS_COMMENT;
                                  var op: OGG_PACKET): Integer; cdecl;

  vorbis_analysis_headerout_t = function(var v: VORBIS_DSP_STATE;
                                   var vc: VORBIS_COMMENT;
                                   var op: OGG_PACKET;
                                   var op_comm: OGG_PACKET;
                                   var op_code: OGG_PACKET): Integer; cdecl;

  vorbis_analysis_buffer_t = function(var v: VORBIS_DSP_STATE;
                                vals: Integer): PPFLOAT; cdecl;

  vorbis_analysis_wrote_t = function(var v: VORBIS_DSP_STATE;
                               vals: Integer): Integer; cdecl;

  vorbis_analysis_blockout_t = function(var v: VORBIS_DSP_STATE;
                                  var vb: VORBIS_BLOCK): Integer; cdecl;

  vorbis_analysis_t = function(var vb: VORBIS_BLOCK;
                         op: POGG_PACKET): Integer; cdecl;

  vorbis_bitrate_addblock_t = function(var vb: VORBIS_BLOCK): Integer; cdecl;

  vorbis_bitrate_flushpacket_t = function(var vd: VORBIS_DSP_STATE;
                                    var op: OGG_PACKET): Integer; cdecl;

// Vorbis PRIMITIVES: synthesis layer

  vorbis_synthesis_headerin_t = function(var vi: VORBIS_INFO;
                                   var vc: VORBIS_COMMENT;
                                   var op: OGG_PACKET): Integer; cdecl;

  vorbis_synthesis_init_t = function(var v: VORBIS_DSP_STATE;
                               var vi: VORBIS_INFO): Integer; cdecl;

  vorbis_synthesis_t = function(var vb: VORBIS_BLOCK;
                          var op: OGG_PACKET): Integer; cdecl;

  vorbis_synthesis_blockin_t = function(var v: VORBIS_DSP_STATE;
                                  var vb: VORBIS_BLOCK): Integer; cdecl;

  vorbis_synthesis_pcmout_t = function(var v: VORBIS_DSP_STATE;
                                 var pcm: PPFLOAT): Integer; cdecl;

  vorbis_synthesis_read_t = function(var v: VORBIS_DSP_STATE;
                               samples: Integer): Integer; cdecl;

  vorbis_packet_blocksize_t = function(var vi: VORBIS_INFO;
                                       var op: OGG_PACKET): LongInt; cdecl;
var

  vorbis_info_init : vorbis_info_init_t;

  vorbis_info_clear : vorbis_info_clear_t;

  vorbis_info_blocksize : vorbis_info_blocksize_t;

  vorbis_comment_init : vorbis_comment_init_t;

  vorbis_comment_add : vorbis_comment_add_t;

  vorbis_comment_add_tag : vorbis_comment_add_tag_t;

  vorbis_comment_query : vorbis_comment_query_t;

  vorbis_comment_query_count : vorbis_comment_query_count_t;

  vorbis_comment_clear : vorbis_comment_clear_t;

  vorbis_block_init : vorbis_block_init_t;

  vorbis_block_clear : vorbis_block_clear_t;

  vorbis_dsp_clear : vorbis_dsp_clear_t;

  vorbis_analysis_init : vorbis_analysis_init_t;

  vorbis_commentheader_out : vorbis_commentheader_out_t;

  vorbis_analysis_headerout : vorbis_analysis_headerout_t;

  vorbis_analysis_buffer : vorbis_analysis_buffer_t;

  vorbis_analysis_wrote : vorbis_analysis_wrote_t;

  vorbis_analysis_blockout : vorbis_analysis_blockout_t;

  vorbis_analysis : vorbis_analysis_t;

  vorbis_bitrate_addblock : vorbis_bitrate_addblock_t;

  vorbis_bitrate_flushpacket : vorbis_bitrate_flushpacket_t;

  vorbis_synthesis_headerin : vorbis_synthesis_headerin_t;

  vorbis_synthesis_init : vorbis_synthesis_init_t;

  vorbis_synthesis : vorbis_synthesis_t;

  vorbis_synthesis_blockin : vorbis_synthesis_blockin_t;

  vorbis_synthesis_pcmout : vorbis_synthesis_pcmout_t;

  vorbis_synthesis_read : vorbis_synthesis_read_t;

  vorbis_packet_blocksize : vorbis_packet_blocksize_t;

// Vorbis ERRORS and return codes

const

  OV_FALSE = -1;
  OV_EOF = -2;
  OV_HOLE = -3;
  OV_EREAD = -128;
  OV_EFAULT = -129;
  OV_EIMPL = -130;
  OV_EINVAL = -131;
  OV_ENOTVORBIS = -132;
  OV_EBADHEADER = -133;
  OV_EVERSION = -134;
  OV_ENOTAUDIO = -135;
  OV_EBADPACKET = -136;
  OV_EBADLINK = -137;
  OV_ENOSEEK = -138;

implementation

{$IFDEF LINUX}

var
  Libhandle : Pointer;

{$IFDEF SEARCH_LIBS}
  Path : String;
{$ENDIF}

initialization

{$IFDEF SEARCH_LIBS}

  Libhandle := nil;
  Path := FindLibs(LibvorbisPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LibvorbisPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LibvorbisLoaded := True;
    vorbis_info_init := dlsym(Libhandle, 'vorbis_info_init');
    vorbis_info_clear := dlsym(Libhandle, 'vorbis_info_clear');
    vorbis_info_blocksize := dlsym(Libhandle, 'vorbis_info_blocksize');
    vorbis_comment_init := dlsym(Libhandle, 'vorbis_comment_init');
    vorbis_comment_add := dlsym(Libhandle, 'vorbis_comment_add');
    vorbis_comment_add_tag := dlsym(Libhandle, 'vorbis_comment_add_tag');
    vorbis_comment_query := dlsym(Libhandle, 'vorbis_comment_query');
    vorbis_comment_query_count := dlsym(Libhandle, 'vorbis_comment_query_count');
    vorbis_comment_clear := dlsym(Libhandle, 'vorbis_comment_clear');
    vorbis_block_init := dlsym(Libhandle, 'vorbis_block_init');
    vorbis_block_clear := dlsym(Libhandle, 'vorbis_block_clear');
    vorbis_dsp_clear := dlsym(Libhandle, 'vorbis_dsp_clear');
    vorbis_analysis_init := dlsym(Libhandle, 'vorbis_analysis_init');
    vorbis_commentheader_out := dlsym(Libhandle, 'vorbis_commentheader_out');
    vorbis_analysis_headerout := dlsym(Libhandle, 'vorbis_analysis_headerout');
    vorbis_analysis_buffer := dlsym(Libhandle, 'vorbis_analysis_buffer');
    vorbis_analysis_wrote := dlsym(Libhandle, 'vorbis_analysis_wrote');
    vorbis_analysis_blockout := dlsym(Libhandle, 'vorbis_analysis_blockout');
    vorbis_analysis := dlsym(Libhandle, 'vorbis_analysis');
    vorbis_bitrate_addblock := dlsym(Libhandle, 'vorbis_bitrate_addblock');
    vorbis_bitrate_flushpacket := dlsym(Libhandle, 'vorbis_bitrate_flushpacket');
    vorbis_synthesis_headerin := dlsym(Libhandle, 'vorbis_synthesis_headerin');
    vorbis_synthesis_init := dlsym(Libhandle, 'vorbis_synthesis_init');
    vorbis_synthesis := dlsym(Libhandle, 'vorbis_synthesis');
    vorbis_synthesis_blockin := dlsym(Libhandle, 'vorbis_synthesis_blockin');
    vorbis_synthesis_pcmout := dlsym(Libhandle, 'vorbis_synthesis_pcmout');
    vorbis_synthesis_read := dlsym(Libhandle, 'vorbis_synthesis_read');
    vorbis_packet_blocksize := dlsym(Libhandle, 'vorbis_packet_blocksize');
  end;

  finalization
    if libhandle <> nil then dlclose(libhandle);

{$ENDIF}

{$IFDEF WIN32}

var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(LibvorbisPath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibvorbisLoaded := True;
    vorbis_info_init := GetProcAddress(Libhandle, 'vorbis_info_init');
    vorbis_info_clear := GetProcAddress(Libhandle, 'vorbis_info_clear');
    vorbis_info_blocksize := GetProcAddress(Libhandle, 'vorbis_info_blocksize');
    vorbis_comment_init := GetProcAddress(Libhandle, 'vorbis_comment_init');
    vorbis_comment_add := GetProcAddress(Libhandle, 'vorbis_comment_add');
    vorbis_comment_add_tag := GetProcAddress(Libhandle, 'vorbis_comment_add_tag');
    vorbis_comment_query := GetProcAddress(Libhandle, 'vorbis_comment_query');
    vorbis_comment_query_count := GetProcAddress(Libhandle, 'vorbis_comment_query_count');
    vorbis_comment_clear := GetProcAddress(Libhandle, 'vorbis_comment_clear');
    vorbis_block_init := GetProcAddress(Libhandle, 'vorbis_block_init');
    vorbis_block_clear := GetProcAddress(Libhandle, 'vorbis_block_clear');
    vorbis_dsp_clear := GetProcAddress(Libhandle, 'vorbis_dsp_clear');
    vorbis_analysis_init := GetProcAddress(Libhandle, 'vorbis_analysis_init');
    vorbis_commentheader_out := GetProcAddress(Libhandle, 'vorbis_commentheader_out');
    vorbis_analysis_headerout := GetProcAddress(Libhandle, 'vorbis_analysis_headerout');
    vorbis_analysis_buffer := GetProcAddress(Libhandle, 'vorbis_analysis_buffer');
    vorbis_analysis_wrote := GetProcAddress(Libhandle, 'vorbis_analysis_wrote');
    vorbis_analysis_blockout := GetProcAddress(Libhandle, 'vorbis_analysis_blockout');
    vorbis_analysis := GetProcAddress(Libhandle, 'vorbis_analysis');
    vorbis_bitrate_addblock := GetProcAddress(Libhandle, 'vorbis_bitrate_addblock');
    vorbis_bitrate_flushpacket := GetProcAddress(Libhandle, 'vorbis_bitrate_flushpacket');
    vorbis_synthesis_headerin := GetProcAddress(Libhandle, 'vorbis_synthesis_headerin');
    vorbis_synthesis_init := GetProcAddress(Libhandle, 'vorbis_synthesis_init');
    vorbis_synthesis := GetProcAddress(Libhandle, 'vorbis_synthesis');
    vorbis_synthesis_blockin := GetProcAddress(Libhandle, 'vorbis_synthesis_blockin');
    vorbis_synthesis_pcmout := GetProcAddress(Libhandle, 'vorbis_synthesis_pcmout');
    vorbis_synthesis_read := GetProcAddress(Libhandle, 'vorbis_synthesis_read');
    vorbis_packet_blocksize := GetProcAddress(Libhandle, 'vorbis_packet_blocksize');
  end;

  finalization
    if Libhandle <> 0 then FreeLibrary(Libhandle);

{$ENDIF}


end.
