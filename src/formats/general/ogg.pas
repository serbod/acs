(*
  delphi/kylix headers for oggvorbis software codec.
  translated from ogg.h and os_types.h headers
  by andrei borovsky, acs@compiler4.net
  the original c/c++ headers and libraries (c) copyright 1994-2001
  by the xiphophorus company http://www.xiph.org/
*)

{
$Log: ogg.pas,v $
Revision 1.4  2005/12/29 20:46:00  z0m3ie
fixed some problems with vorbis in lazarus

Revision 1.3  2005/12/19 18:36:56  z0m3ie
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

unit ogg;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses

 ACS_Procs,

{$IFDEF LINUX}
  baseunix,dl;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

type

  (* Type declarations from os_types.h header with
     some Delphi-specific types added *)

  OGG_INT64_T = int64;

  POGG_INT64_T = ^OGG_INT64_T;

  OGG_INT32_T = Integer;

  OGG_UINT32_T = LongWord;

  OGG_INT16_T = SmallInt;  // Word ?

  (* Note by AB: The following three type declarations,
   strange as they seem, are required to make C
   pointer/array stuff work in Delphi/Kylix.
   See TVorbisOut class for details. *)

  FLOAT_T = array[0..0] of Single;

  PFLOAT = ^FLOAT_T;

  PPFLOAT = ^PFLOAT;

  // Type declarations from ogg.h header

  POGGPACK_BUFFER = ^OGGPACK_BUFFER;

  OGGPACK_BUFFER = record
    endbyte: LongInt;
    endbit: Integer;
    buffer: PByte;
    ptr: PByte;
    storage: LongInt;
  end;

// ogg_page is used to encapsulate the data in one Ogg bitstream page

  OGG_PAGE = record
    header: PByte;
    header_len: LongInt;
    body: PByte;
    body_len: LongInt;
  end;

(* ogg_stream_state contains the current encode/decode state of a logical
 Ogg bitstream *)

  OGG_STREAM_STATE = record
    body_data: PByte;        // bytes from packet bodies
    body_storage: LongInt;   // storage elements allocated
    body_fill: LongInt;      // elements stored; fill mark
    body_returned: LongInt;  // elements of fill returned
    lacing_vals: PInteger;   // The values that will go to the segment table
    granule_vals: POGG_INT64_T; (* granulepos values for headers. Not compact
                                  this way, but it is simple coupled to the
                                  lacing fifo *)
    lacing_storage: LongInt;
    lacing_fill: LongInt;
    lacing_packet: LongInt;
    lacing_returned: LongInt;
    header: array[0..281] of Byte;  // working space for header encode
    header_fill: Integer;
    e_o_s: Integer;    (* set when we have buffered the last packet in the
                         logical bitstream *)
    b_o_s: Integer;    (* set after we've written the initial page
                         of a logical bitstream *)
    serialno: LongInt;
    pageno: LongInt;
    packetno: OGG_INT64_T;
    (* sequence number for decode; the framing
       knows where there's a hole in the data,
       but we need coupling so that the codec
      (which is in a seperate abstraction
      layer) also knows about the gap *)
    granulepos: OGG_INT64_T;
  end;

(* ogg_packet is used to encapsulate the data and metadata belonging
  to a single raw Ogg/Vorbis packet *)

  POGG_PACKET = ^OGG_PACKET;

  OGG_PACKET = record
    packet: PByte;
    bytes: LongInt;
    b_o_s: LongInt;
    e_o_s: LongInt;
    granulepos: OGG_INT64_T;
    packetno: OGG_INT64_T;
    (* sequence number for decode; the framing
       knows where there's a hole in the data,
       but we need coupling so that the codec
       (which is in a seperate abstraction
       layer) also knows about the gap  *)
  end;

  OGG_SYNC_STATE = record
    data: PByte;
    storage: Integer;
    fill: Integer;
    returned: Integer;
    unsynced: Integer;
    headerbytes: Integer;
    bodybytes: Integer;
  end;


const

{$IFDEF LINUX}
  LiboggPath = 'libogg.so*'; // '/usr/lib/libogg.so';
  {$DEFINE SEARCH_LIBS}
{$ENDIF}

{$IFDEF WIN32}
  LiboggPath = 'ogg.dll';
{$ENDIF}


var
  LiboggLoaded : Boolean = False;

type

// Ogg BITSTREAM PRIMITIVES: bitstream

  oggpack_writeinit_t = procedure(var b: OGGPACK_BUFFER); cdecl;

  oggpack_reset_t = procedure(var b: OGGPACK_BUFFER); cdecl;

  oggpack_writeclear_t = procedure(var b: OGGPACK_BUFFER); cdecl;

  oggpack_readinit_t = procedure(var b: OGGPACK_BUFFER;
                           buf: PByte;
                           bytes: Integer); cdecl;

  oggpack_write_t = procedure(var b: OGGPACK_BUFFER;
                        value: LongInt;
                        bits: Integer); cdecl;

  oggpack_look_t = function(var b: OGGPACK_BUFFER;
                      bits: Integer): LongInt; cdecl;

  oggpack_look_huff_t = function(var b: OGGPACK_BUFFER;
                           bits: Integer): LongInt; cdecl;

  oggpack_look1_t = function(var b: OGGPACK_BUFFER): LongInt; cdecl;

  oggpack_adv_t = procedure(var b: OGGPACK_BUFFER;
                      bits: Integer); cdecl;

  oggpack_adv_huff_t = function(var b: OGGPACK_BUFFER;
                          bits: Integer): Integer; cdecl;

  oggpack_adv1_t = procedure(var b: OGGPACK_BUFFER); cdecl;

  oggpack_read_t = function(var b: OGGPACK_BUFFER;
                      bits: Integer): LongInt; cdecl;

  oggpack_read1_t = function(var b: OGGPACK_BUFFER): LongInt; cdecl;

  oggpack_bytes_t = function(var b: OGGPACK_BUFFER): LongInt; cdecl;

  oggpack_bits_t = function(var b: OGGPACK_BUFFER): LongInt; cdecl;

  oggpack_get_buffer_t = function(var b: OGGPACK_BUFFER): PByte; cdecl;

// Ogg BITSTREAM PRIMITIVES: encoding

  ogg_stream_packetin_t = function(var os: OGG_STREAM_STATE;
                             var op: OGG_PACKET): Integer; cdecl;

  ogg_stream_pageout_t = function(var os: OGG_STREAM_STATE;
                            var og: OGG_PAGE): Integer; cdecl;

  ogg_stream_flush_t = function(var os: OGG_STREAM_STATE;
                          var og: OGG_PAGE): Integer; cdecl;

// Ogg BITSTREAM PRIMITIVES: decoding

  ogg_sync_init_t = function(var oy: OGG_SYNC_STATE): Integer; cdecl;

  ogg_sync_clear_t = function(var oy: OGG_SYNC_STATE): Integer; cdecl;

  ogg_sync_reset_t = function(var oy: OGG_SYNC_STATE): Integer; cdecl;

  ogg_sync_destroy_t = function(var oy: OGG_SYNC_STATE): Integer; cdecl;

  ogg_sync_buffer_t = function(var oy: OGG_SYNC_STATE;
                         size: LongInt): PChar; cdecl;

  ogg_sync_wrote_t = function(var oy: OGG_SYNC_STATE;
                        bytes: LongInt): Integer; cdecl;

  ogg_sync_pageseek_t = function(var oy: OGG_SYNC_STATE;
                           var og: OGG_PAGE): LongInt; cdecl;

  ogg_sync_pageout_t = function(var oy: OGG_SYNC_STATE;
                          var og: OGG_PAGE): Integer; cdecl;

  ogg_stream_pagein_t = function(var os: OGG_STREAM_STATE;
                           var og: OGG_PAGE): Integer; cdecl;

  ogg_stream_packetout_t = function(var os: OGG_STREAM_STATE;
                              var op: OGG_PACKET): Integer; cdecl;

  ogg_stream_packetpeek_t = function(var os: OGG_STREAM_STATE;
                               var op: OGG_PACKET): Integer; cdecl;


// Ogg BITSTREAM PRIMITIVES: general

  ogg_stream_init_t = function(var os: OGG_STREAM_STATE;
                         serialno: Integer): Integer; cdecl;

  ogg_stream_clear_t = function(var os: OGG_STREAM_STATE): Integer; cdecl;

  ogg_stream_reset_t = function(var os: OGG_STREAM_STATE): Integer; cdecl;

  ogg_stream_destroy_t = function(var os: OGG_STREAM_STATE): Integer; cdecl;

  ogg_stream_eos_t = function(var os: OGG_STREAM_STATE): Integer; cdecl;

  ogg_page_checksum_set_t = procedure(var og: OGG_PAGE); cdecl;

  ogg_page_version_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_page_continued_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_page_bos_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_page_eos_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_page_granulepos_t = function(var og: OGG_PAGE): OGG_INT64_T; cdecl;

  ogg_page_serialno_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_page_pageno_t = function(var og: OGG_PAGE): LongInt; cdecl;

  ogg_page_packets_t = function(var og: OGG_PAGE): Integer; cdecl;

  ogg_packet_clear_t = procedure(var op: OGG_PACKET); cdecl;

var

  ogg_stream_init : ogg_stream_init_t;

  ogg_stream_clear : ogg_stream_clear_t;

  ogg_stream_reset : ogg_stream_reset_t;

  ogg_stream_destroy : ogg_stream_destroy_t;

  ogg_stream_eos : ogg_stream_eos_t;

  ogg_page_checksum_set : ogg_page_checksum_set_t;

  ogg_page_version : ogg_page_version_t;

  ogg_page_continued : ogg_page_continued_t;

  ogg_page_bos : ogg_page_bos_t;

  ogg_page_eos : ogg_page_eos_t;

  ogg_page_granulepos : ogg_page_granulepos_t;

  ogg_page_serialno : ogg_page_serialno_t;

  ogg_page_pageno : ogg_page_pageno_t;

  ogg_page_packets : ogg_page_packets_t;

  ogg_packet_clear : ogg_packet_clear_t;

  oggpack_writeinit : oggpack_writeinit_t;

  oggpack_reset : oggpack_reset_t;

  oggpack_writeclear : oggpack_writeclear_t;

  oggpack_readinit : oggpack_readinit_t;

  oggpack_write : oggpack_write_t;

  oggpack_look : oggpack_look_t;

  oggpack_look_huff : oggpack_look_huff_t;

  oggpack_look1 : oggpack_look1_t;

  oggpack_adv : oggpack_adv_t;

  oggpack_adv_huff : oggpack_adv_huff_t;

  oggpack_adv1 : oggpack_adv1_t;

  oggpack_read : oggpack_read_t;

  oggpack_read1 : oggpack_read1_t;

  oggpack_bytes : oggpack_bytes_t;

  oggpack_bits : oggpack_bits_t;

  oggpack_get_buffer : oggpack_get_buffer_t;

  ogg_stream_packetin : ogg_stream_packetin_t;

  ogg_stream_pageout : ogg_stream_pageout_t;

  ogg_stream_flush : ogg_stream_flush_t;

  ogg_sync_init : ogg_sync_init_t;

  ogg_sync_clear : ogg_sync_clear_t;

  ogg_sync_reset : ogg_sync_reset_t;

  ogg_sync_destroy : ogg_sync_destroy_t;

  ogg_sync_buffer : ogg_sync_buffer_t;

  ogg_sync_wrote : ogg_sync_wrote_t;

  ogg_sync_pageseek : ogg_sync_pageseek_t;

  ogg_sync_pageout : ogg_sync_pageout_t;

  ogg_stream_pagein : ogg_stream_pagein_t;

  ogg_stream_packetout : ogg_stream_packetout_t;

  ogg_stream_packetpeek : ogg_stream_packetpeek_t;

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
  Path := FindLibs(LiboggPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LiboggPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LiboggLoaded := True;
    ogg_stream_init := dlsym(Libhandle, 'ogg_stream_init');
    ogg_stream_clear := dlsym(Libhandle, 'ogg_stream_clear');
    ogg_stream_reset := dlsym(Libhandle, 'ogg_stream_reset');
    ogg_stream_destroy := dlsym(Libhandle, 'ogg_stream_destroy');
    ogg_stream_eos := dlsym(Libhandle, 'ogg_stream_eos');
    ogg_page_checksum_set := dlsym(Libhandle, 'ogg_page_checksum_set');
    ogg_page_version := dlsym(Libhandle, 'ogg_page_version');
    ogg_page_continued := dlsym(Libhandle, 'ogg_page_continued');
    ogg_page_bos := dlsym(Libhandle, 'ogg_page_bos');
    ogg_page_eos := dlsym(Libhandle, 'ogg_page_eos');
    ogg_page_granulepos := dlsym(Libhandle, 'ogg_page_granulepos');
    ogg_page_serialno := dlsym(Libhandle, 'ogg_page_serialno');
    ogg_page_pageno := dlsym(Libhandle, 'ogg_page_pageno');
    ogg_page_packets := dlsym(Libhandle, 'ogg_page_packets');
    ogg_packet_clear := dlsym(Libhandle, 'ogg_packet_clear');
    oggpack_writeinit := dlsym(Libhandle, 'oggpack_writeinit');
    oggpack_reset := dlsym(Libhandle, 'oggpack_reset');
    oggpack_writeclear := dlsym(Libhandle, 'oggpack_writeclear');
    oggpack_readinit := dlsym(Libhandle, 'oggpack_readinit');
    oggpack_write := dlsym(Libhandle, 'oggpack_write');
    oggpack_look := dlsym(Libhandle, 'oggpack_look');
    oggpack_look_huff := dlsym(Libhandle, 'oggpack_look_huff');
    oggpack_look1 := dlsym(Libhandle, 'oggpack_look1');
    oggpack_adv := dlsym(Libhandle, 'oggpack_adv');
    oggpack_adv_huff := dlsym(Libhandle, 'oggpack_adv_huff');
    oggpack_adv1 := dlsym(Libhandle, 'oggpack_adv1');
    oggpack_read := dlsym(Libhandle, 'oggpack_read');
    oggpack_read1 := dlsym(Libhandle, 'oggpack_read1');
    oggpack_bytes := dlsym(Libhandle, 'oggpack_bytes');
    oggpack_bits := dlsym(Libhandle, 'oggpack_bits');
    oggpack_get_buffer := dlsym(Libhandle, 'oggpack_get_buffer');
    ogg_stream_packetin := dlsym(Libhandle, 'ogg_stream_packetin');
    ogg_stream_pageout := dlsym(Libhandle, 'ogg_stream_pageout');
    ogg_stream_flush := dlsym(Libhandle, 'ogg_stream_flush');
    ogg_sync_init := dlsym(Libhandle, 'ogg_sync_init');
    ogg_sync_clear := dlsym(Libhandle, 'ogg_sync_clear');
    ogg_sync_reset := dlsym(Libhandle, 'ogg_sync_reset');
    ogg_sync_destroy := dlsym(Libhandle, 'ogg_sync_destroy');
    ogg_sync_buffer := dlsym(Libhandle, 'ogg_sync_buffer');
    ogg_sync_wrote := dlsym(Libhandle, 'ogg_sync_wrote');
    ogg_sync_pageseek := dlsym(Libhandle, 'ogg_sync_pageseek');
    ogg_sync_pageout := dlsym(Libhandle, 'ogg_sync_pageout');
    ogg_stream_pagein := dlsym(Libhandle, 'ogg_stream_pagein');
    ogg_stream_packetout := dlsym(Libhandle, 'ogg_stream_packetout');
    ogg_stream_packetpeek := dlsym(Libhandle, 'ogg_stream_packetpeek');
  end;

finalization

  if Libhandle <> nil then dlclose(Libhandle);

{$ENDIF}

{$IFDEF WIN32}

var
  Libhandle : HMODULE;

initialization
  Libhandle := LoadLibraryEx(LiboggPath, 0, 0);

  if Libhandle <> 0 then
  begin
    LiboggLoaded := True;
    ogg_stream_init := GetProcAddress(Libhandle, 'ogg_stream_init');
    ogg_stream_clear := GetProcAddress(Libhandle, 'ogg_stream_clear');
    ogg_stream_reset := GetProcAddress(Libhandle, 'ogg_stream_reset');
    ogg_stream_destroy := GetProcAddress(Libhandle, 'ogg_stream_destroy');
    ogg_stream_eos := GetProcAddress(Libhandle, 'ogg_stream_eos');
    ogg_page_checksum_set := GetProcAddress(Libhandle, 'ogg_page_checksum_set');
    ogg_page_version := GetProcAddress(Libhandle, 'ogg_page_version');
    ogg_page_continued := GetProcAddress(Libhandle, 'ogg_page_continued');
    ogg_page_bos := GetProcAddress(Libhandle, 'ogg_page_bos');
    ogg_page_eos := GetProcAddress(Libhandle, 'ogg_page_eos');
    ogg_page_granulepos := GetProcAddress(Libhandle, 'ogg_page_granulepos');
    ogg_page_serialno := GetProcAddress(Libhandle, 'ogg_page_serialno');
    ogg_page_pageno := GetProcAddress(Libhandle, 'ogg_page_pageno');
    ogg_page_packets := GetProcAddress(Libhandle, 'ogg_page_packets');
    ogg_packet_clear := GetProcAddress(Libhandle, 'ogg_packet_clear');
    oggpack_writeinit := GetProcAddress(Libhandle, 'oggpack_writeinit');
    oggpack_reset := GetProcAddress(Libhandle, 'oggpack_reset');
    oggpack_writeclear := GetProcAddress(Libhandle, 'oggpack_writeclear');
    oggpack_readinit := GetProcAddress(Libhandle, 'oggpack_readinit');
    oggpack_write := GetProcAddress(Libhandle, 'oggpack_write');
    oggpack_look := GetProcAddress(Libhandle, 'oggpack_look');
    oggpack_look_huff := GetProcAddress(Libhandle, 'oggpack_look_huff');
    oggpack_look1 := GetProcAddress(Libhandle, 'oggpack_look1');
    oggpack_adv := GetProcAddress(Libhandle, 'oggpack_adv');
    oggpack_adv_huff := GetProcAddress(Libhandle, 'oggpack_adv_huff');
    oggpack_adv1 := GetProcAddress(Libhandle, 'oggpack_adv1');
    oggpack_read := GetProcAddress(Libhandle, 'oggpack_read');
    oggpack_read1 := GetProcAddress(Libhandle, 'oggpack_read1');
    oggpack_bytes := GetProcAddress(Libhandle, 'oggpack_bytes');
    oggpack_bits := GetProcAddress(Libhandle, 'oggpack_bits');
    oggpack_get_buffer := GetProcAddress(Libhandle, 'oggpack_get_buffer');
    ogg_stream_packetin := GetProcAddress(Libhandle, 'ogg_stream_packetin');
    ogg_stream_pageout := GetProcAddress(Libhandle, 'ogg_stream_pageout');
    ogg_stream_flush := GetProcAddress(Libhandle, 'ogg_stream_flush');
    ogg_sync_init := GetProcAddress(Libhandle, 'ogg_sync_init');
    ogg_sync_clear := GetProcAddress(Libhandle, 'ogg_sync_clear');
    ogg_sync_reset := GetProcAddress(Libhandle, 'ogg_sync_reset');
    ogg_sync_destroy := GetProcAddress(Libhandle, 'ogg_sync_destroy');
    ogg_sync_buffer := GetProcAddress(Libhandle, 'ogg_sync_buffer');
    ogg_sync_wrote := GetProcAddress(Libhandle, 'ogg_sync_wrote');
    ogg_sync_pageseek := GetProcAddress(Libhandle, 'ogg_sync_pageseek');
    ogg_sync_pageout := GetProcAddress(Libhandle, 'ogg_sync_pageout');
    ogg_stream_pagein := GetProcAddress(Libhandle, 'ogg_stream_pagein');
    ogg_stream_packetout := GetProcAddress(Libhandle, 'ogg_stream_packetout');
    ogg_stream_packetpeek := GetProcAddress(Libhandle, 'ogg_stream_packetpeek');
  end;

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

{$ENDIF}

end.
