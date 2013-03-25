(* OCaml bindings for libvirt.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the OCaml linking exception described in ../COPYING.LIB.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*)

type uuid = string

type xml = string

type filename = string

external get_version : ?driver:string -> unit -> int * int = "ocaml_libvirt_get_version"

let uuid_length = 16
let uuid_string_length = 36

(* http://caml.inria.fr/pub/ml-archives/caml-list/2004/07/80683af867cce6bf8fff273973f70c95.en.html *)
type rw = [`R|`W]
type ro = [`R]

module Connect =
struct
  type 'rw t

  type node_info = {
    model : string;
    memory : int64;
    cpus : int;
    mhz : int;
    nodes : int;
    sockets : int;
    cores : int;
    threads : int;
  }

  type list_flag =
    | ListNoState | ListRunning | ListBlocked
    | ListPaused | ListShutdown | ListShutoff | ListCrashed
    | ListActive
    | ListInactive
    | ListAll

  external connect : ?name:string -> unit -> rw t = "ocaml_libvirt_connect_open"
  external connect_readonly : ?name:string -> unit -> ro t = "ocaml_libvirt_connect_open_readonly"
  external close : [>`R] t -> unit = "ocaml_libvirt_connect_close"
  external get_type : [>`R] t -> string = "ocaml_libvirt_connect_get_type"
  external get_version : [>`R] t -> int = "ocaml_libvirt_connect_get_version"
  external get_hostname : [>`R] t -> string = "ocaml_libvirt_connect_get_hostname"
  external get_uri : [>`R] t -> string = "ocaml_libvirt_connect_get_uri"
  external get_max_vcpus : [>`R] t -> ?type_:string -> unit -> int = "ocaml_libvirt_connect_get_max_vcpus"
  external list_domains : [>`R] t -> int -> int array = "ocaml_libvirt_connect_list_domains"
  external num_of_domains : [>`R] t -> int = "ocaml_libvirt_connect_num_of_domains"
  external get_capabilities : [>`R] t -> xml = "ocaml_libvirt_connect_get_capabilities"
  external num_of_defined_domains : [>`R] t -> int = "ocaml_libvirt_connect_num_of_defined_domains"
  external list_defined_domains : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_defined_domains"
  external num_of_networks : [>`R] t -> int = "ocaml_libvirt_connect_num_of_networks"
  external list_networks : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_networks"
  external num_of_defined_networks : [>`R] t -> int = "ocaml_libvirt_connect_num_of_defined_networks"
  external list_defined_networks : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_defined_networks"
  external num_of_pools : [>`R] t -> int = "ocaml_libvirt_connect_num_of_storage_pools"
  external list_pools : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_storage_pools"
  external num_of_defined_pools : [>`R] t -> int = "ocaml_libvirt_connect_num_of_defined_storage_pools"
  external list_defined_pools : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_defined_storage_pools"

  external get_node_info : [>`R] t -> node_info = "ocaml_libvirt_connect_get_node_info"
  external node_get_free_memory : [> `R] t -> int64 = "ocaml_libvirt_connect_node_get_free_memory"
  external node_get_cells_free_memory : [> `R] t -> int -> int -> int64 array = "ocaml_libvirt_connect_node_get_cells_free_memory"

  (* See VIR_NODEINFO_MAXCPUS macro defined in <libvirt.h>. *)
  let maxcpus_of_node_info { nodes = nodes; sockets = sockets;
			     cores = cores; threads = threads } =
    nodes * sockets * cores * threads

  (* See VIR_CPU_MAPLEN macro defined in <libvirt.h>. *)
  let cpumaplen nr_cpus =
    (nr_cpus + 7) / 8

  (* See VIR_USE_CPU, VIR_UNUSE_CPU, VIR_CPU_USABLE macros defined in <libvirt.h>. *)
  let use_cpu cpumap cpu =
    cpumap.[cpu/8] <-
      Char.chr (Char.code cpumap.[cpu/8] lor (1 lsl (cpu mod 8)))
  let unuse_cpu cpumap cpu =
    cpumap.[cpu/8] <-
      Char.chr (Char.code cpumap.[cpu/8] land (lnot (1 lsl (cpu mod 8))))
  let cpu_usable cpumaps maplen vcpu cpu =
    Char.code cpumaps.[vcpu*maplen + cpu/8] land (1 lsl (cpu mod 8)) <> 0

  external set_keep_alive : [>`R] t -> int -> int -> unit = "ocaml_libvirt_connect_set_keep_alive"

  external const : [>`R] t -> ro t = "%identity"
end

module Virterror =
struct
  type code =
    | VIR_ERR_OK
    | VIR_ERR_INTERNAL_ERROR
    | VIR_ERR_NO_MEMORY
    | VIR_ERR_NO_SUPPORT
    | VIR_ERR_UNKNOWN_HOST
    | VIR_ERR_NO_CONNECT
    | VIR_ERR_INVALID_CONN
    | VIR_ERR_INVALID_DOMAIN
    | VIR_ERR_INVALID_ARG
    | VIR_ERR_OPERATION_FAILED
    | VIR_ERR_GET_FAILED
    | VIR_ERR_POST_FAILED
    | VIR_ERR_HTTP_ERROR
    | VIR_ERR_SEXPR_SERIAL
    | VIR_ERR_NO_XEN
    | VIR_ERR_XEN_CALL
    | VIR_ERR_OS_TYPE
    | VIR_ERR_NO_KERNEL
    | VIR_ERR_NO_ROOT
    | VIR_ERR_NO_SOURCE
    | VIR_ERR_NO_TARGET
    | VIR_ERR_NO_NAME
    | VIR_ERR_NO_OS
    | VIR_ERR_NO_DEVICE
    | VIR_ERR_NO_XENSTORE
    | VIR_ERR_DRIVER_FULL
    | VIR_ERR_CALL_FAILED
    | VIR_ERR_XML_ERROR
    | VIR_ERR_DOM_EXIST
    | VIR_ERR_OPERATION_DENIED
    | VIR_ERR_OPEN_FAILED
    | VIR_ERR_READ_FAILED
    | VIR_ERR_PARSE_FAILED
    | VIR_ERR_CONF_SYNTAX
    | VIR_ERR_WRITE_FAILED
    | VIR_ERR_XML_DETAIL
    | VIR_ERR_INVALID_NETWORK
    | VIR_ERR_NETWORK_EXIST
    | VIR_ERR_SYSTEM_ERROR
    | VIR_ERR_RPC
    | VIR_ERR_GNUTLS_ERROR
    | VIR_WAR_NO_NETWORK
    | VIR_ERR_NO_DOMAIN
    | VIR_ERR_NO_NETWORK
    | VIR_ERR_INVALID_MAC
    | VIR_ERR_AUTH_FAILED
    | VIR_ERR_INVALID_STORAGE_POOL
    | VIR_ERR_INVALID_STORAGE_VOL
    | VIR_WAR_NO_STORAGE
    | VIR_ERR_NO_STORAGE_POOL
    | VIR_ERR_NO_STORAGE_VOL
    | VIR_ERR_UNKNOWN of int

  let string_of_code = function
    | VIR_ERR_OK -> "VIR_ERR_OK"
    | VIR_ERR_INTERNAL_ERROR -> "VIR_ERR_INTERNAL_ERROR"
    | VIR_ERR_NO_MEMORY -> "VIR_ERR_NO_MEMORY"
    | VIR_ERR_NO_SUPPORT -> "VIR_ERR_NO_SUPPORT"
    | VIR_ERR_UNKNOWN_HOST -> "VIR_ERR_UNKNOWN_HOST"
    | VIR_ERR_NO_CONNECT -> "VIR_ERR_NO_CONNECT"
    | VIR_ERR_INVALID_CONN -> "VIR_ERR_INVALID_CONN"
    | VIR_ERR_INVALID_DOMAIN -> "VIR_ERR_INVALID_DOMAIN"
    | VIR_ERR_INVALID_ARG -> "VIR_ERR_INVALID_ARG"
    | VIR_ERR_OPERATION_FAILED -> "VIR_ERR_OPERATION_FAILED"
    | VIR_ERR_GET_FAILED -> "VIR_ERR_GET_FAILED"
    | VIR_ERR_POST_FAILED -> "VIR_ERR_POST_FAILED"
    | VIR_ERR_HTTP_ERROR -> "VIR_ERR_HTTP_ERROR"
    | VIR_ERR_SEXPR_SERIAL -> "VIR_ERR_SEXPR_SERIAL"
    | VIR_ERR_NO_XEN -> "VIR_ERR_NO_XEN"
    | VIR_ERR_XEN_CALL -> "VIR_ERR_XEN_CALL"
    | VIR_ERR_OS_TYPE -> "VIR_ERR_OS_TYPE"
    | VIR_ERR_NO_KERNEL -> "VIR_ERR_NO_KERNEL"
    | VIR_ERR_NO_ROOT -> "VIR_ERR_NO_ROOT"
    | VIR_ERR_NO_SOURCE -> "VIR_ERR_NO_SOURCE"
    | VIR_ERR_NO_TARGET -> "VIR_ERR_NO_TARGET"
    | VIR_ERR_NO_NAME -> "VIR_ERR_NO_NAME"
    | VIR_ERR_NO_OS -> "VIR_ERR_NO_OS"
    | VIR_ERR_NO_DEVICE -> "VIR_ERR_NO_DEVICE"
    | VIR_ERR_NO_XENSTORE -> "VIR_ERR_NO_XENSTORE"
    | VIR_ERR_DRIVER_FULL -> "VIR_ERR_DRIVER_FULL"
    | VIR_ERR_CALL_FAILED -> "VIR_ERR_CALL_FAILED"
    | VIR_ERR_XML_ERROR -> "VIR_ERR_XML_ERROR"
    | VIR_ERR_DOM_EXIST -> "VIR_ERR_DOM_EXIST"
    | VIR_ERR_OPERATION_DENIED -> "VIR_ERR_OPERATION_DENIED"
    | VIR_ERR_OPEN_FAILED -> "VIR_ERR_OPEN_FAILED"
    | VIR_ERR_READ_FAILED -> "VIR_ERR_READ_FAILED"
    | VIR_ERR_PARSE_FAILED -> "VIR_ERR_PARSE_FAILED"
    | VIR_ERR_CONF_SYNTAX -> "VIR_ERR_CONF_SYNTAX"
    | VIR_ERR_WRITE_FAILED -> "VIR_ERR_WRITE_FAILED"
    | VIR_ERR_XML_DETAIL -> "VIR_ERR_XML_DETAIL"
    | VIR_ERR_INVALID_NETWORK -> "VIR_ERR_INVALID_NETWORK"
    | VIR_ERR_NETWORK_EXIST -> "VIR_ERR_NETWORK_EXIST"
    | VIR_ERR_SYSTEM_ERROR -> "VIR_ERR_SYSTEM_ERROR"
    | VIR_ERR_RPC -> "VIR_ERR_RPC"
    | VIR_ERR_GNUTLS_ERROR -> "VIR_ERR_GNUTLS_ERROR"
    | VIR_WAR_NO_NETWORK -> "VIR_WAR_NO_NETWORK"
    | VIR_ERR_NO_DOMAIN -> "VIR_ERR_NO_DOMAIN"
    | VIR_ERR_NO_NETWORK -> "VIR_ERR_NO_NETWORK"
    | VIR_ERR_INVALID_MAC -> "VIR_ERR_INVALID_MAC"
    | VIR_ERR_AUTH_FAILED -> "VIR_ERR_AUTH_FAILED"
    | VIR_ERR_INVALID_STORAGE_POOL -> "VIR_ERR_INVALID_STORAGE_POOL"
    | VIR_ERR_INVALID_STORAGE_VOL -> "VIR_ERR_INVALID_STORAGE_VOL"
    | VIR_WAR_NO_STORAGE -> "VIR_WAR_NO_STORAGE"
    | VIR_ERR_NO_STORAGE_POOL -> "VIR_ERR_NO_STORAGE_POOL"
    | VIR_ERR_NO_STORAGE_VOL -> "VIR_ERR_NO_STORAGE_VOL"
    | VIR_ERR_UNKNOWN i -> "VIR_ERR_" ^ string_of_int i

  type domain =
    | VIR_FROM_NONE
    | VIR_FROM_XEN
    | VIR_FROM_XEND
    | VIR_FROM_XENSTORE
    | VIR_FROM_SEXPR
    | VIR_FROM_XML
    | VIR_FROM_DOM
    | VIR_FROM_RPC
    | VIR_FROM_PROXY
    | VIR_FROM_CONF
    | VIR_FROM_QEMU
    | VIR_FROM_NET
    | VIR_FROM_TEST
    | VIR_FROM_REMOTE
    | VIR_FROM_OPENVZ
    | VIR_FROM_XENXM
    | VIR_FROM_STATS_LINUX
    | VIR_FROM_STORAGE
    | VIR_FROM_UNKNOWN of int

  let string_of_domain = function
    | VIR_FROM_NONE -> "VIR_FROM_NONE"
    | VIR_FROM_XEN -> "VIR_FROM_XEN"
    | VIR_FROM_XEND -> "VIR_FROM_XEND"
    | VIR_FROM_XENSTORE -> "VIR_FROM_XENSTORE"
    | VIR_FROM_SEXPR -> "VIR_FROM_SEXPR"
    | VIR_FROM_XML -> "VIR_FROM_XML"
    | VIR_FROM_DOM -> "VIR_FROM_DOM"
    | VIR_FROM_RPC -> "VIR_FROM_RPC"
    | VIR_FROM_PROXY -> "VIR_FROM_PROXY"
    | VIR_FROM_CONF -> "VIR_FROM_CONF"
    | VIR_FROM_QEMU -> "VIR_FROM_QEMU"
    | VIR_FROM_NET -> "VIR_FROM_NET"
    | VIR_FROM_TEST -> "VIR_FROM_TEST"
    | VIR_FROM_REMOTE -> "VIR_FROM_REMOTE"
    | VIR_FROM_OPENVZ -> "VIR_FROM_OPENVZ"
    | VIR_FROM_XENXM -> "VIR_FROM_XENXM"
    | VIR_FROM_STATS_LINUX -> "VIR_FROM_STATS_LINUX"
    | VIR_FROM_STORAGE -> "VIR_FROM_STORAGE"
    | VIR_FROM_UNKNOWN i -> "VIR_FROM_" ^ string_of_int i

  type level =
    | VIR_ERR_NONE
    | VIR_ERR_WARNING
    | VIR_ERR_ERROR
    | VIR_ERR_UNKNOWN_LEVEL of int

  let string_of_level = function
    | VIR_ERR_NONE -> "VIR_ERR_NONE"
    | VIR_ERR_WARNING -> "VIR_ERR_WARNING"
    | VIR_ERR_ERROR -> "VIR_ERR_ERROR"
    | VIR_ERR_UNKNOWN_LEVEL i -> "VIR_ERR_LEVEL_" ^ string_of_int i

  type t = {
    code : code;
    domain : domain;
    message : string option;
    level : level;
    str1 : string option;
    str2 : string option;
    str3 : string option;
    int1 : int32;
    int2 : int32;
  }

  let to_string { code = code; domain = domain; message = message } =
    let buf = Buffer.create 128 in
    Buffer.add_string buf "libvirt: ";
    Buffer.add_string buf (string_of_code code);
    Buffer.add_string buf ": ";
    Buffer.add_string buf (string_of_domain domain);
    Buffer.add_string buf ": ";
    (match message with Some msg -> Buffer.add_string buf msg | None -> ());
    Buffer.contents buf

  external get_last_error : unit -> t option = "ocaml_libvirt_virterror_get_last_error"
  external get_last_conn_error : [>`R] Connect.t -> t option = "ocaml_libvirt_virterror_get_last_conn_error"
  external reset_last_error : unit -> unit = "ocaml_libvirt_virterror_reset_last_error"
  external reset_last_conn_error : [>`R] Connect.t -> unit = "ocaml_libvirt_virterror_reset_last_conn_error"

  let no_error () =
    { code = VIR_ERR_OK; domain = VIR_FROM_NONE;
      message = None; level = VIR_ERR_NONE;
      str1 = None; str2 = None; str3 = None;
      int1 = 0_l; int2 = 0_l }
end

exception Virterror of Virterror.t
exception Not_supported of string

let rec map_ignore_errors f = function
  | [] -> []
  | x :: xs ->
      try f x :: map_ignore_errors f xs
      with Virterror _ -> map_ignore_errors f xs

module Domain =
struct
  type 'rw t

  type state =
    | InfoNoState | InfoRunning | InfoBlocked | InfoPaused
    | InfoShutdown | InfoShutoff | InfoCrashed

  type info = {
    state : state;
    max_mem : int64;
    memory : int64;
    nr_virt_cpu : int;
    cpu_time : int64;
  }

  type vcpu_state = VcpuOffline | VcpuRunning | VcpuBlocked

  type vcpu_info = {
    number : int;
    vcpu_state : vcpu_state;
    vcpu_time : int64;
    cpu : int;
  }

  type sched_param = string * sched_param_value
  and sched_param_value =
    | SchedFieldInt32 of int32 | SchedFieldUInt32 of int32
    | SchedFieldInt64 of int64 | SchedFieldUInt64 of int64
    | SchedFieldFloat of float | SchedFieldBool of bool

  type typed_param = string * typed_param_value
  and typed_param_value =
    | TypedFieldInt32 of int32 | TypedFieldUInt32 of int32
    | TypedFieldInt64 of int64 | TypedFieldUInt64 of int64
    | TypedFieldFloat of float | TypedFieldBool of bool
    | TypedFieldString of string

  type migrate_flag = Live

  type memory_flag = Virtual

  type list_flag =
    | ListActive
    | ListInactive
    | ListAll

  type block_stats = {
    rd_req : int64;
    rd_bytes : int64;
    wr_req : int64;
    wr_bytes : int64;
    errs : int64;
  }

  type interface_stats = {
    rx_bytes : int64;
    rx_packets : int64;
    rx_errs : int64;
    rx_drop : int64;
    tx_bytes : int64;
    tx_packets : int64;
    tx_errs : int64;
    tx_drop : int64;
  }

  (* The maximum size for Domain.memory_peek and Domain.block_peek
   * supported by libvirt.  This may change with different versions
   * of libvirt in the future, hence it's a function.
   *)
  let max_peek _ = 65536

  external create_linux : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_domain_create_linux"
  external lookup_by_id : 'a Connect.t -> int -> 'a t = "ocaml_libvirt_domain_lookup_by_id"
  external lookup_by_uuid : 'a Connect.t -> uuid -> 'a t = "ocaml_libvirt_domain_lookup_by_uuid"
  external lookup_by_uuid_string : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_domain_lookup_by_uuid_string"
  external lookup_by_name : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_domain_lookup_by_name"
  external destroy : [>`W] t -> unit = "ocaml_libvirt_domain_destroy"
  external free : [>`R] t -> unit = "ocaml_libvirt_domain_free"
  external suspend : [>`W] t -> unit = "ocaml_libvirt_domain_suspend"
  external resume : [>`W] t -> unit = "ocaml_libvirt_domain_resume"
  external save : [>`W] t -> filename -> unit = "ocaml_libvirt_domain_save"
  external restore : [>`W] Connect.t -> filename -> unit = "ocaml_libvirt_domain_restore"
  external core_dump : [>`W] t -> filename -> unit = "ocaml_libvirt_domain_core_dump"
  external shutdown : [>`W] t -> unit = "ocaml_libvirt_domain_shutdown"
  external reboot : [>`W] t -> unit = "ocaml_libvirt_domain_reboot"
  external get_name : [>`R] t -> string = "ocaml_libvirt_domain_get_name"
  external get_uuid : [>`R] t -> uuid = "ocaml_libvirt_domain_get_uuid"
  external get_uuid_string : [>`R] t -> string = "ocaml_libvirt_domain_get_uuid_string"
  external get_id : [>`R] t -> int = "ocaml_libvirt_domain_get_id"
  external get_os_type : [>`R] t -> string = "ocaml_libvirt_domain_get_os_type"
  external get_max_memory : [>`R] t -> int64 = "ocaml_libvirt_domain_get_max_memory"
  external set_max_memory : [>`W] t -> int64 -> unit = "ocaml_libvirt_domain_set_max_memory"
  external set_memory : [>`W] t -> int64 -> unit = "ocaml_libvirt_domain_set_memory"
  external get_info : [>`R] t -> info = "ocaml_libvirt_domain_get_info"
  external get_xml_desc : [>`R] t -> xml = "ocaml_libvirt_domain_get_xml_desc"
  external get_scheduler_type : [>`R] t -> string * int = "ocaml_libvirt_domain_get_scheduler_type"
  external get_scheduler_parameters : [>`R] t -> int -> sched_param array = "ocaml_libvirt_domain_get_scheduler_parameters"
  external set_scheduler_parameters : [>`W] t -> sched_param array -> unit = "ocaml_libvirt_domain_set_scheduler_parameters"
  external define_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_domain_define_xml"
  external undefine : [>`W] t -> unit = "ocaml_libvirt_domain_undefine"
  external create : [>`W] t -> unit = "ocaml_libvirt_domain_create"
  external get_autostart : [>`R] t -> bool = "ocaml_libvirt_domain_get_autostart"
  external set_autostart : [>`W] t -> bool -> unit = "ocaml_libvirt_domain_set_autostart"
  external set_vcpus : [>`W] t -> int -> unit = "ocaml_libvirt_domain_set_vcpus"
  external pin_vcpu : [>`W] t -> int -> string -> unit = "ocaml_libvirt_domain_pin_vcpu"
  external get_vcpus : [>`R] t -> int -> int -> int * vcpu_info array * string = "ocaml_libvirt_domain_get_vcpus"
  external get_cpu_stats : [>`R] t -> typed_param list array = "ocaml_libvirt_domain_get_cpu_stats"
  external get_max_vcpus : [>`R] t -> int = "ocaml_libvirt_domain_get_max_vcpus"
  external attach_device : [>`W] t -> xml -> unit = "ocaml_libvirt_domain_attach_device"
  external detach_device : [>`W] t -> xml -> unit = "ocaml_libvirt_domain_detach_device"
  external migrate : [>`W] t -> [>`W] Connect.t -> migrate_flag list -> ?dname:string -> ?uri:string -> ?bandwidth:int -> unit -> rw t = "ocaml_libvirt_domain_migrate_bytecode" "ocaml_libvirt_domain_migrate_native"
  external block_stats : [>`R] t -> string -> block_stats = "ocaml_libvirt_domain_block_stats"
  external interface_stats : [>`R] t -> string -> interface_stats = "ocaml_libvirt_domain_interface_stats"
  external block_peek : [>`W] t -> string -> int64 -> int -> string -> int -> unit = "ocaml_libvirt_domain_block_peek_bytecode" "ocaml_libvirt_domain_block_peek_native"
  external memory_peek : [>`W] t -> memory_flag list -> int64 -> int -> string -> int -> unit = "ocaml_libvirt_domain_memory_peek_bytecode" "ocaml_libvirt_domain_memory_peek_native"

  external const : [>`R] t -> ro t = "%identity"

  let get_domains conn flags =
    (* Old/slow/inefficient method. *)
    let get_active, get_inactive =
      if List.mem ListAll flags then
	(true, true)
      else
	(List.mem ListActive flags, List.mem ListInactive flags) in
    let active_doms =
      if get_active then (
	let n = Connect.num_of_domains conn in
	let ids = Connect.list_domains conn n in
	let ids = Array.to_list ids in
	map_ignore_errors (lookup_by_id conn) ids
      ) else [] in

    let inactive_doms =
      if get_inactive then (
	let n = Connect.num_of_defined_domains conn in
	let names = Connect.list_defined_domains conn n in
	let names = Array.to_list names in
	map_ignore_errors (lookup_by_name conn) names
      ) else [] in

    active_doms @ inactive_doms

  let get_domains_and_infos conn flags =
    (* Old/slow/inefficient method. *)
    let get_active, get_inactive =
      if List.mem ListAll flags then
	(true, true)
      else (List.mem ListActive flags, List.mem ListInactive flags) in
    let active_doms =
      if get_active then (
	let n = Connect.num_of_domains conn in
	let ids = Connect.list_domains conn n in
	let ids = Array.to_list ids in
	map_ignore_errors (lookup_by_id conn) ids
      ) else [] in

    let inactive_doms =
      if get_inactive then (
	let n = Connect.num_of_defined_domains conn in
	let names = Connect.list_defined_domains conn n in
	let names = Array.to_list names in
	map_ignore_errors (lookup_by_name conn) names
      ) else [] in

    let doms = active_doms @ inactive_doms in

    map_ignore_errors (fun dom -> (dom, get_info dom)) doms
end

module DomainEvent =
struct
  type callback =
    | Lifecycle     of ([`R] Domain.t -> (int * int) -> unit)
    | Reboot        of ([`R] Domain.t -> unit -> unit)
    | RtcChange     of ([`R] Domain.t -> int64 -> unit)
    | Watchdog      of ([`R] Domain.t -> int -> unit)
    | IOError       of ([`R] Domain.t -> (string option * string option * int) -> unit)
    | Graphics      of ([`R] Domain.t -> unit)
    | IOErrorReason of ([`R] Domain.t -> (string option * string option * int * string option) -> unit)
    | ControlError  of ([`R] Domain.t -> unit)
    | BlockJob      of ([`R] Domain.t -> unit)
    | DiskChange    of ([`R] Domain.t -> (string option * string option * string option * int) -> unit)
    | TrayChange    of ([`R] Domain.t -> (string option * int) -> unit)
    | PMWakeUp      of ([`R] Domain.t -> unit)
    | PMSuspend     of ([`R] Domain.t -> unit)
    | BalloonChange of ([`R] Domain.t -> int64 -> unit)
    | PMSuspendDisk of ([`R] Domain.t -> unit)

  type callback_id = int64

  let fresh_callback_id =
    let next = ref 0L in
    fun () ->
      let result = !next in
      next := Int64.succ !next;
      result

  let make_callback_table value_name =
    let table = Hashtbl.create 16 in
    let callback callback_id generic x =
      if Hashtbl.mem table callback_id
      then Hashtbl.find table callback_id generic x in
    let _ = Callback.register value_name callback in
    table

  let unit_callback_table = make_callback_table "Libvirt.unit_callback"
  let int_callback_table = make_callback_table "Libvirt.int_callback"
  let int64_callback_table = make_callback_table "Libvirt.int64_callback"
  let int_int_callback_table = make_callback_table "Libvirt.int_int_callback"
  let string_opt_int_callback_table = make_callback_table "Libvirt.string_opt_int_callback"
  let string_opt_string_opt_int_callback_table = make_callback_table "Libvirt.string_opt_string_opt_int_callback"
  let string_opt_string_opt_int_string_opt_callback_table = make_callback_table "Libvirt.string_opt_string_opt_int_string_opt_callback"
  let string_opt_string_opt_string_opt_int_callback_table = make_callback_table "Libvirt.string_opt_string_opt_string_opt_int_callback"

  external register_default_impl : unit -> unit = "ocaml_libvirt_connect_domain_event_register_default_impl"

  external run_default_impl : unit -> unit = "ocaml_libvirt_connect_domain_event_run_default_impl"

  external register_any' : 'a Connect.t -> 'a Domain.t option -> callback -> callback_id -> unit = "ocaml_libvirt_connect_domain_event_register_any"

  let register_any conn ?dom callback =
    let id = fresh_callback_id () in
    begin match callback with
    | Lifecycle f ->
        Hashtbl.add int_int_callback_table id f
    | Reboot f ->
        Hashtbl.add unit_callback_table id f
    | RtcChange f ->
        Hashtbl.add int64_callback_table id f
    | Watchdog f ->
        Hashtbl.add int_callback_table id f
    | IOError f ->
        Hashtbl.add string_opt_string_opt_int_callback_table id f
    | Graphics f ->
        failwith "unsupported"
    | IOErrorReason f ->
        Hashtbl.add string_opt_string_opt_int_string_opt_callback_table id f
    | ControlError f
    | BlockJob f ->
        failwith "unsupported"
    | DiskChange f ->
        Hashtbl.add string_opt_string_opt_string_opt_int_callback_table id f 
    | TrayChange f ->
        Hashtbl.add string_opt_int_callback_table id f
    | PMWakeUp f
    | PMSuspend f ->
        failwith "unsupported"
    | BalloonChange f ->
        Hashtbl.add int64_callback_table id f
    | PMSuspendDisk f ->
        failwith "unsupported"
    end;
    register_any' conn dom callback id

end

module Network =
struct
  type 'rw t

  external lookup_by_name : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_network_lookup_by_name"
  external lookup_by_uuid : 'a Connect.t -> uuid -> 'a t = "ocaml_libvirt_network_lookup_by_uuid"
  external lookup_by_uuid_string : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_network_lookup_by_uuid_string"
  external create_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_network_create_xml"
  external define_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_network_define_xml"
  external undefine : [>`W] t -> unit = "ocaml_libvirt_network_undefine"
  external create : [>`W] t -> unit = "ocaml_libvirt_network_create"
  external destroy : [>`W] t -> unit = "ocaml_libvirt_network_destroy"
  external free : [>`R] t -> unit = "ocaml_libvirt_network_free"
  external get_name : [>`R] t -> string = "ocaml_libvirt_network_get_name"
  external get_uuid : [>`R] t -> uuid = "ocaml_libvirt_network_get_uuid"
  external get_uuid_string : [>`R] t -> string = "ocaml_libvirt_network_get_uuid_string"
  external get_xml_desc : [>`R] t -> xml = "ocaml_libvirt_network_get_xml_desc"
  external get_bridge_name : [>`R] t -> string = "ocaml_libvirt_network_get_bridge_name"
  external get_autostart : [>`R] t -> bool = "ocaml_libvirt_network_get_autostart"
  external set_autostart : [>`W] t -> bool -> unit = "ocaml_libvirt_network_set_autostart"

  external const : [>`R] t -> ro t = "%identity"
end

module Pool =
struct
  type 'rw t
  type pool_state = Inactive | Building | Running | Degraded
  type pool_build_flags = New | Repair | Resize
  type pool_delete_flags = Normal | Zeroed
  type pool_info = {
    state : pool_state;
    capacity : int64;
    allocation : int64;
    available : int64;
  }

  external lookup_by_name : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_storage_pool_lookup_by_name"
  external lookup_by_uuid : 'a Connect.t -> uuid -> 'a t = "ocaml_libvirt_storage_pool_lookup_by_uuid"
  external lookup_by_uuid_string : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_storage_pool_lookup_by_uuid_string"
  external create_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_storage_pool_create_xml"
  external define_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_storage_pool_define_xml"
  external build : [>`W] t -> pool_build_flags -> unit = "ocaml_libvirt_storage_pool_build"
  external undefine : [>`W] t -> unit = "ocaml_libvirt_storage_pool_undefine"
  external create : [>`W] t -> unit = "ocaml_libvirt_storage_pool_create"
  external destroy : [>`W] t -> unit = "ocaml_libvirt_storage_pool_destroy"
  external delete : [>`W] t -> unit = "ocaml_libvirt_storage_pool_delete"
  external free : [>`R] t -> unit = "ocaml_libvirt_storage_pool_free"
  external refresh : [`R] t -> unit = "ocaml_libvirt_storage_pool_refresh"
  external get_name : [`R] t -> string = "ocaml_libvirt_storage_pool_get_name"
  external get_uuid : [`R] t -> uuid = "ocaml_libvirt_storage_pool_get_uuid"
  external get_uuid_string : [`R] t -> string = "ocaml_libvirt_storage_pool_get_uuid_string"
  external get_info : [`R] t -> pool_info = "ocaml_libvirt_storage_pool_get_info"
  external get_xml_desc : [`R] t -> xml = "ocaml_libvirt_storage_pool_get_xml_desc"
  external get_autostart : [`R] t -> bool = "ocaml_libvirt_storage_pool_get_autostart"
  external set_autostart : [>`W] t -> bool -> unit = "ocaml_libvirt_storage_pool_set_autostart"
  external num_of_volumes : [`R] t -> int = "ocaml_libvirt_storage_pool_num_of_volumes"
  external list_volumes : [`R] t -> int -> string array = "ocaml_libvirt_storage_pool_list_volumes"
  external const : [>`R] t -> ro t = "%identity"
end

module Volume =
struct
  type 'rw t
  type vol_type = File | Block
  type vol_delete_flags = Normal | Zeroed
  type vol_info = {
    typ : vol_type;
    capacity : int64;
    allocation : int64;
  }

  external lookup_by_name : 'a Pool.t -> string -> 'a t = "ocaml_libvirt_storage_vol_lookup_by_name"
  external lookup_by_key : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_storage_vol_lookup_by_key"
  external lookup_by_path : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_storage_vol_lookup_by_path"
  external pool_of_volume : 'a t -> 'a Pool.t = "ocaml_libvirt_storage_pool_lookup_by_volume"
  external get_name : [`R] t -> string = "ocaml_libvirt_storage_vol_get_name"
  external get_key : [`R] t -> string = "ocaml_libvirt_storage_vol_get_key"
  external get_path : [`R] t -> string = "ocaml_libvirt_storage_vol_get_path"
  external get_info : [`R] t -> vol_info = "ocaml_libvirt_storage_vol_get_info"
  external get_xml_desc : [`R] t -> xml = "ocaml_libvirt_storage_vol_get_xml_desc"
  external create_xml : [>`W] Pool.t -> xml -> unit = "ocaml_libvirt_storage_vol_create_xml"
  external delete : [>`W] t -> vol_delete_flags -> unit = "ocaml_libvirt_storage_vol_delete"
  external free : [>`R] t -> unit = "ocaml_libvirt_storage_vol_free"
  external const : [>`R] t -> ro t = "%identity"
end

(* Initialization. *)
external c_init : unit -> unit = "ocaml_libvirt_init"
let () =
  Callback.register_exception
    "ocaml_libvirt_virterror" (Virterror (Virterror.no_error ()));
  Callback.register_exception
    "ocaml_libvirt_not_supported" (Not_supported "");
  c_init ()
