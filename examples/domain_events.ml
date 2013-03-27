(* Simple demo program showing how to receive domain events.
   Usage: domain_events [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   (C) Copyright 2013 Citrix Inc
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module E = Libvirt.Event
module N = Libvirt.Network

let string_of_state = function
  | D.InfoNoState -> "no state"
  | D.InfoRunning -> "running"
  | D.InfoBlocked -> "blocked"
  | D.InfoPaused -> "paused"
  | D.InfoShutdown -> "shutdown"
  | D.InfoShutoff -> "shutoff"
  | D.InfoCrashed -> "crashed"

let printd dom fmt =
  let prefix dom =
    let id = D.get_id dom in
    try
      let name = D.get_name dom in
      let info = D.get_info dom in
      let state = string_of_state info.D.state in
      sprintf "%8d %-20s %s " id name state
  with _ ->
      sprintf "%8d " id in
  let write x =
    output_string stdout (prefix dom);
    output_string stdout x;
    output_string stdout "\n";
    flush stdout in
  Printf.ksprintf write fmt

let string_option = function
  | None -> "None"
  | Some x -> "Some " ^ x

let string_of_graphics_address (family, node, service) =
  Printf.sprintf "{ family=%d; node=%s; service=%s }" family (string_option node) (string_option service)

let string_of_graphics_subject_identity (ty, name) =
  Printf.sprintf "{ type=%s; name=%s }" (string_option ty) (string_option name)

let string_of_graphics_subject xs = String.concat "; " (List.map string_of_graphics_subject_identity (Array.to_list xs))

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let () =
  try
    E.register_default_impl ();
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_readonly ?name () in

    E.register_any conn (E.Lifecycle (fun dom e ->
        printd dom "Lifecycle %s"
            (string_option (map_option E.string_of_event e))
    ));
    E.register_any conn (E.Reboot (fun dom () -> printd dom "Reboot"));
    E.register_any conn (E.RtcChange (fun dom x -> printd dom "RtcChange = %Lx" x));
    E.register_any conn (E.Watchdog (fun dom e ->
        printd dom "Watchdog %s" (E.string_of_watchdog_action e)
    ));
    E.register_any conn (E.IOError (fun dom e ->
        printd dom "IOError %s" (E.Io_error.to_string e)
    ));
    E.register_any conn (E.IOErrorReason (fun dom (src, dst, action, reason) -> printd dom "IOErrorReason src=%s dst=%s action=%d reason=%s" (string_option src) (string_option dst) action (string_option reason)));
    E.register_any conn (E.Graphics (fun dom e ->
        printd dom "Graphics %s" (E.Graphics.to_string e)
    ));
    E.register_any conn (E.ControlError (fun dom () -> printd dom "ControlError"));
    E.register_any conn (E.BlockJob (fun dom (disk, ty, status) -> printd dom "BlockJob disk=%s ty=%d status=%d" (string_option disk) ty status));
    E.register_any conn (E.DiskChange (fun dom (oldpath, newpath, alias, reason) -> printd dom "DiskChange oldpath=%s newpath=%s alias=%s reason=%d" (string_option oldpath) (string_option newpath) (string_option alias) reason));
    E.register_any conn (E.TrayChange (fun dom (alias, reason) -> printd dom "TrayChange alias=%s reason=%d" (string_option alias) reason));
    E.register_any conn (E.PMWakeUp (fun dom reason -> printd dom "PMWakeup reason=%d" reason));
    E.register_any conn (E.PMSuspend (fun dom reason -> printd dom "PMSuspend reason=%d" reason));
    E.register_any conn (E.BalloonChange (fun dom x -> printd dom "BalloonChange actual = %Ld" x));
    E.register_any conn (E.PMSuspendDisk (fun dom reason -> printd dom "PMSuspendDisk reason=%d" reason));
    C.set_keep_alive conn 5 3;
    while true do
	E.run_default_impl ()
    done
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
