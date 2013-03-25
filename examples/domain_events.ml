(* Simple demo program showing how to receive domain events.
   Usage: domain_events [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   (C) Copyright 2013 Citrix Inc
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module DE = Libvirt.DomainEvent
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

let () =
  try
    DE.register_default_impl ();
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_readonly ?name () in

    DE.register_any conn (DE.Lifecycle (fun dom (event, detail) -> printd dom "Lifecycle event = %d; detail = %d" event detail));
    DE.register_any conn (DE.Reboot (fun dom () -> printd dom "Reboot"));
    DE.register_any conn (DE.RtcChange (fun dom x -> printd dom "RtcChange = %Lx" x));
    DE.register_any conn (DE.Watchdog (fun dom x -> printd dom "Watchdog = %d" x));
    DE.register_any conn (DE.IOError (fun dom (src, dst, action) -> printd dom "IOError src=%s dst=%s action=%d" (string_option src) (string_option dst) action));
    DE.register_any conn (DE.IOErrorReason (fun dom (src, dst, action, reason) -> printd dom "IOErrorReason src=%s dst=%s action=%d reason=%s" (string_option src) (string_option dst) action (string_option reason)));
(*
    DE.register_any conn (DE.Graphics print_dom);
*)
    DE.register_any conn (DE.ControlError (fun dom () -> printd dom "ControlError"));
(*
    DE.register_any conn (DE.BlockJob print_dom);
*)
    DE.register_any conn (DE.DiskChange (fun dom (oldpath, newpath, alias, reason) -> printd dom "DiskChange oldpath=%s newpath=%s alias=%s reason=%d" (string_option oldpath) (string_option newpath) (string_option alias) reason));
    DE.register_any conn (DE.TrayChange (fun dom (alias, reason) -> printd dom "TrayChange alias=%s reason=%d" (string_option alias) reason));
    DE.register_any conn (DE.PMWakeUp (fun dom reason -> printd dom "PMWakeup reason=%d" reason));
    DE.register_any conn (DE.PMSuspend (fun dom reason -> printd dom "PMSuspend reason=%d" reason));
    DE.register_any conn (DE.BalloonChange (fun dom x -> printd dom "BalloonChange actual = %Ld" x));
    DE.register_any conn (DE.PMSuspendDisk (fun dom reason -> printd dom "PMSuspendDisk reason=%d" reason));
    C.set_keep_alive conn 5 3;
    while true do
        fprintf stderr "run_default_impl\n%!";
	DE.run_default_impl ()
    done
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
