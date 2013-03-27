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

    let spinner = [| '|'; '/'; '-'; '\\' |] in

    let timeouts = ref 0 in
    (* Check add/remove works *)
    let id = E.add_timeout conn 250 (fun () -> Printf.printf "This callback is immediately deregistered\n%!") in
    E.remove_timeout conn id;

    let (_: E.timer_id) = E.add_timeout conn 250 (* ms *)
        (fun () ->
            incr timeouts;
            Printf.printf "\r%c  %d timeout callbacks%!" (spinner.(!timeouts mod (Array.length spinner))) !timeouts;
            (* Check for GC errors: *)
            Gc.compact ()
        ) in

    (* Check add/remove works *)
    let id = E.register_any conn (E.Lifecycle (fun dom e ->
        printd dom "Removed Lifecycle callback %s" (E.Lifecycle.to_string e)
    )) in
    E.deregister_any conn id;

    let (_: E.callback_id) = E.register_any conn (E.Lifecycle (fun dom e ->
        printd dom "Lifecycle %s" (E.Lifecycle.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Reboot (fun dom e ->
        printd dom "Reboot %s" (E.Reboot.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.RtcChange (fun dom e ->
        printd dom "RtcChange %s" (E.Rtc_change.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Watchdog (fun dom e ->
        printd dom "Watchdog %s" (E.Watchdog.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOError (fun dom e ->
        printd dom "IOError %s" (E.Io_error.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOErrorReason (fun dom e ->
        printd dom "IOErrorReason %s" (E.Io_error.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Graphics (fun dom e ->
        printd dom "Graphics %s" (E.Graphics.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.ControlError (fun dom e ->
        printd dom "ControlError %s" (E.Control_error.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BlockJob (fun dom e ->
        printd dom "BlockJob %s" (E.Block_job.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.DiskChange (fun dom e ->
        printd dom "DiskChange %s" (E.Disk_change.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.TrayChange (fun dom e ->
        printd dom "TrayChange %s" (E.Tray_change.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMWakeUp (fun dom e ->
        printd dom "PMWakeup %s" (E.PM_wakeup.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspend (fun dom e ->
        printd dom "PMSuspend %s" (E.PM_suspend.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BalloonChange (fun dom e ->
        printd dom "BalloonChange %s" (E.Balloon_change.to_string e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspendDisk (fun dom x ->
        printd dom "PMSuspendDisk %s" (E.PM_suspend_disk.to_string x)
    )) in
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
