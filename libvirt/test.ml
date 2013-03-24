
(* Naming convention:
   [[typedef virConnectDomainEventXCallback]] = "type x = { x_field: t }"
   [[enum virDomainEventXType {
       VIR_DOMAIN_EVENT_YY_ZZ = 0
   ]] = "type x =
         | YyZz"
*)

(* Rather than using the connection pointer returned via the event
   callback we encourage people to use some other reference to the
   connection captured in the callback's environment. *)

type generic = unit       (** The domain the event refers to *)

type defined_detail =
  | DefinedAdded          (** Newly created config file *)
  | DefinedUpdated        (** Changed config file *)

type undefined_detail =
  | UndefinedRemoved      (** Deleted the config file *)

type started_detail =
  | StartedBooted         (** Normal startup from boot *)
  | StartedMigrated       (** Incoming migration from another host *)
  | StartedRestored       (** Restored from a state file *)
  | StartedFromSnapshot   (** Restored from snapshot *)
  | StartedWakeup         (** Started due to wakeup event *)

type suspended_detail =
  | SuspendedPaused       (** Normal suspend due to admin pause *)
  | SuspendedMigrated     (** Suspended for offline migration *)
  | SuspendedIOError      (** Suspended due to a disk I/O error *)
  | SuspendedWatchdog     (** Suspended due to a watchdog firing *)
  | SuspendedRestored     (** Restored from paused state file *)
  | SuspendedFromSnapshot (** Restored from paused snapshot *)
  | SuspendedAPIError     (** suspended after failure during libvirt API call *)

type resumed_detail =
  | ResumedUnpaused       (** Normal resume due to admin unpause *)
  | ResumedMigrated       (** Resumed for completion of migration *)
  | ResumedFromSnapshot   (** Resumed from snapshot *)

type stopped_detail =
  | StoppedShutdown       (** Normal shutdown *)
  | StoppedDestroyed      (** Forced poweroff from host *)
  | StoppedCrashed        (** Guest crashed *)
  | StoppedMigrated       (** Migrated off to another host *)
  | StoppedSaved          (** Saved to a state file *)
  | StoppedFailed         (** Host emulator/mgmt failed *)
  | StoppedFromSnapshot   (** offline snapshot loaded *)

type pm_suspended_detail =
  | PMSuspendedMemory     (** Guest was PM suspended to memory *)
  | PMSuspendedDisk       (** Guest was PM suspended to disk *)

type event =
  | Defined of defined_detail
  | Undefined of undefined_detail
  | Started of started_detail
  | Suspended of suspended_detail
  | Resumed of resumed_detail
  | Stopped of stopped_detail
  | Shutdown (* no detail defined yet *)
  | PMSuspended of pm_suspended_detail

type watchdog_action =
  | WatchdogNone     (** No action, watchdog ignored *)
  | WatchdogPause    (** Guest CPUs are paused *)
  | WatchdogReset    (** Guest CPUs are reset *)
  | WatchdogPoweroff (** Guest is forcably powered off *)
  | WatchdogShutdown (** Guest is requested to gracefully shutdown *)
  | WatchdogDebug    (** No action, a debug message logged *)

type watchdog = {
  watchdog_action: watchdog_action; (** action that is to be taken due to the watchdog firing *)
}

type io_error_action =
  | IOErrorNone      (** No action, IO error ignored *)
  | IOErrorPause     (** Guest CPUs are paused *)
  | IOErrorReport    (** IO error reported to guest O *)

type io_error = {
  io_error_src_path: string;        (** The host file on which the IO error occurred *)
  io_error_dev_alias: string;       (** The guest device alias associated with the path *)
  io_error_action: io_error_action; (** action that is to be taken due to the IO error *)
}

type io_error_reason = {
  io_error_reason_src_path: string;        (** The host file on which the IO error occurred *)
  io_error_reason_dev_alias: string;       (** The guest device alias associated with the path *)
  io_error_reason_action: io_error_action; (** action that is to be taken due to the IO error *)
  io_error_reason_reason: string;          (** the cause of the IO error *)
}

type graphics_family =
  | GraphicsAddressIpv4 (** IPv4 address *)
  | GraphicsAddressIpv6	(** IPv6 address *)
  | GraphicsAddressUnix (** UNIX socket path *)

type graphics_phase =
  | GraphicsConnect     (** Initial socket connection established *)
  | GraphicsInitialize  (** Authentication & setup completed *)
  | GraphicsDisconnect  (** Final socket disconnection *)

type graphics_address = {
  graphics_family: graphics_family; (** Address family *)
  graphics_node: string;            (** Address of node (eg IP address, or UNIX path) *)
  graphics_service: string;         (** Service name/number (eg TCP port, or NULL) *)
}

type graphics_subject_identity = {
  graphics_subject_type: string;    (** Type of identify *)
  graphics_subject_name: string;    (** Identity value *)
}

type graphics = {
  graphics_phase: int;
  graphics_local: graphics_address;
  graphics_remote: graphics_address;
  graphics_auth_scheme: string;
  graphics_subject: graphics_subject_identity list;
}

type block_job_type =
  | BlockJobTypeUnknown
  | BlockJobTypePull
  | BlockJobTypeCopy
  | BlockJobTypeCommit
  | BlockJobTypeLast

type block_job_status =
  | BlockJobCompleted
  | BlockJobFailed
  | BlockJobCanceled
  | BlockJobReady

type block_job = {
  block_job_disk: string;             (** fully-qualified filename of the affected disk *)
  block_job_type: block_job_type;     (** type of block job *)
  block_job_status: block_job_status; (** final status of the operation *)
}

type disk_change_reason =
  | DiskChangeMissingOnStart

type disk_change = {
  disk_change_old_src_path: string option; (** old source path *)
  disk_change_new_src_path: string option; (** new source path *)
  disk_change_dev_alias: string;           (** device alias name *)
  disk_change_reason: disk_change_reason;  (** reason why this callback was called *)
}

type tray_change_reason =
  | TrayChangeOpen
  | TrayChangeClose

type tray_change = {
  tray_change_dev_alias: string;          (** device alias *)
  tray_change_reason: tray_change_reason; (** why the tray status was changed? *)
}

type balloon_change = {
  balloon_change_actual: int64;           (** the new balloon level measured in kibibytes (blocks of 1024 bytes) *)
}

type rtc_change = {
  rtc_change_utcoffset: int64;            (** the new RTC offset from UTC, measured in seconds *)
}

(* A combination of virDomainEventId and virConnectDomainEvent*Callback *)
type event_callback =
  | Lifecycle     of (generic -> event -> unit)
  | Reboot        of (generic -> unit)
  | RtcChange     of (generic -> rtc_change -> unit)
  | Watchdog      of (generic -> watchdog -> unit)
  | IOError       of (generic -> io_error -> unit)
  | Graphics      of (generic -> graphics -> unit)
  | IOErrorReason of (generic -> io_error_reason -> unit)
  | ControlError  of (generic -> unit)
  | BlockJob      of (generic -> block_job -> unit)
  | DiskChange    of (generic -> disk_change -> unit)
  | TrayChange    of (generic -> tray_change -> unit)
  | PMWakeUp      of (generic -> unit)
  | PMSuspend     of (generic -> unit)
  | BalloonChange of (generic -> balloon_change -> unit)
  | PMSuspendDisk of (generic -> unit)

type callback_id = int64

let fresh_callback_id =
  let next = ref 0L in
  fun () ->
    let result = !next in
    next := Int64.succ !next;
    result

let lifecycle_callback_table = Hashtbl.create 16
let lifecycle_callback callback_id generic event =
  if Hashtbl.mem lifecycle_callback_table callback_id
  then Hashtbl.find lifecycle_callback_table callback_id generic event
let _ = Callback.register "Libvirt.lifecycle_callback" lifecycle_callback

let generic_callback_table = Hashtbl.create 16
let generic_callback callback_id generic =
  if Hashtbl.mem generic_callback_table callback_id
  then Hashtbl.find generic_callback_table callback_id generic
let _ = Callback.register "Libvirt.generic_callback" generic_callback

let event_register_any conn dom callback =
  ()

let event_register_default_impl () = ()

let event_run_default_impl () = ()

