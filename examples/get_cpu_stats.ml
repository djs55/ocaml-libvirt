(* List CPU stats for a domain.
 * Usage: get_cpu_stats domain
 * http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let () =
  try
    if Array.length Sys.argv <> 2 then (
      eprintf "error: get_cpu_stats domain\n";
      exit 1
    );
    let domname = Sys.argv.(1) in

    let conn = C.connect_readonly () in

    let nr_pcpus =
      let info = C.get_node_info conn in
      C.maxcpus_of_node_info info in

    let stats =
      let dom = D.lookup_by_name conn domname in
      D.get_cpu_stats dom in

    Array.iteri (
      fun n params ->
        printf "pCPU %d:" n;
        List.iter (
          fun (name, value) ->
            printf " %s=" name;
            match value with
            | D.TypedFieldInt32 i -> printf "%ld" i
            | D.TypedFieldUInt32 i -> printf "%ld" i
            | D.TypedFieldInt64 i -> printf "%Ld" i
            | D.TypedFieldUInt64 i -> printf "%Ld" i
            | D.TypedFieldFloat f -> printf "%g" f
            | D.TypedFieldBool b -> printf "%b" b
            | D.TypedFieldString s -> printf "%S" s
        ) params;
        printf "\n"
    ) stats
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
