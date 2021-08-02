open Lwt.Infix

type 'a t = 'a Lwt.t

let danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a8 p = p

let return =
  Lwt.return

let bind =
  Lwt.bind

let join ps =
  ps |> Lwt_list.fold_left_s (fun l p -> p >|= fun v -> v :: l) []
    >|= List.fold_left (fun vs v -> v::vs) []

let pick =
  Lwt.pick

let send s h =
  Lwt_main.run begin Io.connect h >>= fun (_, oc) -> s |> Lwt_io.fprintl oc end

let recv h =
  Io.connect h >>= fun (ic, _) -> Lwt_io.read_line ic

let spawn f a =
  Lwt_main.run begin
    Io.connect !Io.server
    >>= fun (ic, oc) ->
      let s = Printf.sprintf "[\"w\",[\"t\",\"spawn\"],[\"w\",%s,%s]]" f a in
      Lwt_io.fprintl oc s
    >>= fun () -> Lwt_io.read_line ic
    >>= fun port -> Lwt_unix.sleep 0.05
    >|= fun _ -> 
       match Yojson.Safe.from_string port with
       | `List [_;`Int port] -> port
       | _ -> failwith "spawn: received a malformed response from the server"
  end

let sleep ms =
  let ms' = float_of_int ms in
  Lwt_unix.sleep (ms' /. 1000.) 

module Infix = struct
  let (>>=) = Lwt.bind
end
