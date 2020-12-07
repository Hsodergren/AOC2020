open Core
open Stdio

let timeit f =
  let t0 = Time.now () in
  let result = f () in
  let t = Time.Span.to_ms @@  Time.(diff (now ()) t0)  in
  printf "%f" t;
  result
