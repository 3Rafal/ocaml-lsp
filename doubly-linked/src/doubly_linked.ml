(* Implementation of a doubly linked list, based on Cormen et al. "Introduction
   to Algorithms" and
   https://github.com/janestreet/jenga/blob/114.04/tenacious/lib/ring.ml for
   hacky OCaml-specific pieces. *)

type 'a t =
  { data : 'a
  ; mutable prev : 'a t
        (* Invariant: if [node.prev == node] then [node] has been removed the
           list. Easy to hold invariant that frees us from having a separate
           mutable record field to hold this information. *)
  ; mutable next : 'a t
        (* invariant: if [node.next == node] then it's the sentinel *)
  }

type 'a node = 'a t

let create () : 'a t =
  let rec sentinel =
    { data =
        Obj.magic None
        (* sentinel doesn't hold [data]; so this field will never be accessed,
           so it's safe to put a dummy value; taken from the [ring]
           implementation link *)
    ; prev = sentinel
    ; next = sentinel
    }
  in
  sentinel

let is_empty (sentinel : 'a t) : bool = sentinel.next == sentinel

let length (sentinel : 'a t) : int =
  let head = ref sentinel.next in
  let count = ref 0 in
  while not (!head == sentinel) do
    incr count;
    head := !head.next
  done;
  !count

let prepend : 'a. 'a t -> 'a -> 'a node =
 fun sentinel v ->
  let inserted_node = { data = v; prev = sentinel; next = sentinel.next } in
  sentinel.next.prev <- inserted_node;
  sentinel.next <- inserted_node;
  inserted_node

let append : 'a. 'a t -> 'a -> 'a node =
 fun sentinel v ->
  let inserted_node = { data = v; prev = sentinel.prev; next = sentinel } in
  sentinel.prev.next <- inserted_node;
  sentinel.prev <- inserted_node;
  inserted_node

let mark_as_detached (node : 'a node) = node.prev <- node

let is_detached (node : 'a node) = node.prev == node

let detach_head : 'a. 'a t -> 'a option =
 fun sentinel ->
  if is_empty sentinel then
    None
  else
    let removed_node = sentinel.next in
    sentinel.next <- removed_node.next;
    removed_node.next.prev <- sentinel;
    mark_as_detached removed_node;
    Some removed_node.data

let detach_tail : 'a. 'a t -> 'a option =
 fun sentinel ->
  if is_empty sentinel then
    None
  else
    let removed_node = sentinel.prev in
    removed_node.prev.next <- sentinel;
    sentinel.prev <- removed_node.prev;
    mark_as_detached removed_node;
    Some removed_node.data

let detach (node : 'a node) : (unit, [ `Already_detached ]) result =
  if is_detached node then
    Error `Already_detached
  else (
    node.prev.next <- node.next;
    node.next.prev <- node.prev;
    mark_as_detached node;
    Ok ()
  )