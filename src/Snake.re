
let module Vector = Immutable.Vector;

type direction =
  | Up
  | Down
  | Left
  | Right;

let directionToPos direction => switch direction {
  | Up => (0, -1)
  | Down => (0, 1)
  | Left => (-1, 0)
  | Right => (1, 0)
};

type t = {
  body: Vector.t Tile.pos,
  head: Tile.pos,
  direction: direction,
};

let setDirection state direction => {...state, direction};

let initial (w, h) => {
  body: Vector.init 5 (fun x => (x, 5)),
  head: (5, 5),
  direction: Right,
};

let wrap n max => n < 0 ? n + max : (n >= max ? n - max : n);

let calcNextHead (x, y) direction (w, h) => {
  let (dx, dy) = directionToPos direction;
  (wrap (x + dx) w, wrap (y + dy) h)
};

let move {body, head, direction} size => {
  let last = Vector.firstOrRaise body;
  let lastHead = head;
  let head = calcNextHead head direction size;

  let body = Vector.skip 1 body;
  let body = Vector.addLast lastHead body;
  ({body, head, direction}, Some last, lastHead, head)
};
