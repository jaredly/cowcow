
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

let opposite direction => switch direction {
  | Up => Down
  | Down => Up
  | Left => Right
  | Right => Left
};

type t = {
  body: Vector.t Tile.pos,
  head: Tile.pos,
  direction: direction,
  lastDirection: direction,
  newBody: int,
};

let setDirection state direction => direction == opposite state.lastDirection
  ? state
  : {...state, direction};

let initial (w, h) => {
  body: Vector.init 1 (fun _ => (4, 5)),
  /* body: Vector.init 5 (fun x => (x, 5)), */
  head: (5, 5),
  direction: Right,
  lastDirection: Right,
  newBody: 5,
};

let eat snake => {...snake, newBody: snake.newBody + 1};

let wrap n max => n < 0 ? n + max : (n >= max ? n - max : n);

let calcNextHead (x, y) direction (w, h) => {
  let (dx, dy) = directionToPos direction;
  (wrap (x + dx) w, wrap (y + dy) h)
};

let move {body, head, direction, newBody} size => {
  let last = Vector.firstOrRaise body;
  let lastHead = head;
  let head = calcNextHead head direction size;
  /* Js.log newBody; */
  let body = newBody > 0 ? body : Vector.skip 1 body;
  let body = Vector.addLast lastHead body;
  let snake = {
    body,
    head,
    direction,
    lastDirection: direction,
    newBody: newBody > 0 ? newBody - 1: newBody
  };
  (snake, newBody > 0 ? None : Some last, lastHead, head)
};
