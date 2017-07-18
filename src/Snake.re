
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
  size: int,
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
  size: 5,
  head: (5, 5),
  direction: Right,
  lastDirection: Right,
  newBody: 5,
};

let eat snake => {...snake, newBody: snake.newBody + 1, size: snake.size + 1};

let wrap n max => n < 0 ? n + max : (n >= max ? n - max : n);

let calcNextHead (x, y) direction (w, h) => {
  let (dx, dy) = directionToPos direction;
  (wrap (x + dx) w, wrap (y + dy) h)
};

let tail snake => Vector.firstOrRaise snake.body;

let move snake size => {
  let {body, head, direction, newBody} = snake;
  let last = Vector.firstOrRaise body;
  let lastHead = head;
  let head = calcNextHead head direction size;
  let body = newBody > 0 ? body : Vector.skip 1 body;
  let body = Vector.addLast lastHead body;
  let bodyDirection = switch (snake.lastDirection, snake.direction) {
    | (Up, Up) => Tile.U
    | (Down, Down) => Tile.D
    | (Left, Left) => Tile.L
    | (Right, Right) => Tile.R
    | (Up, Left) => Tile.BL
    | (Right, Down) => Tile.LB
    | (Up, Right) => Tile.BR
    | (Left, Down) => Tile.RB
    | (Down, Left) => Tile.TL
    | (Right, Up) => Tile.LT
    | (Down, Right) => Tile.TR
    | (Left, Up) => Tile.RT
    | _ => assert false
  };
  let snake = {
    ...snake,
    body,
    head,
    lastDirection: direction,
    newBody: newBody > 0 ? newBody - 1: newBody
  };
  (snake, newBody > 0 ? None : Some last, lastHead, bodyDirection, head)
};
