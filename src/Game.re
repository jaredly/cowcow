
let module HashMap = Immutable.HashMap;
let module Vector = Immutable.Vector;

type gameStatus =
    /* TODO intro */
    | Alive
  | Dead;

type state = {
  tiles: HashMap.t Tile.pos Tile.t,
  size: (int, int),
  snake: Snake.t,
  status: gameStatus,
};

let iterTiles fn state => {
  HashMap.reduce
  (fun () pos tile => fn pos tile)
  ()
  state.tiles
};

let keyToSnakeDirection key => {
  open Reprocessing.Events;
  switch key {
    | Up => Some Snake.Up
    | Left => Some Snake.Left
    | Right => Some Snake.Right
    | Down => Some Snake.Down
    | _ => None
  }
};

let step state => {
  open Tile;
  let {tiles, snake, size, status} = state;
  [%guard let Alive = status][@else state];

  let (snake, cleared, newBody, newHead) = Snake.move snake size;
  let tiles = switch cleared {
    | Some tile => HashMap.put tile Empty tiles
    | None => tiles
  };
  let status = switch (HashMap.getOrRaise newHead tiles) {
    | Empty => {
      Alive
    }
    | _ => {
      Dead
    }
  };
  let tiles = HashMap.put newBody SnakeBody tiles;
  let tiles = HashMap.put newHead SnakeHead tiles;
  {...state, tiles, snake, status}
};

let makeBoard w h => {
  let res = ref [];
  for x in 0 to (w - 1) {
    for y in 0 to (h - 1) {
      res := [((x, y), Tile.Empty), ...!res];
    }
  };
  !res
};

let addSnake snake board => {
  let board = Vector.reduce
  (fun board pos => HashMap.put pos Tile.SnakeBody board)
  board
  snake.Snake.body;
  HashMap.put snake.Snake.head Tile.SnakeHead board;
};

let universalCompare x y => {
  let c = compare x y;
  c < 0
    ? Immutable.Ordering.lessThan
    : (c > 0
      ? Immutable.Ordering.greaterThan
      : Immutable.Ordering.equal)
};

let initialState (w, h) => {
  let snake = Snake.initial (w, h);
  let tiles = (HashMap.fromEntriesWith
    hash::Hashtbl.hash
    comparator::universalCompare
    (Immutable.List.toIterable
      (makeBoard w h)))
    |> addSnake snake;
  {
    size: (w, h),
    snake,
    tiles,
    status: Alive,
  }
};

let handleKey state key => {
  switch (keyToSnakeDirection key) {
    | Some key => {...state, snake: Snake.setDirection state.snake key}
    | None => state
  }
};
