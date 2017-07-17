
let module HashMap = Immutable.HashMap;
let module Vector = Immutable.Vector;

type state = {
  tiles: HashMap.t Tile.pos Tile.t,
  size: (int, int),
  snake: Snake.t,
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

let handleKey state key => {
  switch (keyToSnakeDirection key) {
    | Some key => {...state, snake: Snake.setDirection state.snake key}
    | None => state
  }
};

let step state => {
  open Tile;
  let {tiles, snake, size} = state;

  let (snake, cleared, newBody, newHead) = Snake.move snake size;
  let tiles = switch cleared {
    | Some tile => HashMap.put tile Empty tiles
    | None => tiles
  };
  let tiles = HashMap.put newBody SnakeBody tiles;
  let tiles = HashMap.put newHead SnakeHead tiles;

  {...state, tiles, snake}
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
  {
    size: (w, h),
    snake: snake,
    tiles: (addSnake snake
      (HashMap.fromEntriesWith
    hash::Hashtbl.hash
    comparator::universalCompare
    (Immutable.List.toIterable
      (makeBoard w h))
    )),
  }
};
