
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
  paused: bool,
};

let iterTiles fn state => {
  HashMap.reduce
  (fun () pos tile => fn pos tile)
  ()
  state.tiles;
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

let rec newCowPlace tiles (w, h) => {
  let x = Random.int w;
  let y = Random.int h;
  switch (HashMap.getOrRaise (x, y) tiles) {
    | Tile.Empty => (x, y)
    | _ => newCowPlace tiles (w, h)
  }
};

let step state => {
  [%guard let Alive = state.status][@else state];
  [%guard let false = state.paused][@else state];
  let {tiles, snake, size, status} = state;

  let (snake, cleared, newBody, newHead) = Snake.move snake size;
  let tiles = switch cleared {
    | Some tile => HashMap.put tile Tile.Empty tiles
    | None => tiles
  };
  let (status, ate) = switch (HashMap.getOrRaise newHead tiles) {
    | Tile.Empty => (Alive, false)
    | Tile.Cow => (Alive, true)
    | _ => (Dead, false)
  };
  let tiles = HashMap.put newBody Tile.SnakeBody tiles;
  let tiles = HashMap.put newHead Tile.SnakeHead tiles;
  let tiles = ate ? HashMap.put (newCowPlace tiles size) Tile.Cow tiles : tiles;
  {...state, tiles, snake: ate ? Snake.eat snake : snake, status}
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
  let cow = newCowPlace tiles (w, h);
  let tiles = HashMap.put cow Tile.Cow tiles;
  {
    size: (w, h),
    snake,
    tiles,
    status: Alive,
    paused: false,
  }
};

let handleKey state key => {
  /* [%bail if (Reprocessing.Events.Space == key) {

  }]; */

  switch key {
    | Reprocessing.Events.Space => {
      {...state, paused: not state.paused}
    }
    | _ => {
      switch (keyToSnakeDirection key) {
        | Some key => {...state, snake: Snake.setDirection state.snake key}
        | None => state
      }
    }
  }
};
