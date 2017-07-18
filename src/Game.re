
let module HashMap = Immutable.HashMap;
let module Vector = Immutable.Vector;

type gameStatus =
  /* TODO intro */
  | Alive
  | Dead;

type state = {
  mongooseTimer: int,
  mongeese: list Tile.pos,
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

let rec emptySpot tiles (w, h) => {
  let x = Random.int w;
  let y = Random.int h;
  switch (HashMap.getOrRaise (x, y) tiles) {
    | Tile.Empty => (x, y)
    | _ => emptySpot tiles (w, h)
  }
};

let rec firstEmptyOrSnake tiles choices => {
  switch choices {
    | [] => (None, false)
    | [pos, ...rest] => {
      switch (HashMap.getOrRaise pos tiles) {
        | Tile.Empty => (Some pos, false)
        | Tile.SnakeHead => (Some pos, true)
        | _ => firstEmptyOrSnake tiles rest
      }
    }
  }
};

let moveMongoose tiles (x, y) (sx, sy) => {
  let dx = compare sx x;
  let dy = compare sy y;
  let choices = Random.bool () ? [
    (x, y + dy),
    (x + dx, y),
  ] : [
    (x + dx, y),
    (x, y + dy),
  ];
  switch (firstEmptyOrSnake tiles choices) {
    | (Some pos, snake) => (pos, snake)
    | (None, snake) => ((x, y), snake)
  }
};

let mongooseStep ate state => {
  let {tiles, snake, size, mongooseTimer, mongeese} = state;
  let mongooseMoveWait = snake.Snake.size > 20 ? 2 : 4;
  let (tiles, mongeese, snakeIsDead) = List.fold_left
  (fun (tiles, mongeese, snakeIsDead) mongoose => {
    if (Random.int 10 > mongooseMoveWait) {
      let (newMongoose, killedSnake) = moveMongoose tiles mongoose snake.Snake.head;
      (
        HashMap.put mongoose Tile.Empty tiles
        |> HashMap.put newMongoose Tile.Mongoose,
        [newMongoose, ...mongeese],
        killedSnake || snakeIsDead
      )
    } else {
      (tiles, [mongoose, ...mongeese], snakeIsDead)
    }
  })
  (tiles, [], false)
  mongeese;

  let status = state.status == Alive ? (snakeIsDead ? Dead : Alive) : state.status;

  [%guard let true = ate][@else {...state, tiles, mongeese, status}];

  let (mongooseTimer, newMongoose) = switch mongooseTimer {
    | -1 => (snake.Snake.size >= 10 ? 0 : -1, None)
    | 0 => {
      (5, Some (emptySpot tiles size))
    }
    | _ => (mongooseTimer - 1, None)
  };

  switch newMongoose {
    | Some tile => {
      let tiles = HashMap.put tile Tile.Mongoose tiles;
      let mongeese = [tile, ...mongeese];
      {...state, mongooseTimer, mongeese, tiles, status}
    }
    | None => {...state, mongooseTimer, mongeese, tiles, status}
  }
};

let step state => {
  [%guard let Alive = state.status][@else state];
  [%guard let false = state.paused][@else state];
  let {tiles, snake, size} = state;

  let (snake, cleared, newBody, newBodyDirection, newHead) = Snake.move snake size;
  let tiles = switch cleared {
    | Some tile => HashMap.put tile Tile.Empty tiles
    | None => tiles
  };
  let (status, ate) = switch (HashMap.getOrRaise newHead tiles) {
    | Tile.Empty => (Alive, false)
    | Tile.Cow => (Alive, true)
    | _ => (Dead, false)
  };
  let tiles = HashMap.put newBody (Tile.SnakeBody newBodyDirection) tiles;
  let tiles = HashMap.put newHead Tile.SnakeHead tiles;
  let tiles = ate ? HashMap.put (emptySpot tiles size) Tile.Cow tiles : tiles;
  mongooseStep ate {...state, tiles, snake: ate ? Snake.eat snake : snake, status}
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
  (fun board pos => HashMap.put pos (Tile.SnakeBody Tile.H) board)
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
  let cow = emptySpot tiles (w, h);
  let tiles = HashMap.put cow Tile.Cow tiles;
  {
    size: (w, h),
    snake,
    tiles,
    status: Alive,
    paused: false,
    mongooseTimer: -1,
    mongeese: [],
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
