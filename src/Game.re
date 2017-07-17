
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

let rec firstEmpty tiles choices => {
  switch choices {
    | [] => None
    | [pos, ...rest] => {
      switch (HashMap.getOrRaise pos tiles) {
        | Tile.Empty => Some pos
        | _ => firstEmpty tiles rest
      }
    }
  }
};

let moveMongoose tiles (x, y) (sx, sy) => {
  let dx = compare sx x;
  let dy = compare sy y;
  let choices = [
    (x + dx, y + dy),
    (x, y + dy),
    (x + dx, y),
  ];
  switch (firstEmpty tiles choices) {
    | Some pos => pos
    | None => (x, y)
  }
};

let mongooseStep ate state => {
  let {tiles, snake, size, mongooseTimer, mongeese} = state;
  /* TODO move mongeese */
  let (tiles, mongeese) = List.fold_left
  (fun (tiles, mongeese) mongoose => {
    if (Random.int 10 > 4) {
      let newMongoose = moveMongoose tiles mongoose snake.Snake.head;
      (
        HashMap.put mongoose Tile.Empty tiles
        |> HashMap.put newMongoose Tile.Mongoose,
        [newMongoose, ...mongeese]
      )
    } else {
      (tiles, [mongoose, ...mongeese])
    }
  })
  (tiles, [])
  mongeese;

  [%guard let true = ate][@else {...state, tiles, mongeese}];

  let (mongooseTimer, newMongoose) = switch mongooseTimer {
    | -1 => (snake.Snake.size == 10 ? 10 : -1, None)
    | 0 => {
      (50 + Random.int 20, Some (emptySpot tiles size))
    }
    | _ => (mongooseTimer - 1, None)
  };

  switch newMongoose {
    | Some tile => {
      let tiles = HashMap.put tile Tile.Mongoose tiles;
      let mongeese = [tile, ...mongeese];
      {...state, mongooseTimer, mongeese, tiles}
    }
    | None => {...state, mongooseTimer, mongeese, tiles}
  }
};

let step state => {
  [%guard let Alive = state.status][@else state];
  [%guard let false = state.paused][@else state];
  let {tiles, snake, size} = state;

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
  let cow = emptySpot tiles (w, h);
  let tiles = HashMap.put cow Tile.Cow tiles;
  {
    size: (w, h),
    snake,
    tiles,
    status: Alive,
    paused: false,
    mongooseTimer: -1,
    mongeese: [(10, 10)],
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
