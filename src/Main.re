open Reprocessing;
let module Vector = Immutable.Vector;
let module HashMap = Immutable.HashMap;

type board = HashMap.t Tile.pos Tile.t;

type state = {
  game: Game.state,
  tick: int,
};


let setup env => {
  Env.size width::600 height::600 env;
  Draw.fill Constants.red env;
  Draw.noStroke env;
  {
    game: Game.initialState (30, 30),
    tick: 0,
  }
};

let scale = 20;

let draw state env => {
  Draw.background Constants.black env;

  HashMap.reduce
  (fun () (x, y) tile => {
    Draw.fill (Tile.tileColor tile) env;
    Draw.rect pos::(x * scale, y * scale) width::scale height::scale env;
    ()
  })
  ()
  state.game.Game.tiles;
  let state = {...state, tick: state.tick + 1};
  if (state.tick mod 5 == 0) {
    {...state, game: Game.step state.game}
  } else {
    state
  }
};

run ::setup ::draw ();
