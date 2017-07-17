
type state = {
  game: Game.state,
  tick: int,
};

let setup env => {
  open Reprocessing;
  Env.size width::600 height::600 env;
  Draw.fill Constants.red env;
  Draw.noStroke env;
  {
    game: Game.initialState (20, 20),
    tick: 0,
  }
};

let scale = 30;

let keyPressed state env => {
  {...state, game: Game.handleKey state.game (Reprocessing.Env.keyCode env)}
};

let background = Reprocessing.Utils.color r::200 g::200 b::200;

let draw state env => {
  open Reprocessing;
  Draw.background background env;

  Game.iterTiles
  (fun (x, y) tile => {
    if (tile != Tile.Empty) {
      Draw.fill (Tile.tileColor tile) env;
      Draw.rect pos::(x * scale, y * scale) width::scale height::scale env;
    };
    ()
  }) state.game;

  let state = {...state, tick: state.tick + 1};
  if (state.tick mod 10 == 0) {
    {...state, game: Game.step state.game}
  } else {
    state
  }
};

Reprocessing.run ::setup ::draw ::keyPressed ();
