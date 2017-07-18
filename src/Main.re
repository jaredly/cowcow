
type images = {
  headUp: Reprocessing_Types.Types.imageT,
  headDown: Reprocessing_Types.Types.imageT,
  headLeft: Reprocessing_Types.Types.imageT,
  headRight: Reprocessing_Types.Types.imageT,
  bodyTL: Reprocessing_Types.Types.imageT,
  bodyTR: Reprocessing_Types.Types.imageT,
  bodyBL: Reprocessing_Types.Types.imageT,
  bodyBR: Reprocessing_Types.Types.imageT,
  bodyH: Reprocessing_Types.Types.imageT,
  bodyV: Reprocessing_Types.Types.imageT,
  mongoose: Reprocessing_Types.Types.imageT,
  apple: Reprocessing_Types.Types.imageT,
};

type state = {
  game: Game.state,
  font: Reprocessing_Types.Types.fontT,
  images: images,
  tick: int,
};

let setup env => {
  open Reprocessing;
  Env.size width::600 height::600 env;
  Draw.fill Constants.red env;
  Draw.noStroke env;
  {
    font: Draw.loadFont filename::"font/font.fnt" env,
    game: Game.initialState (20, 20),
    images: {
      headUp: Draw.loadImage filename::"images/head_up.png" env,
      headDown: Draw.loadImage filename::"images/head_down.png" env,
      headLeft: Draw.loadImage filename::"images/head_left.png" env,
      headRight: Draw.loadImage filename::"images/head_right.png" env,
      bodyBL: Draw.loadImage filename::"images/body_bl.png" env,
      bodyBR: Draw.loadImage filename::"images/body_br.png" env,
      bodyTL: Draw.loadImage filename::"images/body_tl.png" env,
      bodyTR: Draw.loadImage filename::"images/body_tr.png" env,
      bodyH: Draw.loadImage filename::"images/body_h.png" env,
      bodyV: Draw.loadImage filename::"images/body_v.png" env,
      mongoose: Draw.loadImage filename::"images/mongoose.png" env,
      apple: Draw.loadImage filename::"images/cow.png" env,
    },
    tick: 0,
  }
};

let scale = 30;

let keyPressed state env => {
  {...state, game: Game.handleKey state.game (Reprocessing.Env.keyCode env)}
};

let background = Reprocessing.Utils.color r::250 g::250 b::250;

let draw state env => {
  open Reprocessing;
  Draw.background background env;

  Game.iterTiles
  (fun (x, y) tile => {
    open Tile;
    let image = switch tile {
      | SnakeHead => {
        open Snake;
        Some (switch state.game.Game.snake.lastDirection {
          | Up => state.images.headUp
          | Down => state.images.headDown
          | Left => state.images.headLeft
          | Right => state.images.headRight
        });
      }
      | Empty => None
      | SnakeBody dir => Some (switch dir {
        | TL => state.images.bodyTL
        | TR => state.images.bodyTR
        | BL => state.images.bodyBL
        | BR => state.images.bodyBR
        | H => state.images.bodyH
        | V => state.images.bodyV
      })
      | Mongoose => Some state.images.mongoose
      | Cow => Some state.images.apple
      | _ => {
        Draw.fill (Tile.tileColor tile) env;
        Draw.rect pos::(x * scale, y * scale) width::scale height::scale env;
        None
      }
    };
    switch image {
      | Some image => Draw.image image pos::(x * scale, y * scale) width::scale height::scale env;
      | None => ()
    };
    ()
  }) state.game;

  Draw.text state.font ("Score: " ^ (string_of_int (state.game.Game.snake.Snake.size - 5))) (0, 0) env;

  let state = {...state, tick: state.tick + 1};
  if (state.tick mod 10 == 0) {
    {...state, game: Game.step state.game}
  } else {
    state
  }
};

Reprocessing.run ::setup ::draw ::keyPressed ();
