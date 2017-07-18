
let module Constants = Reprocessing.Constants;

type bodyDirection =
  | U | D
  | L | R
  | TL | LT
  | TR | RT
  | BL | LB
  | BR | RB;

type tailDirection =
  | Up
  | Down
  | Left
  | Right;

type pos = (int, int);
type t =
  | Empty
  | Mongoose
  | Barrier
  | Cow
  | Portal (string, pos)
  | SnakeBody bodyDirection
  | SnakeTail tailDirection
  | SnakeHead
  ;

let tailDirection tile => switch tile {
  | SnakeBody d => switch d {
    | D | RB | LB => Down
    | U | LT | RT => Up
    | R | TR | BR => Right
    | L | TL | BL => Left
  }
  | _ => Up
};

let tileColor tile => switch tile {
  /* | Empty => Constants.white */
  | Empty => Reprocessing.Utils.color 200 200 200
  | Cow => Constants.black
  | _
  | Barrier
  | Portal _
  | Mongoose => Constants.blue
  | SnakeBody _ => Reprocessing.Utils.color 100 0 0
  | SnakeHead => Reprocessing.Utils.color 255 0 0
};
