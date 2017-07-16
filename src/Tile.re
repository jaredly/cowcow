
let module Constants = Reprocessing.Constants;

type pos = (int, int);
type t =
  | Empty
  | Mongoose
  | Barrier
  | Portal (string, pos)
  | SnakeBody
  | SnakeHead
  ;

let tileColor tile => switch tile {
  | Empty => Constants.white
  | Barrier
  | Portal _
  | Mongoose => Constants.black
  | SnakeBody => Constants.red
  | SnakeHead => Constants.green
};
