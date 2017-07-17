
let module Constants = Reprocessing.Constants;

type pos = (int, int);
type t =
  | Empty
  | Mongoose
  | Barrier
  | Cow
  | Portal (string, pos)
  | SnakeBody
  | SnakeHead
  ;

let tileColor tile => switch tile {
  | Empty => Constants.white
  | Cow => Constants.black
  | Barrier
  | Portal _
  | Mongoose => Constants.blue
  | SnakeBody => Constants.red
  | SnakeHead => Constants.green
};
