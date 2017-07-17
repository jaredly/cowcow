
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
  /* | Empty => Constants.white */
  | Empty => Reprocessing.Utils.color 200 200 200
  | Cow => Constants.black
  | Barrier
  | Portal _
  | Mongoose => Constants.blue
  | SnakeBody => Reprocessing.Utils.color 100 0 0
  | SnakeHead => Reprocessing.Utils.color 255 0 0
};
