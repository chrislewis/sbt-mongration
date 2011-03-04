package mongo

class RichEither[L, R](val either: Either[L, R]) {
  def map[RR](f: R => RR): Either[L, RR] = either match {
    case Left(l) => Left(l)
    case Right(r) => Right(f(r))
  }
  def flatMap[RR](f: R => Either[L, RR]): Either[L, RR] = either match {
    case Left(l) => Left(l)
    case Right(r) => f(r)
  }
}

class Eithers[L, R](es: List[Either[L, R]]) {
  
  def lefts: Either[List[L], List[R]] = es partition (_ isLeft) match {
    case (ls @ l :: tail, _) =>
      ls.foldLeft(Left[List[L], List[R]](Nil)) {
        case (Left(rs), Left(r)) => Left(r :: rs)
      }
    case (_, rs) =>
      rs.foldLeft(Right[List[L], List[R]](Nil)) {
        case (Right(rs), Right(r)) => Right(r :: rs)
      }
  }
}
