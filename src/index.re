open Reprocessing;

let pi = 4.0 *. atan(1.0)

let size = 600
let sizef = float(size)

module Color = {
  let black = Utils.color(~r=0, ~g=0, ~b=0, ~a=255)
  let white = Utils.color(~r=255, ~g=255, ~b=255, ~a=255)
}

module Point = {
  type t = { x: float, y: float }

  let tuple = t => (t.x, t.y)

  let add = (t1, t2) => {
    { x: t1.x +. t2.x,
      y: t1.y +. t2.y,
    }
  }

  let mult = (t, ~by) => {
    { x: t.x *. by,
      y: t.y *. by,
    }
  }

  let dot = (t1, t2) => {
    t1.x *. t2.x +. t1.y *. t2.y
  }

  let length = t => {
    dot(t, t) ** 0.5
  }
}

module Vector = {
  include Point
  let scale = mult;
  
  let rotate = (t, theta) => {
    let theta = atan(t.y /. t.x) +. theta;
    let a = Point.length(t);
    { x: a *. cos(theta),
      y: a *. sin(theta)
    }
  }
}

module Ship = {
  type t = {
    tip : Point.t,
    direction : Vector.t,
    velocity : Vector.t,
  }

  let draw = (t, env) => {
    let negDirection = Vector.scale(t.direction, ~by=-1.);
    let backMiddle = Point.add(t.tip, negDirection);
    let backLeft = Point.add(backMiddle, Vector.scale(Vector.rotate(negDirection, -.pi/.2.), ~by=0.3));
    let backRight = Point.add(backMiddle, Vector.scale(Vector.rotate(negDirection, pi/.2.), ~by=0.3));
    Draw.trianglef(
      ~p1=Point.tuple(t.tip),
      ~p2=Point.tuple(backLeft),
      ~p3=Point.tuple(backRight),
      env
    )
  }
}

type state = {
  ship : Ship.t,
}

let setup = (env) : state => {
  Env.size(~width=size, ~height=size, env);

}

let draw = (_state, env) : state => {
  Draw.background(Color.black, env);
}

run(~setup, ~draw, ())
