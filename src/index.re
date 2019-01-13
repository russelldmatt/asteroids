open Reprocessing;

let pi = 4.0 *. atan(1.0)

/* Game constants */
let dAngle = 0.2;
let acceleration = 1.0;
let dt = 0.1;

Js.log2("dAngle:", dAngle);
Js.log2("acceleration:", acceleration);
Js.log2("dt:", dt);

let size = 600
let sizef = float(size)

module Color = {
  let black = Utils.color(~r=0, ~g=0, ~b=0, ~a=255)
  let white = Utils.color(~r=255, ~g=255, ~b=255, ~a=255)
}

module Point = {
  type t = { x: float, y: float }
  
  let zero = { x: 0., y: 0. }

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
    let theta = atan2(t.y, t.x) +. theta;
    let a = Point.length(t);
    { x: a *. cos(theta),
      y: a *. sin(theta)
    }
  }

  let rotate' = (t, theta) => {
    Js.log(t);
    Js.log(theta);
    let t = rotate(t, theta);
    Js.log(t);
    t
  }

}

module Ship = {
  type t = {
    centerOfMass : Point.t,
    direction : Vector.t,
    velocity : Vector.t,
  }

  let timeStep = (t, dt) : t => {
    let { centerOfMass, direction, velocity } = t;
    { ...t, 
      centerOfMass: Point.add(centerOfMass, Vector.scale(t.velocity, ~by=dt))
    }
  };

  let accelerate = (t, dt) : t => {
    let a = Vector.scale(t.direction, ~by=acceleration*.dt);
    { ...t, 
      velocity: Vector.add(t.velocity, a)
    }
  }
  
  let draw = (t, env) => {
    /* Center of mass is 2/3 from tip to middle of base */
    let negDirection = Vector.scale(t.direction, ~by=-1.);
    let tip = Point.add(t.centerOfMass, Vector.scale(t.direction, ~by=2.));
    let backMiddle = Point.add(t.centerOfMass, negDirection);
    let backLeft = Point.add(backMiddle, Vector.rotate(negDirection, -.pi/.2.));
    let backRight = Point.add(backMiddle, Vector.rotate(negDirection, pi/.2.));
    Draw.trianglef(
      ~p1=Point.tuple(tip),
      ~p2=Point.tuple(backLeft),
      ~p3=Point.tuple(backRight),
      env
    )
  }
}

module State = {
  type t = { ship : Ship.t }
}

let setup = (env) : State.t => {
  Env.size(~width=size, ~height=size, env);
  let ship = { 
    Ship.centerOfMass: Point.{x: sizef /. 2., y: sizef /. 2.},
    direction: Vector.{x: -20., y: 0.},
    velocity: Vector.zero,
  };
  { ship: ship }
}

let draw = (state, env) : State.t => {
  let { State.ship } = state;
  Draw.background(Color.black, env);
  Draw.strokeWeight(2, env);
  Draw.stroke(Color.white, env);
  Ship.draw(ship, env);
  let ship = Ship.timeStep(ship, dt);
  { State.ship : ship }
}

let keyTyped = (state, env) : State.t => {
  let { State.ship } = state;
  let key = Env.keyCode(env);
  let rotate = (ship: Ship.t, angle) => {
    {...ship, Ship.direction: Vector.rotate(ship.direction, angle)};
  };
  let ship =
    switch (key) {
    | Left => rotate(ship, -. dAngle)
    | Right => rotate(ship, dAngle)
    | Up => Ship.accelerate(ship, dt)
    | _ => ship
    };
  { State.ship : ship }
};

run(~setup, ~draw, ~keyTyped, ())
