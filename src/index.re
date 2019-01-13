open Reprocessing;

module List = {
  include List;
  let rec filterMap = (t, f) => {
    switch(t) {
      | [] => []
      | [x, ...tl] => 
        switch(f(x)) {
          | Some(x) => [x, ...filterMap(tl, f)]
          | None => filterMap(tl, f)
        }
    }
  }
};

let pi = 4.0 *. atan(1.0)

/* Game constants */
let dAngle = 0.2;
let acceleration = 1.0;
let dt = 0.1;
let bulletAcceleration = 5.0;

/* Js.log2("dAngle:", dAngle); */
/* Js.log2("acceleration:", acceleration); */
/* Js.log2("dt:", dt); */

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

  let onScreen = t => {
    t.x <= sizef && t.x >= 0. &&
    t.y <= sizef && t.y >= 0. 
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
}

module Bullet = { 
  type t = {
    pos : Point.t,
    velocity : Vector.t,
  }

  let timeStep = (t, dt) => {
    let { pos, velocity } = t;
    let pos = Point.add(pos, Vector.scale(velocity, ~by=dt));
    if (Point.onScreen(pos)) {
      Some({ ...t, pos })
    } else None
  }

  let draw = (t, env) : unit => {
    Draw.pixelf(
      ~pos=Point.tuple(t.pos), 
      ~color=Color.white,
      env
    )
  }
}

module Ship = {
  type t = {
    centerOfMass : Point.t,
    direction : Vector.t,
    velocity : Vector.t,
  }

  let wrap = t => {
    let round = f => int_of_float (f +. 0.5)
    let wrap = f => float((round(f) + size) mod size);
    {
      ...t,
      centerOfMass:
        Point.{x: wrap(t.centerOfMass.x), y: wrap(t.centerOfMass.y)},
    };
  }

  let move = (t, dt) : t => {
    let { centerOfMass, velocity } = t;
    { ...t, 
      centerOfMass: Point.add(centerOfMass, Vector.scale(velocity, ~by=dt))
    }
  };

  let timeStep = (t, dt) : t => {
    move(t, dt) |> wrap
  }
  
  let accelerate = (t, dt) : t => {
    let a = Vector.scale(t.direction, ~by=acceleration*.dt);
    { ...t, 
      velocity: Vector.add(t.velocity, a)
    }
  }

  let tip = t => {
    Point.add(t.centerOfMass, Vector.scale(t.direction, ~by=2.))
  }
  
  let draw = (t, env) => {
    /* Center of mass is 2/3 from tip to middle of base */
    let negDirection = Vector.scale(t.direction, ~by=-1.);
    let tip = tip(t);
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

  let shoot = t : Bullet.t => {
    let tip = tip(t);
    let a = Vector.scale(t.direction, ~by=bulletAcceleration);
    let velocity = Vector.add(t.velocity, a);
    {Bullet.pos: tip, velocity};
  }

}

module State = {
  type t = { 
    ship : Ship.t,
    bullets : list(Bullet.t),
  }
}

let setup = (env) : State.t => {
  Env.size(~width=size, ~height=size, env);
  let ship = { 
    Ship.centerOfMass: Point.{x: sizef /. 2., y: sizef /. 2.},
    direction: Vector.{x: -10., y: 0.},
    velocity: Vector.zero,
  };
  {ship, bullets: []};
}

let draw = (state, env) : State.t => {
  let { State.ship, bullets } = state;
  Draw.background(Color.black, env);
  Draw.strokeWeight(2, env);
  Draw.stroke(Color.white, env);
  Ship.draw(ship, env);
  bullets
  |> List.iter(bullet => Bullet.draw(bullet, env));
  let ship = Ship.timeStep(ship, dt);
  let bullets = bullets |. List.filterMap(b => Bullet.timeStep(b, dt));
  { ship, bullets }
}

let keyTyped = (state, env) : State.t => {
  let { State.ship } = state;
  let key = Env.keyCode(env);
  let rotate = (ship: Ship.t, angle) => {
    {...ship, Ship.direction: Vector.rotate(ship.direction, angle)};
  };
  switch (key) {
  | Left => {...state, ship: rotate(ship, -. dAngle)}
  | Right => {...state, ship: rotate(ship, dAngle)}
  | Up => {...state, ship: Ship.accelerate(ship, dt)}
  | Space => {
    let bullet = Ship.shoot(ship);
    { ...state, bullets: [ bullet, ...state.bullets ] }
  }
  | _ => state
  };
};

run(~setup, ~draw, ~keyTyped, ())
