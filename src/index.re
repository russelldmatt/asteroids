open Reprocessing;

/* TODO: 
   - random generation of more asteroids
   - different sizes of asteroids, and larger ones split up into multiple of the next smaller size
   - score!
   - different game over state than crashing
   - multiple lives?
   - non-circular asteroids (sounds hard...)
 */
module List = {
  include List;
  let init = (n, f) => Array.init(n, f) |> Array.to_list

  let findMapi = (t, f) => {
    let rec loop = (t, i, f) => 
      switch(t) {
        | [] => None
        | [x, ...tl] => 
          switch(f(i, x)) {
            | Some(a) => Some(a)
            | None => loop(tl, i+1, f)
          }
      };
    loop(t, 0, f)
  }

  let find = (t, f) => {
    let f = (_, x) => f(x) ? Some(x) : None;
    findMapi(t, f)
  }

  let filterMapi = (t, f) => {
    let rec loop = (t, i, f) => 
      switch(t) {
        | [] => []
        | [x, ...tl] => 
          switch(f(i, x)) {
            | Some(x) => [x, ...loop(tl, i+1, f)]
            | None => loop(tl, i+1, f)
          }
      };
    loop(t, 0, f)
  }

  let filterMap = (t, f) => {
    filterMapi(t, (_, x) => f(x))
  }

  let filteri = (t, f) => {
    let f = (i, x) => f(i, x) ? Some(x) : None;
    filterMapi(t, f)
  }
};

let pi = 4.0 *. atan(1.0)

/* Game constants */
let dAngle = 0.1;
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
  
  let sub = (t1, t2) => {
    add(t1, mult(t2, ~by=-1.))
  }

  let dot = (t1, t2) => {
    t1.x *. t2.x +. t1.y *. t2.y
  }

  let length = t => {
    dot(t, t) ** 0.5
  }

  let distance = (t1, t2) => {
    length(sub(t2, t1))
  }

  let onScreen = t => {
    t.x <= sizef && t.x >= 0. &&
    t.y <= sizef && t.y >= 0. 
  }

  let wrap = t => {
    /* ... what? */
    let wrap = f => f < 0. ? f +. sizef : f > sizef ? f -. sizef : f;
    onScreen(t) ? t : { x: wrap(t.x), y: wrap(t.y) }
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

  let randomUnit = (()) => {
    let theta = Random.float(2. *. pi);
    rotate({ x: 1., y: 0.}, theta)
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
    let centerOfMass = Point.wrap(t.centerOfMass);
    {
      ...t,
      centerOfMass
    }
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
    /* Js.log(a); */
    let velocity = Vector.add(t.velocity, a);
    /* Js.log(velocity); */
    {...t, velocity};
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

module Asteroid = {
  /* Circle, for now */
  type t = {
    center: Point.t,
    velocity : Vector.t,
    radius: float,
  }
  
  /* Copied from bullet */
  /* Would be better if I detected when any part of the circle was onscreen instead of just center */
  let timeStep = (t, dt) => {
    let { center, velocity } = t;
    let center =
      Point.add(center, Vector.scale(velocity, ~by=dt))
    |> Point.wrap;
    { ...t, center }
  }

  let isWithin = (t, point) => {
    Point.distance(t.center, point) <= t.radius
  }
  
  let draw = (t, env) : unit => {
    Draw.ellipsef(
      ~center=Point.tuple(t.center),
      ~radx=t.radius,
      ~rady=t.radius,
      env
    )
  }
}

module State = {
  type t = { 
    ship : Ship.t,
    bullets : list(Bullet.t),
    asteroids : list(Asteroid.t),
  }
}

let setup = (env) : State.t => {
  Env.size(~width=size, ~height=size, env);
  let ship = { 
    Ship.centerOfMass: Point.{x: sizef /. 2., y: sizef /. 2.},
    direction: Vector.{x: -10., y: 0.},
    velocity: Vector.zero,
  };
  let numAsteroids = 5 + Random.int(10);
  let asteroids = 
    List.init(numAsteroids, _ => {
      let center = { Point.x: Random.float(sizef), y: Random.float(sizef) };
      let velocity = Vector.scale(Vector.randomUnit(), ~by=20.);
      { Asteroid.center, velocity, radius: 10. }
  });
  {ship, bullets: [], asteroids}
}

let respondToKey = (state, env) : State.t => {
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

let draw = (state, env) : State.t => {
  let { State.ship, bullets, asteroids } = state;
  Draw.background(Color.black, env);
  Draw.strokeWeight(2, env);
  Draw.stroke(Color.white, env);
  Ship.draw(ship, env);
  bullets |> List.iter(bullet => Bullet.draw(bullet, env));
  asteroids |> List.iter(a => Asteroid.draw(a, env));
  let ship = Ship.timeStep(ship, dt);
  let bullets = bullets |. List.filterMap(b => Bullet.timeStep(b, dt));
  let asteroids = asteroids |> List.map(a => Asteroid.timeStep(a, dt));
  /* Detect collisions */
  /* if any bullet and asteroid intersect, remove them both */
  let bulletAsteroidIntersections = {
    bullets
    |. List.filterMapi((i, b) => {
        asteroids
        |. List.findMapi((j, a) => Asteroid.isWithin(a, b.pos) ? Some((i, j)) : None)
      })
  };
  let bulletsToRemove = List.map(fst, bulletAsteroidIntersections);
  let asteroidsToRemove = List.map(snd, bulletAsteroidIntersections);
  let bullets = bullets |. List.filteri((i, _) => !List.mem(i, bulletsToRemove));
  let asteroids = asteroids |. List.filteri((i, _) => !List.mem(i, asteroidsToRemove));

  let gameOver = {
    asteroids 
    /* Should be better */
    |. List.find(a => Asteroid.isWithin(a, ship.centerOfMass))
    |> Belt.Option.isSome
  };
  if (gameOver) failwith("game over");

  let state = { State.ship, bullets, asteroids };
  let relevantKeyIsPressed =
    List.exists(key => Env.keyPressed(key, env), [ Space ]);
  let relevantKeyIsHeld = 
    List.exists(
    key => Env.key(key, env),
    [ Left,
      Right,
      Up,
    ]);
  (relevantKeyIsPressed || relevantKeyIsHeld) ? respondToKey(state, env) : state
}

run(~setup, ~draw, ())
