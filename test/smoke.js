'use strict';

require('../js/heightfield.js');
require('../js/flight.js');

const { heightAt } = globalThis.Heightfield;
const FlightModel = globalThis.FlightModel;

let failures = 0;
function check(name, cond) {
  if (!cond) {
    failures++;
    console.error('FAIL:', name);
  } else {
    console.log('ok  :', name);
  }
}

let minH = Infinity, maxH = -Infinity, bad = 0;
for (let i = 0; i < 20000; i++) {
  const x = (Math.random() - 0.5) * 200000;
  const z = (Math.random() - 0.5) * 200000;
  const h = heightAt(x, z);
  if (!Number.isFinite(h)) bad++;
  minH = Math.min(minH, h);
  maxH = Math.max(maxH, h);
}
check('heights finite over 200km sample', bad === 0);
check('height range sane', minH > -200 && maxH < 800 && maxH > 100);
console.log('      height range:', minH.toFixed(1), '..', maxH.toFixed(1));

let foundMountain = false;
for (let i = 0; i < 8000 && !foundMountain; i++) {
  const x = (Math.random() - 0.5) * 80000;
  const z = (Math.random() - 0.5) * 80000;
  if (heightAt(x, z) > 250) foundMountain = true;
}
check('mountains exist', foundMountain);

let flatOk = true;
for (let z = -280; z <= 280; z += 40) {
  for (const x of [-100, -20, 0, 20, 100]) {
    if (Math.abs(heightAt(x, z)) > 0.01) flatOk = false;
  }
}
check('runway zone flat', flatOk);
check('deterministic', heightAt(1234.5, -987.6) === heightAt(1234.5, -987.6));

const fm = new FlightModel(heightAt);
const dt = 1 / 60;
check('starts on ground at runway', fm.onGround && Math.abs(heightAt(fm.pos.x, fm.pos.z)) < 0.01);

let t = 0;
while (t < 30) {
  fm.throttle = 1;
  const sp = fm.speed();
  if (sp > 40 && t < 12) fm.input.pitch = 0.6;
  else if (t < 16) fm.input.pitch = 0.25;
  else fm.input.pitch = 0;
  fm.update(dt);
  t += dt;
  if (fm.crashed) break;
}
console.log('      after takeoff: alt', fm.pos.y.toFixed(1), 'm, spd', fm.speed().toFixed(1),
  'm/s, onGround', fm.onGround, 'crashed', fm.crashed);
check('no crash on takeoff roll', !fm.crashed);
check('airborne', !fm.onGround);
check('climbed above 100m', fm.pos.y > 100);
check('speed sane', fm.speed() > 35 && fm.speed() < 140);

const fj = new FlightModel(heightAt, FlightModel.SPECS.jet);
let tj = 0;
while (tj < 20) {
  fj.throttle = 1;
  fj.input.pitch = (fj.speed() > 40 && tj < 8) ? 0.5 : 0;
  fj.update(dt);
  tj += dt;
  if (fj.crashed) break;
}
check('jet takeoff no crash', !fj.crashed);
check('jet airborne', !fj.onGround);
check('jet climbed above 150m', fj.pos.y > 150);
fj.input.pitch = 0;
fj.pitch = 0;
fj.gear = 0;
fj.gearTarget = 0;
for (let i = 0; i < 60 * 100; i++) {
  fj.update(dt);
  if (fj.crashed) break;
}
console.log('      jet dash speed:', fj.speed().toFixed(1), 'm/s = Mach', (fj.speed() / 343).toFixed(2));
check('jet reaches ~Mach 3', fj.speed() > 1010);
check('jet speed bounded at Mach 3', fj.speed() < 1060);
check('jet survives dash', !fj.crashed);

const fg = new FlightModel(heightAt);
check('gear retract rejected on ground', fg.toggleGear() === false && fg.gearTarget === 1);
fg.onGround = false;
check('gear toggle allowed airborne', fg.toggleGear() === true && fg.gearTarget === 0);

function steadySpeed(spec, gear) {
  const f = new FlightModel(heightAt, spec);
  f.pos = { x: 0, y: 600, z: 0 };
  f.vel = { x: 0, y: 0, z: -70 };
  f.onGround = false;
  f.gear = gear;
  f.gearTarget = gear;
  f.throttle = 1;
  for (let i = 0; i < 60 * 40; i++) f.update(dt);
  return f.speed();
}
const sUp = steadySpeed(FlightModel.SPECS.prop, 0);
const sDown = steadySpeed(FlightModel.SPECS.prop, 1);
console.log('      prop vmax gear up / down:', sUp.toFixed(1), '/', sDown.toFixed(1), 'm/s');
check('gear drag slows aircraft', sDown < sUp * 0.85);

const fb = new FlightModel(heightAt);
fb.pos = { x: 0, y: 5, z: 0 };
fb.vel = { x: 0, y: -2, z: -40 };
fb.onGround = false;
fb.gear = 0;
fb.gearTarget = 0;
for (let i = 0; i < 60 * 3 && !fb.crashed; i++) fb.update(dt);
check('belly landing crashes', fb.crashed);

const fl = new FlightModel(heightAt);
fl.pos = { x: 0, y: 5, z: 0 };
fl.vel = { x: 0, y: -2, z: -40 };
fl.onGround = false;
for (let i = 0; i < 60 * 2; i++) fl.update(dt);
check('gear-down touchdown safe', !fl.crashed);

const hdg0 = fm.headingDeg();
fm.input.pitch = 0;
for (let i = 0; i < 60 * 5; i++) {
  fm.input.roll = 1;
  fm.update(dt);
  if (fm.crashed) break;
}
fm.input.roll = 0;
let dHdg = Math.abs(fm.headingDeg() - hdg0);
if (dHdg > 180) dHdg = 360 - dHdg;
console.log('      heading change after 5s right bank:', dHdg.toFixed(0), 'deg');
check('banked turn works', dHdg > 60);
check('still flying after turn', !fm.crashed && Number.isFinite(fm.pos.y));

fm.flaps = 1;
fm.throttle = 0.4;
for (let i = 0; i < 60 * 6; i++) {
  fm.update(dt);
  if (fm.crashed) break;
}
console.log('      flaps 100% at 40% throttle: spd', fm.speed().toFixed(1), 'm/s, crashed', fm.crashed);
check('finite after flaps test', Number.isFinite(fm.pos.x + fm.pos.y + fm.pos.z + fm.speed()));

const fm2 = new FlightModel(heightAt);
let seed = 42;
const rnd = () => ((seed = (seed * 1103515245 + 12345) >>> 0) / 4294967296);
for (let i = 0; i < 60 * 90; i++) {
  if (i % 60 === 0) {
    fm2.input.pitch = rnd() * 2 - 1;
    fm2.input.roll = rnd() * 2 - 1;
    fm2.input.yaw = rnd() * 2 - 1;
    fm2.throttle = rnd();
    if (rnd() < 0.2) fm2.adjustFlaps(1);
    if (rnd() < 0.2) fm2.adjustFlaps(-1);
  }
  fm2.update(dt);
  if (fm2.crashed) fm2.reset();
}
const allFinite = [fm2.pos.x, fm2.pos.y, fm2.pos.z, fm2.vel.x, fm2.vel.y, fm2.vel.z,
  fm2.pitch, fm2.roll, fm2.yaw, fm2.speed()].every(Number.isFinite);
check('90s chaos run stays finite', allFinite);
check('90s chaos: never below terrain', fm2.pos.y >= heightAt(fm2.pos.x, fm2.pos.z) - 0.01);

if (failures) {
  console.error(failures + ' check(s) failed');
  process.exit(1);
}
console.log('All checks passed.');
