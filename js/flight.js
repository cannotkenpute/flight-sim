(() => {
  'use strict';

  const G = 9.81;
  const GEAR_H = 1.6;
  const DRAG_BASE = 0.02;

  const SPECS = {
    prop: {
      name: 'PIPER HAWK',
      thrust: 14,
      dragK: 0.0022,
      liftRef: 42,
      stall: 26,
      pitchRate: 1.5,
      rollRate: 2.6,
      yawRate: 0.6,
      align: 1.4,
      flapDrag: 0.8,
      flapLift: 0.5,
      gearDrag: 0.45,
    },
    jet: {
      name: 'F-16 VECTOR',
      thrust: 75,
      dragK: 0.0000514,
      liftRef: 55,
      stall: 35,
      pitchRate: 1.3,
      rollRate: 3.4,
      yawRate: 0.5,
      align: 1.7,
      flapDrag: 0.5,
      flapLift: 0.35,
      gearDrag: 0.8,
    },
  };

  function clamp(v, a, b) {
    return v < a ? a : v > b ? b : v;
  }

  class FlightModel {
    constructor(heightAt, spec) {
      this.heightAt = heightAt;
      this.spec = spec || SPECS.prop;
      this.reset();
    }

    setSpec(spec) {
      this.spec = spec;
    }

    reset() {
      this.pos = { x: 0, y: GEAR_H, z: 280 };
      this.vel = { x: 0, y: 0, z: 0 };
      this.pitch = 0;
      this.roll = 0;
      this.yaw = 0;
      this.throttle = 0;
      this.flaps = 0;
      this.brake = false;
      this.gear = 1;
      this.gearTarget = 1;
      this.onGround = true;
      this.crashed = false;
      this.input = { pitch: 0, roll: 0, yaw: 0 };
    }

    toggleGear() {
      if (this.onGround && this.gearTarget === 1) return false;
      this.gearTarget = this.gearTarget ? 0 : 1;
      return true;
    }

    axes() {
      const cp = Math.cos(this.pitch), sp = Math.sin(this.pitch);
      const cy = Math.cos(this.yaw), sy = Math.sin(this.yaw);
      const cr = Math.cos(this.roll), sr = Math.sin(this.roll);
      const forward = { x: -sy * cp, y: sp, z: -cy * cp };
      const up = {
        x: -cy * sr + sy * sp * cr,
        y: cp * cr,
        z: sy * sr + cy * sp * cr,
      };
      const right = {
        x: cy * cr + sy * sp * sr,
        y: cp * sr,
        z: -sy * cr + cy * sp * sr,
      };
      return { forward, up, right };
    }

    speed() {
      return Math.hypot(this.vel.x, this.vel.y, this.vel.z);
    }

    agl() {
      return this.pos.y - GEAR_H - this.heightAt(this.pos.x, this.pos.z);
    }

    headingDeg() {
      let d = (-this.yaw * 180 / Math.PI) % 360;
      if (d < 0) d += 360;
      return d;
    }

    adjustFlaps(dir) {
      this.flaps = clamp(this.flaps + dir * 0.25, 0, 1);
    }

    update(dt) {
      const s = this.spec;
      if (this.gear !== this.gearTarget) {
        const dir = this.gearTarget > this.gear ? 1 : -1;
        this.gear += dir * 0.55 * dt;
        this.gear = dir > 0 ? Math.min(this.gear, this.gearTarget) : Math.max(this.gear, this.gearTarget);
      }
      const { forward, up } = this.axes();
      let speed = this.speed();
      const authority = clamp(speed / 50, 0, 1) * (this.onGround ? 0.5 : 1);

      const pitchRate = this.input.pitch * s.pitchRate * authority;
      const rollRate = -this.input.roll * s.rollRate * authority;
      let yawRate = -this.input.yaw * s.yawRate * authority;
      if (!this.onGround && speed > 5) {
        yawRate += (G * Math.tan(clamp(this.roll, -1.05, 1.05))) / Math.max(speed, 20);
      }

      this.pitch += pitchRate * dt;
      this.roll += rollRate * dt;
      if (!this.input.roll) this.roll -= this.roll * Math.min(1, 0.8 * dt);
      if (!this.onGround && speed < s.stall) {
        this.pitch -= (s.stall - speed) * 0.03 * dt;
      }
      this.pitch = clamp(this.pitch, -1.2, 1.2);
      this.roll = clamp(this.roll, -1.2, 1.2);
      this.yaw += yawRate * dt;

      let axx = 0, ayy = -G, azz = 0;
      const thrust = this.throttle * s.thrust;
      axx += forward.x * thrust;
      ayy += forward.y * thrust;
      azz += forward.z * thrust;

      const dragK = s.dragK * (1 + s.flapDrag * this.flaps) * (1 + s.gearDrag * this.gear) * (this.brake ? 3.2 : 1);
      const drag = speed * speed * dragK + DRAG_BASE * speed;
      if (speed > 0.01) {
        axx -= (this.vel.x / speed) * drag;
        ayy -= (this.vel.y / speed) * drag;
        azz -= (this.vel.z / speed) * drag;
      }

      if (speed > 3) {
        const liftK = G / (s.liftRef * s.liftRef);
        const lift = Math.min(speed * speed * liftK * (1 + s.flapLift * this.flaps), 2.4 * G);
        axx += up.x * lift;
        ayy += up.y * lift;
        azz += up.z * lift;
      }

      this.vel.x += axx * dt;
      this.vel.y += ayy * dt;
      this.vel.z += azz * dt;

      speed = this.speed();
      if (speed > 1) {
        const t = Math.min(1, (this.onGround ? 5 : s.align) * dt);
        let dx = this.vel.x / speed;
        let dy = this.vel.y / speed;
        let dz = this.vel.z / speed;
        dx += (forward.x - dx) * t;
        dy += (forward.y - dy) * t;
        dz += (forward.z - dz) * t;
        const n = Math.hypot(dx, dy, dz) || 1;
        let s2 = speed;
        if (this.onGround) {
          const fr = this.brake ? 7 : 0.5;
          s2 = Math.max(0, s2 - fr * dt);
        }
        this.vel.x = (dx / n) * s2;
        this.vel.y = (dy / n) * s2;
        this.vel.z = (dz / n) * s2;
      }

      this.pos.x += this.vel.x * dt;
      this.pos.y += this.vel.y * dt;
      this.pos.z += this.vel.z * dt;

      const groundY = this.heightAt(this.pos.x, this.pos.z) + GEAR_H;
      if (this.pos.y <= groundY + 0.01) {
        const impact = this.vel.y;
        this.pos.y = groundY;
        if (impact < -13 || Math.abs(this.roll) > 0.55 || Math.abs(this.pitch) > 0.5 || this.gear < 0.9) {
          this.crashed = true;
        }
        if (this.vel.y < 0) this.vel.y = 0;
        this.onGround = true;
        if (!this.crashed) {
          this.roll *= Math.max(0, 1 - 5 * dt);
          this.pitch = clamp(this.pitch, -0.15, 0.35);
        }
      } else if (this.pos.y > groundY + 0.3) {
        this.onGround = false;
      }
    }
  }

  FlightModel.GEAR_H = GEAR_H;
  FlightModel.SPECS = SPECS;
  globalThis.FlightModel = FlightModel;
})();
