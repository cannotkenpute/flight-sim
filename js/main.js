(() => {
  'use strict';

  const SKY = 0x9fc7e8;

  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  renderer.setSize(window.innerWidth, window.innerHeight);
  document.body.appendChild(renderer.domElement);

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(SKY);
  scene.fog = new THREE.Fog(SKY, 900, 4300);

  const camera = new THREE.PerspectiveCamera(62, window.innerWidth / window.innerHeight, 0.1, 9000);

  const hemi = new THREE.HemisphereLight(0xd8ecff, 0x5a6b46, 0.8);
  scene.add(hemi);
  const sun = new THREE.DirectionalLight(0xffeecf, 1.05);
  sun.position.set(400, 600, -300);
  scene.add(sun);

  const terrain = new Terrain(scene, Heightfield.heightAt);

  const water = new THREE.Mesh(
    new THREE.PlaneGeometry(15000, 15000).rotateX(-Math.PI / 2),
    new THREE.MeshLambertMaterial({ color: 0x1d5f8f, transparent: true, opacity: 0.85 })
  );
  water.position.y = Heightfield.WATER;
  scene.add(water);

  const runway = new THREE.Mesh(
    new THREE.BoxGeometry(40, 0.2, 580),
    new THREE.MeshLambertMaterial({ color: 0x3a3d42 })
  );
  runway.position.set(0, 0.1, 0);
  scene.add(runway);
  const stripeMat = new THREE.MeshLambertMaterial({ color: 0xd8d8d8 });
  for (let z = -260; z <= 260; z += 40) {
    const stripe = new THREE.Mesh(new THREE.BoxGeometry(1.4, 0.06, 12), stripeMat);
    stripe.position.set(0, 0.25, z);
    scene.add(stripe);
  }

  const AIRFRAMES = [
    {
      key: 'prop',
      model: PlaneModel.build(),
      spec: FlightModel.SPECS.prop,
      cockpit: { up: 1.1, fwd: 1.8 },
    },
    {
      key: 'jet',
      model: PlaneModel.buildJet(),
      spec: FlightModel.SPECS.jet,
      cockpit: { up: 1.2, fwd: 2.4 },
    },
  ];
  let activeIdx = 0;
  for (const a of AIRFRAMES) {
    a.model.group.rotation.order = 'YXZ';
    scene.add(a.model.group);
  }

  const fm = new FlightModel(Heightfield.heightAt, AIRFRAMES[0].spec);
  terrain.update(fm.pos.x, fm.pos.z);

  const sound = new SoundManager();

  const CAM_MODES = ['CHASE', 'COCKPIT', 'TOWER', 'ORBIT'];
  let camIndex = 0;
  let orbitAngle = 0;
  let wireframe = false;
  let paused = false;

  const keys = Object.create(null);
  const els = {
    spd: document.getElementById('hud-spd'),
    alt: document.getElementById('hud-alt'),
    agl: document.getElementById('hud-agl'),
    vsi: document.getElementById('hud-vsi'),
    hdg: document.getElementById('hud-hdg'),
    mach: document.getElementById('hud-mach'),
    thr: document.getElementById('hud-thr'),
    thrBar: document.getElementById('hud-thr-bar'),
    flaps: document.getElementById('hud-flaps'),
    gear: document.getElementById('hud-gear'),
    airframe: document.getElementById('hud-ac'),
    cam: document.getElementById('cam-label'),
    stall: document.getElementById('warn-stall'),
    pullup: document.getElementById('warn-pullup'),
    message: document.getElementById('message'),
    adi: document.getElementById('adi-inner'),
  };

  let messageTimer = null;
  function showMessage(text, ms) {
    els.message.textContent = text;
    if (messageTimer) clearTimeout(messageTimer);
    if (ms) messageTimer = setTimeout(() => { els.message.textContent = ''; }, ms);
  }

  function setWireframe(on) {
    wireframe = on;
    terrain.setWireframe(on);
    for (const a of AIRFRAMES) {
      for (const m of a.model.materials) m.wireframe = on;
    }
    showMessage('WIREFRAME ' + (on ? 'ON' : 'OFF'), 1500);
  }

  function syncAircraft() {
    AIRFRAMES.forEach((a, i) => { a.model.group.visible = i === activeIdx; });
    const jet = AIRFRAMES[activeIdx].model.burner;
    if (jet) jet.visible = false;
    els.airframe.textContent = AIRFRAMES[activeIdx].spec.name;
  }

  function switchAircraft() {
    activeIdx = (activeIdx + 1) % AIRFRAMES.length;
    const a = AIRFRAMES[activeIdx];
    fm.setSpec(a.spec);
    syncAircraft();
    showMessage('AIRFRAME: ' + a.spec.name, 1800);
  }

  window.addEventListener('keydown', (e) => {
    sound.unlock();
    if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Space'].includes(e.code)) {
      e.preventDefault();
    }
    if (e.repeat) {
      keys[e.code] = true;
      return;
    }
    keys[e.code] = true;
    switch (e.code) {
      case 'KeyF':
        fm.adjustFlaps(1);
        break;
      case 'KeyR':
        fm.adjustFlaps(-1);
        break;
      case 'KeyC':
        camIndex = (camIndex + 1) % CAM_MODES.length;
        break;
      case 'Digit1': camIndex = 0; break;
      case 'Digit2': camIndex = 1; break;
      case 'Digit3': camIndex = 2; break;
      case 'Digit4': camIndex = 3; break;
      case 'KeyT':
        setWireframe(!wireframe);
        break;
      case 'KeyV':
        switchAircraft();
        break;
      case 'KeyG':
        if (fm.toggleGear()) {
          const up = fm.gearTarget === 0;
          showMessage('GEAR ' + (up ? 'UP' : 'DOWN'), 1500);
          sound.speak(up ? 'Gear up.' : 'Gear down.', 1200);
        } else {
          showMessage('GEAR LOCKED — WEIGHT ON WHEELS', 1800);
        }
        break;
      case 'KeyM':
        showMessage(sound.toggleMute() ? 'SOUND MUTED' : 'SOUND ON', 1500);
        break;
      case 'KeyP':
        paused = !paused;
        showMessage(paused ? 'PAUSED — press P to resume' : '', paused ? 0 : 100);
        break;
      case 'Space':
        fm.reset();
        showMessage('RESET TO RUNWAY', 1800);
        break;
    }
  });
  window.addEventListener('keyup', (e) => {
    keys[e.code] = false;
  });
  window.addEventListener('blur', () => {
    for (const k in keys) keys[k] = false;
  });

  window.addEventListener('resize', () => {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  });

  const v1 = new THREE.Vector3();
  const v2 = new THREE.Vector3();

  function toV3(o, target) {
    return target.set(o.x, o.y, o.z);
  }

  function clampAboveTerrain(p) {
    const minY = Heightfield.heightAt(p.x, p.z) + 4;
    if (p.y < minY) p.y = minY;
    return p;
  }

  function updateCamera(dt) {
    const { forward, up } = fm.axes();
    const pos = toV3(fm.pos, v1);
    const mode = CAM_MODES[camIndex];

    if (mode === 'CHASE') {
      v2.copy(pos).addScaledVector(toV3(forward, new THREE.Vector3()), -24).addScaledVector(new THREE.Vector3(0, 1, 0), 7);
      clampAboveTerrain(v2);
      camera.position.lerp(v2, Math.min(1, 4 * dt));
      camera.up.set(0, 1, 0);
      v2.copy(pos).addScaledVector(toV3(forward, new THREE.Vector3()), 15);
      camera.lookAt(v2);
    } else if (mode === 'COCKPIT') {
      const cp = AIRFRAMES[activeIdx].cockpit;
      v2.copy(pos)
        .addScaledVector(toV3(up, new THREE.Vector3()), cp.up)
        .addScaledVector(toV3(forward, new THREE.Vector3()), cp.fwd);
      camera.position.copy(v2);
      camera.up.copy(toV3(up, new THREE.Vector3()));
      v2.copy(pos).addScaledVector(toV3(forward, new THREE.Vector3()), 300);
      camera.lookAt(v2);
    } else if (mode === 'TOWER') {
      camera.position.set(70, 30, 430);
      camera.up.set(0, 1, 0);
      camera.lookAt(pos);
    } else {
      orbitAngle += dt * 0.35;
      v2.set(
        pos.x + Math.cos(orbitAngle) * 42,
        pos.y + 14,
        pos.z + Math.sin(orbitAngle) * 42
      );
      clampAboveTerrain(v2);
      camera.position.copy(v2);
      camera.up.set(0, 1, 0);
      camera.lookAt(pos);
    }
    els.cam.textContent = mode + ' CAM';
  }

  function updateHud() {
    const speed = fm.speed();
    els.spd.textContent = (speed * 1.94384).toFixed(0) + ' kt';
    els.mach.textContent = (speed / 343).toFixed(2);
    els.alt.textContent = fm.pos.y.toFixed(0) + ' m';
    els.agl.textContent = Math.max(0, fm.agl()).toFixed(0) + ' m AGL';
    els.vsi.textContent = (fm.vel.y >= 0 ? '+' : '') + fm.vel.y.toFixed(1) + ' m/s';
    els.hdg.textContent = fm.headingDeg().toFixed(0).padStart(3, '0') + '°';
    const thrPct = Math.round(fm.throttle * 100);
    els.thr.textContent = thrPct + '%';
    els.thrBar.style.width = thrPct + '%';
    els.flaps.textContent = Math.round(fm.flaps * 100) + '%';
    const gearLocked = fm.gear === fm.gearTarget;
    els.gear.textContent = gearLocked ? (fm.gearTarget ? 'DOWN' : 'UP') : 'IN TRANSIT';
    els.gear.style.color = (gearLocked && fm.gearTarget) ? '' : '#ffd97a';
    els.stall.style.display = (!fm.onGround && speed < fm.spec.stall + 4) ? 'block' : 'none';
    els.pullup.style.display = threatDist > 0 ? 'block' : 'none';
    const rollDeg = fm.roll * 57.2958;
    const pitchDeg = fm.pitch * 57.2958;
    const shift = Math.max(-55, Math.min(55, pitchDeg * 1.6));
    els.adi.style.transform = 'rotate(' + (-rollDeg).toFixed(1) + 'deg) translateY(' + shift.toFixed(1) + 'px)';
  }

  syncAircraft();
  showMessage('Throttle up with W — rotate with ↑ near takeoff speed — V swaps aircraft, C camera', 9000);

  function terrainThreat() {
    const { forward } = fm.axes();
    const horiz = Math.hypot(forward.x, forward.z);
    if (horiz < 0.15) return 0;
    const dx = forward.x / horiz;
    const dz = forward.z / horiz;
    const speed = Math.max(fm.speed(), 1);
    const slope = fm.vel.y / speed;
    const lookDist = Math.min(speed * 3.5 + 150, 4000);
    for (let d = 30; d <= lookDist; d += 30) {
      const h = Heightfield.heightAt(fm.pos.x + dx * d, fm.pos.z + dz * d);
      if (h >= fm.pos.y + slope * d - 15) return d;
    }
    return 0;
  }

  const clock = new THREE.Clock();

  function update() {
    const dt = Math.min(clock.getDelta(), 0.05);

    if (keys.KeyW) fm.throttle = Math.min(1, fm.throttle + 0.55 * dt);
    if (keys.KeyS) fm.throttle = Math.max(0, fm.throttle - 0.55 * dt);
    fm.input.pitch = (keys.ArrowUp ? 1 : 0) - (keys.ArrowDown ? 1 : 0);
    fm.input.roll = (keys.ArrowRight ? 1 : 0) - (keys.ArrowLeft ? 1 : 0);
    fm.input.yaw = (keys.KeyD ? 1 : 0) - (keys.KeyA ? 1 : 0);
    fm.brake = !!keys.KeyB;

    if (!paused) {
      fm.update(dt);
      if (fm.crashed) {
        showMessage('CRASHED — RESETTING', 2200);
        sound.crash();
        sound.speak('Aircraft destroyed.', 500);
        fm.reset();
      }
    }

    let threatDist = 0;
    if (!paused && !fm.onGround) {
      if (fm.speed() > 45) threatDist = terrainThreat();
      if (threatDist > 0) {
        if (!sound.playOnce('pullup', 4500, 0.9)) sound.speak('Terrain! Terrain! Pull up!', 4500);
      }
      if (fm.speed() < fm.spec.stall + 3) {
        if (!sound.playOnce('stall', 6000, 0.9)) sound.speak('Stall!', 3500);
      }
    }

    sound.updateEngine(AIRFRAMES[activeIdx].key, fm.throttle, fm.speed());

    for (const a of AIRFRAMES) {
      a.model.group.position.set(fm.pos.x, fm.pos.y, fm.pos.z);
      a.model.group.rotation.set(fm.pitch, fm.yaw, fm.roll);
      a.model.gearGroup.scale.y = Math.max(0.04, fm.gear);
    }
    const active = AIRFRAMES[activeIdx];
    if (active.model.prop) {
      active.model.prop.rotation.z += (2 + fm.throttle * 70) * dt;
    }
    if (active.model.burner) {
      const ab = fm.throttle > 0.6 && !fm.crashed;
      active.model.burner.visible = ab;
      if (ab) {
        const flicker = 1 + 0.25 * Math.sin(performance.now() * 0.045);
        const power = (fm.throttle - 0.6) / 0.4;
        active.model.burner.scale.set(0.7 + power * 0.5, 0.7 + power * 0.5, (0.4 + power) * flicker);
      }
    }

    water.position.x = fm.pos.x;
    water.position.z = fm.pos.z;
    terrain.update(fm.pos.x, fm.pos.z);

    updateCamera(dt);
    updateHud();
    renderer.render(scene, camera);
  }

  renderer.setAnimationLoop(update);
})();
