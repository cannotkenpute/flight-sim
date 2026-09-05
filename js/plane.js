(() => {
  'use strict';

  function build() {
    const group = new THREE.Group();
    const materials = [];

    const mat = (color) => {
      const m = new THREE.MeshLambertMaterial({ color, flatShading: true });
      materials.push(m);
      return m;
    };

    const body = mat(0xd9dde3);
    const accent = mat(0xd0342c);
    const dark = mat(0x2b2f36);
    const glass = mat(0x5fb7e8);

    const fuselage = new THREE.Mesh(new THREE.CylinderGeometry(0.8, 1, 9, 10), body);
    fuselage.geometry.rotateX(-Math.PI / 2);
    fuselage.position.set(0, 0, -0.5);
    group.add(fuselage);

    const nose = new THREE.Mesh(new THREE.ConeGeometry(0.8, 2.2, 10), accent);
    nose.geometry.rotateX(-Math.PI / 2);
    nose.position.set(0, 0, -6.1);
    group.add(nose);

    const cockpit = new THREE.Mesh(new THREE.BoxGeometry(1.2, 0.9, 2.2), glass);
    cockpit.position.set(0, 0.85, -2.6);
    group.add(cockpit);

    const wing = new THREE.Mesh(new THREE.BoxGeometry(15, 0.25, 2.8), accent);
    wing.position.set(0, 0.35, -1);
    group.add(wing);

    const tailWing = new THREE.Mesh(new THREE.BoxGeometry(5.5, 0.2, 1.6), accent);
    tailWing.position.set(0, 0.3, 4);
    group.add(tailWing);

    const fin = new THREE.Mesh(new THREE.BoxGeometry(0.2, 2.2, 1.8), accent);
    fin.position.set(0, 1.3, 4.1);
    group.add(fin);

    const prop = new THREE.Group();
    const bladeGeo = new THREE.BoxGeometry(0.3, 3.6, 0.12);
    const blade1 = new THREE.Mesh(bladeGeo, dark);
    const blade2 = new THREE.Mesh(bladeGeo, dark);
    blade2.rotation.z = Math.PI / 2;
    prop.add(blade1, blade2);
    prop.position.set(0, 0, -7.35);
    group.add(prop);

    const gearGroup = new THREE.Group();
    const wheelGeo = new THREE.CylinderGeometry(0.35, 0.35, 0.3, 10);
    wheelGeo.rotateZ(Math.PI / 2);
    const strutGeo = new THREE.BoxGeometry(0.12, 0.8, 0.12);
    for (const sx of [-1.6, 1.6]) {
      const strut = new THREE.Mesh(strutGeo, dark);
      strut.position.set(sx, -0.85, -1.5);
      gearGroup.add(strut);
      const wheel = new THREE.Mesh(wheelGeo, dark);
      wheel.position.set(sx, -1.25, -1.5);
      gearGroup.add(wheel);
    }
    const tailStrut = new THREE.Mesh(strutGeo, dark);
    tailStrut.position.set(0, -0.6, 3.6);
    gearGroup.add(tailStrut);
    const tailWheel = new THREE.Mesh(wheelGeo, dark);
    tailWheel.position.set(0, -1, 3.6);
    gearGroup.add(tailWheel);
    group.add(gearGroup);

    return { group, prop, gearGroup, materials };
  }

  function wingGeometry(rootFront, rootBack, tipSpan, tipFront, tipBack, thickness) {
    const shape = new THREE.Shape();
    shape.moveTo(0, rootFront);
    shape.lineTo(tipSpan, tipFront);
    shape.lineTo(tipSpan, tipBack);
    shape.lineTo(0, rootBack);
    shape.closePath();
    const geo = new THREE.ExtrudeGeometry(shape, { depth: thickness, bevelEnabled: false });
    geo.rotateX(Math.PI / 2);
    return geo;
  }

  function buildJet() {
    const group = new THREE.Group();
    const materials = [];

    const mat = (color, opts) => {
      const m = new THREE.MeshLambertMaterial(Object.assign({ color, flatShading: true }, opts || {}));
      materials.push(m);
      return m;
    };

    const hull = mat(0x77808c);
    const dark = mat(0x3d434c);
    const accent = mat(0xc73b30);
    const glass = mat(0x6fc3e8);
    const wingMat = mat(0x6d7681, { side: THREE.DoubleSide });

    const fuselage = new THREE.Mesh(new THREE.CylinderGeometry(0.65, 0.9, 11, 10), hull);
    fuselage.geometry.rotateX(-Math.PI / 2);
    fuselage.position.set(0, 0, 0);
    group.add(fuselage);

    const nose = new THREE.Mesh(new THREE.ConeGeometry(0.65, 3.4, 10), hull);
    nose.geometry.rotateX(-Math.PI / 2);
    nose.position.set(0, 0, -7);
    group.add(nose);

    const canopy = new THREE.Mesh(new THREE.BoxGeometry(0.95, 0.75, 2.8), glass);
    canopy.position.set(0, 0.8, -3);
    group.add(canopy);

    const wingGeo = wingGeometry(-2.4, 1.4, 6.4, 2.2, 3.6, 0.22);
    const wingR = new THREE.Mesh(wingGeo, wingMat);
    wingR.position.set(0, 0.15, 0.4);
    group.add(wingR);
    const wingL = new THREE.Mesh(wingGeo, wingMat);
    wingL.position.set(0, 0.15, 0.4);
    wingL.scale.x = -1;
    group.add(wingL);

    const stabGeo = wingGeometry(-0.9, 1.0, 3.1, 0.9, 1.8, 0.16);
    const stabR = new THREE.Mesh(stabGeo, wingMat);
    stabR.position.set(0, 0.25, 4.2);
    group.add(stabR);
    const stabL = new THREE.Mesh(stabGeo, wingMat);
    stabL.position.set(0, 0.25, 4.2);
    stabL.scale.x = -1;
    group.add(stabL);

    const finGeo = new THREE.BoxGeometry(0.14, 1.9, 2.4);
    const finR = new THREE.Mesh(finGeo, accent);
    finR.position.set(1.05, 1.25, 4.6);
    finR.rotation.z = -0.32;
    group.add(finR);
    const finL = new THREE.Mesh(finGeo, accent);
    finL.position.set(-1.05, 1.25, 4.6);
    finL.rotation.z = 0.32;
    group.add(finL);

    const intakeGeo = new THREE.BoxGeometry(0.5, 0.7, 2.2);
    for (const sx of [-0.95, 0.95]) {
      const intake = new THREE.Mesh(intakeGeo, dark);
      intake.position.set(sx, -0.35, -2.2);
      group.add(intake);
    }

    const nozzle = new THREE.Mesh(new THREE.CylinderGeometry(0.5, 0.65, 1.4, 10), dark);
    nozzle.geometry.rotateX(Math.PI / 2);
    nozzle.position.set(0, 0, 5.9);
    group.add(nozzle);

    const burnerMat = new THREE.MeshBasicMaterial({ color: 0xff8a2a, transparent: true, opacity: 0.9 });
    materials.push(burnerMat);
    const burner = new THREE.Mesh(new THREE.ConeGeometry(0.42, 2.6, 10), burnerMat);
    burner.geometry.rotateX(Math.PI / 2);
    burner.position.set(0, 0, 7.8);
    burner.visible = false;
    group.add(burner);

    const gearGroup = new THREE.Group();
    const wheelGeo = new THREE.CylinderGeometry(0.32, 0.32, 0.28, 10);
    wheelGeo.rotateZ(Math.PI / 2);
    const strutGeo = new THREE.BoxGeometry(0.12, 0.8, 0.12);
    for (const sx of [-1.3, 1.3]) {
      const strut = new THREE.Mesh(strutGeo, dark);
      strut.position.set(sx, -0.8, 1.2);
      gearGroup.add(strut);
      const wheel = new THREE.Mesh(wheelGeo, dark);
      wheel.position.set(sx, -1.22, 1.2);
      gearGroup.add(wheel);
    }
    const noseStrut = new THREE.Mesh(strutGeo, dark);
    noseStrut.position.set(0, -0.8, -4);
    gearGroup.add(noseStrut);
    const noseWheel = new THREE.Mesh(wheelGeo, dark);
    noseWheel.position.set(0, -1.22, -4);
    gearGroup.add(noseWheel);
    group.add(gearGroup);

    return { group, prop: null, burner, gearGroup, materials };
  }

  globalThis.PlaneModel = { build, buildJet };
})();
