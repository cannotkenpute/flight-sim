(() => {
  'use strict';

  const CHUNK = 512;
  const SEG = 32;
  const RADIUS = 5;
  const MAX_BUILD_PER_FRAME = 32;

  const BANDS = [
    { h: -7, c: [0.78, 0.72, 0.52] },
    { h: 8, c: [0.32, 0.51, 0.25] },
    { h: 90, c: [0.23, 0.38, 0.20] },
    { h: 190, c: [0.50, 0.47, 0.45] },
    { h: 300, c: [0.94, 0.95, 0.97] },
  ];

  function hashNoise(x, z) {
    const v = Math.sin(x * 12.9898 + z * 78.233) * 43758.5453;
    return v - Math.floor(v);
  }

  function colorFor(h, x, z, out) {
    let c;
    if (h <= BANDS[0].h) {
      c = BANDS[0].c;
    } else if (h >= BANDS[BANDS.length - 1].h) {
      c = BANDS[BANDS.length - 1].c;
    } else {
      let i = 0;
      while (h > BANDS[i + 1].h) i++;
      const a = BANDS[i];
      const b = BANDS[i + 1];
      const t = (h - a.h) / (b.h - a.h);
      c = [
        a.c[0] + (b.c[0] - a.c[0]) * t,
        a.c[1] + (b.c[1] - a.c[1]) * t,
        a.c[2] + (b.c[2] - a.c[2]) * t,
      ];
    }
    const v = (hashNoise(x, z) - 0.5) * 0.06;
    out[0] = c[0] + v;
    out[1] = c[1] + v;
    out[2] = c[2] + v;
  }

  class Terrain {
    constructor(scene, heightAt) {
      this.scene = scene;
      this.heightAt = heightAt;
      this.chunks = new Map();
      this.material = new THREE.MeshLambertMaterial({
        vertexColors: true,
        flatShading: true,
      });
      this.lastCenter = null;
      this._color = [0, 0, 0];
    }

    setWireframe(on) {
      this.material.wireframe = on;
    }

    buildChunk(cx, cz) {
      const geo = new THREE.PlaneGeometry(CHUNK, CHUNK, SEG, SEG);
      geo.rotateX(-Math.PI / 2);
      const pos = geo.attributes.position;
      const colors = new Float32Array(pos.count * 3);
      for (let i = 0; i < pos.count; i++) {
        const wx = cx * CHUNK + pos.getX(i);
        const wz = cz * CHUNK + pos.getZ(i);
        const h = this.heightAt(wx, wz);
        pos.setY(i, h);
        colorFor(h, wx, wz, this._color);
        colors[i * 3] = this._color[0];
        colors[i * 3 + 1] = this._color[1];
        colors[i * 3 + 2] = this._color[2];
      }
      geo.setAttribute('color', new THREE.BufferAttribute(colors, 3));
      geo.computeVertexNormals();
      const mesh = new THREE.Mesh(geo, this.material);
      mesh.position.set(cx * CHUNK, 0, cz * CHUNK);
      this.scene.add(mesh);
      this.chunks.set(cx + ',' + cz, mesh);
    }

    disposeChunk(key) {
      const mesh = this.chunks.get(key);
      if (!mesh) return;
      this.scene.remove(mesh);
      mesh.geometry.dispose();
      this.chunks.delete(key);
    }

    update(x, z) {
      const ccx = Math.round(x / CHUNK);
      const ccz = Math.round(z / CHUNK);
      const center = ccx + ',' + ccz;
      if (center === this.lastCenter) return;
      this.lastCenter = center;

      const wanted = new Set();
      for (let dx = -RADIUS; dx <= RADIUS; dx++) {
        for (let dz = -RADIUS; dz <= RADIUS; dz++) {
          wanted.add((ccx + dx) + ',' + (ccz + dz));
        }
      }

      for (const key of Array.from(this.chunks.keys())) {
        if (!wanted.has(key)) this.disposeChunk(key);
      }

      const missing = [];
      for (const key of wanted) {
        if (!this.chunks.has(key)) {
          const [kx, kz] = key.split(',').map(Number);
          missing.push({ key, kx, kz, d: Math.max(Math.abs(kx - ccx), Math.abs(kz - ccz)) });
        }
      }
      missing.sort((a, b) => a.d - b.d);
      for (let i = 0; i < Math.min(missing.length, MAX_BUILD_PER_FRAME); i++) {
        this.buildChunk(missing[i].kx, missing[i].kz);
      }
      if (missing.length > MAX_BUILD_PER_FRAME) this.lastCenter = null;
    }
  }

  globalThis.Terrain = Terrain;
})();
