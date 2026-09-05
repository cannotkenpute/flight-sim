(() => {
  'use strict';

  const WATER = -10;
  const RUNWAY_Z0 = -280;
  const RUNWAY_Z1 = 280;

  const grad3 = [[1, 1], [-1, 1], [1, -1], [-1, -1], [1, 0], [-1, 0], [0, 1], [0, -1]];
  const perm = new Uint8Array(512);
  (function seed(s) {
    const p = new Uint8Array(256);
    for (let i = 0; i < 256; i++) p[i] = i;
    let n = s >>> 0;
    const rnd = () => ((n = (n * 1664525 + 1013904223) >>> 0) / 4294967296);
    for (let i = 255; i > 0; i--) {
      const j = (rnd() * (i + 1)) | 0;
      const t = p[i];
      p[i] = p[j];
      p[j] = t;
    }
    for (let i = 0; i < 512; i++) perm[i] = p[i & 255];
  })(1337);

  const F2 = 0.5 * (Math.sqrt(3) - 1);
  const G2 = (3 - Math.sqrt(3)) / 6;

  function simplex2(xin, yin) {
    let n0 = 0, n1 = 0, n2 = 0;
    const s = (xin + yin) * F2;
    const i = Math.floor(xin + s);
    const j = Math.floor(yin + s);
    const t = (i + j) * G2;
    const x0 = xin - (i - t);
    const y0 = yin - (j - t);
    let i1, j1;
    if (x0 > y0) { i1 = 1; j1 = 0; } else { i1 = 0; j1 = 1; }
    const x1 = x0 - i1 + G2;
    const y1 = y0 - j1 + G2;
    const x2 = x0 - 1 + 2 * G2;
    const y2 = y0 - 1 + 2 * G2;
    const ii = i & 255;
    const jj = j & 255;
    let t0 = 0.5 - x0 * x0 - y0 * y0;
    if (t0 >= 0) {
      t0 *= t0;
      const g = grad3[perm[ii + perm[jj]] % 8];
      n0 = t0 * t0 * (g[0] * x0 + g[1] * y0);
    }
    let t1 = 0.5 - x1 * x1 - y1 * y1;
    if (t1 >= 0) {
      t1 *= t1;
      const g = grad3[perm[ii + i1 + perm[jj + j1]] % 8];
      n1 = t1 * t1 * (g[0] * x1 + g[1] * y1);
    }
    let t2 = 0.5 - x2 * x2 - y2 * y2;
    if (t2 >= 0) {
      t2 *= t2;
      const g = grad3[perm[ii + 1 + perm[jj + 1]] % 8];
      n2 = t2 * t2 * (g[0] * x2 + g[1] * y2);
    }
    return 70 * (n0 + n1 + n2);
  }

  function clamp(v, a, b) {
    return v < a ? a : v > b ? b : v;
  }

  function smoothstep(e0, e1, x) {
    const t = clamp((x - e0) / (e1 - e0), 0, 1);
    return t * t * (3 - 2 * t);
  }

  function fbm(x, y, octaves) {
    let amp = 1, freq = 1, sum = 0, norm = 0;
    for (let i = 0; i < octaves; i++) {
      sum += amp * simplex2(x * freq, y * freq);
      norm += amp;
      amp *= 0.5;
      freq *= 2;
    }
    return sum / norm;
  }

  function ridged(x, y, octaves) {
    let amp = 0.5, freq = 1, sum = 0, norm = 0;
    for (let i = 0; i < octaves; i++) {
      sum += amp * (1 - Math.abs(simplex2(x * freq, y * freq)));
      norm += amp;
      amp *= 0.5;
      freq *= 2.13;
    }
    return sum / norm;
  }

  function distToRunway(x, z) {
    const zc = clamp(z, RUNWAY_Z0, RUNWAY_Z1);
    return Math.hypot(x, z - zc);
  }

  function heightAt(x, z) {
    const base = fbm(x * 0.0006, z * 0.0006, 4) * 45;
    const m = fbm(x * 0.00013 + 37.7, z * 0.00013 - 11.2, 3) * 0.5 + 0.5;
    const mask = smoothstep(0.48, 0.72, m);
    const r = ridged(x * 0.00035 + 5.1, z * 0.00035 + 9.3, 4);
    let h = base + Math.pow(r, 2.1) * 640 * mask;
    h *= smoothstep(260, 960, distToRunway(x, z));
    return h;
  }

  globalThis.Heightfield = { heightAt, fbm, WATER, RUNWAY_Z0, RUNWAY_Z1 };
})();
