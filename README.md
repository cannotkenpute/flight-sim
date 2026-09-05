# Flight Sim — Datom.World PostGraphics / WebGPU Port

A browser flight simulator ported from the original Three.js prototype to ClojureScript using Datom.World's PostGraphics renderer and `dao.gui.event` input pipeline.

The active simulator runtime does **not** depend on Three.js. Rendering is expressed as PostGraphics draw data and submitted through Datom.World's WebGPU terminal. The old root-level `js/` directory is retained only as legacy/reference source and is excluded from validated release artifacts.

## Validated status

The `datom-webgpu-port` branch is continuously validated in GitHub Actions against the exact checked-in simulator entrypoint.

Current release gate:

- 12 ClojureScript tests
- 285 assertions
- 0 failures / 0 errors
- 0 compiler warnings
- optimized Shadow-CLJS browser release succeeds
- Chromium reports `navigator.gpu === true`
- simulator reports the active renderer as `WEBGPU`
- PostGraphics canvas mounts without a frame rejection
- a browser `C` key event passes through `dao.gui.event` and changes `CHASE CAM` to `COCKPIT CAM`
- synthetic GLB test validates accessor decoding, node transforms, and GLB-to-PostGraphics mesh conversion

## Stack

- Clojure / ClojureScript
- Shadow-CLJS 3.3.5
- Reagent 2.0.1
- React 17
- Datom.World pinned to git SHA `b3a4be7bf2197dac3de380e76620fe8c60fccbfe`
- `dao.postgraphics` / PostGraphics WebGPU terminal
- `dao.gui.event` for normalized input events

## Requirements

- Java 21
- Clojure CLI
- Node.js 22+
- npm
- a browser with WebGPU support for hardware rendering

The simulator also contains a software/fallback path, but CI requires the WebGPU path itself to become active during the runtime smoke test.

## Run locally

Install Java, the Clojure CLI, and Node.js first. Then from the project root:

```bash
npm install
npm run dev
```

Open:

```text
http://localhost:8080
```

## Tests

```bash
npm run test:cljs
```

The test suite covers the deterministic heightfield, flight model behavior, geometry generation, scene data, and the custom static GLB importer.

## Production build

```bash
npm run build
```

Shadow-CLJS writes the optimized browser output into `public/js/`.

To serve the production output locally, use any static HTTP server with `public/` as its document root.

## Architecture

### Simulation core

Portable simulation logic lives under `src/cljc/flight_sim/`. It includes the flight model, deterministic terrain/heightfield functions, vector math, and geometry helpers. Keeping these namespaces portable makes the core straightforward to exercise from the Node-based ClojureScript test target.

### Browser runtime

Browser-specific namespaces live under `src/cljs/flight_sim/`.

The main runtime coordinates:

- a fixed-step flight simulation loop
- terrain streaming and runway geometry
- procedural aircraft and scenery meshes
- camera modes and HUD state
- radar rendering
- Web Audio engine / alert sounds
- GLB loading for static aircraft assets
- Datom.World PostGraphics frame submission
- normalized keyboard input through `dao.gui.event`

### Rendering

Scene data is converted into PostGraphics `:draw3d/mesh` operations rather than Three.js scene objects. Datom.World owns the browser terminal and WebGPU submission path.

### Input

Native browser keyboard packets are wrapped in Datom terminal envelopes, processed through `dao.gui.event`, then delivered to the simulator as normalized keyboard events. CI exercises this path end-to-end instead of testing only the key-handling function in isolation.

### GLB assets

`flight-sim.gltf` implements the static subset of GLB 2.0 required by the simulator: standard buffer views/accessors, triangle primitives, node TRS/matrices, normals/UVs, indices, and PBR base-color factors. It converts those assets directly into PostGraphics-compatible mesh data without a Three.js loader.

## Important directories

```text
src/cljc/flight_sim/   portable simulation + geometry
src/cljs/flight_sim/   browser runtime + PostGraphics integration
test/                  ClojureScript tests
public/                browser document root and assets
.github/workflows/     compile, test, WebGPU runtime, release packaging
js/                    legacy Three.js prototype only
```

## Release artifact

Every successful CI run packages a clean `flight-sim-datom-webgpu-release.zip`. The artifact excludes the legacy Three.js runtime and includes the optimized Shadow-CLJS browser bundle, source, tests, configuration, assets, a validation summary, and SHA-256 checksum.

## Branch status

This port is isolated on `datom-webgpu-port`. The repository's `main` branch is intentionally left unchanged until the port is reviewed and explicitly merged.
