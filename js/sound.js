(() => {
  'use strict';

  const SOURCES = {
    prop: 'sfx/prop-loop.mp3',
    jet: 'sfx/jet-loop.mp3',
    pullup: 'sfx/pull-up.mp3',
    stall: 'sfx/stall.mp3',
  };

  class SoundManager {
    constructor() {
      this.ctx = null;
      this.buffers = {};
      this.engines = {};
      this.playing = {};
      this.lastPlayed = {};
      this.muted = false;
      this.voice = null;
      this.lastSpoken = {};
      this.speechReady = 'speechSynthesis' in window;
      if (this.speechReady) {
        const pick = () => {
          const voices = window.speechSynthesis.getVoices();
          this.voice = voices.find((v) => /^en/i.test(v.lang)) || voices[0] || null;
        };
        pick();
        window.speechSynthesis.onvoiceschanged = pick;
      }
    }

    unlock() {
      if (!this.ctx) {
        const AC = window.AudioContext || window.webkitAudioContext;
        if (!AC) return;
        this.ctx = new AC();
        for (const key of Object.keys(SOURCES)) this.load(key, SOURCES[key]);
        this.startEngine('prop');
        this.startEngine('jet');
      }
      if (this.ctx.state === 'suspended') this.ctx.resume();
    }

    async load(key, url) {
      try {
        const res = await fetch(url);
        if (!res.ok) return;
        const buf = await res.arrayBuffer();
        this.buffers[key] = await this.ctx.decodeAudioData(buf);
      } catch (e) {
        this.buffers[key] = null;
      }
    }

    startEngine(key) {
      if (!this.ctx) return;
      const gain = this.ctx.createGain();
      gain.gain.value = 0;
      gain.connect(this.ctx.destination);
      this.engines[key] = { gain, source: null };
    }

    ensureSource(key) {
      const eng = this.engines[key];
      if (!eng || eng.source || !this.buffers[key]) return;
      const src = this.ctx.createBufferSource();
      src.buffer = this.buffers[key];
      src.loop = true;
      src.connect(eng.gain);
      src.start();
      eng.source = src;
    }

    updateEngine(activeKey, throttle, speed) {
      if (!this.ctx) return;
      for (const key of Object.keys(this.engines)) {
        this.ensureSource(key);
        const eng = this.engines[key];
        if (!eng.source) continue;
        let targetGain = 0;
        let rate = 1;
        if (key === activeKey && !this.muted) {
          if (key === 'prop') {
            targetGain = 0.12 + throttle * 0.55;
            rate = 0.85 + throttle * 0.55;
          } else {
            targetGain = 0.1 + throttle * 0.7;
            rate = 0.6 + throttle * 0.75 + Math.min(speed, 1100) * 0.00018;
          }
        }
        eng.gain.gain.setTargetAtTime(targetGain, this.ctx.currentTime, 0.12);
        eng.source.playbackRate.setTargetAtTime(rate, this.ctx.currentTime, 0.2);
      }
    }

    playOnce(key, minGapMs, volume = 1) {
      if (!this.ctx || this.muted) return true;
      if (!this.buffers[key]) return false;
      if (this.playing[key]) return true;
      const now = performance.now();
      if (minGapMs && now - (this.lastPlayed[key] || 0) < minGapMs) return true;
      this.lastPlayed[key] = now;
      const src = this.ctx.createBufferSource();
      src.buffer = this.buffers[key];
      const gain = this.ctx.createGain();
      gain.gain.value = volume;
      src.connect(gain).connect(this.ctx.destination);
      this.playing[key] = true;
      src.onended = () => { this.playing[key] = false; };
      src.start();
      return true;
    }

    crash() {
      if (!this.ctx || this.muted) return;
      const dur = 1.6;
      const buf = this.ctx.createBuffer(1, this.ctx.sampleRate * dur, this.ctx.sampleRate);
      const data = buf.getChannelData(0);
      for (let i = 0; i < data.length; i++) {
        const t = i / data.length;
        data[i] = (Math.random() * 2 - 1) * Math.pow(1 - t, 2.2);
      }
      const src = this.ctx.createBufferSource();
      src.buffer = buf;
      const filter = this.ctx.createBiquadFilter();
      filter.type = 'lowpass';
      filter.frequency.setValueAtTime(3000, this.ctx.currentTime);
      filter.frequency.exponentialRampToValueAtTime(120, this.ctx.currentTime + dur);
      const gain = this.ctx.createGain();
      gain.gain.value = 0.9;
      src.connect(filter).connect(gain).connect(this.ctx.destination);
      src.start();
    }

    speak(text, minGapMs) {
      if (!this.speechReady || this.muted) return;
      const now = performance.now();
      if (minGapMs && now - (this.lastSpoken[text] || 0) < minGapMs) return;
      this.lastSpoken[text] = now;
      window.speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(text);
      if (this.voice) u.voice = this.voice;
      u.rate = 1.05;
      u.pitch = 0.85;
      u.volume = 0.95;
      window.speechSynthesis.speak(u);
    }

    toggleMute() {
      this.muted = !this.muted;
      if (this.muted && this.speechReady) window.speechSynthesis.cancel();
      return this.muted;
    }
  }

  globalThis.SoundManager = SoundManager;
})();
