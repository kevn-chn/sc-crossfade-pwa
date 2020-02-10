import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    sc_api_client_id: process.env.ELM_APP_SC_API_CLIENT_ID,
    sc_app_version: process.env.ELM_APP_SC_APP_VERSION,
    sc_user_id: process.env.ELM_APP_SC_USER_ID,
  },
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

// Ports for Web Audio API playback
let audio = initAudio();
let prevAudio;

function initAudio() {
  const newAudio = new Audio();
  newAudio.controls = true;

  newAudio.onended = () => {
    app.ports.end.send(null);
  };

  return newAudio;
}

function initPlayback(src) {
  if (src !== audio.src) {
    audio.pause();
    audio.src = src;
  }

  const playback = audio.play();
  if (prevAudio) prevAudio.play();

  if (playback instanceof Promise) {
    playback
      .then(() => app.ports.playbackSuccess.send(null))
      // TODO: Add error handling
      .catch(() => app.ports.playbackError.send(null));
  }
}

function resetPrevAudio() {
  // Ensure that seek clears audio transition
  if (prevAudio) {
    prevAudio.pause();
    prevAudio = undefined;
    audio.volume = 1;
  }
}

app.ports.play.subscribe((src) => {
  resetPrevAudio();
  initPlayback(src);
});

app.ports.resume.subscribe(initPlayback);

app.ports.pause.subscribe(() => {
  audio.pause();
  if (prevAudio) prevAudio.pause();
});

app.ports.seek.subscribe((time) => {
  resetPrevAudio();
  audio.currentTime = time / 1000;
});

app.ports.fadeInNextTrack.subscribe((src) => {
  prevAudio = audio;
  prevAudio.onended = null;
  audio = initAudio();
  audio.volume = 0;

  initPlayback(src);
});

app.ports.volumeFade.subscribe((amount) => {
  if (!prevAudio || audio.volume >= 1) return;

  const volume = Math.min(1, audio.volume + amount);
  audio.volume = volume;
  prevAudio.volume = 1 - volume;
});
