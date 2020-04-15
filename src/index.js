import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    sc_api_client_id: process.env.ELM_APP_SC_API_CLIENT_ID,
    sc_app_version: process.env.ELM_APP_SC_APP_VERSION,
    sc_user_id: Number(localStorage.getItem('sc_user_id') || process.env.ELM_APP_SC_USER_ID),
  },
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();

app.ports.setDefaultUserId.subscribe((id) => {
  localStorage.setItem('sc_user_id', id);
});

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
  if (src && src !== audio.src) {
    pauseAudio();
    audio.src = src;
  }

  const playback = audio.play();
  if (prevAudio) prevAudio.play();

  if (playback instanceof Promise) {
    playback
      .then(() => {
        app.ports.playbackSuccess.send(null);

        if ('mediaSession' in window.navigator) {
          window.navigator.mediaSession.playbackState = 'playing';
        }
      })
      // TODO: Add error handling
      .catch(() => app.ports.playbackError.send(null));
  }
}

function pauseAudio() {
  audio.pause();
  if (prevAudio) prevAudio.pause();

  if ('mediaSession' in window.navigator) {
    window.navigator.mediaSession.playbackState = 'paused';
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

app.ports.setTrackMetadata.subscribe((metadata) => {
  const skipTime = 10;

  if ('mediaSession' in window.navigator) {
    window.navigator.mediaSession.metadata = new MediaMetadata(metadata);
    window.navigator.mediaSession.setActionHandler('play', () => {
      app.ports.mediaPlay.send(null);
    });
    window.navigator.mediaSession.setActionHandler('pause', () => {
      app.ports.mediaPause.send(null);
    });
    window.navigator.mediaSession.setActionHandler('seekbackward', () => {
      const time = Math.floor(Math.max(audio.currentTime - skipTime, 0) * 1000);
      app.ports.mediaSeekBackward.send(time);
    });
    window.navigator.mediaSession.setActionHandler('seekforward', () => {
      const time = Math.floor(Math.min(audio.currentTime + skipTime, audio.duration) * 1000);
      app.ports.mediaSeekForward.send(time);
    });
    window.navigator.mediaSession.setActionHandler('previoustrack', () => {
      app.ports.mediaPreviousTrack.send(null);
    });
    window.navigator.mediaSession.setActionHandler('nexttrack', () => {
      app.ports.mediaNextTrack.send(null);
    });
  }
});

app.ports.play.subscribe((src) => {
  resetPrevAudio();
  initPlayback(src);
});

app.ports.resume.subscribe(initPlayback);

app.ports.pause.subscribe(pauseAudio);

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
