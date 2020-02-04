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
let audio = new Audio();

audio.onended = () => {
  app.ports.end.send(null);
};

app.ports.play.subscribe((src) => {
  if (src !== audio.src) {
    audio.pause();
    audio.src = src;
  }

  audio.play();
});

app.ports.pause.subscribe(() => {
  audio.pause();
});

app.ports.seek.subscribe((time) => {
  audio.currentTime = time / 1000;
});
