// Import modules
var Elm = require('./elm/Main.elm');
require('./css/style.css');

// Set config
var config = {
  STORAGE: {
    TOKEN: 'token'
  }
};

// Set flags
var flags = {
  token: getItem(config.STORAGE.TOKEN)
};

// Run the application
var app = Elm.Main.fullscreen(flags);

// Set subscriptions
app.ports.syncToken.subscribe(setItem.bind(null, config.STORAGE.TOKEN));

function getItem(key) {
  return sessionStorage.getItem(key);
}

function setItem(key, value) {
  if (!value) {
    sessionStorage.removeItem(key);
  } else {
    sessionStorage.setItem(key, value);
  }
}