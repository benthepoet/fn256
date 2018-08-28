// Import modules
var Elm = require('./elm/Main.elm');

// Set config
var config = {
  STORAGE: {
    USER: 'user'
  }
};

// Set flags
var flags = {
  user: getItem(config.STORAGE.USER)
};

// Run the application
var app = Elm.Main.init({ node: document.querySelector('main'), flags: flags });

// Set subscriptions
app.ports.getDocumentPosition.subscribe(getDocumentPosition);
app.ports.syncUser.subscribe(setItem.bind(null, config.STORAGE.USER));

function getDocumentPosition(vector) {
  var svg = document.querySelector('svg'),
    rect = svg.getBoundingClientRect(),
    position = [
      vector[0] - Math.round(rect.left), 
      vector[1] - Math.round(rect.top)
    ];

  app.ports.documentPosition.send(position);
}

function getItem(key) {
  var value = sessionStorage.getItem(key);
  return JSON.parse(value);
}

function setItem(key, value) {
  if (!value) {
    sessionStorage.removeItem(key);
  } else {
    sessionStorage.setItem(key, JSON.stringify(value));
  }
}