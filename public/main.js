require.config({
  baseUrl: '/',
  shim: {
    elm: {
      exports: 'Elm'
    }
  }
});

require(['elm'], function (Elm) {
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
  var app = Elm.Main.init({ flags: flags });
  
  // Set subscriptions
  app.ports.syncUser.subscribe(setItem.bind(null, config.STORAGE.USER));
  
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
});