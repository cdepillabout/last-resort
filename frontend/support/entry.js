const ClientEntry = require('../output/Client');

const initialState = ClientEntry.readState(window.__puxInitialState);

// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot) {
  let initState = window.__puxLastState || initialState
  let app = ClientEntry.main(window.location.pathname)(initState)()

  // Hook for pux devtools
  window.__puxApp = app;

  app.state.subscribe((state) => {
   window.__puxLastState = state;
  });

  module.hot.accept();
} else {
  ClientEntry.main(window.location.pathname)(initialState)()
}
