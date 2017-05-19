const LastResort = require('../output/LastResort');

const initialState = LastResort.readState(window.__puxInitialState);

// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot) {
  let initState = window.__puxLastState || initialState
  let app = LastResort.main(window.location.pathname)(initState)()

  // Hook for pux devtools
  window.__puxApp = app;

  app.state.subscribe((state) => {
   window.__puxLastState = state;
  });

  module.hot.accept();
} else {
  LastResort.main(window.location.pathname)(initialState)()
}
