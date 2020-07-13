exports.exposeGlobals = function (fn) {
  return function () {
    console.log("SETTING FULCRUM GLOBAL");
    window.sweetspot = {};
    window.sweetspot.reapply = fn;
  };
};
