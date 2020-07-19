exports.exposeGlobals = function (fn) {
  return function () {
    window.sweetspot = {};
    window.sweetspot.reapply = fn;
  };
};
