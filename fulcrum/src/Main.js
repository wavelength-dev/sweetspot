exports.exposeGlobals = function (fn) {
  return function () {
    console.log("SETTING FULCRUM GLOBAL");
    window.fulcrum = {};
    window.fulcrum.reapply = fn;
  };
};
