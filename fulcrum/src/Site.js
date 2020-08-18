var isDebugging = false;
exports.getIsDebugSession = function () {
  return isDebugging;
};

exports.setIsDebugSession = function (nextIsDebugging) {
  return function () {
    isDebugging = nextIsDebugging;
  };
};
