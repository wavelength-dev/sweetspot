let runQueue = null;

exports.getRunQueue = function() {
  return runQueue;
};

exports.setRunQueue = function(initialRunQueue) {
  return function() {
    runQueue = initialRunQueue;
  };
};

let isRunning = false;

exports.getIsRunning = function() {
  return isRunning;
};
exports.setIsRunning = function(nextIsRunning) {
  return function() {
    isRunning = nextIsRunning;
  };
};
