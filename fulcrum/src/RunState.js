let runQueue = null;

exports.getRunQueue = function() {
  return runQueue;
}

exports.setRunQueue = function(initialRunQueue) {
  return function() { runQueue = initialRunQueue }
}
