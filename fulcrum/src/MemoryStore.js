let memoryStore = {};

exports.get_ = function() {
  return memoryStore;
};

exports.set_ = function(memoryStore) {
  return function() {
    memoryStore = memoryStore;
  };
};
