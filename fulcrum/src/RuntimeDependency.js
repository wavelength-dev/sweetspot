exports.getIsFetchRunnable = function() {
  return typeof window !== "undefined" && typeof window.fetch !== "undefined";
};

exports.getIsPromiseRunnable = function() {
  return typeof window !== "undefined" && typeof window.Promise !== "undefined";
};
