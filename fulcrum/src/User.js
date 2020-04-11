exports.readTrekkieToken = function() {
  try {
    return window.user().traits().uniqToken;
  } catch (err) {
    return null;
  }
};
