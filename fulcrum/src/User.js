exports.readTrekkieToken = function() {
  try {
    return window.trekkie.user().traits().uniqToken;
  } catch (err) {
    return null;
  }
};
