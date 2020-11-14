exports.parentRedirect = function(url) {
  return function() {
    window.top.location.href = url
  }
}
