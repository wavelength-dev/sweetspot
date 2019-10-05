exports.setCachedButtonHtml = function(element) {
  return function(rawHtml) {
    return function() {
      $(element).data("cartbtn", rawHtml);
    }
  };
};
