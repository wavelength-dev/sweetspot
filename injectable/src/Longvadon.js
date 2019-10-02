exports.setCachedButtonHtml = function(element) {
  return function(rawHtml) {
    $(element).data("cartbtn", rawHtml);
  };
};
