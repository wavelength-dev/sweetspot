"use strict";

exports.formatNumber = function(numberFormat) {
  return function(number) {
    return function() {
      return numberFormat.format(number);
    };
  };
};

exports._numberFormat = function(locale) {
  return function(options) {
    return function() {
      return new Intl.NumberFormat(locale, options);
    };
  };
};
