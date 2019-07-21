"use strict";

exports.formatNumber = function (number) {
    return function (numberFormat) {
        return function () {
            return numberFormat.format(number)
        }
    }
}

exports._numberFormat = function (locale) {
    return function (options) {
        return function () {
            return new Intl.NumberFormat(locale, options);
        }
    }
}
