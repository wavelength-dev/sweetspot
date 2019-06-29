"use strict";

exports.formatNumber = function (number) {
    return function (numberFormat) {
        return function () {
            return numberFormat.format(number)
        }
    }
}

exports.numberFormatImpl = function (locale) {
    return function (options) {
        return function () {
            return new Intl.NumberFormat(locale, options);
        }
    }
}
