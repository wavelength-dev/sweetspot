"use strict";

exports.checkoutA = typeof window.Shopify === 'object' &&
    typeof window.Shopify.Checkout === 'object'
    ? window.Shopify.Checkout
    : null

exports.checkoutB = typeof window.Shopify === 'object' &&
    typeof window.Shopify.checkout === 'object'
    ? window.Shopify.checkout
    : null
