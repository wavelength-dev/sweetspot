# Fulcrum
Short guide on how to apply Fulcrum to a Shopify store.

## Identification
Fulcrum needs to be aware of where to apply its magic. For example, Fulcrum needs to know where to put a controlled price. For this we need the variantId we're price testing. We could look for elements with variantIds that are meant for us but this kind of fuzzy searching is slow. Looking up classes is extremely fast O(1). Thus, we tag elements fulcrum needs to act on with a fulcrum class. In order to know what it is we need to do to the element, we also add the action type. These are the currently supported classes:
* `sweetspot__price`, indicates the text content of this element should be a price. The same element should have a `data-sweetspot-id` to indicate what variant this price is from.
* `sweetspot__option`, indicates the `value` attribute on this element should be the variantId to add to cart.

## Acting
Fulcrum currently supports only one action: apply. Meaning Fulcrum will swap prices and checkout variantIds. This means dealing with reactive pages that introduce prices that weren't visible on initial render, or only get added on interaction requires fulcrum to re-run. For this Fulcrum exposes a function on the window `window.Fulcrum.apply`.
