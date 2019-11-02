Bits and pieces of knowledge we learned, no longer in the current code, but possibly relevant in the future.

## Identifying the visible product
Identifying which product a user is looking at can be done by having the Shopify template engine inject the product information into the HTML server-side.
```
<div class="sweetspot__product" style="display: none">{{ product | json }}</div>
```
