# Shopify Integration Steps
Steps to take manually to integrate supple with a shopify store.

## Add hide price class
Find product-price.liquid
Store SKU in variable
```
{%- assign supple_sku = product.first_available_variant.sku -%}
```
Update main price span to be
```
<span class="price-item price-item--regular supple__price--hidden supple__price_id--{{ supple_sku }}" data-regular-price>
```

## Add product information
Find product-price.liquid
Add code to inject product data
```
<div class="supple__product" style="display: none">{{ product | json }}</div>
```
