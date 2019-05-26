# SweetSpot Install Steps
  1. Create app in Shopify partner dashboard
  2. Whitelist redirect url: $app-domain/api/oauth/redirect
  3. Make sure app proxy is correctly configured
  4. Navigate to https://monvadon.myshopify.com/admin/oauth/authorize?client_id={$CLIENT_ID}&scope=read_products,write_products,read_themes,write_themes,read_orders,read_inventory,read_analytics,read_checkouts&redirect_uri={$DOMAIN/api/oauth/redirect}&state=123
  5. Click install


## Enable App Proxy
  1. Go to partner dashboard and select the app
  2. Click the "Extensions" icon
  3. Select "Online store" tab, if not there, enable first via "Manage extension areas"
  4. Click "Manage app proxy" and enter the details
