# Run DB queries in REPL

```haskell
import Database.PostgreSQL.Simple
conn <- connect defaultConnectInfo { connectUser = "sweetspot", connectDatabase = "sweetspot" }
```

# Installing app on dev shop
 - Create ngrok tunnel to local api instance
 - Log in to Shopify partner dashboard, whitelist oauth redirect uri with newly created ngrok domain
 - Update `SHOPIFY_OAUTH_REDIRECT_URI` in `api/.env`
 - Navigate to `https://XXXXXXXX.ngrok.io/api/oauth/install?shop=libertyprice.myshopify.com&timestamp=12345&hmac=lol`
