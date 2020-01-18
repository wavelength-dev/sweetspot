```haskell
import Database.PostgreSQL.Simple
conn <- connect defaultConnectInfo { connectUser = "sweetspot", connectDatabase = "sweetspot" }
```
