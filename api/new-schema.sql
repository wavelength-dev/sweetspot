CREATE TABLE "events" (
  "event" jsonb
);

CREATE TABLE "checkout_events" (
  "created" "timestamp with time zone" NOT NULL DEFAULT (now()),
  "campaign_id" uuid,
  "order_id" text,
  "shop_id" uuid NOT NULL,
  "user_id" uuid NOT NULL,
  "items" text[] NOT NULL
);

CREATE TABLE "shops" (
  "shop_id" uuid PRIMARY KEY,
  "created" "timestamp with time zone" DEFAULT (now()),
  "shopify_id" text NOT NULL,
  "name" text NOT NULL,
  "oauth_token" text NOT NULL,
  "client_token" text NOT NULL
);

CREATE TABLE "users" (
  "user_id" uuid PRIMARY KEY,
  "created" "timestamp with time zone" DEFAULT (now())
);

CREATE TABLE "campaigns" (
  "campaign_id" uuid PRIMARY KEY,
  "shop_id" uuid NOT NULL,
  "start" "timestamp with time zone" NOT NULL DEFAULT (now()),
  "end" "timestamp with time zone"
);

CREATE TABLE "product_variants" (
  "variant_id" uuid PRIMARY KEY,
  "shop_id" uuid,
  "title" text NOT NULL,
  "sku" text NOT NULL,
  "shopify_product_id" text NOT NULL,
  "shopify_variant_id" text UNIQUE NOT NULL,
  "price" text NOT NULL,
  "currency" text NOT NULL
);

CREATE TABLE "price_variants" (
  "campaign_id" uuid,
  "treatment" int,
  "product_variant_id" uuid,
  PRIMARY KEY ("campaign_id", "treatment")
);

CREATE TABLE "user_active_experiments" (
  "user_id" uuid UNIQUE NOT NULL,
  "treatment" int NOT NULL,
  "campaign_id" uuid
);

ALTER TABLE "checkout_events" ADD FOREIGN KEY ("campaign_id") REFERENCES "campaigns" ("campaign_id");

ALTER TABLE "checkout_events" ADD FOREIGN KEY ("shop_id") REFERENCES "shops" ("shop_id");

ALTER TABLE "checkout_events" ADD FOREIGN KEY ("user_id") REFERENCES "users" ("user_id");

ALTER TABLE "campaigns" ADD FOREIGN KEY ("shop_id") REFERENCES "shops" ("shop_id");

ALTER TABLE "product_variants" ADD FOREIGN KEY ("shop_id") REFERENCES "shops" ("shop_id");

ALTER TABLE "price_variants" ADD FOREIGN KEY ("campaign_id") REFERENCES "campaigns" ("campaign_id");

ALTER TABLE "price_variants" ADD FOREIGN KEY ("product_variant_id") REFERENCES "product_variants" ("variant_id");

ALTER TABLE "user_active_experiments" ADD FOREIGN KEY ("user_id") REFERENCES "users" ("user_id");

ALTER TABLE "user_active_experiments" ADD FOREIGN KEY ("campaign_id") REFERENCES "campaigns" ("campaign_id");

CREATE INDEX ON "user_active_experiments" ("user_id");

COMMENT ON COLUMN "checkout_events"."items" IS 'shopify variant ids as strings';

COMMENT ON COLUMN "shops"."shopify_id" IS 'The ID for the shop.';

COMMENT ON COLUMN "product_variants"."price" IS 'some amount in cents';

COMMENT ON COLUMN "product_variants"."currency" IS 'ISO4217 three letter currency code';

COMMENT ON COLUMN "user_active_experiments"."user_id" IS 'only one campaign per user, so only one row per user_id';
