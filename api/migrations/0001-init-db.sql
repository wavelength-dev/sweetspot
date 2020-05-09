create extension pgcrypto;

create table shops
  (
    id uuid primary key,
    created timestamptz not null,
    shop_domain text not null,
    oauth_token text not null unique
  );

create table install_nonces
  (
    -- This table is used before shop is created,
    -- which is why shop_domain doesn't reference shops
    shop_domain text primary key,
    nonce uuid not null
  );

create table users
  (
    id uuid primary key,
    created timestamptz not null
  );

create table campaigns
  (
    id uuid primary key,
    shop_id uuid references shops(id),
    campaign_name text not null,
    start_date timestamptz,
    end_date timestamptz
  );

create table product_variants
  (
    id uuid primary key,
    shop_id uuid references shops(id),
    title text not null,
    sku text not null,
    shopify_product_id text not null,
    shopify_variant_id text not null,
    price numeric(12, 2) not null,
    currency text not null
  );

create table treatments
  (
    campaign_id uuid references campaigns(id),
    treatment int not null,
    product_variant_id uuid references product_variants(id),
    primary key (campaign_id, product_variant_id)
  );

create table user_experiments
  (
    user_id uuid references users(id),
    campaign_id uuid references campaigns(id),
    treatment int not null,
    primary key (user_id, campaign_id)
  );

create table checkout_events
  (
    id uuid primary key,
    created timestamptz not null,
    campaign_id uuid references campaigns(id),
    order_id text not null,
    shop_id uuid references shops(id),
    user_id uuid references users(id)
  );

create table checkout_items
  (
    id uuid primary key,
    checkout_event_id uuid references checkout_events(id),
    quantity int not null,
    shopify_variant_id text not null
  );

create table events
  (
    id uuid primary key,
    payload jsonb not null
  );

create table sessions
  (
    id text primary key,
    shop_id uuid references shops(id)
  );

create table user_cart_tokens
  (
    cart_token text primary key,
    user_id uuid references users(id)
  );
