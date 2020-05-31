create table app_charges
  (
    id uuid primary key,
    status text not null,
    shopify_charge_id text not null,
    shop_id uuid references shops(id),
    name text not null,
    price text not null,
    return_url text not null,
    confirmation_url text not null
  );
