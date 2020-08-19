create table unaccounted_orders
  (
    id uuid primary key,
    shop_id uuid references shops(id),
    payload jsonb not null
  );
