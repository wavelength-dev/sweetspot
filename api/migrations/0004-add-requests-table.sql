create table action_requests
  (
    id uuid primary key,
    shop_id uuid references shops(id),
    request_type text not null,
    payload jsonb not null
  );
