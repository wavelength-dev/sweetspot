create table stats_cache
  (
    id uuid primary key,
    campaign_id uuid references campaigns(id) unique,
    payload jsonb not null
  );
