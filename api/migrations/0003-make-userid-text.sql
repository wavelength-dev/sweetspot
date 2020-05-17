alter table user_cart_tokens
  drop constraint user_cart_tokens_user_id_fkey;
alter table user_cart_tokens
  alter column user_id type text;

alter table user_experiments
  drop constraint user_experiments_user_id_fkey;
alter table user_experiments
  alter column user_id type text;

alter table checkout_events
  drop constraint checkout_events_user_id_fkey;
alter table checkout_events
  alter column user_id type text;

alter table users alter column id type text;

alter table user_cart_tokens
  add constraint user_cart_tokens_user_id_fkey foreign key (user_id) references users(id);

alter table user_experiments
  add constraint user_experiments_user_id_fkey foreign key (user_id) references users(id);

alter table checkout_events
  add constraint checkout_events_user_id_fkey foreign key (user_id) references users(id);
