CREATE TABLE users (user_id integer primary key);
CREATE TABLE buckets (variant_id integer primary key, sku text, price integer);
CREATE TABLE user_buckets (user_id integer, variant_id integer,
  foreign key (user_id) references users(user_id),
  foreign key (variant_id) references buckets(variant_id));
