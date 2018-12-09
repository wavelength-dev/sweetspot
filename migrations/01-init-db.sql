CREATE TABLE users (user_id integer primary key);
CREATE TABLE buckets (variant_id bigint primary key, sku text, price integer);
CREATE TABLE user_buckets (user_id integer, variant_id bigint,
  foreign key (user_id) references users(user_id),
  foreign key (variant_id) references buckets(variant_id));


INSERT INTO users (user_id) VALUES (123);
INSERT INTO buckets (variant_id, sku, price) VALUES (17043734724672, 'MEH5680S', 24);
INSERT INTO user_buckets (user_id, variant_id) VALUES (123, 17043734724672);
