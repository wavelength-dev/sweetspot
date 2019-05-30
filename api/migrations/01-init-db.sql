CREATE TABLE users
(user_id SERIAL PRIMARY KEY);

CREATE TABLE experiment_groups
(exp_group_id SERIAL PRIMARY KEY);

CREATE TABLE experiments
(exp_id SERIAL PRIMARY KEY,
 campaign_id TEXT,
 sku TEXT,
 name TEXT,
 min_profit_increase INTEGER);

CREATE TABLE buckets
(bucket_id SERIAL PRIMARY KEY,
 bucket_type TEXT NOT NULL,
 svid BIGINT,
 sku TEXT,
 price NUMERIC(12, 2));

CREATE TABLE bucket_users
(bucket_id SERIAL REFERENCES buckets(bucket_id),
 user_id SERIAL REFERENCES users(user_id));

CREATE TABLE experiment_group_users
(exp_group_id SERIAL REFERENCES experiment_groups(exp_group_id),
 user_id SERIAL REFERENCES users(user_id));

CREATE TABLE experiment_group_experiments
(exp_group_id SERIAL REFERENCES experiment_groups(exp_group_id),
 exp_id SERIAL REFERENCES experiments(exp_id));

CREATE TABLE experiment_buckets
(exp_id SERIAL REFERENCES experiments(exp_id),
 bucket_id SERIAL REFERENCES buckets(bucket_id));

CREATE TABLE events
(id SERIAL PRIMARY KEY,
 type TEXT,
 timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
 payload JSONB);
