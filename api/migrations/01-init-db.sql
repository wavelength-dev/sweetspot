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

INSERT INTO users (user_id) VALUES (DEFAULT);
INSERT INTO experiment_groups (exp_group_id) VALUES (DEFAULT);
INSERT INTO experiments (exp_id, sku, name, campaign_id, min_profit_increase) VALUES (DEFAULT, '3', 'Kanpeki uber knife experiment', 'knp123', 10);
INSERT INTO buckets (bucket_id, bucket_type, svid, sku, price) VALUES (DEFAULT, 'control', 18765024952384, '3', 19.90);
INSERT INTO buckets (bucket_id, bucket_type, svid, sku, price) VALUES (DEFAULT, 'test', 18764920946752, '3', 29.90);
INSERT INTO experiment_group_users (exp_group_id, user_id) VALUES (1, 1);
INSERT INTO experiment_group_experiments (exp_group_id, exp_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 2);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1, 1);
