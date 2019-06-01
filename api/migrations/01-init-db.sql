CREATE TABLE users
(user_id SERIAL PRIMARY KEY);

CREATE TABLE campaigns
(campaign_id TEXT PRIMARY KEY,
 name TEXT,
 min_profit_increase INTEGER,
 start_date TIMESTAMPTZ,
 end_date TIMESTAMPTZ);

CREATE TABLE experiments
(exp_id SERIAL PRIMARY KEY,
 sku TEXT,
 product_name TEXT);

CREATE TABLE buckets
(bucket_id SERIAL PRIMARY KEY,
 bucket_type TEXT NOT NULL,
 original_svid BIGINT,
 test_svid BIGINT,
 price NUMERIC(12, 2));

CREATE TABLE bucket_users
(bucket_id SERIAL REFERENCES buckets(bucket_id),
 user_id SERIAL REFERENCES users(user_id));

CREATE TABLE campaign_users
(campaign_id TEXT REFERENCES campaigns(campaign_id),
 user_id SERIAL REFERENCES users(user_id));

CREATE TABLE campaign_experiments
(campaign_id TEXT REFERENCES campaigns(campaign_id),
 exp_id SERIAL REFERENCES experiments(exp_id));

CREATE TABLE experiment_buckets
(exp_id SERIAL REFERENCES experiments(exp_id),
 bucket_id SERIAL REFERENCES buckets(bucket_id));

CREATE TABLE events
(id SERIAL PRIMARY KEY,
 type TEXT,
 timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
 payload JSONB);
