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

INSERT INTO users (user_id) VALUES (DEFAULT);
INSERT INTO campaigns (campaign_id, name, min_profit_increase, start_date, end_date) VALUES ('longv123', 'Longvadon initial test', 10, now()::date -1, now()::date + 7);
INSERT INTO campaign_users (campaign_id, user_id) VALUES ('longv123', 1);
INSERT INTO experiments (exp_id, sku, product_name) VALUES (DEFAULT, '714449933422', 'Black watchband');
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (DEFAULT, 'control', 12502066561095, 12502066561095, 197.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (DEFAULT, 'test', 12502066561095, 12502066561095, 249.90);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (2, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 2);
