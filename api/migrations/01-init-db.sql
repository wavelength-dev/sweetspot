CREATE TABLE users
(user_id SERIAL PRIMARY KEY);

CREATE TABLE experiment_groups
(exp_group_id SERIAL PRIMARY KEY);

CREATE TABLE experiments
(exp_id SERIAL PRIMARY KEY,
 sku TEXT,
 name TEXT);

CREATE TABLE buckets
(bucket_id SERIAL PRIMARY KEY,
 svid BIGINT,
 sku TEXT,
 price NUMERIC(12, 2));

CREATE TABLE bucket_users
(bucket_id SERIAL,
 user_id SERIAL,
 FOREIGN KEY (bucket_id) REFERENCES buckets(bucket_id),
 FOREIGN KEY (user_id) REFERENCES users(user_id));

CREATE TABLE experiment_group_users
(exp_group_id SERIAL,
 user_id SERIAL,
 FOREIGN KEY (exp_group_id) REFERENCES experiment_groups(exp_group_id),
 FOREIGN KEY (user_id) REFERENCES users(user_id));

CREATE TABLE experiment_group_experiments
(exp_group_id SERIAL,
 exp_id SERIAL,
 FOREIGN KEY (exp_group_id) REFERENCES experiment_groups(exp_group_id),
 FOREIGN KEY (exp_id) REFERENCES experiments(exp_id));

CREATE TABLE experiment_buckets
(exp_id SERIAL,
 bucket_id SERIAL,
 FOREIGN KEY (exp_id) REFERENCES experiments(exp_id),
 FOREIGN KEY (bucket_id) REFERENCES buckets(bucket_id));

CREATE TABLE events
(id SERIAL,
 type TEXT,
 timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
 payload JSONB);

INSERT INTO users (user_id) VALUES (DEFAULT);
INSERT INTO experiment_groups (exp_group_id) VALUES (DEFAULT);
INSERT INTO experiments (exp_id, sku, name) VALUES (DEFAULT, '3', 'Kanpeki uber knife experiment');
INSERT INTO buckets (bucket_id, svid, sku, price) VALUES (DEFAULT, 18765024952384, '3', 19.90);
INSERT INTO buckets (bucket_id, svid, sku, price) VALUES (DEFAULT, 18764920946752, '3', 29.90);
INSERT INTO experiment_group_users (exp_group_id, user_id) VALUES (1, 1);
INSERT INTO experiment_group_experiments (exp_group_id, exp_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 2);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1, 1);
