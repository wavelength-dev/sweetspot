INSERT INTO users (user_id) VALUES (DEFAULT);
INSERT INTO experiment_groups (exp_group_id) VALUES (DEFAULT);
INSERT INTO experiments (exp_id, sku, name, campaign_id, min_profit_increase) VALUES (DEFAULT, '714449933422', 'Kanpeki knife experiment', 'knp123', 10);
INSERT INTO buckets (bucket_id, bucket_type, svid, sku, price) VALUES (DEFAULT, 'control', 12502066561095, '714449933422', 197.90);
INSERT INTO buckets (bucket_id, bucket_type, svid, sku, price) VALUES (DEFAULT, 'test', 12502066561095, '714449933422', 249.90);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 1);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1, 2);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1, 1);
