INSERT INTO users (user_id) VALUES (1000);
INSERT INTO users (user_id) VALUES (1001);
INSERT INTO users (user_id) VALUES (1002);

INSERT INTO campaigns (campaign_id, name, min_profit_increase, start_date, end_date) VALUES ('longv123', 'Longvadon initial test', 10, now()::date -1, now()::date + 7);
INSERT INTO campaigns (campaign_id, name, min_profit_increase, start_date, end_date)
VALUES ('expired_campaign', 'Some name', 10, now()::date -100, now()::date - 1);

INSERT INTO campaigns (campaign_id, name, min_profit_increase, start_date, end_date)
VALUES ('not_yet_active_campaign', 'Some other name', 10, now()::date +5, NULL);

INSERT INTO campaign_users (campaign_id, user_id) VALUES ('longv123', 1000);
INSERT INTO campaign_users (campaign_id, user_id) VALUES ('expired_campaign', 1001);
INSERT INTO campaign_users (campaign_id, user_id) VALUES ('not_yet_active_campaign', 1002);

INSERT INTO experiments (exp_id, sku, product_name) VALUES (1000, '714449933422', 'Black watchband');
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1001, '714449933422', 'Brown watchband');
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1002, '714449933422', 'Yellow watchband');

INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1000);
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1001);
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1002);
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('expired_campaign', 1001);
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('not_yet_active_campaign', 1002);

INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1000, 'control', 12502066561095, 12502066561095, 197.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1001, 'test', 12502066561095, 12502066561095, 249.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1002, 'control', 12502066561095, 12502066561095, 349.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1003, 'test', 12502066561095, 12502066561095, 399.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1004, 'control', 12502066561095, 12502066561095, 549.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price) VALUES (1005, 'test', 12502066561095, 12502066561095, 599.90);

INSERT INTO bucket_users (bucket_id, user_id) VALUES (1001, 1000);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1003, 1000);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1005, 1000);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1002, 1001);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1003, 1002);

INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1000, 1000);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1000, 1001);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1001, 1002);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1001, 1003);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1002, 1004);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1002, 1005);
