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

-- longv123
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1000, '714449933422', 'Black watchband');
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1001, '714449933423', 'Brown watchband');
-- expired_campaign
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1002, '714449933424', 'Yellow watchband');
-- not_yet_active_campaign
INSERT INTO experiments (exp_id, sku, product_name) VALUES (1003, '714449933427', 'Blue iPad case');

-- longv123
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1000);
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('longv123', 1001);
-- expired_campaign
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('expired_campaign', 1002);
-- not_yet_active_campaign
INSERT INTO campaign_experiments (campaign_id, exp_id) VALUES ('not_yet_active_campaign', 1003);

-- longv123
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1000, 'control', 12502066561095, 12502066561095, 197.90, 100.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1001, 'test', 12502066561095, 12502066561096, 249.90, 159.23);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1002, 'control', 12502066561095, 12502066561097, 249.90, 159.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1003, 'test', 12502066561095, 12502066561098, 249.90, 159.90);
-- expired_campaign
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1004, 'control', 12502066561095, 12502066561099, 349.90, 229.90);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1005, 'test', 12502066561095, 12502066561010, 399.90, 249.90);
-- not_yet_active_campaign
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1006, 'control', 12502066561095, 12502066561011, 549.90, 300.00);
INSERT INTO buckets (bucket_id, bucket_type, original_svid, test_svid, price, cost) VALUES (1007, 'test', 12502066561095, 12502066561012, 599.90, 459.90);

-- longv123
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1001, 1000);
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1003, 1000);
-- expired_campaign
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1004, 1001);
-- not_yet_active_campaign
INSERT INTO bucket_users (bucket_id, user_id) VALUES (1006, 1002);

-- longv123
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1000, 1000);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1000, 1001);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1001, 1002);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1001, 1003);
-- expired_campaign
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1002, 1004);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1002, 1005);
-- not_yet_active_campaign
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1003, 1006);
INSERT INTO experiment_buckets (exp_id, bucket_id) VALUES (1003, 1007);
