INSERT
      INTO buckets
            (bucket_type, original_svid, test_svid, price, original_price)
      VALUES
            ('control', {ORIGINAL_VARIANT_ID}, {ORIGINAL_VARIANT_ID}, {ORIGINAL_VARIANT_PRICE}, {ORIGINAL_VARIANT_PRICE}),
            ('test', {ORIGINAL_VARIANT_ID}, {TEST_VARIANT_ID}, {TEST_VARIANT_PRICE}, {ORIGINAL_VARIANT_PRICE});
INSERT INTO experiments
            (sku, product_name)
      VALUES
            ({SKU}, {PRODUCT_NAME});
INSERT INTO campaigns
            (campaign_id, name, min_profit_increase, start_date, end_date)
      VALUES
            ({CAMPAIGN_ID}, {EXP_ID}, {NAME}, {MIN_PROFIT_INCREASE}, NOW(), NOW() + interval '30 days');
INSERT INTO campaign_experiments
      VALUES
            ({CAMPAIGN_ID}, {EXP_ID});
INSERT INTO experiment_buckets
      VALUES
            (1,1) (1,2);
