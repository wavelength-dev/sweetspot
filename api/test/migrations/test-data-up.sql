-- shops
INSERT INTO shops (shop_id, created, shop_domain, oauth_token)
VALUES ('61fbb484-59cf-45b2-b394-bbe830c95f75', now(), 'test-shop.myshopify.com', 'oauthtokenlol');

-- campaigns
INSERT INTO campaigns (campaign_id, shop_id, campaign_name, start_date, end_date)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a991', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Test Campaign', now()::date - 1, now()::date + 7);

INSERT INTO campaigns (campaign_id, shop_id, campaign_name, start_date, end_date)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a992', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Expired Campaign', now()::date - 100, now()::date - 1);

INSERT INTO campaigns (campaign_id, shop_id, campaign_name, start_date, end_date)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a993', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Not Yet Active Campaign', now()::date + 5, now()::date + 100);

-- product_variants
INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e761', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Black watchband', 'black_wb_sku', '123', '321', 79.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e762', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Black watchband', 'black_wb_sku', '234', '432', 89.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e763', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Brown watchband', 'brown_wb_sku', '345', '543', 79.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e764', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Brown watchband', 'brown_wb_sku', '456', '654', 89.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e765', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Blue watchband', 'blue_wb_sku', '567', '765', 79.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e766', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Blue watchband', 'blue_wb_sku', '678', '876', 89.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e767', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Blue iPad Case', 'blue_ic_sku', '789', '987', 100.90, 'USD');

INSERT INTO product_variants (product_variant_id, shop_id, title, sku, shopify_product_id, shopify_variant_id, price, currency)
VALUES ('01a972a9-b9c0-47b3-81af-4c6c8e68e768', '61fbb484-59cf-45b2-b394-bbe830c95f75', 'Blue iPad Case', 'blue_ic_sku', '899', '998', 110.90, 'USD');

-- treatments
INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a991', 0, '01a972a9-b9c0-47b3-81af-4c6c8e68e761');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a991', 1, '01a972a9-b9c0-47b3-81af-4c6c8e68e762');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a992', 0, '01a972a9-b9c0-47b3-81af-4c6c8e68e763');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a992', 1, '01a972a9-b9c0-47b3-81af-4c6c8e68e764');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a993', 0, '01a972a9-b9c0-47b3-81af-4c6c8e68e765');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a993', 1, '01a972a9-b9c0-47b3-81af-4c6c8e68e766');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a993', 0, '01a972a9-b9c0-47b3-81af-4c6c8e68e767');

INSERT INTO treatments (campaign_id, treatment, product_variant_id)
VALUES ('6072b6ea-7c37-4b26-80cd-f8f87d05a993', 1, '01a972a9-b9c0-47b3-81af-4c6c8e68e768');

-- users
INSERT INTO users (user_id, created) VALUES ('2eb6a046-6609-4518-ab23-87f1ad56bbaa', now());
INSERT INTO users (user_id, created) VALUES ('e3b937e7-ac65-4324-9d67-040cdc35b555', now());
INSERT INTO users (user_id, created) VALUES ('85271f15-683b-4972-bd68-b7aaacdeb70d', now());

-- user_experiments
-- test campaign
INSERT INTO user_experiments (user_id, campaign_id, treatment)
VALUES ('2eb6a046-6609-4518-ab23-87f1ad56bbaa', '6072b6ea-7c37-4b26-80cd-f8f87d05a991', 0);
-- expired campaign
INSERT INTO user_experiments (user_id, campaign_id, treatment)
VALUES ('e3b937e7-ac65-4324-9d67-040cdc35b555', '6072b6ea-7c37-4b26-80cd-f8f87d05a992', 1);
-- not yet active campaign
INSERT INTO user_experiments (user_id, campaign_id, treatment)
VALUES ('85271f15-683b-4972-bd68-b7aaacdeb70d', '6072b6ea-7c37-4b26-80cd-f8f87d05a993', 1);


INSERT INTO checkout_events (event_id, created, campaign_id, order_id, shop_id, user_id)
VALUES ('365b9947-a296-4d01-909c-63d9846b7aa1', now(), '6072b6ea-7c37-4b26-80cd-f8f87d05a991', '12345678',
        '61fbb484-59cf-45b2-b394-bbe830c95f75', '2eb6a046-6609-4518-ab23-87f1ad56bbaa');

INSERT INTO checkout_items (checkout_item_id, checkout_event_id, quantity, shopify_variant_id)
VALUES ('46e1cb23-e68b-4af6-8709-1034ebe43e73', '365b9947-a296-4d01-909c-63d9846b7aa1', 1, '321');
