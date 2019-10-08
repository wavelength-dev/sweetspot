SELECT
  c.campaign_id as _dcsCampaignId,
  c.name as _dcsCampaignName,
  c.min_profit_increase as _dcsMinProfitIncrease,
  c.start_date as _dcsStartDate,
  c.end_date as _dcsEndDate,
  (
    SELECT coalesce(json_agg(json_build_object(
      '_desExpId', e.exp_id,
      '_desProductName', e.product_name,
      '_desBuckets', (
        SELECT coalesce(json_agg(json_build_object(
          '_dbsBucketId', b.bucket_id,
          '_dbsBucketType', b.bucket_type,
          '_dbsOriginalSvid', b.original_svid,
          '_dbsTestSvid', b.test_svid,
          '_dbsPrice', b.price,
          '_dbsUserCount', (
              SELECT count(DISTINCT user_id)
              FROM bucket_users bu
              WHERE bu.bucket_id = b.bucket_id
          ),
          '_dbsCheckoutEvents', (
              SELECT coalesce(json_agg(json_build_object(
                '_chkId', e.id,
                '_chkUserId', bu.user_id,
                '_chkBucketId', bu.bucket_id,
                '_chkOrderId', e.payload->'orderId',
                '_chkTimestamp', e.timestamp,
                '_chkLineItems', e.payload->'lineItems'
                )), '[]'::json)
                FROM events e
                INNER JOIN bucket_users bu ON bu.bucket_id = b.bucket_id
                WHERE e.type = 'checkout'
          )
        )), '[]'::json)
          FROM experiment_buckets eb
          INNER JOIN buckets b ON b.bucket_id = eb.bucket_id
          WHERE eb.exp_id = e.exp_id
        )
    )), '[]'::json)
    FROM experiments e
    INNER JOIN campaign_experiments ce ON ce.campaign_id = c.campaign_id
  ) as _dcsExperiments
  FROM campaigns c
  WHERE c.campaign_id = 'longv123'
