import { Client } from "pg";
import { addWeeks } from "date-fns";
import R from "ramda";

// const thing = {
//   id: 1857601339456,
//   title: "Stripy Pants",
//   handle: "stripy-pants",
//   description: "",
//   published_at: "2019-01-16T15:54:00-05:00",
//   created_at: "2018-12-16T07:59:58-05:00",
//   vendor: "libertyprice",
//   type: "",
//   tags: [],
//   price: 1800,
//   price_min: 1800,
//   price_max: 2000,
//   available: true,
//   price_varies: true,
//   compare_at_price: null,
//   compare_at_price_min: 0,
//   compare_at_price_max: 0,
//   compare_at_price_varies: false,
//   variants: [
//     {
//       id: 27231263785024,
//       title: "S",
//       option1: "S",
//       option2: null,
//       option3: null,
//       sku: "2",
//       requires_shipping: true,
//       taxable: true,
//       featured_image: null,
//       available: true,
//       name: "Stripy Pants - S",
//       public_title: "S",
//       options: ["S"],
//       price: 1800,
//       weight: 0,
//       compare_at_price: null,
//       inventory_management: "shopify",
//       barcode: ""
//     },
//     {
//       id: 28574371020864,
//       title: "M",
//       option1: "M",
//       option2: null,
//       option3: null,
//       sku: "3",
//       requires_shipping: true,
//       taxable: true,
//       featured_image: null,
//       available: true,
//       name: "Stripy Pants - M",
//       public_title: "M",
//       options: ["M"],
//       price: 2000,
//       weight: 0,
//       compare_at_price: null,
//       inventory_management: "shopify",
//       barcode: ""
//     }
//   ],
//   images: [
//     "//cdn.shopify.com/s/files/1/0119/3243/4496/products/nagy-arnold-654839-unsplash.jpg?v=1547671946"
//   ],
//   featured_image:
//     "//cdn.shopify.com/s/files/1/0119/3243/4496/products/nagy-arnold-654839-unsplash.jpg?v=1547671946",
//   options: ["Size"],
//   content: ""
// };
// const priceVariant = {
//   id: 4092397060160,
//   title: "Stripy Pants",
//   handle: "stripy-pants-ssv",
//   description: "",
//   published_at: "2019-01-16T15:54:00-05:00",
//   created_at: "2019-09-12T12:38:17-04:00",
//   vendor: "libertyprice",
//   type: "sweetspot-variant",
//   tags: [],
//   price: 8900,
//   price_min: 8900,
//   price_max: 8900,
//   available: true,
//   price_varies: false,
//   compare_at_price: null,
//   compare_at_price_min: 0,
//   compare_at_price_max: 0,
//   compare_at_price_varies: false,
//   variants: [
//     {
//       id: 30079118671936,
//       title: "S",
//       option1: "S",
//       option2: null,
//       option3: null,
//       sku: "2",
//       requires_shipping: true,
//       taxable: true,
//       featured_image: null,
//       available: true,
//       name: "Stripy Pants - S",
//       public_title: "S",
//       options: ["S"],
//       price: 8900,
//       weight: 0,
//       compare_at_price: null,
//       inventory_management: "shopify",
//       barcode: ""
//     },
//     {
//       id: 30079118704704,
//       title: "M",
//       option1: "M",
//       option2: null,
//       option3: null,
//       sku: "3",
//       requires_shipping: true,
//       taxable: true,
//       featured_image: null,
//       available: true,
//       name: "Stripy Pants - M",
//       public_title: "M",
//       options: ["M"],
//       price: 8900,
//       weight: 0,
//       compare_at_price: null,
//       inventory_management: "shopify",
//       barcode: ""
//     }
//   ],
//   images: [
//     "//cdn.shopify.com/s/files/1/0119/3243/4496/products/nagy-arnold-654839-unsplash_b88ff67f-1f26-471d-a1e7-55af8554d6b2.jpg?v=1568306297"
//   ],
//   featured_image:
//     "//cdn.shopify.com/s/files/1/0119/3243/4496/products/nagy-arnold-654839-unsplash_b88ff67f-1f26-471d-a1e7-55af8554d6b2.jpg?v=1568306297",
//   options: ["Size"],
//   content: ""
// };
const main = async () => {
  const client = new Client();
  await client.connect();
  // const res = await client.query("SELECT NOW()");
  const campaign = {
    id: "live-test",
    name: "a live sweetspot test",
    minProfitIncrease: 1000,
    startDate: new Date().toISOString(),
    endDate: addWeeks(new Date(), 2).toISOString()
  };
  const tests = [
    {
      sku: "2",
      originalVariantId: "27231263785024",
      testVariantId: "30079118671936",
      originalVariantPrice: 1800,
      testVariantPrice: 8900,
      productName: "Stripy Pants - S"
    },
    {
      sku: "3",
      originalVariantId: "28574371020864",
      testVariantId: "30079118704704",
      originalVariantPrice: 1800,
      testVariantPrice: 8900,
      productName: "Stripy Pants - M"
    }
  ];
  buildSql(campaign, tests);
  await client.end();
};
main();

interface Campaign {
  id: string;
  name: string;
  minProfitIncrease: number; // in USD cents
  startDate: string; // ISO8601
  endDate: string; // ISO8601
}

interface Test {
  sku: string;
  originalVariantId: string;
  testVariantId: string;
  originalVariantPrice: number; // in cents
  testVariantPrice: number; // in cents
  productName: string;
}

const buildSql = (campaign: Campaign, tests: Test[]) => {
  // Build experiments that will be associated with buckets and campaigns.
  const experimentValues = tests
    .map(test => `('${test.sku}', '${test.productName}')`)
    .join(",\n");
  const insertExperimentSql = `
  INSERT INTO experiments
      (sku, product_name)
    VALUES
      ${experimentValues}
    ;
  `;
  console.log("==Experiments", insertExperimentSql);

  const bucketValues = tests
    .map(
      ({
        originalVariantId,
        originalVariantPrice,
        testVariantId,
        testVariantPrice
      }) => [
        `('control', '${originalVariantId}', '${originalVariantId}', '${originalVariantPrice}', '${originalVariantPrice}')`,
        `('test', '${originalVariantId}', '${testVariantId}', '${testVariantPrice}', '${originalVariantPrice}')`
      ]
    )
    .reduce(R.concat, [])
    .join(",\n");
  const experiments: any[] = [];
  const experimentBucketRelationships = experiments
    .map((_, index) => [`(${index}, 1), (${index}, 2)`])
    .reduce(R.concat, [])
    .join(",\n");
  // const bucketExperimentRelationships
  const insertBucketsSql = `
  INSERT INTO buckets
    (bucket_type, original_svid, test_svid, price, original_price)
  VALUES
    ${bucketValues}
    ;
  INSERT INTO experiment_buckets
  VALUES
    ${experimentBucketRelationships}
  `;
  console.log("==Buckets", insertBucketsSql);

  const campaignExperimentRelationships = experiments
    .map(experiment => `('${campaign.id}', '${experiment.id}')`)
    .join(",\n");
  const insertCampaignSql = `
    INSERT INTO campaigns
      (campaign_id, name, min_profit_increase, start_date, end_date)
    VALUES
      ('${campaign.id}', '${campaign.name}', '${campaign.minProfitIncrease}', '${campaign.startDate}', '${campaign.endDate}');
    INSERT INTO campaign_experiments
      (campaign_id, exp_id)
    VALUES
      ${campaignExperimentRelationships}
      ;
    `;
  console.log("==Campaign", insertCampaignSql);
};
