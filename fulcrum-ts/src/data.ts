// Stock keeping unit identifying the object to be shipped.
type Sku = string;

interface TestMap {
  sku: Sku;
  swapId: string;
  swapPrice: number;
  variantId: string;
  userId: string;
}
