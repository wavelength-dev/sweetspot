type Slug = string
/* ISO8601 datetime */
type DateTime = string
// TODO: before using double check API docs are wrong and this is a number
// https://help.shopify.com/en/api/reference/products/product-variant
/* Price in local currency in cents */
type Price = string
type ProductId = number
type VariantId = number
type URL = string
type SKU = string
interface TemplateVariant {
  id: VariantId
  /* null for products with a single non-consumer facing variant? */
  sku: SKU
  price: Price
}
interface TemplateProduct {
  available: boolean
  id: ProductId
  price: Price
  price_min: Price
  price_max: Price
  available: boolean
  price_varies: boolean
  compare_at_price: Price | null
  compare_at_price_min: 0 // number | 0 ??
  compare_at_price_max: 0 // number | 0 ??
  compare_at_price_varies: boolean
  variants: Variant[]
}

interface ViewEvent {
  campaign: string | null
  kind: "view"
  page: string
  userId: string | null
}
interface ProductListingsViewEvent extends ViewEvent {
  productIds: string[]
}
interface ProductDetailsViewEvent extends ViewEvent {
  productId: string
}
type SuppleEvent =
  | ProductDetailsViewEvent
  | ProductListingsViewEvent
  | ViewEvent

const eventsURL = "http://localhost/api/events"

const extractInjectedProductJSON = (el: Element): Product => {
  if (!(el instanceof HTMLDivElement)) {
    throw new Error(
      "SUPPLE -- injected product JSON element of unexpected kind"
    )
  }

  try {
    const product = JSON.parse(el.innerText) as Product
    return product
  } catch (err) {
    console.error("SUPPLE -- failed to parse injected product JSON")
    throw err
  }
}
const readInjectedProducts = (): Product[] => {
  const els = document.getElementsByClassName("supple__product")

  if (els.length < 1) {
    throw new Error("SUPPLE -- no injected product JSON found")
  }

  return Array.from(els).map(extractInjectedProductJSON)
}

// TODO: Use on product pages only
export const detectProduct = (): Product => {
  const products = readInjectedProducts()
  return products[0];
}

type Page = "product" | "collection" | "collections" | "unknown"

type Pathname = string

const collectionURLRegExp = RegExp("^/collections/w+$")
const isCollectionURL = (path: Pathname): boolean =>
  collectionURLRegExp.test(path)

const collectionsURLRegExp = RegExp("^/collections$")
const isCollectionsURL = (path: Pathname): boolean =>
  collectionsURLRegExp.test(path)

const productDetailsURLRegExp = RegExp("^/collections/w+/products")
const isProductDetailsURL = (path: Pathname): boolean =>
  productDetailsURLRegExp.test(path)

export const detectPage = (): Page => {
  const path = window.location.pathname as Pathname

  if (isProductDetailsURL(path)) {
    return "product"
  }

  if (isCollectionURL(path)) {
    return "collection"
  }

  if (isCollectionsURL(path)) {
    return "collections"
  }

  return "unknown"
}

interface QueryParameters {
  [key: string]: string
}

const parseQuery = (queryString: string): QueryParameters => {
  const queryParameters: QueryParameters = {}
  const cleanQuery =
    queryString[0] === "?" ? queryString.substr(1) : queryString
  const pairs = cleanQuery.split("&")
  for (const kvStr of pairs) {
    const pair = kvStr.split("=")
    queryParameters[decodeURIComponent(pair[0])] = decodeURIComponent(
      pair[1] || ""
    )
  }
  return queryParameters
}

export const detectCampaign = (): string | null => {
  const search = window.location.search
  const params = parseQuery(search)
  return params.campaign || null
}

export const trackEvent = (event: SuppleEvent) => {
  // console.log(`SUPPLE -- tracking event, kind: ${event.kind}`)
  console.log("SUPPLE -- tracking event", event)
  fetch(eventsURL, {
    body: JSON.stringify(event),
    headers: {
      "Content-Type": "application/json"
    },
    method: "POST"
  })
}
