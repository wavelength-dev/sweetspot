import { eventsURL } from "./constants"
import { log } from "./logging"
import { isSome, map, none, Option, some } from "./option"
import { isElementsFound } from "./util"

// Shopify --
/* Price in local currency in cents */
type Price = number
type ProductId = number
type VariantId = number
type SKU = string
/* Variant as injected into Liquid templates by the shopify backend */
interface Variant {
  id: VariantId
  /* null for products with a single non-consumer facing variant? */
  sku: SKU
  price: Price
}
/* Product as injected into Liquid templates by the shopify backend */
interface Product {
  available: boolean
  id: ProductId
  price: Price
  price_min: Price
  price_max: Price
  price_varies: boolean
  compare_at_price: Price | null
  compare_at_price_min: 0 // number | 0 ??
  compare_at_price_max: 0 // number | 0 ??
  compare_at_price_varies: boolean
  variants: Variant[]
}
// --

// Events --
/* Page types recognized by supple injectable */
type Page = "product" | "collection" | "collections" | "checkout"
/* Location WebAPI pathname */
type Pathname = string
interface BaseViewEvent {
  expId: number | null
  page: Page
  pageUrl: string
  userId: string | null
}
interface CollectionListingsViewEvent extends BaseViewEvent {
  page: "collections"
}
interface ProductListingsViewEvent extends BaseViewEvent {
  page: "collection"
  productIds: number[] | null
}
interface ProductDetailsViewEvent extends BaseViewEvent {
  page: "product"
  productId: number | null
}
interface CheckoutViewEvent extends BaseViewEvent {
  page: "checkout"
}
interface UnknownViewEvent {
  expId: number | null
  page: "unknown"
  pageUrl: string
  userId: string | null
}
type ViewEvent =
  | CollectionListingsViewEvent
  | ProductDetailsViewEvent
  | ProductListingsViewEvent
  | CheckoutViewEvent
  | UnknownViewEvent

export interface LineItem {
  product_id: number
  variant_id: number
}
interface CheckoutEvent {
  page: "checkout"
  token: string | null
  step: "thank_you" | "payment_method" | null
  lineItems: LineItem[] | null
}
// --

const extractInjectedProductJSON = (el: Element): Option<Product> => {
  if (!(el instanceof HTMLDivElement)) {
    log({
      message: "Injected product JSON element of unexpected kind",
      severity: "warn",
    })
    return none
  }

  try {
    const product = JSON.parse(el.innerText) as Product
    return some(product)
  } catch (err) {
    log({ message: "Failed to parse injected product JSON", severity: "error" })
    return none
  }
}

const readInjectedProducts = (): Option<Product[]> => {
  const els = document.getElementsByClassName("supple__product")

  if (!isElementsFound(els)) {
    log({
      message: "Failed to read injected products from DOM",
      severity: "warn",
    })
    return none
  }

  return Array.from(els)
    .map(extractInjectedProductJSON)
    .reduce(
      (ps: Option<Product[]>, p: Option<Product>) =>
        isSome(ps)
          ? map<Product, Product[]>(p1 => [...ps.value, p1])(p)
          : map<Product, Product[]>(p1 => [p1])(p),
      none,
    )
}

const collectionURLRegExp = RegExp("^/collections/\\w+$")
const isCollectionURL = (path: Pathname): boolean =>
  collectionURLRegExp.test(path)

const collectionsURLRegExp = RegExp("^/collections$")
const isCollectionsURL = (path: Pathname): boolean =>
  collectionsURLRegExp.test(path)

const productDetailsURLRegExp = RegExp("^/collections/\\w+/products")
const isProductDetailsURL = (path: Pathname): boolean =>
  productDetailsURLRegExp.test(path)

const ordersURLRegExp = RegExp("^/\\w+/orders/")
const checkoutURLRegExp = RegExp("^/\\w+/checkouts/")
const isCheckoutURL = (path: Pathname): boolean =>
  checkoutURLRegExp.test(path) || ordersURLRegExp.test(path)

export const detectPage = (): Page | null => {
  const path = window.location.pathname as Pathname

  const page = isProductDetailsURL(path)
    ? "product"
    : isCollectionURL(path)
    ? "collection"
    : isCollectionsURL(path)
    ? "collections"
    : isCheckoutURL(path)
    ? "checkout"
    : null

  log({ message: `Detected page: ${page}`, severity: "info" })

  return page
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
      pair[1] || "",
    )
  }
  return queryParameters
}

export const detectCampaign = (): string | null => {
  const search = window.location.search
  const params = parseQuery(search)
  return params.campaign || null
}

const getViewMeta = (baseMeta: BaseViewEvent): ViewEvent => {
  const { page } = baseMeta

  switch (page) {
    case "collection": {
      const mProducts = readInjectedProducts()
      if (!isSome(mProducts)) {
        log({
          message:
            "Failed to read injected products when building collection view event, sending without product ids",
          severity: "error",
        })
        return {
          ...baseMeta,
          page: "collection",
          productIds: null,
        }
      }

      const products = mProducts.value
      const productIds = products.map(p => p.id)
      return {
        ...baseMeta,
        page: "collection",
        productIds,
      }
    }
    case "collections": {
      return {
        ...baseMeta,
        page: "collections",
      }
    }
    case "product": {
      const mProducts = readInjectedProducts()
      if (!isSome(mProducts)) {
        log({
          message:
            "Failed to read injected products when building product view event, sending without product id",
          severity: "error",
        })
        return {
          ...baseMeta,
          page: "product",
          productId: null,
        }
      }

      const products = mProducts.value
      const productId = products[0].id
      return {
        ...baseMeta,
        page: "product",
        productId,
      }
    }
    case "checkout": {
      return {
        ...baseMeta,
        page: "checkout",
      }
    }
  }
}

export const trackView = (
  expId: number | null,
  bucketId: number | null,
): void => {
  const userId = localStorage.getItem("supple_uid")
  const page = detectPage()

  const baseMeta = {
    bucketId,
    expId,
    pageUrl: window.location.href,
    userId,
  }

  if (page === null) {
    const event: UnknownViewEvent = { ...baseMeta, page: "unknown" }
    trackEvent(event)
    return
  }

  const viewMeta = getViewMeta({ ...baseMeta, page })
  trackEvent(viewMeta)
}

export const trackEvent = (event: ViewEvent | CheckoutEvent): void => {
  log({ message: "Tracking event" })
  log({ message: JSON.stringify(event) })
  fetch(eventsURL, {
    body: JSON.stringify(event),
    headers: {
      "Content-Type": "application/json",
    },
    method: "POST",
  }).catch(err => {
    log({ message: err, severity: "error" })
  })
}
