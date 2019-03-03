import { eventsUrl } from "./constants"
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
  campaign: string | null
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
  campaign: string | null
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

interface LineItem {
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
    log("Injected product JSON element of unexpected kind", "warn")
    return none
  }

  try {
    const product = JSON.parse(el.innerText) as Product
    return some(product)
  } catch (err) {
    log("Failed to parse injected product JSON", "error")
    return none
  }
}

const readInjectedProducts = (): Option<Product[]> => {
  const els = document.getElementsByClassName("supple__product")

  if (!isElementsFound(els)) {
    log("Failed to read injected products from DOM", "warn")
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

  log(`Detected page: ${page}`, "info")

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
        log(
          "Failed to read injected products when building collection view event, sending without product ids",
          "error",
        )
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
        log(
          "Failed to read injected products when building product view event, sending without product id",
          "error",
        )
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

export const trackView = (): void => {
  const campaign = detectCampaign()
  const userId = localStorage.getItem("supple_uid")
  const page = detectPage()

  const baseMeta = {
    campaign,
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

/* ISO8601 DateTime string */
interface DateTime {
  kind: "datetime"
  value: string
}
declare global {
  interface Window {
    Shopify?: {
      /* Shopify checkout. Data relating to the order, cart, billing of the current checkout. */
      checkout?: {
        billing_address?: {}
        created_at?: DateTime
        credit_card?: {}
        currency?: string
        customer_id?: number
        customer_locale?: string
        discount?: null
        email?: string
        gift_cards?: unknown[]
        line_items?: LineItem[]
        location_id?: number | null
        order_id?: number
        payment_due?: string
        payment_url?: string
        phone?: unknown
        presentment_currency?: string
        requires_shipping: boolean
        reservation_time?: unknown
        reservation_time_left?: unknown
        shipping_address?: {}
        shipping_rate?: {}
        source_identifier?: unknown
        source_name?: string
        source_url?: unknown
        subtotal_price?: string
        tax_exempt?: false
        tax_lines?: unknown[]
        taxes_included: boolean
        token?: string
        total_price?: string
        total_tax?: string
        updated_at?: string,
      }
      /* Shopify Checkout State, status of Shopify's checkout process */
      Checkout?: { step?: "thank_you" | "payment_method"; token?: string },
    }
  }
}

export const trackEvent = (event: ViewEvent | CheckoutEvent): void => {
  log("Tracking event")
  log(JSON.stringify(event))
  fetch(eventsUrl, {
    body: JSON.stringify(event),
    headers: {
      "Content-Type": "application/json",
    },
    method: "POST",
  }).catch(err => {
    log(err, "error")
  })
}
