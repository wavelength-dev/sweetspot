import { eventsUrl } from "./constants"
import { log } from "./logging"
import { isElementsFound, isEmpty } from "./util"

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
// --

const extractInjectedProductJSON = (el: Element): Product | null => {
  if (!(el instanceof HTMLDivElement)) {
    log("Injected product JSON element of unexpected kind", "warn")
    return null
  }

  try {
    const product = JSON.parse(el.innerText) as Product
    return product
  } catch (err) {
    log("Failed to parse injected product JSON", "error")
    return null
  }
}

const readInjectedProducts = (): Product[] | null => {
  const els = document.getElementsByClassName("supple__product")

  if (!isElementsFound(els)) {
    log("Failed to read injected products from DOM", "warn")
    return null
  }

  const mProducts = Array.from(els).map(extractInjectedProductJSON)
  const products = mProducts.reduce(
    (ps, p) => (p === null ? ps : [...ps, p]),
    [] as Product[],
  )

  return isEmpty(products) ? null : products
}

const collectionURLRegExp = RegExp("^/collections/w+$")
const isCollectionURL = (path: Pathname): boolean =>
  collectionURLRegExp.test(path)

const collectionsURLRegExp = RegExp("^/collections$")
const isCollectionsURL = (path: Pathname): boolean =>
  collectionsURLRegExp.test(path)

const productDetailsURLRegExp = RegExp("^/collections/w+/products")
const isProductDetailsURL = (path: Pathname): boolean =>
  productDetailsURLRegExp.test(path)

const checkoutURLRegExp = RegExp("^\/checkouts\/")
const isCheckoutURL = (path: Pathname): boolean =>
  checkoutURLRegExp.test(path)

export const detectPage = (): Page | null => {
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

  if (isCheckoutURL) {
    return "checkout"
  }

  return null
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
      const products = readInjectedProducts()
      if (products === null || isEmpty(products)) {
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
      const products = readInjectedProducts()
      if (products === null || isEmpty(products)) {
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

export const trackView = () => {
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

export const trackEvent = (event: ViewEvent) => {
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
