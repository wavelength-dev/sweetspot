import { experimentsUrl } from "./constants"
import { trackView } from "./events"
import { log } from "./logging"

console.time("supple_complete")
console.time("supple_dom_ready")

log("init")

interface ApiExperiment {
  readonly _ubPrice: number
  readonly _ubSku: string
  readonly _ubSvid: number
  readonly _ubUserId: number
  readonly _ubExpId: number
  readonly _ubBucketId: number
}

interface Experiment {
  price: number
  sku: string
  svid: number
  expId: number
  bucketId: number
}

// Assumes non-empty list
const getExperiments = async (): Promise<ApiExperiment[]> => {
  const uid = localStorage.getItem("supple_uid")
  const qs = typeof uid === "string" ? `?uid=${uid}` : ""
  log("fetching experiments")
  return fetch(`${experimentsUrl}${qs}`).then(res => {
    if (res.status !== 200) {
      throw new Error(`failed to fetch experiments, got ${res.status}`)
    }

    return res.json()
  })
}

const revealProductPrice = (el: Element, price: number | null) => {
  if (typeof price === "number") {
    log("loading local price")
    el.innerHTML = String(price)
    el.classList.add("supple__price--exp")
  } else {
    log("loading non-local price")
    el.classList.add("supple__price--no-exp")
  }

  log("revealing price")
  el.classList.remove("supple__price--hidden")
}

const setCheckoutSvid = (svid: number): void => {
  const el = document.getElementById("ProductSelect-product-template")
  if (el === null) {
    throw new Error("SUPPLE -- could not find product selector for checkout")
  }
  if (!(el instanceof HTMLSelectElement)) {
    throw new Error("SUPPLE -- expected element to be an HTMLOptionElement")
  }
  const option = el.children[0]
  if (!(option instanceof HTMLOptionElement)) {
    throw new Error("SUPPLE -- expected element to be an HTMLSelectElement")
  }
  option.value = String(svid)
}

const getIdFromPriceElement = (el: Element) => {
  const priceIdClass = Array.from(el.classList).find(cl =>
    cl.startsWith("supple__price_id--"),
  )
  if (priceIdClass === undefined) {
    return null
  }
  return priceIdClass.split("--")[1]
}

// TODO: unify detecting of compatibility and required targets.
// i.e. the function that detects a compatible page should return
// all functions required to to update, reveal and add-to-cart.
const getIsDebutCheckout = (): boolean => {
  const el = document.getElementById("ProductSelect-product-template")
  if (!(el instanceof HTMLSelectElement)) {
    log("not a debut checkout")
    return false
  }
  const option = el.children[0]
  if (!(option instanceof HTMLOptionElement)) {
    log("failed to descend what looks like a Debut checkout", "warn")
    return false
  }

  log("detected debut checkout")
  return true
}

const applyExperiments = (exps: Experiment[]): void => {
  const els = Array.from(
    document.getElementsByClassName("supple__price--hidden"),
  )

  els.forEach(el => {
    const id = getIdFromPriceElement(el)
    if (id === null) {
      log("SUPPLE -- hidden price with no id! Unhiding price as-is", "error")
      revealProductPrice(el, null)
      return
    }

    const exp = exps.find(e => e.sku === id)

    if (exp === undefined) {
      log(
        "SUPPLE -- price with no matching experiment! Unhiding price as-is",
        "warn",
      )
      revealProductPrice(el, null)
      return
    }

    // TODO: support more than one checkout (Debut)
    if (getIsDebutCheckout()) {
      setCheckoutSvid(exp.svid)
    }
    revealProductPrice(el, exp.price)
  })
}

// We expect this to always resolve
const getDOMContentLoaded = () =>
  new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", () => {
      console.timeEnd("supple_dom_ready")
      resolve()
    })
  })

// TODO: Browser Compatibility
// IE <9: DOMContentLoaded does not fire
// IE 9: Can't use console.time
// IE >=9: fetch and Promise polyfill present?

const DOMPromise = getDOMContentLoaded()
const expPromise = getExperiments()

Promise.all([DOMPromise, expPromise])
  .then(([_, apiExps]) => ({
    exps: (apiExps as ApiExperiment[]).map(
      (exp: ApiExperiment): Experiment => ({
        bucketId: exp._ubBucketId,
        expId: exp._ubExpId,
        price: exp._ubPrice,
        sku: exp._ubSku,
        svid: exp._ubSvid,
      }),
    ),
    userId: (apiExps as ApiExperiment[])[0]._ubUserId,
  }))
  .then(({ userId, exps }) => {
    // TODO: carefully consider when to set the userId
    localStorage.setItem("supple_uid", String(userId))
    applyExperiments(exps)

    // Assumes there's only one experiment running per user
    const exp: Experiment | undefined = exps[0]
    const expId: number | null = (exp && exp.expId) || null
    const bucketId: number | null = (exp && exp.bucketId) || null
    trackView(expId, bucketId)

    log("success!")
    console.timeEnd("supple_complete")
  })
  // Experiments failed to resolve, unhide base price
  .catch(err => {
    log("Experiments failed to resolve, unhiding price", "error")
    applyExperiments([])
    throw err
  })
