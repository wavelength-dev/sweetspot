import { detectCampaign, detectPage, detectProduct, trackEvent } from "./events"
import { log } from "./logging"

console.time("supple_complete")
console.time("supple_dom_ready")

log("init")

interface ApiExperiment {
  readonly bucket_price: number
  readonly bucket_sku: string
  readonly bucket_svid: number
  readonly user_id: number
}

interface Experiment {
  price: number
  sku: string
  svid: number
}

const apiURL = "http://localhost/api/bucket"
const getExperiments = async (): Promise<ApiExperiment[]> => {
  const uid = localStorage.getItem("supple_uid")
  const qs = uid === "string" ? `?uid=${uid}` : ""
  log("fetching experiments")
  return fetch(`${apiURL}${qs}`).then(res => {
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
    cl.startsWith("supple__price_id--")
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
    document.getElementsByClassName("supple__price--hidden")
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
        "warn"
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

const getDOMContentLoaded = () =>
  new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", () => {
      console.timeEnd("supple_dom_ready")
      resolve()
    })
  })

trackEvent({
  campaign: detectCampaign(),
  kind: "view",
  page: detectPage(),
  productId: detectProduct(),
  userId: localStorage.getItem("supple_uid")
})

Promise.all([getDOMContentLoaded(), getExperiments()])
  .then(([_, apiExps]) => ({
    exps: apiExps.map(
      (exp: ApiExperiment): Experiment => ({
        price: exp.bucket_price,
        sku: exp.bucket_sku,
        svid: exp.bucket_svid
      })
    ),
    userId: apiExps[0].user_id
  }))
  .then(({ userId, exps }) => {
    // TODO: carefully consider when to set the userId
    localStorage.setItem("supple_uid", String(userId))
    applyExperiments(exps)
    log("success!")
    console.timeEnd("supple_complete")
  })
  .catch(err => {
    console.error("failed to apply experiments")
    throw err
  })
