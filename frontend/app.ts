console.time("supple_complete")
console.time("supple_dom_ready")
console.log("SUPPLE -- init")

interface IApiExperiment {
  user_id: number
  bucket_sku: string
  bucket_svid: number
  bucket_price: number
}

interface IExperiment {
  price: number
  sku: string
  svid: number
}

const apiUrl = "https://7b7ba380.ngrok.io"
const queryString = {
  stringify: (kvs: { [key: string]: string | null }) =>
    Object.keys(kvs).reduce(
      (qs, k) =>
        kvs[k] == null
          ? qs
          : qs === ""
          ? `${k}=${kvs[k]}`
          : `${qs}&${k}=${kvs[k]}`,
      ""
    )
}

const getExperiments = (): Promise<IApiExperiment[]> => {
  const path = `/bucket/`
  const maybeUid = localStorage.getItem("supple_uid")
  const qs =
    maybeUid === "string" ? `?${queryString.stringify({ uid: maybeUid })}` : ""
  console.log("SUPPLE -- fetching experiments")
  return fetch(`${apiUrl}${path}${qs}`)
    .then(res => {
      if (res.status !== 200) {
        throw new Error(
          `SUPPLE -- bad response for experiment fetch, got ${
            res.status
          }, with body: ${res.body}`
        )
      }

      if (res.body == null) {
        throw new Error("SUPPLE -- response body for experiment fetch empty")
      }

      return res.json()
    })
    .catch(err => {
      console.error("SUPPLE -- failed to fetch experiments")
      throw err
    })
}

const revealProductPrice = (el: Element, price: number | null) => {
  if (typeof price === "string") {
    el.innerHTML = String(price)
  }
  el.classList.remove("supple__price--hidden")
  el.classList.add("supple__price")
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

const setDebutCheckoutSvid = (svid: number) => {
  const el = document.getElementById("ProductSelect-product-template")
  if (el === null) {
    throw new Error("SUPPLE -- failed to find Debut checkout element to set")
  }
  const option = el.children[0]
  if (!(option instanceof HTMLOptionElement)) {
    throw new Error("SUPPLE -- failed to find Debut checkout element to set")
  }
  option.value = String(svid)
}

const getIsDebutCheckout = (): boolean => {
  const el = document.getElementById("ProductSelect-product-template")
  if (!(el instanceof HTMLSelectElement)) {
    console.log("SUPPLE -- failed to find Debut checkout")
    return false
  }
  const option = el.children[0]
  if (!(option instanceof HTMLOptionElement)) {
    console.warn("SUPPLE -- failed to descend what looks like a Debut checkout")
    return false
  }
  return true
}

const applyExperiments = (exps: IExperiment[]): void => {
  const els = Array.from(
    document.getElementsByClassName("supple__price--hidden")
  )

  els.map(el => {
    const id = getIdFromPriceElement(el)
    if (id === null) {
      console.error("SUPPLE -- Hidden price with no id! Unhiding price as-is")
      revealProductPrice(el, null)
      return
    }

    const exp = exps.find(e => e.sku === id)

    if (exp === undefined) {
      console.warn(
        "SUPPLE -- Price with no matching experiment! Unhiding price as-is"
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

const getDOMAccessible = () =>
  new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", () => {
      console.timeEnd("supple_dom_ready")
      resolve()
    })
  })

Promise.all([getDOMAccessible(), getExperiments()])
  .then(([_, apiExps]) => ({
    exps: apiExps.map((exp: IApiExperiment) => ({
      price: exp.bucket_price,
      sku: exp.bucket_sku,
      svid: exp.bucket_svid
    })),
    userId: apiExps[0].user_id
  }))
  .then(({ userId, exps }) => {
    // TODO: carefully consider when to set the userId
    localStorage.setItem("supple_uid", String(userId))
    applyExperiments(exps)
    console.log("SUPPLE -- success!")
    console.timeEnd("supple_complete")
  })
  .catch(err => {
    console.error("SUPPLE -- failed to apply experiments")
    throw err
  })
