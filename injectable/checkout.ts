import { eventsUrl } from "./constants"
import { detectPage } from "./events"
import { log } from "./logging"

const trackCheckout = (): void => {
  log("Tracking checkout")
  const page = detectPage()
  if (page !== "checkout") {
    log("Not a checkout page, skipping tracking")
    return
  }

  const { step, token } =
    typeof window.Shopify === "object" &&
    typeof window.Shopify.Checkout === "object"
      ? window.Shopify.Checkout
      : { step: null, token: null }

  const { line_items: lineItems } =
    typeof window.Shopify === "object" &&
    typeof window.Shopify.checkout === "object"
      ? window.Shopify.checkout
      : { line_items: null }

  const pageUrl = window.location.href

  fetch(eventsUrl, {
    body: JSON.stringify({
      lineItems: lineItems || null,
      page,
      pageUrl,
      step: step || null,
      token: token || null,
    }),
    headers: {
      "Content-Type": "application/json",
    },
    method: "POST",
  }).catch(err => {
    log(err, "error")
  })
}

trackCheckout()
