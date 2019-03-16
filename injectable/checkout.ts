import { eventsURL } from "./constants"
import { detectPage } from "./events"
import { log } from "./logging"

const trackCheckout = (): void => {
  log({ message: "Tracking checkout" })
  const page = detectPage()
  if (page !== "checkout") {
    log({ message: "Not a checkout page, skipping tracking", severity: 'warn' })
    return
  }

  const { step, token } =
    typeof window.Shopify === "object" &&
    typeof window.Shopify.Checkout === "object"
      ? window.Shopify.Checkout
      : { step: null, token: null }

  const { line_items: lineItems, order_id: orderId } =
    typeof window.Shopify === "object" &&
    typeof window.Shopify.checkout === "object"
      ? window.Shopify.checkout
      : { line_items: null, order_id: null }

  const pageUrl = window.location.href

  fetch(eventsURL, {
    body: JSON.stringify({
      lineItems: lineItems || null,
      orderId: orderId || null,
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
    log({ message: err, severity: "error" })
  })
}

trackCheckout()
