import { LineItem } from './events';

/* Unix Timestamp in miliseconds */
export type Timestamp = number

/* ISO8601 DateTime string */
export interface DateTime {
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
