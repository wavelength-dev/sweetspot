// more in here than we've typed
type LineItem = {
  variant_id: number;
  quantity: number;
};

// more in here than we've typed
type Checkout = {
  order_id: number;
  token: string;
  created_at: string; // ISO 8601
  line_items: LineItem[];
};
type CheckoutWindow = Window &
  typeof globalThis & {
    Shopify?: {
      checkout?: Checkout;
    };
    trekkie?: {
      user: () => {
        traits: () => {
          uniqToken: string;
        };
      };
    };
  };

const captureCheckout = (): Checkout | undefined => {
  return (window as CheckoutWindow).Shopify?.checkout;
};

const mCheckout = captureCheckout();

const captureUserToken = (cb: (user_id: string) => void): void => {
  const intervalId = setInterval(() => {
    const mUserId = (window as CheckoutWindow).trekkie?.user().traits()
      .uniqToken;
    if (mUserId !== undefined) {
      clearInterval(intervalId);
      cb(mUserId);
    }
  }, 100);
};

type CheckoutPayload = {
  order: {
    id: number;
    created_at: string;
    cart_token: string;
    line_items: { variant_id: number; quantity: number }[];
  };
  user_id: string;
};

if (mCheckout !== undefined) {
  const order = {
    id: mCheckout.order_id,
    created_at: mCheckout.created_at,
    cart_token: mCheckout.token,
    line_items: mCheckout.line_items.map((rawLineItem) => ({
      variant_id: rawLineItem.variant_id,
      quantity: rawLineItem.quantity,
    })),
  };

  captureUserToken((user_id) => {
    const payload: CheckoutPayload = {
      order,
      // this should never be undefined, but if it's not there
      // the backend errors is how we'll know
      user_id: user_id!,
    };

    fetch("/apps/sweetspot/api/fulcrum/checkout", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });
  });
}
