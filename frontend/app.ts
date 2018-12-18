console.time("supple_complete");
console.log("SUPPLE -- init");

const apiUrl = "https://7b7ba380.ngrok.io";
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
};

const getExperiments = (): Promise<any> => {
  return Promise.resolve([{ user_id: 1, bucket_price: 20 }]);
  const path = `/bucket/`;
  const maybeUid = localStorage.getItem("supple_uid");
  let qs = null;
  if (typeof maybeUid === "string") {
    qs = `?${queryString.stringify({ uid: maybeUid })}`;
  } else {
    qs = "";
  }
  console.log("SUPPLE -- fetching experiments");
  return fetch(`${apiUrl}${path}${qs}`)
    .then(res => {
      if (res.status !== 200) {
        throw new Error(
          `SUPPLE -- bad response for experiment fetch, got ${
            res.status
          }, with body: ${res.body}`
        );
      }

      if (res.body == null) {
        throw new Error("SUPPLE -- response body for experiment fetch empty");
      }

      return res.json();
    })
    .catch(err => {
      console.error("SUPPLE -- failed to fetch experiments");
      throw err;
    });
};

const getPageType = (): PageType => {
  const path = window.location.pathname;
  const pathComponents = path.split("/");

  if (pathComponents.length < 2) {
    throw new Error(`SUPPLE -- unrecognized page type, path was: ${path}`);
  }

  const pageType = pathComponents[1];
  if (pageType === "") {
    throw new Error(`SUPPLE -- unrecognized page type, path was ${path}`);
  }

  if (pageType === PageType.Collections) {
    return PageType.Collections;
  } else if (pageType === PageType.Products) {
    return PageType.Products;
  }

  return PageType.Unknown;
};

const identifyProductPageSku = (): string => {
  const el = document.getElementById("supple__sku");

  if (el === null) {
    throw new Error("SUPPLE -- failed to identify product page variant");
  }

  // TODO: make sure to grab the visible base variant
  return el.innerText;
};

// Inject price with id based on base variant id
// Check for id in experiments, reveal correct price
const identifyListingsPageVariants = () => {};

const revealProductPrice = (price: number) => {
  const el = document.getElementsByClassName("supple__price--hidden")[0];
  el.innerHTML = String(price);
  el.classList.remove("supple__price--hidden");
  el.className = `${el.className} supple__price`;
};

const setCheckoutSvid = (svid: number): void => {
  const el = document.getElementById("ProductSelect-product-template");
  if (el === null) {
    throw new Error("SUPPLE -- could not find product selector for checkout");
  }
  if (!(el instanceof HTMLSelectElement)) {
    throw new Error("SUPPLE -- expected element to be an HTMLOptionElement");
  }
  const option = el.children[0];
  if (!(option instanceof HTMLOptionElement)) {
    throw new Error("SUPPLE -- expected element to be an HTMLSelectElement");
  }
  option.value = String(svid);
};

interface Experiment {
  price: number;
  sku: string;
  svid: number;
}

enum PageType {
  Unknown = "",
  Collections = "collections",
  Products = "products"
}

const handleProductPage = (exps: Experiment[]): void => {
  const sku = identifyProductPageSku();
  const exp = exps.find(exp => exp.sku === sku);

  if (exp === undefined) {
    // TODO: handle product not in experiment
    throw new Error("SUPPLE -- no experiment for visible svid");
  }

  setCheckoutSvid(exp.svid);
  revealProductPrice(exp.price);
};

const applyExperiments = (pageType: PageType, exps: Experiment[]): void => {
  if (pageType === PageType.Collections) {
    // TODO: handle collections page
    throw new Error("SUPPLE -- unable to apply prices for collections page");
  } else if (pageType === PageType.Products) {
    handleProductPage(exps);
    return;
  } else if (pageType === PageType.Unknown) {
    // TODO: handle unknown pages. Simply unhide price?
    throw new Error(`SUPPLE -- unrecognized page type, page type: ${pageType}`);
  }

  const assertNever: never = pageType;
};

const getDOMAccessible = () =>
  new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", resolve);
  });

Promise.all([getDOMAccessible(), getExperiments()])
  .then(([_, rawExps]) => ({
    userId: rawExps[0].user_id,
    // exps: rawExps.map((exp: any) => ({ price: exp.bucket_price, sku: exp.bucket_sku, svid: exp.bucket_svid }))
    exps: rawExps.map((exp: any) => ({
      price: exp.bucket_price,
      sku: "3",
      svid: 18250765205568
    }))
  }))
  .then(({ userId, exps }) => {
    // TODO: carefully consider when to set the userId
    localStorage.setItem("supple_uid", userId);
    const pageType = getPageType();
    applyExperiments(pageType, exps);
    console.log("SUPPLE -- success!");
    console.timeEnd("supple_complete");
  })
  .catch(err => {
    console.error("SUPPLE -- failed to apply experiments");
    throw err;
  });
