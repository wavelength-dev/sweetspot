// TODO: Update add-to-cart hidden form to add correct variantID.

const sku = "";
const apiUrl = 'https://7b7ba380.ngrok.io'
const queryString = {
  stringify: (kvs) => Object
    .keys(kvs)
    .reduce((qs, k) => (
      kvs[k] == null
      ? qs
      : qs === ''
      ? `${k}=${kvs[k]}`
      : `${qs}&${k}=${kvs[k]}`
      , ''))
};

const getExperiments = (maybeSuppleUid) => {
  const path = `/bucket/`;
  const maybeUid = localStorage.getItem('suppleUid');
  let qs = null;
  if (typeof maybeSuppleUid === 'string') {
    qs = `?${queryString.strigify({ uid: maybeUid })}`;
  } else {
    qs = '';
  }
  console.log('SUPPLE -- fetching experiments')
  return fetch(`${apiUrl}${path}${qs}`)
    .then(res => {    
      if (res.status !== 200) {
        throw new Error(`SUPPLE -- bad response for experiment fetch, got ${res.status}, with body: ${res.body}`)
      }

      if (res.body == null) {
        throw new Error('SUPPLE -- response body for experiment fetch empty')
      }

      return res.json();
    })
    .catch(err => {
      console.error('SUPPLE -- failed to fetch experiments')
      throw err;
    })
}

const getPageType = () => {
  const path = window.location.pathname;
  const pathComponents = path.split('/');

  if (pathComponents.length < 2) {
    throw new Error(`SUPPLE -- unrecognized page type, path was: ${path}`);
  }

  const pageType = pathComponents[2];
  if (pageType === '') {
    throw new Error(`SUPPLE -- unrecognized page type, path was ${path}`);
  }

  const knownPages = new Set(['collections', 'products']);
  if (!knownPages.has(pageType)) {
    return null;
  }

  return pageType;
};

const identifyProductPageVariant = () => {
    const maybeEl = document.getElementById('ProductJson-product-template');

    if (maybeEl === null) {
      throw new Error('Failed to identify product variant');
    }

    const product = JSON.parse(maybeEl.innerText);
    return product.id;
}

const identifyListingsPageVariants = () => {}

const revealProductPrice = (price) => {
  const el = document.getElementsByClassName('supple__price--hidden')[0]
  el.innerHTML = price;
  el.classList.remove("supple__price--hidden");
}

const handleProductPage = (svid, exps) => {
  const svid = identifyProductPageVariant();
  const maybeExp = exps.find(exp => exp.svid === svid);
  if (maybeExp === undefined) {
    // TODO: handle product not in experiment
    throw new Error('SUPPLE -- no experiment for visible svid');
  }

  const exp = maybeExp;
  revealProductPrice(exp.bucket_price);
}

const applyExperiments = (pageType, exps) => {
  if (pageType === 'collections') {
    // TODO: handle collections page
    throw new Error('SUPPLE -- unable to apply prices for collections page')
  } else if (pageType === 'products') {
    const svid = identifyProductPageVariant();
    handleProductPage(svid, exps)
  } else {
    // TODO: handle unknown pages. Simply unhide price?
    throw new Error(`SUPPLE -- unrecognized page type, page type: ${pageType}`)
  }
}

const maybeSuppleUid = localStorage.getItem('supple_uid');

Promise.resolve()
  .then(() => { console.log('SUPPLE -- init') })
  .then(() => getExperiments(maybeSuppleUid))
  .then((exp) => {
    const pageType = getPageType();
    applyExperiments(pageType, exps);
  })
  .catch((err) => { console.error('SUPPLE -- failed to apply experiments'); throw err; })

