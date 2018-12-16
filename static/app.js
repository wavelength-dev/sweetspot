console.time('supple_complete');

const apiUrl = 'https://7b7ba380.ngrok.io'
const queryString = {
  stringify: (kvs) => Object
  .keys(kvs)
  .reduce((qs, k) => (
    kvs[k] == null
    ? qs
    : (qs === '')
    ? `${k}=${kvs[k]}`
    : `${qs}&${k}=${kvs[k]}`
  ), '')
};

const getExperiments = () => {
  const path = `/bucket/`;
  const maybeUid = localStorage.getItem('supple_uid');
  let qs = null;
  if (typeof maybeUid === 'string') {
    qs = `?${queryString.stringify({ uid: maybeUid })}`;
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

  const pageType = pathComponents[1];
  if (pageType === '') {
    throw new Error(`SUPPLE -- unrecognized page type, path was ${path}`);
  }

  const knownPages = new Set(['collections', 'products']);
  if (!knownPages.has(pageType)) {
    return null;
  }

  return pageType;
};

const identifyProductPageSku = () => {
  const maybeEl = document.getElementById('ProductJson-product-template');

  if (maybeEl === null) {
    throw new Error('Failed to identify product page variant');
  }

  const product = JSON.parse(maybeEl.innerText);
  // TODO: make sure to grab the visible variant
  return product.variants[0].sku;
}

const identifyListingsPageVariants = () => {}

const revealProductPrice = (price) => {
  const el = document.getElementsByClassName('supple__price--hidden')[0]
  el.innerHTML = price;
  el.classList.remove("supple__price--hidden");
  el.className = `${el.className} supple__price`;
}

const setCheckoutSvid = (svid) => {
  const option = document.getElementById('ProductSelect-product-template')[0];
  option.value = svid;
}

const handleProductPage = (exps) => {
  const sku = identifyProductPageSku();
  const maybeExp = exps.find(exp => exp.sku === sku);

  if (maybeExp === undefined) {
    // TODO: handle product not in experiment
    throw new Error('SUPPLE -- no experiment for visible svid');
  }
  const exp = maybeExp;

  setCheckoutSvid(exp.svid);
  revealProductPrice(exp.price);
}

const applyExperiments = (pageType, exps) => {
  if (pageType === 'collections') {
    // TODO: handle collections page
    throw new Error('SUPPLE -- unable to apply prices for collections page')
  } else if (pageType === 'products') {
    handleProductPage(exps);
  } else {
    // TODO: handle unknown pages. Simply unhide price?
    throw new Error(`SUPPLE -- unrecognized page type, page type: ${pageType}`)
  }
}

Promise.resolve()
  .then(() => { console.log('SUPPLE -- init'); })
  .then(() => getExperiments())
  .then(rawExps => ({
    userId: rawExps[0].user_id,
    // exps: rawExps.map(exp => ({ price: exp.bucket_price, sku: exp.bucket_sku, svid: exp.bucket_svid }))
    exps: rawExps.map(exp => ({ price: exp.bucket_price, sku: "3", svid: 18250765205568 }))
  }))
  .then(({userId, exps}) => {
    // TODO: carefully consider when to set the userId
    localStorage.setItem('supple_uid', userId);
    const pageType = getPageType();
    applyExperiments(pageType, exps);
    console.log('SUPPLE -- success!');
    console.timeEnd('supple_complete');
  })
  .catch((err) => {
    console.error('SUPPLE -- failed to apply experiments');
    throw err;
  });

