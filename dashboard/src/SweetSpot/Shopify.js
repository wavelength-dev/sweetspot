const enTranslations = require("@shopify/polaris/locales/en.json");
const {
  AppProvider,
  Button,
  Card,
  EmptyState,
  Heading,
  Page,
  ResourceList
} = require("@shopify/polaris");
const createApp = require('@shopify/app-bridge');
const { Redirect } = require('@shopify/app-bridge/actions');

exports.appProvider = AppProvider;
exports.button = Button;
exports.card = Card;
exports.emptyState = EmptyState;
exports.enTranslations = enTranslations;
exports.heading = Heading;
exports.page = Page;
exports.resourceList = ResourceList;

exports.ensureEmbedded = (shopOrigin) => {
    const apiKey = '634b531a6568d6eb076c2ad5c7e0265a'
    const permissionUrl = `/oauth/authorize?client_id=${apiKey}&scope=read_products,read_content&redirect_uri=${redirectUri}`;

    if (window.top == window.self) {
        window.location.assign(`https://${shopOrigin}/admin${permissionUrl}`);
    } else {
        const app = createApp({
            apiKey: apiKey,
            shopOrigin: shopOrigin
        });
        Redirect.create(app).dispatch(Redirect.Action.ADMIN_PATH, permissionUrl);
    }

}
