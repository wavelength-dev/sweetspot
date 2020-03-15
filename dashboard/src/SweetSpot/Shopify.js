const enTranslations = require('@shopify/polaris/locales/en.json')
const {
  AppProvider,
  Button,
  Card,
  EmptyState,
  Heading,
  Page,
  ResourceList,
} = require('@shopify/polaris')
const createApp = require('@shopify/app-bridge')
const { Redirect } = require('@shopify/app-bridge/actions')

exports.appProvider = AppProvider
exports.button = Button
exports.card = Card
exports.emptyState = EmptyState
exports.enTranslations = enTranslations
exports.heading = Heading
exports.page = Page
exports.resourceList = ResourceList

exports.createApp = function(apiKey) {
  return function(shopOrigin) {
    return function() {
      return createApp.default({ apiKey, shopOrigin })
    }
  }
}
