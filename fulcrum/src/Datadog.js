const { datadogLogs } = require("@datadog/browser-logs")

datadogLogs.init({
  clientToken: "pub9bcedba5d6eb2fdc00d7a2be91142bd6",
  datacenter: 'eu',
  forwardErrorsToLogs: true,
  sampleRate: 100,
  service: "sweetspot-fulcrum"
})

exports.logInfoImpl = datadogLogs.logger.info.bind(datadogLogs.logger)
exports.logWarnImpl = datadogLogs.logger.warn.bind(datadogLogs.logger)
exports.logErrorImpl = datadogLogs.logger.error.bind(datadogLogs.logger)
