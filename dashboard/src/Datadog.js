const { datadogLogs } = require("@datadog/browser-logs")

datadogLogs.init({
  clientToken: "puba47c091de0204f10ec39b3e889b7cf0e",
  datacenter: 'eu',
  forwardErrorsToLogs: true,
  sampleRate: 100,
  service: "sweetspot-dashboard"
})

exports.logInfoImpl = datadogLogs.logger.info
exports.logWarnImpl = datadogLogs.logger.warn
exports.logErrorImpl = datadogLogs.logger.error
