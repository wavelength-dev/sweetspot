const { datadogLogs } = require("@datadog/browser-logs")

datadogLogs.init({
  clientToken: "06e25fedcf0488ca790add8d23f534f7",
  datacenter: 'eu',
  forwardErrorsToLogs: true,
  sampleRate: 100,
  service: "sweetspot-dashboard"
})

exports.logInfoImpl = datadogLogs.logger.info
exports.logWarnImpl = datadogLogs.logger.warn
exports.logErrorImpl = datadogLogs.logger.error
