import { logURL } from "./constants"

type Severity = "info" | "warn" | "error"
interface Log {
  message: string
  payload?: {}
  severity?: Severity
}
type LogFn = (arg0: Log) => void

const consoleLog: LogFn = ({ message, payload, severity }: Log) => {
  switch (severity) {
    case "error": {
      console.error(message, payload)
      break
    }
    case "info": {
      console.log(message, payload)
      break
    }
    case "warn": {
      console.warn(message, payload)
      break
    }
  }
}

const transportOverHTTPLog: LogFn = ({ message, payload, severity }: Log) => {
  fetch(logURL, {
    body: JSON.stringify({ message, payload, severity }),
    headers: {
      "Content-Type": "application/json",
    },
    method: "POST",
  })
  // .catch
  // Well this is bad. We're in the dark.
  // Is there something we could still do?
}

// injected by envify
declare var process: {
  env: {
    ENV: "production" | "staging" | "development",
  },
}
export const log: LogFn =
  process.env.ENV === "staging" ? transportOverHTTPLog : consoleLog
