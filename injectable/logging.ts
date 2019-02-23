type Severity = "info" | "warn" | "error"
type LogFn = (msg: string, severity?: Severity) => void

const DEBUG = true
const noop = () => undefined

const consoleLog = (msg: string, severity = "info") => {
  switch (severity) {
    case "error": {
      console.error(msg)
      break
    }
    case "info": {
      console.log(msg)
      break
    }
    case "warn": {
      console.warn(msg)
      break
    }
  }
}

export const log: LogFn = DEBUG ? consoleLog : noop
