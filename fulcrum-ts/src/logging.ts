import { sendLog } from "./service";
import { fold } from "fp-ts/lib/Either";

type LogLevel = "info" | "warn" | "error";

type JsonEncodable = any;

const getIsDebugging = (): boolean =>
  new URLSearchParams(window.location.search).get("ssdebug") === "true";

const noop = () => undefined;

export const log = async (
  level: LogLevel,
  message: JsonEncodable,
): Promise<void> => {
  const isDebugging = getIsDebugging();
  if (isDebugging) {
    switch (level) {
      case "error": {
        console.error(message);
        break;
      }
      case "warn": {
        console.warn(message);
        break;
      }
      case "info": {
        console.info(message);
        break;
      }
      default: {
        throw new Error("Partial switch case");
      }
    }
  }

  const eSendLogResult = await sendLog({ level, message });
  const onLeft = (err: Error) => {
    console.info("failed to send log message, falling back to console.");
    console.error(`send log error: ${err}`);
    console.error(`app log, level: ${level}, message: ${message}`);
  };
  fold(onLeft, noop)(eSendLogResult);
};
