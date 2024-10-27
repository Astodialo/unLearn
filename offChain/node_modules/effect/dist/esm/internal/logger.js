import * as Arr from "../Array.js";
import * as Context from "../Context.js";
import * as FiberRefs from "../FiberRefs.js";
import { constVoid, dual, pipe } from "../Function.js";
import { globalValue } from "../GlobalValue.js";
import * as HashMap from "../HashMap.js";
import * as Inspectable from "../Inspectable.js";
import * as List from "../List.js";
import * as LogSpan from "../LogSpan.js";
import * as Option from "../Option.js";
import { pipeArguments } from "../Pipeable.js";
import * as Cause from "./cause.js";
import * as defaultServices from "./defaultServices.js";
import { consoleTag } from "./defaultServices/console.js";
import * as _fiberId from "./fiberId.js";
/** @internal */
const LoggerSymbolKey = "effect/Logger";
/** @internal */
export const LoggerTypeId = /*#__PURE__*/Symbol.for(LoggerSymbolKey);
const loggerVariance = {
  /* c8 ignore next */
  _Message: _ => _,
  /* c8 ignore next */
  _Output: _ => _
};
/** @internal */
export const makeLogger = log => ({
  [LoggerTypeId]: loggerVariance,
  log,
  pipe() {
    return pipeArguments(this, arguments);
  }
});
/** @internal */
export const mapInput = /*#__PURE__*/dual(2, (self, f) => makeLogger(options => self.log({
  ...options,
  message: f(options.message)
})));
/** @internal */
export const mapInputOptions = /*#__PURE__*/dual(2, (self, f) => makeLogger(options => self.log(f(options))));
/** @internal */
export const filterLogLevel = /*#__PURE__*/dual(2, (self, f) => makeLogger(options => f(options.logLevel) ? Option.some(self.log(options)) : Option.none()));
/** @internal */
export const map = /*#__PURE__*/dual(2, (self, f) => makeLogger(options => f(self.log(options))));
/** @internal */
export const none = {
  [LoggerTypeId]: loggerVariance,
  log: constVoid,
  pipe() {
    return pipeArguments(this, arguments);
  }
};
/** @internal */
export const simple = log => ({
  [LoggerTypeId]: loggerVariance,
  log: ({
    message
  }) => log(message),
  pipe() {
    return pipeArguments(this, arguments);
  }
});
/** @internal */
export const succeed = value => {
  return simple(() => value);
};
/** @internal */
export const sync = evaluate => {
  return simple(evaluate);
};
/** @internal */
export const zip = /*#__PURE__*/dual(2, (self, that) => makeLogger(options => [self.log(options), that.log(options)]));
/** @internal */
export const zipLeft = /*#__PURE__*/dual(2, (self, that) => map(zip(self, that), tuple => tuple[0]));
/** @internal */
export const zipRight = /*#__PURE__*/dual(2, (self, that) => map(zip(self, that), tuple => tuple[1]));
/** @internal */
export const stringLogger = /*#__PURE__*/makeLogger(({
  annotations,
  cause,
  date,
  fiberId,
  logLevel,
  message,
  spans
}) => {
  const nowMillis = date.getTime();
  const outputArray = [`timestamp=${date.toISOString()}`, `level=${logLevel.label}`, `fiber=${_fiberId.threadName(fiberId)}`];
  let output = outputArray.join(" ");
  const messageArr = Arr.ensure(message);
  for (let i = 0; i < messageArr.length; i++) {
    const stringMessage = Inspectable.toStringUnknown(messageArr[i]);
    if (stringMessage.length > 0) {
      output = output + " message=";
      output = appendQuoted(stringMessage, output);
    }
  }
  if (cause != null && cause._tag !== "Empty") {
    output = output + " cause=";
    output = appendQuoted(Cause.pretty(cause, {
      renderErrorCause: true
    }), output);
  }
  if (List.isCons(spans)) {
    output = output + " ";
    let first = true;
    for (const span of spans) {
      if (first) {
        first = false;
      } else {
        output = output + " ";
      }
      output = output + pipe(span, LogSpan.render(nowMillis));
    }
  }
  if (HashMap.size(annotations) > 0) {
    output = output + " ";
    let first = true;
    for (const [key, value] of annotations) {
      if (first) {
        first = false;
      } else {
        output = output + " ";
      }
      output = output + filterKeyName(key);
      output = output + "=";
      output = appendQuoted(Inspectable.toStringUnknown(value), output);
    }
  }
  return output;
});
/** @internal */
const escapeDoubleQuotes = str => `"${str.replace(/\\([\s\S])|(")/g, "\\$1$2")}"`;
const textOnly = /^[^\s"=]+$/;
/** @internal */
const appendQuoted = (label, output) => output + (label.match(textOnly) ? label : escapeDoubleQuotes(label));
/** @internal */
export const logfmtLogger = /*#__PURE__*/makeLogger(({
  annotations,
  cause,
  date,
  fiberId,
  logLevel,
  message,
  spans
}) => {
  const nowMillis = date.getTime();
  const outputArray = [`timestamp=${date.toISOString()}`, `level=${logLevel.label}`, `fiber=${_fiberId.threadName(fiberId)}`];
  let output = outputArray.join(" ");
  const messageArr = Arr.ensure(message);
  for (let i = 0; i < messageArr.length; i++) {
    const stringMessage = Inspectable.toStringUnknown(messageArr[i], 0);
    if (stringMessage.length > 0) {
      output = output + " message=";
      output = appendQuotedLogfmt(stringMessage, output);
    }
  }
  if (cause != null && cause._tag !== "Empty") {
    output = output + " cause=";
    output = appendQuotedLogfmt(Cause.pretty(cause, {
      renderErrorCause: true
    }), output);
  }
  if (List.isCons(spans)) {
    output = output + " ";
    let first = true;
    for (const span of spans) {
      if (first) {
        first = false;
      } else {
        output = output + " ";
      }
      output = output + pipe(span, renderLogSpanLogfmt(nowMillis));
    }
  }
  if (HashMap.size(annotations) > 0) {
    output = output + " ";
    let first = true;
    for (const [key, value] of annotations) {
      if (first) {
        first = false;
      } else {
        output = output + " ";
      }
      output = output + filterKeyName(key);
      output = output + "=";
      output = appendQuotedLogfmt(Inspectable.toStringUnknown(value, 0), output);
    }
  }
  return output;
});
/** @internal */
export const structuredLogger = /*#__PURE__*/makeLogger(({
  annotations,
  cause,
  date,
  fiberId,
  logLevel,
  message,
  spans
}) => {
  const now = date.getTime();
  const annotationsObj = {};
  const spansObj = {};
  if (HashMap.size(annotations) > 0) {
    for (const [k, v] of annotations) {
      annotationsObj[k] = structuredMessage(v);
    }
  }
  if (List.isCons(spans)) {
    for (const span of spans) {
      spansObj[span.label] = now - span.startTime;
    }
  }
  const messageArr = Arr.ensure(message);
  return {
    message: messageArr.length === 1 ? structuredMessage(messageArr[0]) : messageArr.map(structuredMessage),
    logLevel: logLevel.label,
    timestamp: date.toISOString(),
    cause: Cause.isEmpty(cause) ? undefined : Cause.pretty(cause, {
      renderErrorCause: true
    }),
    annotations: annotationsObj,
    spans: spansObj,
    fiberId: _fiberId.threadName(fiberId)
  };
});
export const structuredMessage = u => {
  switch (typeof u) {
    case "bigint":
    case "function":
    case "symbol":
      {
        return String(u);
      }
    default:
      {
        return Inspectable.toJSON(u);
      }
  }
};
/** @internal */
export const jsonLogger = /*#__PURE__*/map(structuredLogger, Inspectable.stringifyCircular);
/** @internal */
const filterKeyName = key => key.replace(/[\s="]/g, "_");
/** @internal */
const escapeDoubleQuotesLogfmt = str => JSON.stringify(str);
/** @internal */
const appendQuotedLogfmt = (label, output) => output + (label.match(textOnly) ? label : escapeDoubleQuotesLogfmt(label));
/** @internal */
const renderLogSpanLogfmt = now => self => {
  const label = filterKeyName(self.label);
  return `${label}=${now - self.startTime}ms`;
};
/** @internal */
export const isLogger = u => {
  return typeof u === "object" && u != null && LoggerTypeId in u;
};
const withColor = (text, ...colors) => {
  let out = "";
  for (let i = 0; i < colors.length; i++) {
    out += `\x1b[${colors[i]}m`;
  }
  return out + text + "\x1b[0m";
};
const withColorNoop = (text, ..._colors) => text;
const colors = {
  bold: "1",
  red: "31",
  green: "32",
  yellow: "33",
  blue: "34",
  cyan: "36",
  white: "37",
  gray: "90",
  black: "30",
  bgBrightRed: "101"
};
const logLevelColors = {
  None: [],
  All: [],
  Trace: [colors.gray],
  Debug: [colors.blue],
  Info: [colors.green],
  Warning: [colors.yellow],
  Error: [colors.red],
  Fatal: [colors.bgBrightRed, colors.black]
};
const logLevelStyle = {
  None: "",
  All: "",
  Trace: "color:gray",
  Debug: "color:blue",
  Info: "color:green",
  Warning: "color:orange",
  Error: "color:red",
  Fatal: "background-color:red;color:white"
};
const defaultDateFormat = date => `${date.getHours().toString().padStart(2, "0")}:${date.getMinutes().toString().padStart(2, "0")}:${date.getSeconds().toString().padStart(2, "0")}.${date.getMilliseconds().toString().padStart(3, "0")}`;
const hasProcessStdout = typeof process === "object" && process !== null && typeof process.stdout === "object" && process.stdout !== null;
const processStdoutIsTTY = hasProcessStdout && process.stdout.isTTY === true;
const hasProcessStdoutOrDeno = hasProcessStdout || "Deno" in globalThis;
/** @internal */
export const prettyLogger = options => {
  const mode_ = options?.mode ?? "auto";
  const mode = mode_ === "auto" ? hasProcessStdoutOrDeno ? "tty" : "browser" : mode_;
  const isBrowser = mode === "browser";
  const showColors = typeof options?.colors === "boolean" ? options.colors : processStdoutIsTTY || isBrowser;
  const formatDate = options?.formatDate ?? defaultDateFormat;
  return isBrowser ? prettyLoggerBrowser({
    colors: showColors,
    formatDate
  }) : prettyLoggerTty({
    colors: showColors,
    formatDate,
    stderr: options?.stderr === true
  });
};
const prettyLoggerTty = options => {
  const processIsBun = typeof process === "object" && "isBun" in process && process.isBun === true;
  const color = options.colors && processStdoutIsTTY ? withColor : withColorNoop;
  return makeLogger(({
    annotations,
    cause,
    context,
    date,
    fiberId,
    logLevel,
    message: message_,
    spans
  }) => {
    const services = FiberRefs.getOrDefault(context, defaultServices.currentServices);
    const console = Context.get(services, consoleTag).unsafe;
    const log = options.stderr === true ? console.error : console.log;
    const message = Arr.ensure(message_);
    let firstLine = color(`[${options.formatDate(date)}]`, colors.white) + ` ${color(logLevel.label, ...logLevelColors[logLevel._tag])}` + ` (${_fiberId.threadName(fiberId)})`;
    if (List.isCons(spans)) {
      const now = date.getTime();
      const render = renderLogSpanLogfmt(now);
      for (const span of spans) {
        firstLine += " " + render(span);
      }
    }
    firstLine += ":";
    let messageIndex = 0;
    if (message.length > 0) {
      const firstMaybeString = structuredMessage(message[0]);
      if (typeof firstMaybeString === "string") {
        firstLine += " " + color(firstMaybeString, colors.bold, colors.cyan);
        messageIndex++;
      }
    }
    log(firstLine);
    if (!processIsBun) console.group();
    if (!Cause.isEmpty(cause)) {
      log(Cause.pretty(cause, {
        renderErrorCause: true
      }));
    }
    if (messageIndex < message.length) {
      for (; messageIndex < message.length; messageIndex++) {
        log(Inspectable.redact(message[messageIndex]));
      }
    }
    if (HashMap.size(annotations) > 0) {
      for (const [key, value] of annotations) {
        log(color(`${key}:`, colors.bold, colors.white), Inspectable.redact(value));
      }
    }
    if (!processIsBun) console.groupEnd();
  });
};
const prettyLoggerBrowser = options => {
  const color = options.colors ? "%c" : "";
  return makeLogger(({
    annotations,
    cause,
    context,
    date,
    fiberId,
    logLevel,
    message: message_,
    spans
  }) => {
    const services = FiberRefs.getOrDefault(context, defaultServices.currentServices);
    const console = Context.get(services, consoleTag).unsafe;
    const message = Arr.ensure(message_);
    let firstLine = `${color}[${options.formatDate(date)}]`;
    const firstParams = [];
    if (options.colors) {
      firstParams.push("color:gray");
    }
    firstLine += ` ${color}${logLevel.label}${color} (${_fiberId.threadName(fiberId)})`;
    if (options.colors) {
      firstParams.push(logLevelStyle[logLevel._tag], "");
    }
    if (List.isCons(spans)) {
      const now = date.getTime();
      const render = renderLogSpanLogfmt(now);
      for (const span of spans) {
        firstLine += " " + render(span);
      }
    }
    firstLine += ":";
    let messageIndex = 0;
    if (message.length > 0) {
      const firstMaybeString = structuredMessage(message[0]);
      if (typeof firstMaybeString === "string") {
        firstLine += ` ${color}${firstMaybeString}`;
        if (options.colors) {
          firstParams.push("color:deepskyblue");
        }
        messageIndex++;
      }
    }
    console.groupCollapsed(firstLine, ...firstParams);
    if (!Cause.isEmpty(cause)) {
      console.error(Cause.pretty(cause, {
        renderErrorCause: true
      }));
    }
    if (messageIndex < message.length) {
      for (; messageIndex < message.length; messageIndex++) {
        console.log(Inspectable.redact(message[messageIndex]));
      }
    }
    if (HashMap.size(annotations) > 0) {
      for (const [key, value] of annotations) {
        const redacted = Inspectable.redact(value);
        if (options.colors) {
          console.log(`%c${key}:`, "color:gray", redacted);
        } else {
          console.log(`${key}:`, redacted);
        }
      }
    }
    console.groupEnd();
  });
};
/** @internal */
export const prettyLoggerDefault = /*#__PURE__*/globalValue("effect/Logger/prettyLoggerDefault", () => prettyLogger());
//# sourceMappingURL=logger.js.map