"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toFile = void 0;
var internal = _interopRequireWildcard(require("./internal/platformLogger.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * Create a Logger from another string Logger that writes to the specified file.
 *
 * @since 1.0.0
 * @example
 * import { PlatformLogger } from "@effect/platform"
 * import { NodeFileSystem, NodeRuntime } from "@effect/platform-node"
 * import { Effect, Layer, Logger } from "effect"
 *
 * const fileLogger = Logger.logfmtLogger.pipe(
 *   PlatformLogger.toFile("/tmp/log.txt")
 * )
 * const LoggerLive = Logger.replaceScoped(Logger.defaultLogger, fileLogger).pipe(
 *   Layer.provide(NodeFileSystem.layer)
 * )
 *
 * Effect.log("a").pipe(
 *   Effect.zipRight(Effect.log("b")),
 *   Effect.zipRight(Effect.log("c")),
 *   Effect.provide(LoggerLive),
 *   NodeRuntime.runMain
 * )
 */
const toFile = exports.toFile = internal.toFile;
//# sourceMappingURL=PlatformLogger.js.map