"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.make = exports.isManagedRuntime = exports.TypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/managedRuntime.js"));
var circular = _interopRequireWildcard(require("./internal/managedRuntime/circular.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.9.0
 * @category symbol
 */
const TypeId = exports.TypeId = circular.TypeId;
/**
 * Checks if the provided argument is a `ManagedRuntime`.
 *
 * @param input - The value to be checked if it is a `ManagedRuntime`.

 * @since 3.9.0
 * @category guards
 */
const isManagedRuntime = exports.isManagedRuntime = internal.isManagedRuntime;
/**
 * Convert a Layer into an ManagedRuntime, that can be used to run Effect's using
 * your services.
 *
 * @since 2.0.0
 * @category runtime class
 * @example
 * import { Console, Effect, Layer, ManagedRuntime } from "effect"
 *
 * class Notifications extends Effect.Tag("Notifications")<
 *   Notifications,
 *   { readonly notify: (message: string) => Effect.Effect<void> }
 * >() {
 *   static Live = Layer.succeed(this, { notify: (message) => Console.log(message) })
 * }
 *
 * async function main() {
 *   const runtime = ManagedRuntime.make(Notifications.Live)
 *   await runtime.runPromise(Notifications.notify("Hello, world!"))
 *   await runtime.dispose()
 * }
 *
 * main()
 */
const make = exports.make = internal.make;
//# sourceMappingURL=ManagedRuntime.js.map