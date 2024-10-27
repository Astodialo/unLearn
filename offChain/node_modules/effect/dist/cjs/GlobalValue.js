"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.globalValue = void 0;
var version = _interopRequireWildcard(require("./internal/version.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * The `GlobalValue` module ensures that a single instance of a value is created globally,
 * even when modules are imported multiple times (e.g., due to mixing CommonJS and ESM builds)
 * or during hot-reloading in development environments like Next.js or Remix.
 *
 * It achieves this by using a versioned global store, identified by a unique `Symbol` tied to
 * the current version of the `effect` library. The store holds values that are keyed by an identifier,
 * allowing the reuse of previously computed instances across imports or reloads.
 *
 * This pattern is particularly useful in scenarios where frequent reloading can cause services or
 * single-instance objects to be recreated unnecessarily, such as in development environments with hot-reloading.
 *
 * @since 2.0.0
 */

const globalStoreId = /*#__PURE__*/Symbol.for(`effect/GlobalValue/globalStoreId/${/*#__PURE__*/version.getCurrentVersion()}`);
if (!(globalStoreId in globalThis)) {
  ;
  globalThis[globalStoreId] = /*#__PURE__*/new Map();
}
const globalStore = globalThis[globalStoreId];
/**
 * Retrieves or computes a global value associated with the given `id`. If the value for this `id`
 * has already been computed, it will be returned from the global store. If it does not exist yet,
 * the provided `compute` function will be executed to compute the value, store it, and then return it.
 *
 * This ensures that even in cases where the module is imported multiple times (e.g., in mixed environments
 * like CommonJS and ESM, or during hot-reloading in development), the value is computed only once and reused
 * thereafter.
 *
 * @example
 * import { globalValue } from "effect/GlobalValue"
 *
 * // This cache will persist as long as the module is running,
 * // even if reloaded or imported elsewhere
 * const myCache = globalValue(
 *   Symbol.for("myCache"),
 *   () => new WeakMap<object, number>()
 * )
 *
 * @since 2.0.0
 */
const globalValue = (id, compute) => {
  if (!globalStore.has(id)) {
    globalStore.set(id, compute());
  }
  return globalStore.get(id);
};
exports.globalValue = globalValue;
//# sourceMappingURL=GlobalValue.js.map