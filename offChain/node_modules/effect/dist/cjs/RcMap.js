"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.make = exports.keys = exports.get = exports.TypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/rcMap.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.5.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * An `RcMap` can contain multiple reference counted resources that can be indexed
 * by a key. The resources are lazily acquired on the first call to `get` and
 * released when the last reference is released.
 *
 * Complex keys can extend `Equal` and `Hash` to allow lookups by value.
 *
 * @since 3.5.0
 * @category models
 * @param capacity The maximum number of resources that can be held in the map.
 * @param idleTimeToLive When the reference count reaches zero, the resource will be released after this duration.
 * @example
 * import { Effect, RcMap } from "effect"
 *
 * Effect.gen(function*() {
 *   const map = yield* RcMap.make({
 *     lookup: (key: string) =>
 *       Effect.acquireRelease(
 *         Effect.succeed(`acquired ${key}`),
 *         () => Effect.log(`releasing ${key}`)
 *       )
 *   })
 *
 *   // Get "foo" from the map twice, which will only acquire it once.
 *   // It will then be released once the scope closes.
 *   yield* RcMap.get(map, "foo").pipe(
 *     Effect.andThen(RcMap.get(map, "foo")),
 *     Effect.scoped
 *   )
 * })
 */
const make = exports.make = internal.make;
/**
 * @since 3.5.0
 * @category combinators
 */
const get = exports.get = internal.get;
/**
 * @since 3.8.0
 * @category combinators
 */
const keys = exports.keys = internal.keys;
//# sourceMappingURL=RcMap.js.map