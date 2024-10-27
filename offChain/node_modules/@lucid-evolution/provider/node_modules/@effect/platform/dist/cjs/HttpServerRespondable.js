"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toResponseOrElse = exports.toResponse = exports.symbol = exports.isRespondable = void 0;
var ParseResult = _interopRequireWildcard(require("@effect/schema/ParseResult"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Predicate = require("effect/Predicate");
var ServerResponse = _interopRequireWildcard(require("./HttpServerResponse.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category symbols
 */
const symbol = exports.symbol = /*#__PURE__*/Symbol.for("@effect/platform/HttpServerRespondable");
/**
 * @since 1.0.0
 * @category guards
 */
const isRespondable = u => (0, _Predicate.hasProperty)(u, symbol);
exports.isRespondable = isRespondable;
const badRequest = /*#__PURE__*/ServerResponse.empty({
  status: 400
});
/**
 * @since 1.0.0
 * @category accessors
 */
const toResponse = self => {
  if (ServerResponse.isServerResponse(self)) {
    return Effect.succeed(self);
  }
  return Effect.orDie(self[symbol]());
};
/**
 * @since 1.0.0
 * @category accessors
 */
exports.toResponse = toResponse;
const toResponseOrElse = (u, orElse) => {
  if (ServerResponse.isServerResponse(u)) {
    return Effect.succeed(u);
  } else if (isRespondable(u)) {
    return Effect.catchAllCause(u[symbol](), () => Effect.succeed(orElse));
    // add support for some commmon types
  } else if (ParseResult.isParseError(u)) {
    return Effect.succeed(badRequest);
  }
  return Effect.succeed(orElse);
};
exports.toResponseOrElse = toResponseOrElse;
//# sourceMappingURL=HttpServerRespondable.js.map