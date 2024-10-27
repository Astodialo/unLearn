"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TypeId = exports.ResponseError = exports.RequestError = void 0;
var Error = _interopRequireWildcard(require("@effect/platform/Error"));
var internal = _interopRequireWildcard(require("./internal/httpClientError.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category type id
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category error
 */
class RequestError extends /*#__PURE__*/Error.TypeIdError(TypeId, "RequestError") {
  get methodAndUrl() {
    return `${this.request.method} ${this.request.url}`;
  }
  get message() {
    return this.description ? `${this.reason}: ${this.description} (${this.methodAndUrl})` : `${this.reason} error (${this.methodAndUrl})`;
  }
}
/**
 * @since 1.0.0
 * @category error
 */
exports.RequestError = RequestError;
class ResponseError extends /*#__PURE__*/Error.TypeIdError(TypeId, "ResponseError") {
  get methodAndUrl() {
    return `${this.request.method} ${this.request.url}`;
  }
  get message() {
    const info = `${this.response.status} ${this.methodAndUrl}`;
    return this.description ? `${this.reason}: ${this.description} (${info})` : `${this.reason} error (${info})`;
  }
}
exports.ResponseError = ResponseError;
//# sourceMappingURL=HttpClientError.js.map