"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.encode = exports.decode = void 0;
var Either = _interopRequireWildcard(require("../../Either.js"));
var Base64 = _interopRequireWildcard(require("./base64.js"));
var _common = require("./common.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const encode = data => Base64.encode(data).replace(/=/g, "").replace(/\+/g, "-").replace(/\//g, "_");
/** @internal */
exports.encode = encode;
const decode = str => {
  const stripped = Base64.stripCrlf(str);
  const length = stripped.length;
  if (length % 4 === 1) {
    return Either.left((0, _common.DecodeException)(stripped, `Length should be a multiple of 4, but is ${length}`));
  }
  if (!/^[-_A-Z0-9]*?={0,2}$/i.test(stripped)) {
    return Either.left((0, _common.DecodeException)(stripped, "Invalid input"));
  }
  // Some variants allow or require omitting the padding '=' signs
  let sanitized = length % 4 === 2 ? `${stripped}==` : length % 4 === 3 ? `${stripped}=` : stripped;
  sanitized = sanitized.replace(/-/g, "+").replace(/_/g, "/");
  return Base64.decode(sanitized);
};
exports.decode = decode;
//# sourceMappingURL=base64Url.js.map