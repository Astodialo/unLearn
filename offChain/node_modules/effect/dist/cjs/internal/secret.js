"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.value = exports.unsafeWipe = exports.make = exports.isSecret = exports.fromString = exports.fromIterable = exports.SecretTypeId = void 0;
var Arr = _interopRequireWildcard(require("../Array.js"));
var _Predicate = require("../Predicate.js");
var redacted_ = _interopRequireWildcard(require("./redacted.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @internal
 * @deprecated
 */
const SecretSymbolKey = "effect/Secret";
/**
 * @internal
 * @deprecated
 */
const SecretTypeId = exports.SecretTypeId = /*#__PURE__*/Symbol.for(SecretSymbolKey);
/**
 * @internal
 * @deprecated
 */
const isSecret = u => (0, _Predicate.hasProperty)(u, SecretTypeId);
/**
 * @internal
 * @deprecated
 */
exports.isSecret = isSecret;
const make = bytes => {
  const secret = Object.create({
    ...redacted_.proto,
    [SecretTypeId]: SecretTypeId
  });
  Object.defineProperty(secret, "toString", {
    enumerable: false,
    value() {
      return "Secret(<redacted>)";
    }
  });
  Object.defineProperty(secret, "toJSON", {
    enumerable: false,
    value() {
      return "<redacted>";
    }
  });
  Object.defineProperty(secret, "raw", {
    enumerable: false,
    value: bytes
  });
  redacted_.redactedRegistry.set(secret, bytes.map(byte => String.fromCharCode(byte)).join(""));
  return secret;
};
/**
 * @internal
 * @deprecated
 */
exports.make = make;
const fromIterable = iterable => make(Arr.fromIterable(iterable).map(char => char.charCodeAt(0)));
/**
 * @internal
 * @deprecated
 */
exports.fromIterable = fromIterable;
const fromString = text => {
  return make(text.split("").map(char => char.charCodeAt(0)));
};
/**
 * @internal
 * @deprecated
 */
exports.fromString = fromString;
const value = self => {
  return self.raw.map(byte => String.fromCharCode(byte)).join("");
};
/**
 * @internal
 * @deprecated
 */
exports.value = value;
const unsafeWipe = self => {
  for (let i = 0; i < self.raw.length; i++) {
    self.raw[i] = 0;
  }
  redacted_.redactedRegistry.delete(self);
};
exports.unsafeWipe = unsafeWipe;
//# sourceMappingURL=secret.js.map