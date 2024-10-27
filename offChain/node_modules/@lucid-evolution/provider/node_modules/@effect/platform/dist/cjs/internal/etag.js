"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toString = exports.tag = exports.GeneratorTypeId = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const GeneratorTypeId = exports.GeneratorTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Etag/Generator");
/** @internal */
const tag = exports.tag = /*#__PURE__*/Context.GenericTag("@effect/platform/Etag/Generator");
/** @internal */
const toString = self => {
  switch (self._tag) {
    case "Weak":
      return `W/"${self.value}"`;
    case "Strong":
      return `"${self.value}"`;
  }
};
exports.toString = toString;
//# sourceMappingURL=etag.js.map