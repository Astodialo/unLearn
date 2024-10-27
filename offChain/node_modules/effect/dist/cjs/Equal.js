"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.equals = equals;
exports.symbol = exports.isEqual = exports.equivalence = void 0;
var Hash = _interopRequireWildcard(require("./Hash.js"));
var _Predicate = require("./Predicate.js");
var _Utils = require("./Utils.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 2.0.0
 * @category symbols
 */
const symbol = exports.symbol = /*#__PURE__*/Symbol.for("effect/Equal");
function equals() {
  if (arguments.length === 1) {
    return self => compareBoth(self, arguments[0]);
  }
  return compareBoth(arguments[0], arguments[1]);
}
function compareBoth(self, that) {
  if (self === that) {
    return true;
  }
  const selfType = typeof self;
  if (selfType !== typeof that) {
    return false;
  }
  if (selfType === "object" || selfType === "function") {
    if (self !== null && that !== null) {
      if (isEqual(self) && isEqual(that)) {
        if (Hash.hash(self) === Hash.hash(that) && self[symbol](that)) {
          return true;
        } else {
          return _Utils.structuralRegionState.enabled && _Utils.structuralRegionState.tester ? _Utils.structuralRegionState.tester(self, that) : false;
        }
      } else if (self instanceof Date && that instanceof Date) {
        return self.toISOString() === that.toISOString();
      }
    }
    if (_Utils.structuralRegionState.enabled) {
      if (Array.isArray(self) && Array.isArray(that)) {
        return self.length === that.length && self.every((v, i) => compareBoth(v, that[i]));
      }
      if (Object.getPrototypeOf(self) === Object.prototype && Object.getPrototypeOf(self) === Object.prototype) {
        const keysSelf = Object.keys(self);
        const keysThat = Object.keys(that);
        if (keysSelf.length === keysThat.length) {
          for (const key of keysSelf) {
            // @ts-expect-error
            if (!(key in that && compareBoth(self[key], that[key]))) {
              return _Utils.structuralRegionState.tester ? _Utils.structuralRegionState.tester(self, that) : false;
            }
          }
          return true;
        }
      }
      return _Utils.structuralRegionState.tester ? _Utils.structuralRegionState.tester(self, that) : false;
    }
  }
  return _Utils.structuralRegionState.enabled && _Utils.structuralRegionState.tester ? _Utils.structuralRegionState.tester(self, that) : false;
}
/**
 * @since 2.0.0
 * @category guards
 */
const isEqual = u => (0, _Predicate.hasProperty)(u, symbol);
/**
 * @since 2.0.0
 * @category instances
 */
exports.isEqual = isEqual;
const equivalence = () => equals;
exports.equivalence = equivalence;
//# sourceMappingURL=Equal.js.map