"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ownKeys = exports.memoizeThunk = exports.isSingle = exports.isNonEmpty = exports.getKeysForIndexSignature = exports.formatUnknown = exports.formatPropertyKey = exports.formatPathKey = exports.formatPath = exports.formatDate = void 0;
var array_ = _interopRequireWildcard(require("effect/Array"));
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const getKeysForIndexSignature = (input, parameter) => {
  switch (parameter._tag) {
    case "StringKeyword":
    case "TemplateLiteral":
      return Object.keys(input);
    case "SymbolKeyword":
      return Object.getOwnPropertySymbols(input);
    case "Refinement":
      return getKeysForIndexSignature(input, parameter.from);
  }
};
/**
 * JavaScript does not store the insertion order of properties in a way that
 * combines both string and symbol keys. The internal order groups string keys
 * and symbol keys separately. Hence concatenating the keys is fine.
 *
 * @internal
 */
exports.getKeysForIndexSignature = getKeysForIndexSignature;
const ownKeys = o => Object.keys(o).concat(Object.getOwnPropertySymbols(o));
/** @internal */
exports.ownKeys = ownKeys;
const memoizeThunk = f => {
  let done = false;
  let a;
  return () => {
    if (done) {
      return a;
    }
    a = f();
    done = true;
    return a;
  };
};
/** @internal */
exports.memoizeThunk = memoizeThunk;
const formatDate = date => {
  try {
    return date.toISOString();
  } catch (e) {
    return String(date);
  }
};
/** @internal */
exports.formatDate = formatDate;
const formatUnknown = u => {
  if (Predicate.isString(u)) {
    return JSON.stringify(u);
  } else if (Predicate.isNumber(u) || u == null || Predicate.isBoolean(u) || Predicate.isSymbol(u)) {
    return String(u);
  } else if (Predicate.isDate(u)) {
    return formatDate(u);
  } else if (Predicate.isBigInt(u)) {
    return String(u) + "n";
  } else if (!array_.isArray(u) && Predicate.hasProperty(u, "toString") && Predicate.isFunction(u["toString"]) && u["toString"] !== Object.prototype.toString) {
    return u["toString"]();
  }
  try {
    JSON.stringify(u);
    if (array_.isArray(u)) {
      return `[${u.map(formatUnknown).join(",")}]`;
    } else {
      return `{${ownKeys(u).map(k => `${Predicate.isString(k) ? JSON.stringify(k) : String(k)}:${formatUnknown(u[k])}`).join(",")}}`;
    }
  } catch (e) {
    return String(u);
  }
};
/** @internal */
exports.formatUnknown = formatUnknown;
const formatPropertyKey = name => typeof name === "string" ? JSON.stringify(name) : String(name);
/** @internal */
exports.formatPropertyKey = formatPropertyKey;
const isNonEmpty = x => Array.isArray(x);
/** @internal */
exports.isNonEmpty = isNonEmpty;
const isSingle = x => !Array.isArray(x);
/** @internal */
exports.isSingle = isSingle;
const formatPathKey = key => `[${formatPropertyKey(key)}]`;
/** @internal */
exports.formatPathKey = formatPathKey;
const formatPath = path => isNonEmpty(path) ? path.map(formatPathKey).join("") : formatPathKey(path);
exports.formatPath = formatPath;
//# sourceMappingURL=util.js.map