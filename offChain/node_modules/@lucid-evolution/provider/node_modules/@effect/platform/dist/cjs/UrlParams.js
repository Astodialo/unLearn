"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toString = exports.setAll = exports.set = exports.schemaJson = exports.schema = exports.remove = exports.makeUrl = exports.getLast = exports.getFirst = exports.getAll = exports.fromInput = exports.empty = exports.appendAll = exports.append = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Arr = _interopRequireWildcard(require("effect/Array"));
var Either = _interopRequireWildcard(require("effect/Either"));
var _Function = require("effect/Function");
var Option = _interopRequireWildcard(require("effect/Option"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category constructors
 */
const fromInput = input => {
  const entries = Symbol.iterator in input ? Arr.fromIterable(input) : Object.entries(input);
  const out = [];
  for (const [key, value] of entries) {
    if (Array.isArray(value)) {
      for (let i = 0; i < value.length; i++) {
        if (value[i] !== undefined) {
          out.push([key, String(value[i])]);
        }
      }
    } else if (value !== undefined) {
      out.push([key, String(value)]);
    }
  }
  return out;
};
/**
 * @since 1.0.0
 * @category schemas
 */
exports.fromInput = fromInput;
const schema = exports.schema = /*#__PURE__*/Schema.Array(Schema.Tuple(Schema.String, Schema.String)).annotations({
  identifier: "UrlParams"
});
/**
 * @since 1.0.0
 * @category constructors
 */
const empty = exports.empty = [];
/**
 * @since 1.0.0
 * @category combinators
 */
const getAll = exports.getAll = /*#__PURE__*/(0, _Function.dual)(2, (self, key) => Arr.reduce(self, [], (acc, [k, value]) => {
  if (k === key) {
    acc.push(value);
  }
  return acc;
}));
/**
 * @since 1.0.0
 * @category combinators
 */
const getFirst = exports.getFirst = /*#__PURE__*/(0, _Function.dual)(2, (self, key) => Option.map(Arr.findFirst(self, ([k]) => k === key), ([, value]) => value));
/**
 * @since 1.0.0
 * @category combinators
 */
const getLast = exports.getLast = /*#__PURE__*/(0, _Function.dual)(2, (self, key) => Option.map(Arr.findLast(self, ([k]) => k === key), ([, value]) => value));
/**
 * @since 1.0.0
 * @category combinators
 */
const set = exports.set = /*#__PURE__*/(0, _Function.dual)(3, (self, key, value) => Arr.append(Arr.filter(self, ([k]) => k !== key), [key, String(value)]));
/**
 * @since 1.0.0
 * @category combinators
 */
const setAll = exports.setAll = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => {
  const toSet = fromInput(input);
  const keys = toSet.map(([k]) => k);
  return Arr.appendAll(Arr.filter(self, ([k]) => keys.includes(k)), toSet);
});
/**
 * @since 1.0.0
 * @category combinators
 */
const append = exports.append = /*#__PURE__*/(0, _Function.dual)(3, (self, key, value) => Arr.append(self, [key, String(value)]));
/**
 * @since 1.0.0
 * @category combinators
 */
const appendAll = exports.appendAll = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => Arr.appendAll(self, fromInput(input)));
/**
 * @since 1.0.0
 * @category combinators
 */
const remove = exports.remove = /*#__PURE__*/(0, _Function.dual)(2, (self, key) => Arr.filter(self, ([k]) => k !== key));
/**
 * @since 1.0.0
 * @category combinators
 */
const toString = self => new URLSearchParams(self).toString();
/**
 * @since 1.0.0
 * @category constructors
 */
exports.toString = toString;
const makeUrl = (url, params, hash) => {
  try {
    const urlInstance = new URL(url, baseUrl());
    for (let i = 0; i < params.length; i++) {
      const [key, value] = params[i];
      if (value !== undefined) {
        urlInstance.searchParams.append(key, value);
      }
    }
    if (hash._tag === "Some") {
      urlInstance.hash = hash.value;
    }
    return Either.right(urlInstance);
  } catch (e) {
    return Either.left(e);
  }
};
exports.makeUrl = makeUrl;
const baseUrl = () => {
  if ("location" in globalThis && globalThis.location !== undefined && globalThis.location.origin !== undefined && globalThis.location.pathname !== undefined) {
    return location.origin + location.pathname;
  }
  return undefined;
};
/**
 * @since 1.0.0
 * @category schema
 */
const schemaJson = (schema, options) => {
  const parse = Schema.decodeUnknown(Schema.parseJson(schema), options);
  return (0, _Function.dual)(2, (self, field) => parse(Option.getOrElse(getLast(self, field), () => "")));
};
exports.schemaJson = schemaJson;
//# sourceMappingURL=UrlParams.js.map