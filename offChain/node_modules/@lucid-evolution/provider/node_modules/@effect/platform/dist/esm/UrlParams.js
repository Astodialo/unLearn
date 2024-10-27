import * as Schema from "@effect/schema/Schema";
import * as Arr from "effect/Array";
import * as Either from "effect/Either";
import { dual } from "effect/Function";
import * as Option from "effect/Option";
/**
 * @since 1.0.0
 * @category constructors
 */
export const fromInput = input => {
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
export const schema = /*#__PURE__*/Schema.Array(Schema.Tuple(Schema.String, Schema.String)).annotations({
  identifier: "UrlParams"
});
/**
 * @since 1.0.0
 * @category constructors
 */
export const empty = [];
/**
 * @since 1.0.0
 * @category combinators
 */
export const getAll = /*#__PURE__*/dual(2, (self, key) => Arr.reduce(self, [], (acc, [k, value]) => {
  if (k === key) {
    acc.push(value);
  }
  return acc;
}));
/**
 * @since 1.0.0
 * @category combinators
 */
export const getFirst = /*#__PURE__*/dual(2, (self, key) => Option.map(Arr.findFirst(self, ([k]) => k === key), ([, value]) => value));
/**
 * @since 1.0.0
 * @category combinators
 */
export const getLast = /*#__PURE__*/dual(2, (self, key) => Option.map(Arr.findLast(self, ([k]) => k === key), ([, value]) => value));
/**
 * @since 1.0.0
 * @category combinators
 */
export const set = /*#__PURE__*/dual(3, (self, key, value) => Arr.append(Arr.filter(self, ([k]) => k !== key), [key, String(value)]));
/**
 * @since 1.0.0
 * @category combinators
 */
export const setAll = /*#__PURE__*/dual(2, (self, input) => {
  const toSet = fromInput(input);
  const keys = toSet.map(([k]) => k);
  return Arr.appendAll(Arr.filter(self, ([k]) => keys.includes(k)), toSet);
});
/**
 * @since 1.0.0
 * @category combinators
 */
export const append = /*#__PURE__*/dual(3, (self, key, value) => Arr.append(self, [key, String(value)]));
/**
 * @since 1.0.0
 * @category combinators
 */
export const appendAll = /*#__PURE__*/dual(2, (self, input) => Arr.appendAll(self, fromInput(input)));
/**
 * @since 1.0.0
 * @category combinators
 */
export const remove = /*#__PURE__*/dual(2, (self, key) => Arr.filter(self, ([k]) => k !== key));
/**
 * @since 1.0.0
 * @category combinators
 */
export const toString = self => new URLSearchParams(self).toString();
/**
 * @since 1.0.0
 * @category constructors
 */
export const makeUrl = (url, params, hash) => {
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
export const schemaJson = (schema, options) => {
  const parse = Schema.decodeUnknown(Schema.parseJson(schema), options);
  return dual(2, (self, field) => parse(Option.getOrElse(getLast(self, field), () => "")));
};
//# sourceMappingURL=UrlParams.js.map