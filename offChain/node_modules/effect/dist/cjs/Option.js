"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.zipWith = exports.zipRight = exports.zipLeft = exports.void = exports.toRefinement = exports.toArray = exports.tap = exports.some = exports.reduceCompact = exports.productMany = exports.product = exports.partitionMap = exports.orElseSome = exports.orElseEither = exports.orElse = exports.none = exports.match = exports.map = exports.liftThrowable = exports.liftPredicate = exports.liftNullable = exports.lift2 = exports.let = exports.isSome = exports.isOption = exports.isNone = exports.getRight = exports.getOrder = exports.getOrUndefined = exports.getOrThrowWith = exports.getOrThrow = exports.getOrNull = exports.getOrElse = exports.getLeft = exports.getEquivalence = exports.gen = exports.fromNullable = exports.fromIterable = exports.flatten = exports.flatMapNullable = exports.flatMap = exports.firstSomeOf = exports.filterMap = exports.filter = exports.exists = exports.containsWith = exports.contains = exports.composeK = exports.bindTo = exports.bind = exports.asVoid = exports.as = exports.ap = exports.andThen = exports.all = exports.TypeId = exports.Do = void 0;
var Equal = _interopRequireWildcard(require("./Equal.js"));
var Equivalence = _interopRequireWildcard(require("./Equivalence.js"));
var _Function = require("./Function.js");
var doNotation = _interopRequireWildcard(require("./internal/doNotation.js"));
var either = _interopRequireWildcard(require("./internal/either.js"));
var option = _interopRequireWildcard(require("./internal/option.js"));
var order = _interopRequireWildcard(require("./Order.js"));
var Gen = _interopRequireWildcard(require("./Utils.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @category symbols
 * @since 2.0.0
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("effect/Option");
/**
 * Creates a new `Option` that represents the absence of a value.
 *
 * @category constructors
 * @since 2.0.0
 */
const none = () => option.none;
/**
 * Creates a new `Option` that wraps the given value.
 *
 * @param value - The value to wrap.
 *
 * @category constructors
 * @since 2.0.0
 */
exports.none = none;
const some = exports.some = option.some;
/**
 * Tests if a value is a `Option`.
 *
 * @param input - The value to check.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.isOption(Option.some(1)), true)
 * assert.deepStrictEqual(Option.isOption(Option.none()), true)
 * assert.deepStrictEqual(Option.isOption({}), false)
 *
 * @category guards
 * @since 2.0.0
 */
const isOption = exports.isOption = option.isOption;
/**
 * Determine if a `Option` is a `None`.
 *
 * @param self - The `Option` to check.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.isNone(Option.some(1)), false)
 * assert.deepStrictEqual(Option.isNone(Option.none()), true)
 *
 * @category guards
 * @since 2.0.0
 */
const isNone = exports.isNone = option.isNone;
/**
 * Determine if a `Option` is a `Some`.
 *
 * @param self - The `Option` to check.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.isSome(Option.some(1)), true)
 * assert.deepStrictEqual(Option.isSome(Option.none()), false)
 *
 * @category guards
 * @since 2.0.0
 */
const isSome = exports.isSome = option.isSome;
/**
 * Matches the given `Option` and returns either the provided `onNone` value or the result of the provided `onSome`
 * function when passed the `Option`'s value.
 *
 * @param self - The `Option` to match
 * @param onNone - The value to be returned if the `Option` is `None`
 * @param onSome - The function to be called if the `Option` is `Some`, it will be passed the `Option`'s value and its result will be returned
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * assert.deepStrictEqual(
 *   pipe(Option.some(1), Option.match({ onNone: () => 'a none', onSome: (a) => `a some containing ${a}` })),
 *   'a some containing 1'
 * )
 *
 * assert.deepStrictEqual(
 *   pipe(Option.none(), Option.match({ onNone: () => 'a none', onSome: (a) => `a some containing ${a}` })),
 *   'a none'
 * )
 *
 * @category pattern matching
 * @since 2.0.0
 */
const match = exports.match = /*#__PURE__*/(0, _Function.dual)(2, (self, {
  onNone,
  onSome
}) => isNone(self) ? onNone() : onSome(self.value));
/**
 * Returns a type guard from a `Option` returning function.
 * This function ensures that a type guard definition is type-safe.
 *
 * @example
 * import { Option } from "effect"
 *
 * const parsePositive = (n: number): Option.Option<number> =>
 *   n > 0 ? Option.some(n) : Option.none()
 *
 * const isPositive = Option.toRefinement(parsePositive)
 *
 * assert.deepStrictEqual(isPositive(1), true)
 * assert.deepStrictEqual(isPositive(-1), false)
 *
 * @category conversions
 * @since 2.0.0
 */
const toRefinement = f => a => isSome(f(a));
/**
 * Converts an `Iterable` of values into an `Option`. Returns the first value of the `Iterable` wrapped in a `Some`
 * if the `Iterable` is not empty, otherwise returns `None`.
 *
 * @param collection - The `Iterable` to be converted to an `Option`.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.fromIterable([1, 2, 3]), Option.some(1))
 * assert.deepStrictEqual(Option.fromIterable([]), Option.none())
 *
 * @category constructors
 * @since 2.0.0
 */
exports.toRefinement = toRefinement;
const fromIterable = collection => {
  for (const a of collection) {
    return some(a);
  }
  return none();
};
/**
 * Converts a `Either` to an `Option` discarding the error.
 *
 * @example
 * import { Option, Either } from "effect"
 *
 * assert.deepStrictEqual(Option.getRight(Either.right('ok')), Option.some('ok'))
 * assert.deepStrictEqual(Option.getRight(Either.left('err')), Option.none())
 *
 * @category conversions
 * @since 2.0.0
 */
exports.fromIterable = fromIterable;
const getRight = exports.getRight = either.getRight;
/**
 * Converts a `Either` to an `Option` discarding the value.
 *
 * @example
 * import { Option, Either } from "effect"
 *
 * assert.deepStrictEqual(Option.getLeft(Either.right("ok")), Option.none())
 * assert.deepStrictEqual(Option.getLeft(Either.left("a")), Option.some("a"))
 *
 * @category conversions
 * @since 2.0.0
 */
const getLeft = exports.getLeft = either.getLeft;
/**
 * Returns the value of the `Option` if it is `Some`, otherwise returns `onNone`
 *
 * @param self - The `Option` to get the value of.
 * @param onNone - Function that returns the default value to return if the `Option` is `None`.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * assert.deepStrictEqual(pipe(Option.some(1), Option.getOrElse(() => 0)), 1)
 * assert.deepStrictEqual(pipe(Option.none(), Option.getOrElse(() => 0)), 0)
 *
 * @category getters
 * @since 2.0.0
 */
const getOrElse = exports.getOrElse = /*#__PURE__*/(0, _Function.dual)(2, (self, onNone) => isNone(self) ? onNone() : self.value);
/**
 * Returns the provided `Option` `that` if `self` is `None`, otherwise returns `self`.
 *
 * @param self - The first `Option` to be checked.
 * @param that - The `Option` to return if `self` is `None`.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.none(),
 *     Option.orElse(() => Option.none())
 *   ),
 *   Option.none()
 * )
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.some('a'),
 *     Option.orElse(() => Option.none())
 *   ),
 *   Option.some('a')
 * )
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.none(),
 *     Option.orElse(() => Option.some('b'))
 *   ),
 *   Option.some('b')
 * )
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.some('a'),
 *     Option.orElse(() => Option.some('b'))
 *   ),
 *   Option.some('a')
 * )
 *
 * @category error handling
 * @since 2.0.0
 */
const orElse = exports.orElse = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => isNone(self) ? that() : self);
/**
 * Returns the provided default value as `Some` if `self` is `None`, otherwise returns `self`.
 *
 * @param self - The first `Option` to be checked.
 * @param onNone - Function that returns the default value to return if the `Option` is `None`.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.none(),
 *     Option.orElseSome(() => 'b')
 *   ),
 *   Option.some('b')
 * )
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.some('a'),
 *     Option.orElseSome(() => 'b')
 *   ),
 *   Option.some('a')
 * )
 *
 * @category error handling
 * @since 2.0.0
 */
const orElseSome = exports.orElseSome = /*#__PURE__*/(0, _Function.dual)(2, (self, onNone) => isNone(self) ? some(onNone()) : self);
/**
 * Similar to `orElse`, but instead of returning a simple union, it returns an `Either` object,
 * which contains information about which of the two `Option`s has been chosen.
 *
 * This is useful when it's important to know whether the value was retrieved from the first `Option` or the second option.
 *
 * @param self - The first `Option` to be checked.
 * @param that - The second `Option` to be considered if the first `Option` is `None`.
 *
 * @category error handling
 * @since 2.0.0
 */
const orElseEither = exports.orElseEither = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => isNone(self) ? map(that(), either.right) : map(self, either.left));
/**
 * Given an `Iterable` collection of `Option`s, returns the first `Some` found in the collection.
 *
 * @param collection - An iterable collection of `Option` to be searched.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.firstSomeOf([Option.none(), Option.some(1), Option.some(2)]), Option.some(1))
 *
 * @category error handling
 * @since 2.0.0
 */
const firstSomeOf = collection => {
  let out = none();
  for (out of collection) {
    if (isSome(out)) {
      return out;
    }
  }
  return out;
};
/**
 * Constructs a new `Option` from a nullable type. If the value is `null` or `undefined`, returns `None`, otherwise
 * returns the value wrapped in a `Some`.
 *
 * @param nullableValue - The nullable value to be converted to an `Option`.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.fromNullable(undefined), Option.none())
 * assert.deepStrictEqual(Option.fromNullable(null), Option.none())
 * assert.deepStrictEqual(Option.fromNullable(1), Option.some(1))
 *
 * @category conversions
 * @since 2.0.0
 */
exports.firstSomeOf = firstSomeOf;
const fromNullable = nullableValue => nullableValue == null ? none() : some(nullableValue);
/**
 * This API is useful for lifting a function that returns `null` or `undefined` into the `Option` context.
 *
 * @example
 * import { Option } from "effect"
 *
 * const parse = (s: string): number | undefined => {
 *   const n = parseFloat(s)
 *   return isNaN(n) ? undefined : n
 * }
 *
 * const parseOption = Option.liftNullable(parse)
 *
 * assert.deepStrictEqual(parseOption('1'), Option.some(1))
 * assert.deepStrictEqual(parseOption('not a number'), Option.none())
 *
 * @category conversions
 * @since 2.0.0
 */
exports.fromNullable = fromNullable;
const liftNullable = f => (...a) => fromNullable(f(...a));
/**
 * Returns the value of the `Option` if it is a `Some`, otherwise returns `null`.
 *
 * @param self - The `Option` to extract the value from.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.getOrNull(Option.some(1)), 1)
 * assert.deepStrictEqual(Option.getOrNull(Option.none()), null)
 *
 * @category getters
 * @since 2.0.0
 */
exports.liftNullable = liftNullable;
const getOrNull = exports.getOrNull = /*#__PURE__*/getOrElse(_Function.constNull);
/**
 * Returns the value of the `Option` if it is a `Some`, otherwise returns `undefined`.
 *
 * @param self - The `Option` to extract the value from.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.getOrUndefined(Option.some(1)), 1)
 * assert.deepStrictEqual(Option.getOrUndefined(Option.none()), undefined)
 *
 * @category getters
 * @since 2.0.0
 */
const getOrUndefined = exports.getOrUndefined = /*#__PURE__*/getOrElse(_Function.constUndefined);
/**
 * A utility function that lifts a function that throws exceptions into a function that returns an `Option`.
 *
 * This function is useful for any function that might throw an exception, allowing the developer to handle
 * the exception in a more functional way.
 *
 * @param f - the function that can throw exceptions.
 *
 * @example
 * import { Option } from "effect"
 *
 * const parse = Option.liftThrowable(JSON.parse)
 *
 * assert.deepStrictEqual(parse("1"), Option.some(1))
 * assert.deepStrictEqual(parse(""), Option.none())
 *
 * @category conversions
 * @since 2.0.0
 */
const liftThrowable = f => (...a) => {
  try {
    return some(f(...a));
  } catch (e) {
    return none();
  }
};
/**
 * Extracts the value of an `Option` or throws if the `Option` is `None`.
 *
 * If a default error is sufficient for your use case and you don't need to configure the thrown error, see {@link getOrThrow}.
 *
 * @param self - The `Option` to extract the value from.
 * @param onNone - A function that will be called if the `Option` is `None`. It returns the error to be thrown.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(
 *   Option.getOrThrowWith(Option.some(1), () => new Error('Unexpected None')),
 *   1
 * )
 * assert.throws(() => Option.getOrThrowWith(Option.none(), () => new Error('Unexpected None')))
 *
 * @category conversions
 * @since 2.0.0
 */
exports.liftThrowable = liftThrowable;
const getOrThrowWith = exports.getOrThrowWith = /*#__PURE__*/(0, _Function.dual)(2, (self, onNone) => {
  if (isSome(self)) {
    return self.value;
  }
  throw onNone();
});
/**
 * Extracts the value of an `Option` or throws if the `Option` is `None`.
 *
 * The thrown error is a default error. To configure the error thrown, see  {@link getOrThrowWith}.
 *
 * @param self - The `Option` to extract the value from.
 * @throws `Error("getOrThrow called on a None")`
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.getOrThrow(Option.some(1)), 1)
 * assert.throws(() => Option.getOrThrow(Option.none()))
 *
 * @category conversions
 * @since 2.0.0
 */
const getOrThrow = exports.getOrThrow = /*#__PURE__*/getOrThrowWith(() => new Error("getOrThrow called on a None"));
/**
 * Maps the `Some` side of an `Option` value to a new `Option` value.
 *
 * @param self - An `Option` to map
 * @param f - The function to map over the value of the `Option`
 *
 * @category mapping
 * @since 2.0.0
 */
const map = exports.map = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => isNone(self) ? none() : some(f(self.value)));
/**
 * Maps the `Some` value of this `Option` to the specified constant value.
 *
 * @category mapping
 * @since 2.0.0
 */
const as = exports.as = /*#__PURE__*/(0, _Function.dual)(2, (self, b) => map(self, () => b));
/**
 * Maps the `Some` value of this `Option` to the `void` constant value.
 *
 * This is useful when the value of the `Option` is not needed, but the presence or absence of the value is important.
 *
 * @category mapping
 * @since 2.0.0
 */
const asVoid = exports.asVoid = /*#__PURE__*/as(undefined);
const void_ = exports.void = /*#__PURE__*/some(undefined);
/**
 * Applies a function to the value of an `Option` and flattens the result, if the input is `Some`.
 *
 * @category sequencing
 * @since 2.0.0
 */
const flatMap = exports.flatMap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => isNone(self) ? none() : f(self.value));
/**
 * Executes a sequence of two `Option`s. The second `Option` can be dependent on the result of the first `Option`.
 *
 * @category sequencing
 * @since 2.0.0
 */
const andThen = exports.andThen = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => flatMap(self, a => {
  const b = (0, _Function.isFunction)(f) ? f(a) : f;
  return isOption(b) ? b : some(b);
}));
/**
 * This is `flatMap` + `fromNullable`, useful when working with optional values.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * interface Employee {
 *   company?: {
 *     address?: {
 *       street?: {
 *         name?: string
 *       }
 *     }
 *   }
 * }
 *
 * const employee1: Employee = { company: { address: { street: { name: 'high street' } } } }
 *
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.some(employee1),
 *     Option.flatMapNullable(employee => employee.company?.address?.street?.name),
 *   ),
 *   Option.some('high street')
 * )
 *
 * const employee2: Employee = { company: { address: { street: {} } } }
 *
 * assert.deepStrictEqual(
 *   pipe(
 *     Option.some(employee2),
 *     Option.flatMapNullable(employee => employee.company?.address?.street?.name),
 *   ),
 *   Option.none()
 * )
 *
 * @category sequencing
 * @since 2.0.0
 */
const flatMapNullable = exports.flatMapNullable = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => isNone(self) ? none() : fromNullable(f(self.value)));
/**
 * @category sequencing
 * @since 2.0.0
 */
const flatten = exports.flatten = /*#__PURE__*/flatMap(_Function.identity);
/**
 * @category zipping
 * @since 2.0.0
 */
const zipRight = exports.zipRight = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => flatMap(self, () => that));
/**
 * @category sequencing
 * @since 2.0.0
 */
const composeK = exports.composeK = /*#__PURE__*/(0, _Function.dual)(2, (afb, bfc) => a => flatMap(afb(a), bfc));
/**
 * Sequences the specified `that` `Option` but ignores its value.
 *
 * It is useful when we want to chain multiple operations, but only care about the result of `self`.
 *
 * @param that - The `Option` that will be ignored in the chain and discarded
 * @param self - The `Option` we care about
 *
 * @category zipping
 * @since 2.0.0
 */
const zipLeft = exports.zipLeft = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => tap(self, () => that));
/**
 * Applies the provided function `f` to the value of the `Option` if it is `Some` and returns the original `Option`
 * unless `f` returns `None`, in which case it returns `None`.
 *
 * This function is useful for performing additional computations on the value of the input `Option` without affecting its value.
 *
 * @param f - Function to apply to the value of the `Option` if it is `Some`
 * @param self - The `Option` to apply the function to
 *
 * @example
 * import { Option } from "effect"
 *
 * const getInteger = (n: number) => Number.isInteger(n) ? Option.some(n) : Option.none()
 *
 * assert.deepStrictEqual(Option.tap(Option.none(), getInteger), Option.none())
 * assert.deepStrictEqual(Option.tap(Option.some(1), getInteger), Option.some(1))
 * assert.deepStrictEqual(Option.tap(Option.some(1.14), getInteger), Option.none())
 *
 * @category sequencing
 * @since 2.0.0
 */
const tap = exports.tap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => flatMap(self, a => map(f(a), () => a)));
/**
 * @category combining
 * @since 2.0.0
 */
const product = (self, that) => isSome(self) && isSome(that) ? some([self.value, that.value]) : none();
/**
 * @category combining
 * @since 2.0.0
 */
exports.product = product;
const productMany = (self, collection) => {
  if (isNone(self)) {
    return none();
  }
  const out = [self.value];
  for (const o of collection) {
    if (isNone(o)) {
      return none();
    }
    out.push(o.value);
  }
  return some(out);
};
/**
 * Takes a structure of `Option`s and returns an `Option` of values with the same structure.
 *
 * - If a tuple is supplied, then the returned `Option` will contain a tuple with the same length.
 * - If a struct is supplied, then the returned `Option` will contain a struct with the same keys.
 * - If an iterable is supplied, then the returned `Option` will contain an array.
 *
 * @param fields - the struct of `Option`s to be sequenced.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.all([Option.some(1), Option.some(2)]), Option.some([1, 2]))
 * assert.deepStrictEqual(Option.all({ a: Option.some(1), b: Option.some("hello") }), Option.some({ a: 1, b: "hello" }))
 * assert.deepStrictEqual(Option.all({ a: Option.some(1), b: Option.none() }), Option.none())
 *
 * @category combining
 * @since 2.0.0
 */
// @ts-expect-error
exports.productMany = productMany;
const all = input => {
  if (Symbol.iterator in input) {
    const out = [];
    for (const o of input) {
      if (isNone(o)) {
        return none();
      }
      out.push(o.value);
    }
    return some(out);
  }
  const out = {};
  for (const key of Object.keys(input)) {
    const o = input[key];
    if (isNone(o)) {
      return none();
    }
    out[key] = o.value;
  }
  return some(out);
};
/**
 * Zips two `Option` values together using a provided function, returning a new `Option` of the result.
 *
 * @param self - The left-hand side of the zip operation
 * @param that - The right-hand side of the zip operation
 * @param f - The function used to combine the values of the two `Option`s
 *
 * @example
 * import { Option } from "effect"
 *
 * type Complex = [real: number, imaginary: number]
 *
 * const complex = (real: number, imaginary: number): Complex => [real, imaginary]
 *
 * assert.deepStrictEqual(Option.zipWith(Option.none(), Option.none(), complex), Option.none())
 * assert.deepStrictEqual(Option.zipWith(Option.some(1), Option.none(), complex), Option.none())
 * assert.deepStrictEqual(Option.zipWith(Option.none(), Option.some(1), complex), Option.none())
 * assert.deepStrictEqual(Option.zipWith(Option.some(1), Option.some(2), complex), Option.some([1, 2]))
 *
 * assert.deepStrictEqual(Option.zipWith(Option.some(1), complex)(Option.some(2)), Option.some([2, 1]))
 *
 * @category zipping
 * @since 2.0.0
 */
exports.all = all;
const zipWith = exports.zipWith = /*#__PURE__*/(0, _Function.dual)(3, (self, that, f) => map(product(self, that), ([a, b]) => f(a, b)));
/**
 * @category combining
 * @since 2.0.0
 */
const ap = exports.ap = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => zipWith(self, that, (f, a) => f(a)));
/**
 * Reduces an `Iterable` of `Option<A>` to a single value of type `B`, elements that are `None` are ignored.
 *
 * @param self - The Iterable of `Option<A>` to be reduced.
 * @param b - The initial value of the accumulator.
 * @param f - The reducing function that takes the current accumulator value and the unwrapped value of an `Option<A>`.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * const iterable = [Option.some(1), Option.none(), Option.some(2), Option.none()]
 * assert.deepStrictEqual(pipe(iterable, Option.reduceCompact(0, (b, a) => b + a)), 3)
 *
 * @category folding
 * @since 2.0.0
 */
const reduceCompact = exports.reduceCompact = /*#__PURE__*/(0, _Function.dual)(3, (self, b, f) => {
  let out = b;
  for (const oa of self) {
    if (isSome(oa)) {
      out = f(out, oa.value);
    }
  }
  return out;
});
/**
 * Transforms an `Option` into an `Array`.
 * If the input is `None`, an empty array is returned.
 * If the input is `Some`, the value is wrapped in an array.
 *
 * @param self - The `Option` to convert to an array.
 *
 * @example
 * import { Option } from "effect"
 *
 * assert.deepStrictEqual(Option.toArray(Option.some(1)), [1])
 * assert.deepStrictEqual(Option.toArray(Option.none()), [])
 *
 * @category conversions
 * @since 2.0.0
 */
const toArray = self => isNone(self) ? [] : [self.value];
/**
 * @category filtering
 * @since 2.0.0
 */
exports.toArray = toArray;
const partitionMap = exports.partitionMap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => {
  if (isNone(self)) {
    return [none(), none()];
  }
  const e = f(self.value);
  return either.isLeft(e) ? [some(e.left), none()] : [none(), some(e.right)];
});
/**
 * Maps over the value of an `Option` and filters out `None`s.
 *
 * Useful when in addition to filtering you also want to change the type of the `Option`.
 *
 * @param self - The `Option` to map over.
 * @param f - A function to apply to the value of the `Option`.
 *
 * @example
 * import { Option } from "effect"
 *
 * const evenNumber = (n: number) => n % 2 === 0 ? Option.some(n) : Option.none()
 *
 * assert.deepStrictEqual(Option.filterMap(Option.none(), evenNumber), Option.none())
 * assert.deepStrictEqual(Option.filterMap(Option.some(3), evenNumber), Option.none())
 * assert.deepStrictEqual(Option.filterMap(Option.some(2), evenNumber), Option.some(2))
 *
 * @category filtering
 * @since 2.0.0
 */
const filterMap = exports.filterMap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => isNone(self) ? none() : f(self.value));
/**
 * Filters an `Option` using a predicate. If the predicate is not satisfied or the `Option` is `None` returns `None`.
 *
 * If you need to change the type of the `Option` in addition to filtering, see `filterMap`.
 *
 * @param predicate - A predicate function to apply to the `Option` value.
 * @param fb - The `Option` to filter.
 *
 * @example
 * import { Option } from "effect"
 *
 * // predicate
 * const isEven = (n: number) => n % 2 === 0
 *
 * assert.deepStrictEqual(Option.filter(Option.none(), isEven), Option.none())
 * assert.deepStrictEqual(Option.filter(Option.some(3), isEven), Option.none())
 * assert.deepStrictEqual(Option.filter(Option.some(2), isEven), Option.some(2))
 *
 * // refinement
 * const isNumber = (v: unknown): v is number => typeof v === "number"
 *
 * assert.deepStrictEqual(Option.filter(Option.none(), isNumber), Option.none())
 * assert.deepStrictEqual(Option.filter(Option.some('hello'), isNumber), Option.none())
 * assert.deepStrictEqual(Option.filter(Option.some(2), isNumber), Option.some(2))
 *
 * @category filtering
 * @since 2.0.0
 */
const filter = exports.filter = /*#__PURE__*/(0, _Function.dual)(2, (self, predicate) => filterMap(self, b => predicate(b) ? option.some(b) : option.none));
/**
 * @example
 * import { Option, Number } from "effect"
 *
 * const isEquivalent = Option.getEquivalence(Number.Equivalence)
 * assert.deepStrictEqual(isEquivalent(Option.none(), Option.none()), true)
 * assert.deepStrictEqual(isEquivalent(Option.none(), Option.some(1)), false)
 * assert.deepStrictEqual(isEquivalent(Option.some(1), Option.none()), false)
 * assert.deepStrictEqual(isEquivalent(Option.some(1), Option.some(2)), false)
 * assert.deepStrictEqual(isEquivalent(Option.some(1), Option.some(1)), true)
 *
 * @category equivalence
 * @since 2.0.0
 */
const getEquivalence = isEquivalent => Equivalence.make((x, y) => isNone(x) ? isNone(y) : isNone(y) ? false : isEquivalent(x.value, y.value));
/**
 * The `Order` instance allows `Option` values to be compared with
 * `compare`, whenever there is an `Order` instance for
 * the type the `Option` contains.
 *
 * `None` is considered to be less than any `Some` value.
 *
 * @example
 * import { pipe, Option, Number } from "effect"
 *
 * const O = Option.getOrder(Number.Order)
 * assert.deepStrictEqual(O(Option.none(), Option.none()), 0)
 * assert.deepStrictEqual(O(Option.none(), Option.some(1)), -1)
 * assert.deepStrictEqual(O(Option.some(1), Option.none()), 1)
 * assert.deepStrictEqual(O(Option.some(1), Option.some(2)), -1)
 * assert.deepStrictEqual(O(Option.some(1), Option.some(1)), 0)
 *
 * @category sorting
 * @since 2.0.0
 */
exports.getEquivalence = getEquivalence;
const getOrder = O => order.make((self, that) => isSome(self) ? isSome(that) ? O(self.value, that.value) : 1 : -1);
/**
 * Lifts a binary function into `Option`.
 *
 * @param f - The function to lift.
 *
 * @category lifting
 * @since 2.0.0
 */
exports.getOrder = getOrder;
const lift2 = f => (0, _Function.dual)(2, (self, that) => zipWith(self, that, f));
/**
 * Transforms a `Predicate` function into a `Some` of the input value if the predicate returns `true` or `None`
 * if the predicate returns `false`.
 *
 * @param predicate - A `Predicate` function that takes in a value of type `A` and returns a boolean.
 *
 * @example
 * import { Option } from "effect"
 *
 * const getOption = Option.liftPredicate((n: number) => n >= 0)
 *
 * assert.deepStrictEqual(getOption(-1), Option.none())
 * assert.deepStrictEqual(getOption(1), Option.some(1))
 *
 * @category lifting
 * @since 2.0.0
 */
exports.lift2 = lift2;
const liftPredicate = exports.liftPredicate = /*#__PURE__*/(0, _Function.dual)(2, (b, predicate) => predicate(b) ? some(b) : none());
/**
 * Returns a function that checks if a `Option` contains a given value using a provided `isEquivalent` function.
 *
 * @param equivalent - An `Equivalence` instance to compare values of the `Option`.
 * @param self - The `Option` to apply the comparison to.
 * @param a - The value to compare against the `Option`.
 *
 * @example
 * import { pipe, Option, Number } from "effect"
 *
 * assert.deepStrictEqual(pipe(Option.some(2), Option.containsWith(Number.Equivalence)(2)), true)
 * assert.deepStrictEqual(pipe(Option.some(1), Option.containsWith(Number.Equivalence)(2)), false)
 * assert.deepStrictEqual(pipe(Option.none(), Option.containsWith(Number.Equivalence)(2)), false)
 *
 * @category elements
 * @since 2.0.0
 */
const containsWith = isEquivalent => (0, _Function.dual)(2, (self, a) => isNone(self) ? false : isEquivalent(self.value, a));
exports.containsWith = containsWith;
const _equivalence = /*#__PURE__*/Equal.equivalence();
/**
 * Returns a function that checks if an `Option` contains a given value using the default `Equivalence`.
 *
 * @category elements
 * @since 2.0.0
 */
const contains = exports.contains = /*#__PURE__*/containsWith(_equivalence);
/**
 * Check if a value in an `Option` type meets a certain predicate.
 *
 * @param self - The `Option` to check.
 * @param predicate - The condition to check.
 *
 * @example
 * import { pipe, Option } from "effect"
 *
 * const isEven = (n: number) => n % 2 === 0
 *
 * assert.deepStrictEqual(pipe(Option.some(2), Option.exists(isEven)), true)
 * assert.deepStrictEqual(pipe(Option.some(1), Option.exists(isEven)), false)
 * assert.deepStrictEqual(pipe(Option.none(), Option.exists(isEven)), false)
 *
 * @since 2.0.0
 */
const exists = exports.exists = /*#__PURE__*/(0, _Function.dual)(2, (self, refinement) => isNone(self) ? false : refinement(self.value));
// -------------------------------------------------------------------------------------
// do notation
// -------------------------------------------------------------------------------------
/**
 * The "do simulation" in Effect allows you to write code in a more declarative style, similar to the "do notation" in other programming languages. It provides a way to define variables and perform operations on them using functions like `bind` and `let`.
 *
 * Here's how the do simulation works:
 *
 * 1. Start the do simulation using the `Do` value
 * 2. Within the do simulation scope, you can use the `bind` function to define variables and bind them to `Option` values
 * 3. You can accumulate multiple `bind` statements to define multiple variables within the scope
 * 4. Inside the do simulation scope, you can also use the `let` function to define variables and bind them to simple values
 * 5. Regular `Option` functions like `map` and `filter` can still be used within the do simulation. These functions will receive the accumulated variables as arguments within the scope
 *
 * @see {@link Do}
 * @see {@link bind}
 * @see {@link let_ let}
 *
 * @example
 * import { Option, pipe } from "effect"
 *
 * const result = pipe(
 *   Option.Do,
 *   Option.bind("x", () => Option.some(2)),
 *   Option.bind("y", () => Option.some(3)),
 *   Option.let("sum", ({ x, y }) => x + y),
 *   Option.filter(({ x, y }) => x * y > 5)
 * )
 * assert.deepStrictEqual(result, Option.some({ x: 2, y: 3, sum: 5 }))
 *
 * @category do notation
 * @since 2.0.0
 */
const bindTo = exports.bindTo = /*#__PURE__*/doNotation.bindTo(map);
const let_ = exports.let = /*#__PURE__*/doNotation.let_(map);
/**
 * The "do simulation" in Effect allows you to write code in a more declarative style, similar to the "do notation" in other programming languages. It provides a way to define variables and perform operations on them using functions like `bind` and `let`.
 *
 * Here's how the do simulation works:
 *
 * 1. Start the do simulation using the `Do` value
 * 2. Within the do simulation scope, you can use the `bind` function to define variables and bind them to `Option` values
 * 3. You can accumulate multiple `bind` statements to define multiple variables within the scope
 * 4. Inside the do simulation scope, you can also use the `let` function to define variables and bind them to simple values
 * 5. Regular `Option` functions like `map` and `filter` can still be used within the do simulation. These functions will receive the accumulated variables as arguments within the scope
 *
 * @see {@link Do}
 * @see {@link bindTo}
 * @see {@link let_ let}
 *
 * @example
 * import { Option, pipe } from "effect"
 *
 * const result = pipe(
 *   Option.Do,
 *   Option.bind("x", () => Option.some(2)),
 *   Option.bind("y", () => Option.some(3)),
 *   Option.let("sum", ({ x, y }) => x + y),
 *   Option.filter(({ x, y }) => x * y > 5)
 * )
 * assert.deepStrictEqual(result, Option.some({ x: 2, y: 3, sum: 5 }))
 *
 * @category do notation
 * @since 2.0.0
 */
const bind = exports.bind = /*#__PURE__*/doNotation.bind(map, flatMap);
/**
 * The "do simulation" in Effect allows you to write code in a more declarative style, similar to the "do notation" in other programming languages. It provides a way to define variables and perform operations on them using functions like `bind` and `let`.
 *
 * Here's how the do simulation works:
 *
 * 1. Start the do simulation using the `Do` value
 * 2. Within the do simulation scope, you can use the `bind` function to define variables and bind them to `Option` values
 * 3. You can accumulate multiple `bind` statements to define multiple variables within the scope
 * 4. Inside the do simulation scope, you can also use the `let` function to define variables and bind them to simple values
 * 5. Regular `Option` functions like `map` and `filter` can still be used within the do simulation. These functions will receive the accumulated variables as arguments within the scope
 *
 * @see {@link bindTo}
 * @see {@link bind}
 * @see {@link let_ let}
 *
 * @example
 * import { Option, pipe } from "effect"
 *
 * const result = pipe(
 *   Option.Do,
 *   Option.bind("x", () => Option.some(2)),
 *   Option.bind("y", () => Option.some(3)),
 *   Option.let("sum", ({ x, y }) => x + y),
 *   Option.filter(({ x, y }) => x * y > 5)
 * )
 * assert.deepStrictEqual(result, Option.some({ x: 2, y: 3, sum: 5 }))
 *
 * @category do notation
 * @since 2.0.0
 */
const Do = exports.Do = /*#__PURE__*/some({});
const adapter = /*#__PURE__*/Gen.adapter();
/**
 * @category generators
 * @since 2.0.0
 */
const gen = (...args) => {
  let f;
  if (args.length === 1) {
    f = args[0];
  } else {
    f = args[1].bind(args[0]);
  }
  const iterator = f(adapter);
  let state = iterator.next();
  if (state.done) {
    return some(state.value);
  } else {
    let current = state.value;
    if (Gen.isGenKind(current)) {
      current = current.value;
    } else {
      current = Gen.yieldWrapGet(current);
    }
    if (isNone(current)) {
      return current;
    }
    while (!state.done) {
      state = iterator.next(current.value);
      if (!state.done) {
        current = state.value;
        if (Gen.isGenKind(current)) {
          current = current.value;
        } else {
          current = Gen.yieldWrapGet(current);
        }
        if (isNone(current)) {
          return current;
        }
      }
    }
    return some(state.value);
  }
};
exports.gen = gen;
//# sourceMappingURL=Option.js.map