/**
 * @since 2.0.0
 */

import type { Equal } from "./Equal.js"
import type { Inspectable } from "./Inspectable.js"
import * as HS from "./internal/hashSet.js"
import type { Pipeable } from "./Pipeable.js"
import type { Predicate, Refinement } from "./Predicate.js"
import type { NoInfer } from "./Types.js"

const TypeId: unique symbol = HS.HashSetTypeId as TypeId

/**
 * @since 2.0.0
 * @category symbol
 */
export type TypeId = typeof TypeId

/**
 * @since 2.0.0
 * @category models
 */
export interface HashSet<out A> extends Iterable<A>, Equal, Pipeable, Inspectable {
  readonly [TypeId]: TypeId
}

/**
 * @since 2.0.0
 * @category refinements
 */
export const isHashSet: {
  /**
   * @since 2.0.0
   * @category refinements
   */
  <A>(u: Iterable<A>): u is HashSet<A>
  /**
   * @since 2.0.0
   * @category refinements
   */
  (u: unknown): u is HashSet<unknown>
} = HS.isHashSet

/**
 * Creates an empty `HashSet`.
 *
 * @since 2.0.0
 * @category constructors
 */
export const empty: <A = never>() => HashSet<A> = HS.empty

/**
 * Creates a new `HashSet` from an iterable collection of values.
 *
 * @since 2.0.0
 * @category constructors
 */
export const fromIterable: <A>(elements: Iterable<A>) => HashSet<A> = HS.fromIterable

/**
 * Construct a new `HashSet` from a variable number of values.
 *
 * @since 2.0.0
 * @category constructors
 */
export const make: <As extends ReadonlyArray<any>>(...elements: As) => HashSet<As[number]> = HS.make

/**
 * Checks if the specified value exists in the `HashSet`.
 *
 * @since 2.0.0
 * @category elements
 */
export const has: {
  /**
   * Checks if the specified value exists in the `HashSet`.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(value: A): (self: HashSet<A>) => boolean
  /**
   * Checks if the specified value exists in the `HashSet`.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(self: HashSet<A>, value: A): boolean
} = HS.has

/**
 * Check if a predicate holds true for some `HashSet` element.
 *
 * @since 2.0.0
 * @category elements
 */
export const some: {
  /**
   * Check if a predicate holds true for some `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(f: Predicate<A>): (self: HashSet<A>) => boolean
  /**
   * Check if a predicate holds true for some `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(self: HashSet<A>, f: Predicate<A>): boolean
} = HS.some

/**
 * Check if a predicate holds true for every `HashSet` element.
 *
 * @since 2.0.0
 * @category elements
 */
export const every: {
  /**
   * Check if a predicate holds true for every `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A, B extends A>(refinement: Refinement<NoInfer<A>, B>): (self: HashSet<A>) => self is HashSet<B>
  /**
   * Check if a predicate holds true for every `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(predicate: Predicate<A>): (self: HashSet<A>) => boolean
  /**
   * Check if a predicate holds true for every `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A, B extends A>(self: HashSet<A>, refinement: Refinement<A, B>): self is HashSet<B>
  /**
   * Check if a predicate holds true for every `HashSet` element.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(self: HashSet<A>, predicate: Predicate<A>): boolean
} = HS.every

/**
 * Returns `true` if and only if every element in the this `HashSet` is an
 * element of the second set,
 *
 * **NOTE**: the hash and equal of both sets must be the same.
 *
 * @since 2.0.0
 * @category elements
 */
export const isSubset: {
  /**
   * Returns `true` if and only if every element in the this `HashSet` is an
   * element of the second set,
   *
   * **NOTE**: the hash and equal of both sets must be the same.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(that: HashSet<A>): (self: HashSet<A>) => boolean
  /**
   * Returns `true` if and only if every element in the this `HashSet` is an
   * element of the second set,
   *
   * **NOTE**: the hash and equal of both sets must be the same.
   *
   * @since 2.0.0
   * @category elements
   */
  <A>(self: HashSet<A>, that: HashSet<A>): boolean
} = HS.isSubset

/**
 * Returns an `IterableIterator` of the values in the `HashSet`.
 *
 * @since 2.0.0
 * @category getters
 */
export const values: <A>(self: HashSet<A>) => IterableIterator<A> = HS.values

/**
 * Calculates the number of values in the `HashSet`.
 *
 * @since 2.0.0
 * @category getters
 */
export const size: <A>(self: HashSet<A>) => number = HS.size

/**
 * Marks the `HashSet` as mutable.
 *
 * @since 2.0.0
 */
export const beginMutation: <A>(self: HashSet<A>) => HashSet<A> = HS.beginMutation

/**
 * Marks the `HashSet` as immutable.
 *
 * @since 2.0.0
 */
export const endMutation: <A>(self: HashSet<A>) => HashSet<A> = HS.endMutation

/**
 * Mutates the `HashSet` within the context of the provided function.
 *
 * @since 2.0.0
 */
export const mutate: {
  /**
   * Mutates the `HashSet` within the context of the provided function.
   *
   * @since 2.0.0
   */
  <A>(f: (set: HashSet<A>) => void): (self: HashSet<A>) => HashSet<A>
  /**
   * Mutates the `HashSet` within the context of the provided function.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, f: (set: HashSet<A>) => void): HashSet<A>
} = HS.mutate

/**
 * Adds a value to the `HashSet`.
 *
 * @since 2.0.0
 */
export const add: {
  /**
   * Adds a value to the `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(value: A): (self: HashSet<A>) => HashSet<A>
  /**
   * Adds a value to the `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, value: A): HashSet<A>
} = HS.add

/**
 * Removes a value from the `HashSet`.
 *
 * @since 2.0.0
 */
export const remove: {
  /**
   * Removes a value from the `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(value: A): (self: HashSet<A>) => HashSet<A>
  /**
   * Removes a value from the `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, value: A): HashSet<A>
} = HS.remove

/**
 * Computes the set difference between this `HashSet` and the specified
 * `Iterable<A>`.
 *
 * **NOTE**: the hash and equal of the values in both the set and the iterable
 * must be the same.
 *
 * @since 2.0.0
 */
export const difference: {
  /**
   * Computes the set difference between this `HashSet` and the specified
   * `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(that: Iterable<A>): (self: HashSet<A>) => HashSet<A>
  /**
   * Computes the set difference between this `HashSet` and the specified
   * `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, that: Iterable<A>): HashSet<A>
} = HS.difference

/**
 * Returns a `HashSet` of values which are present in both this set and that
 * `Iterable<A>`.
 *
 * **NOTE**: the hash and equal of the values in both the set and the iterable
 * must be the same.
 *
 * @since 2.0.0
 */
export const intersection: {
  /**
   * Returns a `HashSet` of values which are present in both this set and that
   * `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(that: Iterable<A>): (self: HashSet<A>) => HashSet<A>
  /**
   * Returns a `HashSet` of values which are present in both this set and that
   * `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, that: Iterable<A>): HashSet<A>
} = HS.intersection

/**
 * Computes the set union `(`self` + `that`)` between this `HashSet` and the
 * specified `Iterable<A>`.
 *
 * **NOTE**: the hash and equal of the values in both the set and the iterable
 * must be the same.
 *
 * @since 2.0.0
 */
export const union: {
  /**
   * Computes the set union `(`self` + `that`)` between this `HashSet` and the
   * specified `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(that: Iterable<A>): (self: HashSet<A>) => HashSet<A>
  /**
   * Computes the set union `(`self` + `that`)` between this `HashSet` and the
   * specified `Iterable<A>`.
   *
   * **NOTE**: the hash and equal of the values in both the set and the iterable
   * must be the same.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, that: Iterable<A>): HashSet<A>
} = HS.union

/**
 * Checks if a value is present in the `HashSet`. If it is present, the value
 * will be removed from the `HashSet`, otherwise the value will be added to the
 * `HashSet`.
 *
 * @since 2.0.0
 */
export const toggle: {
  /**
   * Checks if a value is present in the `HashSet`. If it is present, the value
   * will be removed from the `HashSet`, otherwise the value will be added to the
   * `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(value: A): (self: HashSet<A>) => HashSet<A>
  /**
   * Checks if a value is present in the `HashSet`. If it is present, the value
   * will be removed from the `HashSet`, otherwise the value will be added to the
   * `HashSet`.
   *
   * @since 2.0.0
   */
  <A>(self: HashSet<A>, value: A): HashSet<A>
} = HS.toggle

/**
 * Maps over the values of the `HashSet` using the specified function.
 *
 * @since 2.0.0
 * @category mapping
 */
export const map: {
  /**
   * Maps over the values of the `HashSet` using the specified function.
   *
   * @since 2.0.0
   * @category mapping
   */
  <A, B>(f: (a: A) => B): (self: HashSet<A>) => HashSet<B>
  /**
   * Maps over the values of the `HashSet` using the specified function.
   *
   * @since 2.0.0
   * @category mapping
   */
  <A, B>(self: HashSet<A>, f: (a: A) => B): HashSet<B>
} = HS.map

/**
 * Chains over the values of the `HashSet` using the specified function.
 *
 * @since 2.0.0
 * @category sequencing
 */
export const flatMap: {
  /**
   * Chains over the values of the `HashSet` using the specified function.
   *
   * @since 2.0.0
   * @category sequencing
   */
  <A, B>(f: (a: A) => Iterable<B>): (self: HashSet<A>) => HashSet<B>
  /**
   * Chains over the values of the `HashSet` using the specified function.
   *
   * @since 2.0.0
   * @category sequencing
   */
  <A, B>(self: HashSet<A>, f: (a: A) => Iterable<B>): HashSet<B>
} = HS.flatMap

/**
 * Applies the specified function to the values of the `HashSet`.
 *
 * @since 2.0.0
 * @category traversing
 */
export const forEach: {
  /**
   * Applies the specified function to the values of the `HashSet`.
   *
   * @since 2.0.0
   * @category traversing
   */
  <A>(f: (value: A) => void): (self: HashSet<A>) => void
  /**
   * Applies the specified function to the values of the `HashSet`.
   *
   * @since 2.0.0
   * @category traversing
   */
  <A>(self: HashSet<A>, f: (value: A) => void): void
} = HS.forEach

/**
 * Reduces the specified state over the values of the `HashSet`.
 *
 * @since 2.0.0
 * @category folding
 */
export const reduce: {
  /**
   * Reduces the specified state over the values of the `HashSet`.
   *
   * @since 2.0.0
   * @category folding
   */
  <A, Z>(zero: Z, f: (accumulator: Z, value: A) => Z): (self: HashSet<A>) => Z
  /**
   * Reduces the specified state over the values of the `HashSet`.
   *
   * @since 2.0.0
   * @category folding
   */
  <A, Z>(self: HashSet<A>, zero: Z, f: (accumulator: Z, value: A) => Z): Z
} = HS.reduce

/**
 * Filters values out of a `HashSet` using the specified predicate.
 *
 * @since 2.0.0
 * @category filtering
 */
export const filter: {
  /**
   * Filters values out of a `HashSet` using the specified predicate.
   *
   * @since 2.0.0
   * @category filtering
   */
  <A, B extends A>(refinement: Refinement<NoInfer<A>, B>): (self: HashSet<A>) => HashSet<B>
  /**
   * Filters values out of a `HashSet` using the specified predicate.
   *
   * @since 2.0.0
   * @category filtering
   */
  <A>(predicate: Predicate<NoInfer<A>>): (self: HashSet<A>) => HashSet<A>
  /**
   * Filters values out of a `HashSet` using the specified predicate.
   *
   * @since 2.0.0
   * @category filtering
   */
  <A, B extends A>(self: HashSet<A>, refinement: Refinement<A, B>): HashSet<B>
  /**
   * Filters values out of a `HashSet` using the specified predicate.
   *
   * @since 2.0.0
   * @category filtering
   */
  <A>(self: HashSet<A>, predicate: Predicate<A>): HashSet<A>
} = HS.filter

/**
 * Partition the values of a `HashSet` using the specified predicate.
 *
 * If a value matches the predicate, it will be placed into the `HashSet` on the
 * right side of the resulting `Tuple`, otherwise the value will be placed into
 * the left side.
 *
 * @since 2.0.0
 * @category partitioning
 */
export const partition: {
  /**
   * Partition the values of a `HashSet` using the specified predicate.
   *
   * If a value matches the predicate, it will be placed into the `HashSet` on the
   * right side of the resulting `Tuple`, otherwise the value will be placed into
   * the left side.
   *
   * @since 2.0.0
   * @category partitioning
   */
  <A, B extends A>(
    refinement: Refinement<NoInfer<A>, B>
  ): (self: HashSet<A>) => [excluded: HashSet<Exclude<A, B>>, satisfying: HashSet<B>]
  /**
   * Partition the values of a `HashSet` using the specified predicate.
   *
   * If a value matches the predicate, it will be placed into the `HashSet` on the
   * right side of the resulting `Tuple`, otherwise the value will be placed into
   * the left side.
   *
   * @since 2.0.0
   * @category partitioning
   */
  <A>(predicate: Predicate<NoInfer<A>>): (self: HashSet<A>) => [excluded: HashSet<A>, satisfying: HashSet<A>]
  /**
   * Partition the values of a `HashSet` using the specified predicate.
   *
   * If a value matches the predicate, it will be placed into the `HashSet` on the
   * right side of the resulting `Tuple`, otherwise the value will be placed into
   * the left side.
   *
   * @since 2.0.0
   * @category partitioning
   */
  <A, B extends A>(
    self: HashSet<A>,
    refinement: Refinement<A, B>
  ): [excluded: HashSet<Exclude<A, B>>, satisfying: HashSet<B>]
  /**
   * Partition the values of a `HashSet` using the specified predicate.
   *
   * If a value matches the predicate, it will be placed into the `HashSet` on the
   * right side of the resulting `Tuple`, otherwise the value will be placed into
   * the left side.
   *
   * @since 2.0.0
   * @category partitioning
   */
  <A>(self: HashSet<A>, predicate: Predicate<A>): [excluded: HashSet<A>, satisfying: HashSet<A>]
} = HS.partition
