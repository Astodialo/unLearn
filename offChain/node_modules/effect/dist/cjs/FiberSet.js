"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafeAdd = exports.size = exports.runtime = exports.run = exports.makeRuntime = exports.make = exports.join = exports.isFiberSet = exports.clear = exports.add = exports.TypeId = void 0;
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberId = _interopRequireWildcard(require("effect/FiberId"));
var Cause = _interopRequireWildcard(require("./Cause.js"));
var Deferred = _interopRequireWildcard(require("./Deferred.js"));
var Exit = _interopRequireWildcard(require("./Exit.js"));
var Fiber = _interopRequireWildcard(require("./Fiber.js"));
var FiberRef = _interopRequireWildcard(require("./FiberRef.js"));
var _Function = require("./Function.js");
var HashSet = _interopRequireWildcard(require("./HashSet.js"));
var Inspectable = _interopRequireWildcard(require("./Inspectable.js"));
var Iterable = _interopRequireWildcard(require("./Iterable.js"));
var Option = _interopRequireWildcard(require("./Option.js"));
var _Pipeable = require("./Pipeable.js");
var Predicate = _interopRequireWildcard(require("./Predicate.js"));
var Runtime = _interopRequireWildcard(require("./Runtime.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 2.0.0
 */

/**
 * @since 2.0.0
 * @categories type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("effect/FiberSet");
/**
 * @since 2.0.0
 * @categories refinements
 */
const isFiberSet = u => Predicate.hasProperty(u, TypeId);
exports.isFiberSet = isFiberSet;
const Proto = {
  [TypeId]: TypeId,
  [Symbol.iterator]() {
    if (this.state._tag === "Closed") {
      return Iterable.empty();
    }
    return this.state.backing[Symbol.iterator]();
  },
  toString() {
    return Inspectable.format(this.toJSON());
  },
  toJSON() {
    return {
      _id: "FiberMap",
      state: this.state
    };
  },
  [Inspectable.NodeInspectSymbol]() {
    return this.toJSON();
  },
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  }
};
const unsafeMake = (backing, deferred) => {
  const self = Object.create(Proto);
  self.state = {
    _tag: "Open",
    backing
  };
  self.deferred = deferred;
  return self;
};
/**
 * A FiberSet can be used to store a collection of fibers.
 * When the associated Scope is closed, all fibers in the set will be interrupted.
 *
 * You can add fibers to the set using `FiberSet.add` or `FiberSet.run`, and the fibers will
 * be automatically removed from the FiberSet when they complete.
 *
 * @example
 * import { Effect, FiberSet } from "effect"
 *
 * Effect.gen(function*(_) {
 *   const set = yield* _(FiberSet.make())
 *
 *   // run some effects and add the fibers to the set
 *   yield* _(FiberSet.run(set, Effect.never))
 *   yield* _(FiberSet.run(set, Effect.never))
 *
 *   yield* _(Effect.sleep(1000))
 * }).pipe(
 *   Effect.scoped // The fibers will be interrupted when the scope is closed
 * )
 *
 * @since 2.0.0
 * @categories constructors
 */
const make = () => Effect.acquireRelease(Effect.map(Deferred.make(), deferred => unsafeMake(new Set(), deferred)), set => Effect.zipRight(clear(set), Effect.suspend(() => {
  set.state = {
    _tag: "Closed"
  };
  return Deferred.done(set.deferred, Exit.void);
})));
/**
 * Create an Effect run function that is backed by a FiberSet.
 *
 * @since 2.0.0
 * @categories constructors
 */
exports.make = make;
const makeRuntime = () => Effect.flatMap(make(), self => runtime(self)());
exports.makeRuntime = makeRuntime;
const internalFiberIdId = -1;
const internalFiberId = /*#__PURE__*/FiberId.make(internalFiberIdId, 0);
const isInternalInterruption = /*#__PURE__*/Cause.reduceWithContext(undefined, {
  emptyCase: _Function.constFalse,
  failCase: _Function.constFalse,
  dieCase: _Function.constFalse,
  interruptCase: (_, fiberId) => HashSet.has(FiberId.ids(fiberId), internalFiberIdId),
  sequentialCase: (_, left, right) => left || right,
  parallelCase: (_, left, right) => left || right
});
/**
 * Add a fiber to the FiberSet. When the fiber completes, it will be removed.
 *
 * @since 2.0.0
 * @categories combinators
 */
const unsafeAdd = exports.unsafeAdd = /*#__PURE__*/(0, _Function.dual)(args => isFiberSet(args[0]), (self, fiber, options) => {
  if (self.state._tag === "Closed") {
    fiber.unsafeInterruptAsFork(FiberId.combine(options?.interruptAs ?? FiberId.none, internalFiberId));
    return;
  } else if (self.state.backing.has(fiber)) {
    return;
  }
  ;
  fiber.setFiberRef(FiberRef.unhandledErrorLogLevel, Option.none());
  self.state.backing.add(fiber);
  fiber.addObserver(exit => {
    if (self.state._tag === "Closed") {
      return;
    }
    self.state.backing.delete(fiber);
    if (Exit.isFailure(exit) && (options?.propagateInterruption === true ? !isInternalInterruption(exit.cause) : !Cause.isInterruptedOnly(exit.cause))) {
      Deferred.unsafeDone(self.deferred, exit);
    }
  });
});
/**
 * Add a fiber to the FiberSet. When the fiber completes, it will be removed.
 *
 * @since 2.0.0
 * @categories combinators
 */
const add = exports.add = /*#__PURE__*/(0, _Function.dual)(args => isFiberSet(args[0]), (self, fiber, options) => Effect.fiberIdWith(fiberId => Effect.sync(() => unsafeAdd(self, fiber, {
  ...options,
  interruptAs: fiberId
}))));
/**
 * @since 2.0.0
 * @categories combinators
 */
const clear = self => Effect.withFiberRuntime(clearFiber => {
  if (self.state._tag === "Closed") {
    return Effect.void;
  }
  return Effect.forEach(self.state.backing, fiber =>
  // will be removed by the observer
  Fiber.interruptAs(fiber, FiberId.combine(clearFiber.id(), internalFiberId)));
});
/**
 * Fork an Effect and add the forked fiber to the FiberSet.
 * When the fiber completes, it will be removed from the FiberSet.
 *
 * @since 2.0.0
 * @categories combinators
 */
exports.clear = clear;
const run = function () {
  const self = arguments[0];
  if (!Effect.isEffect(arguments[1])) {
    const options = arguments[1];
    return effect => Effect.suspend(() => {
      if (self.state._tag === "Closed") {
        return Effect.interrupt;
      }
      return Effect.uninterruptibleMask(restore => Effect.tap(restore(Effect.forkDaemon(effect)), fiber => add(self, fiber, options)));
    });
  }
  const effect = arguments[1];
  const options = arguments[2];
  return Effect.suspend(() => {
    if (self.state._tag === "Closed") {
      return Effect.interrupt;
    }
    return Effect.uninterruptibleMask(restore => Effect.tap(restore(Effect.forkDaemon(effect)), fiber => add(self, fiber, options)));
  });
};
/**
 * Capture a Runtime and use it to fork Effect's, adding the forked fibers to the FiberSet.
 *
 * @example
 * import { Context, Effect, FiberSet } from "effect"
 *
 * interface Users {
 *   readonly _: unique symbol
 * }
 * const Users = Context.GenericTag<Users, {
 *    getAll: Effect.Effect<Array<unknown>>
 * }>("Users")
 *
 * Effect.gen(function*(_) {
 *   const set = yield* _(FiberSet.make())
 *   const run = yield* _(FiberSet.runtime(set)<Users>())
 *
 *   // run some effects and add the fibers to the set
 *   run(Effect.andThen(Users, _ => _.getAll))
 * }).pipe(
 *   Effect.scoped // The fibers will be interrupted when the scope is closed
 * )
 *
 * @since 2.0.0
 * @categories combinators
 */
exports.run = run;
const runtime = self => () => Effect.map(Effect.runtime(), runtime => {
  const runFork = Runtime.runFork(runtime);
  return (effect, options) => {
    const fiber = runFork(effect, options);
    unsafeAdd(self, fiber);
    return fiber;
  };
});
/**
 * @since 2.0.0
 * @categories combinators
 */
exports.runtime = runtime;
const size = self => Effect.sync(() => self.state._tag === "Closed" ? 0 : self.state.backing.size);
/**
 * Join all fibers in the FiberSet. If any of the Fiber's in the set terminate with a failure,
 * the returned Effect will terminate with the first failure that occurred.
 *
 * @since 2.0.0
 * @categories combinators
 * @example
 * import { Effect, FiberSet } from "effect";
 *
 * Effect.gen(function* (_) {
 *   const set = yield* _(FiberSet.make());
 *   yield* _(FiberSet.add(set, Effect.runFork(Effect.fail("error"))));
 *
 *   // parent fiber will fail with "error"
 *   yield* _(FiberSet.join(set));
 * });
 */
exports.size = size;
const join = self => Deferred.await(self.deferred);
exports.join = join;
//# sourceMappingURL=FiberSet.js.map