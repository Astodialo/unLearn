import * as Context from "../Context.js";
import * as Duration from "../Duration.js";
import { dual, identity } from "../Function.js";
import * as MutableHashMap from "../MutableHashMap.js";
import { pipeArguments } from "../Pipeable.js";
import * as coreEffect from "./core-effect.js";
import * as core from "./core.js";
import * as circular from "./effect/circular.js";
import * as fiberRuntime from "./fiberRuntime.js";
/** @internal */
export const TypeId = /*#__PURE__*/Symbol.for("effect/RcMap");
const variance = {
  _K: identity,
  _A: identity,
  _E: identity
};
class RcMapImpl {
  lookup;
  context;
  scope;
  idleTimeToLive;
  capacity;
  [TypeId];
  state = {
    _tag: "Open",
    map: /*#__PURE__*/MutableHashMap.empty()
  };
  semaphore = /*#__PURE__*/circular.unsafeMakeSemaphore(1);
  constructor(lookup, context, scope, idleTimeToLive, capacity) {
    this.lookup = lookup;
    this.context = context;
    this.scope = scope;
    this.idleTimeToLive = idleTimeToLive;
    this.capacity = capacity;
    this[TypeId] = variance;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
}
/** @internal */
export const make = options => core.withFiberRuntime(fiber => {
  const context = fiber.getFiberRef(core.currentContext);
  const scope = Context.get(context, fiberRuntime.scopeTag);
  const self = new RcMapImpl(options.lookup, context, scope, options.idleTimeToLive ? Duration.decode(options.idleTimeToLive) : undefined, Math.max(options.capacity ?? Number.POSITIVE_INFINITY, 0));
  return core.as(scope.addFinalizer(() => core.suspend(() => {
    if (self.state._tag === "Closed") {
      return core.void;
    }
    const map = self.state.map;
    self.state = {
      _tag: "Closed"
    };
    return core.forEachSequentialDiscard(map, ([, entry]) => core.scopeClose(entry.scope, core.exitVoid)).pipe(core.tap(() => {
      MutableHashMap.clear(map);
    }), self.semaphore.withPermits(1));
  })), self);
});
/** @internal */
export const get = /*#__PURE__*/dual(2, (self_, key) => {
  const self = self_;
  return core.uninterruptibleMask(restore => core.suspend(() => {
    if (self.state._tag === "Closed") {
      return core.interrupt;
    }
    const state = self.state;
    const o = MutableHashMap.get(state.map, key);
    if (o._tag === "Some") {
      const entry = o.value;
      entry.refCount++;
      return entry.fiber ? core.as(core.interruptFiber(entry.fiber), entry) : core.succeed(entry);
    } else if (Number.isFinite(self.capacity) && MutableHashMap.size(self.state.map) >= self.capacity) {
      return core.fail(new core.ExceededCapacityException(`RcMap attempted to exceed capacity of ${self.capacity}`));
    }
    const acquire = self.lookup(key);
    return fiberRuntime.scopeMake().pipe(coreEffect.bindTo("scope"), coreEffect.bind("deferred", () => core.deferredMake()), core.tap(({
      deferred,
      scope
    }) => restore(core.fiberRefLocally(acquire, core.currentContext, Context.add(self.context, fiberRuntime.scopeTag, scope))).pipe(core.exit, core.flatMap(exit => core.deferredDone(deferred, exit)), circular.forkIn(scope))), core.map(({
      deferred,
      scope
    }) => {
      const entry = {
        deferred,
        scope,
        fiber: undefined,
        refCount: 1
      };
      MutableHashMap.set(state.map, key, entry);
      return entry;
    }));
  }).pipe(self.semaphore.withPermits(1), coreEffect.bindTo("entry"), coreEffect.bind("scope", () => fiberRuntime.scopeTag), core.tap(({
    entry,
    scope
  }) => scope.addFinalizer(() => core.suspend(() => {
    entry.refCount--;
    if (entry.refCount > 0) {
      return core.void;
    } else if (self.idleTimeToLive === undefined) {
      if (self.state._tag === "Open") {
        MutableHashMap.remove(self.state.map, key);
      }
      return core.scopeClose(entry.scope, core.exitVoid);
    }
    return coreEffect.sleep(self.idleTimeToLive).pipe(core.interruptible, core.zipRight(core.suspend(() => {
      if (self.state._tag === "Open" && entry.refCount === 0) {
        MutableHashMap.remove(self.state.map, key);
        return core.scopeClose(entry.scope, core.exitVoid);
      }
      return core.void;
    })), fiberRuntime.ensuring(core.sync(() => {
      entry.fiber = undefined;
    })), circular.forkIn(self.scope), core.tap(fiber => {
      entry.fiber = fiber;
    }), self.semaphore.withPermits(1));
  }))), core.flatMap(({
    entry
  }) => restore(core.deferredAwait(entry.deferred)))));
});
/** @internal */
export const keys = self => {
  const impl = self;
  return core.suspend(() => impl.state._tag === "Closed" ? core.interrupt : core.succeed(MutableHashMap.keys(impl.state.map)));
};
//# sourceMappingURL=rcMap.js.map