"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.make = exports.keys = exports.get = exports.TypeId = void 0;
var Context = _interopRequireWildcard(require("../Context.js"));
var Duration = _interopRequireWildcard(require("../Duration.js"));
var _Function = require("../Function.js");
var MutableHashMap = _interopRequireWildcard(require("../MutableHashMap.js"));
var _Pipeable = require("../Pipeable.js");
var coreEffect = _interopRequireWildcard(require("./core-effect.js"));
var core = _interopRequireWildcard(require("./core.js"));
var circular = _interopRequireWildcard(require("./effect/circular.js"));
var fiberRuntime = _interopRequireWildcard(require("./fiberRuntime.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("effect/RcMap");
const variance = {
  _K: _Function.identity,
  _A: _Function.identity,
  _E: _Function.identity
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
    return (0, _Pipeable.pipeArguments)(this, arguments);
  }
}
/** @internal */
const make = options => core.withFiberRuntime(fiber => {
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
exports.make = make;
const get = exports.get = /*#__PURE__*/(0, _Function.dual)(2, (self_, key) => {
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
const keys = self => {
  const impl = self;
  return core.suspend(() => impl.state._tag === "Closed" ? core.interrupt : core.succeed(MutableHashMap.keys(impl.state.map)));
};
exports.keys = keys;
//# sourceMappingURL=rcMap.js.map