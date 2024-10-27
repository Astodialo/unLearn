"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.set = exports.modifyEffect = exports.modify = exports.make = exports.get = exports.SubscriptionRefTypeId = void 0;
var Effect = _interopRequireWildcard(require("../Effect.js"));
var Effectable = _interopRequireWildcard(require("../Effectable.js"));
var _Function = require("../Function.js");
var PubSub = _interopRequireWildcard(require("../PubSub.js"));
var Readable = _interopRequireWildcard(require("../Readable.js"));
var Ref = _interopRequireWildcard(require("../Ref.js"));
var Subscribable = _interopRequireWildcard(require("../Subscribable.js"));
var Synchronized = _interopRequireWildcard(require("../SynchronizedRef.js"));
var _circular = _interopRequireWildcard(require("./effect/circular.js"));
var _ref = _interopRequireWildcard(require("./ref.js"));
var stream = _interopRequireWildcard(require("./stream.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const SubscriptionRefSymbolKey = "effect/SubscriptionRef";
/** @internal */
const SubscriptionRefTypeId = exports.SubscriptionRefTypeId = /*#__PURE__*/Symbol.for(SubscriptionRefSymbolKey);
const subscriptionRefVariance = {
  /* c8 ignore next */
  _A: _ => _
};
/** @internal */
class SubscriptionRefImpl extends Effectable.Class {
  ref;
  pubsub;
  semaphore;
  [Readable.TypeId] = Readable.TypeId;
  [Subscribable.TypeId] = Subscribable.TypeId;
  [Ref.RefTypeId] = _ref.refVariance;
  [Synchronized.SynchronizedRefTypeId] = _circular.synchronizedVariance;
  [SubscriptionRefTypeId] = subscriptionRefVariance;
  constructor(ref, pubsub, semaphore) {
    super();
    this.ref = ref;
    this.pubsub = pubsub;
    this.semaphore = semaphore;
    this.get = Ref.get(this.ref);
  }
  commit() {
    return this.get;
  }
  get;
  get changes() {
    return (0, _Function.pipe)(Ref.get(this.ref), Effect.flatMap(a => Effect.map(stream.fromPubSub(this.pubsub, {
      scoped: true
    }), s => stream.concat(stream.make(a), s))), this.semaphore.withPermits(1), stream.unwrapScoped);
  }
  modify(f) {
    return this.modifyEffect(a => Effect.succeed(f(a)));
  }
  modifyEffect(f) {
    return (0, _Function.pipe)(Ref.get(this.ref), Effect.flatMap(f), Effect.flatMap(([b, a]) => (0, _Function.pipe)(Ref.set(this.ref, a), Effect.as(b), Effect.zipLeft(PubSub.publish(this.pubsub, a)))), this.semaphore.withPermits(1));
  }
}
/** @internal */
const get = self => Ref.get(self.ref);
/** @internal */
exports.get = get;
const make = value => (0, _Function.pipe)(Effect.all([PubSub.unbounded(), Ref.make(value), Effect.makeSemaphore(1)]), Effect.map(([pubsub, ref, semaphore]) => new SubscriptionRefImpl(ref, pubsub, semaphore)));
/** @internal */
exports.make = make;
const modify = exports.modify = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => self.modify(f));
/** @internal */
const modifyEffect = exports.modifyEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => self.modifyEffect(f));
/** @internal */
const set = exports.set = /*#__PURE__*/(0, _Function.dual)(2, (self, value) => (0, _Function.pipe)(Ref.set(self.ref, value), Effect.zipLeft(PubSub.publish(self.pubsub, value)), self.semaphore.withPermits(1)));
//# sourceMappingURL=subscriptionRef.js.map