"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.updateSomeAndGet = exports.updateSome = exports.updateAndGet = exports.update = exports.setAndGet = exports.set = exports.modifySome = exports.modify = exports.make = exports.getAndUpdateSome = exports.getAndUpdate = exports.getAndSet = exports.get = exports.changesStream = exports.changesScoped = exports.changes = exports.TSubscriptionRefTypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/stm/tSubscriptionRef.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.10.0
 * @category symbols
 */
const TSubscriptionRefTypeId = exports.TSubscriptionRefTypeId = internal.TSubscriptionRefTypeId;
/**
 * @since 3.10.0
 * @category mutations
 */
const get = exports.get = internal.get;
/**
 * @since 3.10.0
 * @category mutations
 */
const getAndSet = exports.getAndSet = internal.getAndSet;
/**
 * @since 3.10.0
 * @category mutations
 */
const getAndUpdate = exports.getAndUpdate = internal.getAndUpdate;
/**
 * @since 3.10.0
 * @category mutations
 */
const getAndUpdateSome = exports.getAndUpdateSome = internal.getAndUpdateSome;
/**
 * @since 3.10.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 3.10.0
 * @category mutations
 */
const modify = exports.modify = internal.modify;
/**
 * @since 3.10.0
 * @category mutations
 */
const modifySome = exports.modifySome = internal.modifySome;
/**
 * @since 3.10.0
 * @category mutations
 */
const set = exports.set = internal.set;
/**
 * @since 3.10.0
 * @category mutations
 */
const setAndGet = exports.setAndGet = internal.setAndGet;
/**
 * @since 3.10.0
 * @category mutations
 */
const update = exports.update = internal.update;
/**
 * @since 3.10.0
 * @category mutations
 */
const updateAndGet = exports.updateAndGet = internal.updateAndGet;
/**
 * @since 3.10.0
 * @category mutations
 */
const updateSome = exports.updateSome = internal.updateSome;
/**
 * @since 3.10.0
 * @category mutations
 */
const updateSomeAndGet = exports.updateSomeAndGet = internal.updateSomeAndGet;
/**
 * @since 3.10.0
 * @category mutations
 */
const changesScoped = exports.changesScoped = internal.changesScoped;
/**
 * @since 3.10.0
 * @category mutations
 */
const changesStream = exports.changesStream = internal.changesStream;
/**
 * @since 3.10.0
 * @category mutations
 */
const changes = self => self.changes;
exports.changes = changes;
//# sourceMappingURL=TSubscriptionRef.js.map