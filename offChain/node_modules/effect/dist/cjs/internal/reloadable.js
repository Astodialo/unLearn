"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.reloadableTag = exports.reloadFork = exports.reload = exports.manual = exports.get = exports.autoFromConfig = exports.auto = exports.ReloadableTypeId = void 0;
var Context = _interopRequireWildcard(require("../Context.js"));
var _Function = require("../Function.js");
var effect = _interopRequireWildcard(require("./core-effect.js"));
var core = _interopRequireWildcard(require("./core.js"));
var fiberRuntime = _interopRequireWildcard(require("./fiberRuntime.js"));
var _layer = _interopRequireWildcard(require("./layer.js"));
var _schedule = _interopRequireWildcard(require("./schedule.js"));
var scopedRef = _interopRequireWildcard(require("./scopedRef.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const ReloadableSymbolKey = "effect/Reloadable";
/** @internal */
const ReloadableTypeId = exports.ReloadableTypeId = /*#__PURE__*/Symbol.for(ReloadableSymbolKey);
const reloadableVariance = {
  /* c8 ignore next */
  _A: _ => _
};
/** @internal */
const auto = (tag, options) => _layer.scoped(reloadableTag(tag), (0, _Function.pipe)(_layer.build(manual(tag, {
  layer: options.layer
})), core.map(Context.unsafeGet(reloadableTag(tag))), core.tap(reloadable => fiberRuntime.acquireRelease((0, _Function.pipe)(reloadable.reload, effect.ignoreLogged, _schedule.schedule_Effect(options.schedule), fiberRuntime.forkDaemon), core.interruptFiber))));
/** @internal */
exports.auto = auto;
const autoFromConfig = (tag, options) => _layer.scoped(reloadableTag(tag), (0, _Function.pipe)(core.context(), core.flatMap(env => (0, _Function.pipe)(_layer.build(auto(tag, {
  layer: options.layer,
  schedule: options.scheduleFromConfig(env)
})), core.map(Context.unsafeGet(reloadableTag(tag)))))));
/** @internal */
exports.autoFromConfig = autoFromConfig;
const get = tag => core.flatMap(reloadableTag(tag), reloadable => scopedRef.get(reloadable.scopedRef));
/** @internal */
exports.get = get;
const manual = (tag, options) => _layer.scoped(reloadableTag(tag), (0, _Function.pipe)(core.context(), core.flatMap(env => (0, _Function.pipe)(scopedRef.fromAcquire((0, _Function.pipe)(_layer.build(options.layer), core.map(Context.unsafeGet(tag)))), core.map(ref => ({
  [ReloadableTypeId]: reloadableVariance,
  scopedRef: ref,
  reload: (0, _Function.pipe)(scopedRef.set(ref, (0, _Function.pipe)(_layer.build(options.layer), core.map(Context.unsafeGet(tag)))), core.provideContext(env))
}))))));
/** @internal */
exports.manual = manual;
const reloadableTag = tag => {
  return Context.GenericTag(`effect/Reloadable<${tag.key}>`);
};
/** @internal */
exports.reloadableTag = reloadableTag;
const reload = tag => core.flatMap(reloadableTag(tag), reloadable => reloadable.reload);
/** @internal */
exports.reload = reload;
const reloadFork = tag => core.flatMap(reloadableTag(tag), reloadable => (0, _Function.pipe)(reloadable.reload, effect.ignoreLogged, fiberRuntime.forkDaemon, core.asVoid));
exports.reloadFork = reloadFork;
//# sourceMappingURL=reloadable.js.map