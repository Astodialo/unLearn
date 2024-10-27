"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafeMakeCollector = exports.schema = exports.makeCollector = exports.addAll = exports.Uint8Array = exports.MessagePort = exports.ImageData = exports.Collector = void 0;
var ParseResult = _interopRequireWildcard(require("@effect/schema/ParseResult"));
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Option = _interopRequireWildcard(require("effect/Option"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category tags
 */
class Collector extends /*#__PURE__*/Context.Tag("@effect/platform/Transferable/Collector")() {}
/**
 * @since 1.0.0
 * @category constructors
 */
exports.Collector = Collector;
const unsafeMakeCollector = () => {
  const tranferables = [];
  const unsafeAddAll = transfers => {
    for (const transfer of transfers) {
      tranferables.push(transfer);
    }
  };
  const unsafeRead = () => tranferables;
  const unsafeClear = () => {
    tranferables.length = 0;
  };
  return Collector.of({
    unsafeAddAll,
    addAll: transferables => Effect.sync(() => unsafeAddAll(transferables)),
    unsafeRead,
    read: Effect.sync(unsafeRead),
    unsafeClear,
    clear: Effect.sync(unsafeClear)
  });
};
/**
 * @since 1.0.0
 * @category constructors
 */
exports.unsafeMakeCollector = unsafeMakeCollector;
const makeCollector = exports.makeCollector = /*#__PURE__*/Effect.sync(unsafeMakeCollector);
/**
 * @since 1.0.0
 * @category accessors
 */
const addAll = tranferables => Effect.flatMap(Effect.serviceOption(Collector), Option.match({
  onNone: () => Effect.void,
  onSome: _ => _.addAll(tranferables)
}));
/**
 * @since 1.0.0
 * @category schema
 */
exports.addAll = addAll;
const schema = exports.schema = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => Schema.transformOrFail(Schema.encodedSchema(self), self, {
  strict: true,
  decode: ParseResult.succeed,
  encode: i => Effect.as(addAll(f(i)), i)
}));
/**
 * @since 1.0.0
 * @category schema
 */
const ImageData = exports.ImageData = /*#__PURE__*/schema(Schema.Any, _ => [_.data.buffer]);
/**
 * @since 1.0.0
 * @category schema
 */
const MessagePort = exports.MessagePort = /*#__PURE__*/schema(Schema.Any, _ => [_]);
/**
 * @since 1.0.0
 * @category schema
 */
const Uint8Array = exports.Uint8Array = /*#__PURE__*/schema(Schema.Uint8ArrayFromSelf, _ => [_.buffer]);
//# sourceMappingURL=Transferable.js.map