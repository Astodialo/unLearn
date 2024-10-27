"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isPlatformError = exports.TypeIdError = exports.SystemError = exports.PlatformErrorTypeId = exports.BadArgument = void 0;
var Data = _interopRequireWildcard(require("effect/Data"));
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var internal = _interopRequireWildcard(require("./internal/error.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type id
 */
const PlatformErrorTypeId = exports.PlatformErrorTypeId = internal.PlatformErrorTypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
const isPlatformError = u => Predicate.hasProperty(u, PlatformErrorTypeId);
/**
 * @since 1.0.0
 * @category error
 */
exports.isPlatformError = isPlatformError;
const TypeIdError = (typeId, tag) => {
  class Base extends Data.Error {
    _tag = tag;
  }
  ;
  Base.prototype[typeId] = typeId;
  Base.prototype.name = tag;
  return Base;
};
/**
 * @since 1.0.0
 * @category error
 */
exports.TypeIdError = TypeIdError;
const BadArgument = exports.BadArgument = internal.badArgument;
/**
 * @since 1.0.0
 * @category error
 */
const SystemError = exports.SystemError = internal.systemError;
//# sourceMappingURL=Error.js.map