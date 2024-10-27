"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.layer = exports.TypeId = exports.Path = void 0;
var internal = _interopRequireWildcard(require("./internal/path.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category tag
 */
const Path = exports.Path = internal.Path;
/**
 * An implementation of the Path interface that can be used in all environments
 * (including browsers).
 *
 * It uses the POSIX standard for paths.
 *
 * @since 1.0.0
 * @category layer
 */
const layer = exports.layer = internal.layer;
//# sourceMappingURL=Path.js.map