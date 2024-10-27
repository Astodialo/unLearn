"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.makeExecutor = exports.TypeId = exports.ProcessTypeId = exports.ProcessId = exports.ExitCode = exports.CommandExecutor = void 0;
var internal = _interopRequireWildcard(require("./internal/commandExecutor.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category tags
 */
const CommandExecutor = exports.CommandExecutor = internal.CommandExecutor;
/**
 * @since 1.0.0
 * @category symbols
 */
const ProcessTypeId = exports.ProcessTypeId = internal.ProcessTypeId;
/**
 * @since 1.0.0
 * @category constructors
 */
const ExitCode = exports.ExitCode = internal.ExitCode;
/**
 * @since 1.0.0
 * @category constructors
 */
const ProcessId = exports.ProcessId = internal.ProcessId;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeExecutor = exports.makeExecutor = internal.makeExecutor;
//# sourceMappingURL=CommandExecutor.js.map