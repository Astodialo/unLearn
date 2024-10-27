"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Terminal = exports.QuitException = void 0;
var _Data = require("effect/Data");
var InternalTerminal = _interopRequireWildcard(require("./internal/terminal.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * A `QuitException` represents an exception that occurs when a user attempts to
 * quit out of a `Terminal` prompt for input (usually by entering `ctrl`+`c`).
 *
 * @since 1.0.0
 * @category model
 */
class QuitException extends /*#__PURE__*/(0, _Data.TaggedError)("QuitException") {}
/**
 * @since 1.0.0
 * @category tag
 */
exports.QuitException = QuitException;
const Terminal = exports.Terminal = InternalTerminal.tag;
//# sourceMappingURL=Terminal.js.map