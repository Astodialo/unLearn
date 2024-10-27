"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.test = void 0;
var Cause = _interopRequireWildcard(require("../Cause.js"));
var _Function = require("../Function.js");
var HashMap = _interopRequireWildcard(require("../HashMap.js"));
var List = _interopRequireWildcard(require("../List.js"));
var core = _interopRequireWildcard(require("./core.js"));
var _fiberId = _interopRequireWildcard(require("./fiberId.js"));
var fiberRefs = _interopRequireWildcard(require("./fiberRefs.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const test = exports.test = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => self.log({
  fiberId: _fiberId.none,
  logLevel: core.logLevelInfo,
  message: input,
  cause: Cause.empty,
  context: fiberRefs.empty(),
  spans: List.empty(),
  annotations: HashMap.empty(),
  date: new Date()
}));
//# sourceMappingURL=logger-circular.js.map