"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toFile = void 0;
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Logger = _interopRequireWildcard(require("effect/Logger"));
var FileSystem = _interopRequireWildcard(require("../FileSystem.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const toFile = exports.toFile = /*#__PURE__*/(0, _Function.dual)(args => Logger.isLogger(args[0]), (self, path, options) => Effect.gen(function* (_) {
  const fs = yield* _(FileSystem.FileSystem);
  const logFile = yield* _(fs.open(path, {
    flag: "a+",
    ...options
  }));
  const encoder = new TextEncoder();
  return yield* _(Logger.batched(self, options?.batchWindow ?? 1000, output => Effect.ignore(logFile.write(encoder.encode(output.join("\n") + "\n")))));
}));
//# sourceMappingURL=platformLogger.js.map