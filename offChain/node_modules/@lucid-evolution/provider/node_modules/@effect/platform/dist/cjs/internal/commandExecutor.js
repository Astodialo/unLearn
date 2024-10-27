"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.makeExecutor = exports.TypeId = exports.ProcessTypeId = exports.ProcessId = exports.ExitCode = exports.CommandExecutor = void 0;
var Brand = _interopRequireWildcard(require("effect/Brand"));
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var _Context = require("effect/Context");
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Sink = _interopRequireWildcard(require("effect/Sink"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/CommandExecutor");
/** @internal */
const ProcessTypeId = exports.ProcessTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Process");
/** @internal */
const ExitCode = exports.ExitCode = /*#__PURE__*/Brand.nominal();
/** @internal */
const ProcessId = exports.ProcessId = /*#__PURE__*/Brand.nominal();
/** @internal */
const CommandExecutor = exports.CommandExecutor = /*#__PURE__*/(0, _Context.GenericTag)("@effect/platform/CommandExecutor");
/** @internal */
const makeExecutor = start => {
  const stream = command => Stream.unwrapScoped(Effect.map(start(command), process => process.stdout));
  const streamLines = (command, encoding) => {
    const decoder = new TextDecoder(encoding);
    return Stream.splitLines(Stream.mapChunks(stream(command), Chunk.map(bytes => decoder.decode(bytes))));
  };
  return {
    [TypeId]: TypeId,
    start,
    exitCode: command => Effect.scoped(Effect.flatMap(start(command), process => process.exitCode)),
    stream,
    string: (command, encoding = "utf-8") => {
      const decoder = new TextDecoder(encoding);
      return (0, _Function.pipe)(start(command), Effect.flatMap(process => Stream.run(process.stdout, collectUint8Array)), Effect.map(bytes => decoder.decode(bytes)), Effect.scoped);
    },
    lines: (command, encoding = "utf-8") => {
      return (0, _Function.pipe)(streamLines(command, encoding), Stream.runCollect, Effect.map(Chunk.toReadonlyArray));
    },
    streamLines
  };
};
exports.makeExecutor = makeExecutor;
const collectUint8Array = /*#__PURE__*/Sink.foldLeftChunks( /*#__PURE__*/new Uint8Array(), (bytes, chunk) => Chunk.reduce(chunk, bytes, (acc, curr) => {
  const newArray = new Uint8Array(acc.length + curr.length);
  newArray.set(acc);
  newArray.set(curr, acc.length);
  return newArray;
}));
//# sourceMappingURL=commandExecutor.js.map