"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.workingDirectory = exports.string = exports.streamLines = exports.stream = exports.stdout = exports.stdin = exports.stderr = exports.start = exports.runInShell = exports.pipeTo = exports.make = exports.lines = exports.isCommand = exports.flatten = exports.feed = exports.exitCode = exports.env = exports.CommandTypeId = void 0;
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var HashMap = _interopRequireWildcard(require("effect/HashMap"));
var Inspectable = _interopRequireWildcard(require("effect/Inspectable"));
var Option = _interopRequireWildcard(require("effect/Option"));
var _Pipeable = require("effect/Pipeable");
var Stream = _interopRequireWildcard(require("effect/Stream"));
var commandExecutor = _interopRequireWildcard(require("./commandExecutor.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const CommandTypeId = exports.CommandTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Command");
/** @internal */
const isCommand = u => typeof u === "object" && u != null && CommandTypeId in u;
/** @internal */
exports.isCommand = isCommand;
const env = exports.env = /*#__PURE__*/(0, _Function.dual)(2, (self, environment) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          env: HashMap.union(self.env, HashMap.fromIterable(Object.entries(environment).filter(([v]) => v !== undefined)))
        });
      }
    case "PipedCommand":
      {
        return pipeTo(env(self.left, environment), env(self.right, environment));
      }
  }
});
/** @internal */
const exitCode = self => Effect.flatMap(commandExecutor.CommandExecutor, executor => executor.exitCode(self));
/** @internal */
exports.exitCode = exitCode;
const feed = exports.feed = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => stdin(self, Stream.fromChunk(Chunk.of(new TextEncoder().encode(input)))));
/** @internal */
const flatten = self => Array.from(flattenLoop(self));
/** @internal */
exports.flatten = flatten;
const flattenLoop = self => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return Chunk.of(self);
      }
    case "PipedCommand":
      {
        return Chunk.appendAll(flattenLoop(self.left), flattenLoop(self.right));
      }
  }
};
/** @internal */
const runInShell = exports.runInShell = /*#__PURE__*/(0, _Function.dual)(2, (self, shell) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          shell
        });
      }
    case "PipedCommand":
      {
        return pipeTo(runInShell(self.left, shell), runInShell(self.right, shell));
      }
  }
});
/** @internal */
const lines = (command, encoding = "utf-8") => Effect.flatMap(commandExecutor.CommandExecutor, executor => executor.lines(command, encoding));
exports.lines = lines;
const Proto = {
  [CommandTypeId]: CommandTypeId,
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  },
  ...Inspectable.BaseProto
};
const StandardProto = {
  ...Proto,
  _tag: "StandardCommand",
  toJSON() {
    return {
      _id: "@effect/platform/Command",
      _tag: this._tag,
      command: this.command,
      args: this.args,
      env: Object.fromEntries(this.env),
      cwd: this.cwd.toJSON(),
      shell: this.shell,
      gid: this.gid.toJSON(),
      uid: this.uid.toJSON()
    };
  }
};
const makeStandard = options => Object.assign(Object.create(StandardProto), options);
const PipedProto = {
  ...Proto,
  _tag: "PipedCommand",
  toJSON() {
    return {
      _id: "@effect/platform/Command",
      _tag: this._tag,
      left: this.left.toJSON(),
      right: this.right.toJSON()
    };
  }
};
const makePiped = options => Object.assign(Object.create(PipedProto), options);
/** @internal */
const make = (command, ...args) => makeStandard({
  command,
  args,
  env: HashMap.empty(),
  cwd: Option.none(),
  shell: false,
  stdin: "pipe",
  stdout: "pipe",
  stderr: "pipe",
  gid: Option.none(),
  uid: Option.none()
});
/** @internal */
exports.make = make;
const pipeTo = exports.pipeTo = /*#__PURE__*/(0, _Function.dual)(2, (self, into) => makePiped({
  left: self,
  right: into
}));
/** @internal */
const stderr = exports.stderr = /*#__PURE__*/(0, _Function.dual)(2, (self, output) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          stderr: output
        });
      }
    // For piped commands it only makes sense to provide `stderr` for the
    // right-most command as the rest will be piped in.
    case "PipedCommand":
      {
        return makePiped({
          ...self,
          right: stderr(self.right, output)
        });
      }
  }
});
/** @internal */
const stdin = exports.stdin = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          stdin: input
        });
      }
    // For piped commands it only makes sense to provide `stdin` for the
    // left-most command as the rest will be piped in.
    case "PipedCommand":
      {
        return makePiped({
          ...self,
          left: stdin(self.left, input)
        });
      }
  }
});
/** @internal */
const stdout = exports.stdout = /*#__PURE__*/(0, _Function.dual)(2, (self, output) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          stdout: output
        });
      }
    // For piped commands it only makes sense to provide `stderr` for the
    // right-most command as the rest will be piped in.
    case "PipedCommand":
      {
        return makePiped({
          ...self,
          right: stdout(self.right, output)
        });
      }
  }
});
/** @internal */
const start = command => Effect.flatMap(commandExecutor.CommandExecutor, executor => executor.start(command));
/** @internal */
exports.start = start;
const stream = command => Stream.flatMap(commandExecutor.CommandExecutor, process => process.stream(command));
/** @internal */
exports.stream = stream;
const streamLines = command => Stream.flatMap(commandExecutor.CommandExecutor, process => process.streamLines(command));
/** @internal */
exports.streamLines = streamLines;
const string = exports.string = /*#__PURE__*/(0, _Function.dual)(args => isCommand(args[0]), (command, encoding) => Effect.flatMap(commandExecutor.CommandExecutor, executor => executor.string(command, encoding)));
/** @internal */
const workingDirectory = exports.workingDirectory = /*#__PURE__*/(0, _Function.dual)(2, (self, cwd) => {
  switch (self._tag) {
    case "StandardCommand":
      {
        return makeStandard({
          ...self,
          cwd: Option.some(cwd)
        });
      }
    case "PipedCommand":
      {
        return pipeTo(workingDirectory(self.left, cwd), workingDirectory(self.right, cwd));
      }
  }
});
//# sourceMappingURL=command.js.map