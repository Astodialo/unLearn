"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.tag = exports.makeNoop = exports.make = exports.layerNoop = exports.TiB = exports.Size = exports.PiB = exports.MiB = exports.KiB = exports.GiB = void 0;
var Channel = _interopRequireWildcard(require("effect/Channel"));
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var _Context = require("effect/Context");
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Sink = _interopRequireWildcard(require("effect/Sink"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
var Error = _interopRequireWildcard(require("../Error.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const tag = exports.tag = /*#__PURE__*/(0, _Context.GenericTag)("@effect/platform/FileSystem");
/** @internal */
const Size = bytes => typeof bytes === "bigint" ? bytes : BigInt(bytes);
/** @internal */
exports.Size = Size;
const KiB = n => Size(n * 1024);
/** @internal */
exports.KiB = KiB;
const MiB = n => Size(n * 1024 * 1024);
/** @internal */
exports.MiB = MiB;
const GiB = n => Size(n * 1024 * 1024 * 1024);
/** @internal */
exports.GiB = GiB;
const TiB = n => Size(n * 1024 * 1024 * 1024 * 1024);
exports.TiB = TiB;
const bigint1024 = /*#__PURE__*/BigInt(1024);
const bigintPiB = bigint1024 * bigint1024 * bigint1024 * bigint1024 * bigint1024;
/** @internal */
const PiB = n => Size(BigInt(n) * bigintPiB);
/** @internal */
exports.PiB = PiB;
const make = impl => {
  return tag.of({
    ...impl,
    exists: path => (0, _Function.pipe)(impl.access(path), Effect.as(true), Effect.catchTag("SystemError", e => e.reason === "NotFound" ? Effect.succeed(false) : Effect.fail(e))),
    readFileString: (path, encoding) => Effect.tryMap(impl.readFile(path), {
      try: _ => new TextDecoder(encoding).decode(_),
      catch: () => Error.BadArgument({
        module: "FileSystem",
        method: "readFileString",
        message: "invalid encoding"
      })
    }),
    stream: (path, options) => (0, _Function.pipe)(impl.open(path, {
      flag: "r"
    }), options?.offset ? Effect.tap(file => file.seek(options.offset, "start")) : _Function.identity, Effect.map(file => stream(file, options)), Stream.unwrapScoped),
    sink: (path, options) => (0, _Function.pipe)(impl.open(path, {
      flag: "w",
      ...options
    }), Effect.map(file => Sink.forEach(_ => file.writeAll(_))), Sink.unwrapScoped),
    writeFileString: (path, data, options) => Effect.flatMap(Effect.try({
      try: () => new TextEncoder().encode(data),
      catch: () => Error.BadArgument({
        module: "FileSystem",
        method: "writeFileString",
        message: "could not encode string"
      })
    }), _ => impl.writeFile(path, _, options))
  });
};
exports.make = make;
const notFound = (method, path) => Error.SystemError({
  module: "FileSystem",
  method,
  reason: "NotFound",
  message: "No such file or directory",
  pathOrDescriptor: path
});
/** @internal */
const makeNoop = fileSystem => {
  return {
    access(path) {
      return Effect.fail(notFound("access", path));
    },
    chmod(path) {
      return Effect.fail(notFound("chmod", path));
    },
    chown(path) {
      return Effect.fail(notFound("chown", path));
    },
    copy(path) {
      return Effect.fail(notFound("copy", path));
    },
    copyFile(path) {
      return Effect.fail(notFound("copyFile", path));
    },
    exists() {
      return Effect.succeed(false);
    },
    link(path) {
      return Effect.fail(notFound("link", path));
    },
    makeDirectory() {
      return Effect.die("not implemented");
    },
    makeTempDirectory() {
      return Effect.die("not implemented");
    },
    makeTempDirectoryScoped() {
      return Effect.die("not implemented");
    },
    makeTempFile() {
      return Effect.die("not implemented");
    },
    makeTempFileScoped() {
      return Effect.die("not implemented");
    },
    open(path) {
      return Effect.fail(notFound("open", path));
    },
    readDirectory(path) {
      return Effect.fail(notFound("readDirectory", path));
    },
    readFile(path) {
      return Effect.fail(notFound("readFile", path));
    },
    readFileString(path) {
      return Effect.fail(notFound("readFileString", path));
    },
    readLink(path) {
      return Effect.fail(notFound("readLink", path));
    },
    realPath(path) {
      return Effect.fail(notFound("realPath", path));
    },
    remove() {
      return Effect.void;
    },
    rename(oldPath) {
      return Effect.fail(notFound("rename", oldPath));
    },
    sink(path) {
      return Sink.fail(notFound("sink", path));
    },
    stat(path) {
      return Effect.fail(notFound("stat", path));
    },
    stream(path) {
      return Stream.fail(notFound("stream", path));
    },
    symlink(fromPath) {
      return Effect.fail(notFound("symlink", fromPath));
    },
    truncate(path) {
      return Effect.fail(notFound("truncate", path));
    },
    utimes(path) {
      return Effect.fail(notFound("utimes", path));
    },
    watch(path) {
      return Stream.fail(notFound("watch", path));
    },
    writeFile(path) {
      return Effect.fail(notFound("writeFile", path));
    },
    writeFileString(path) {
      return Effect.fail(notFound("writeFileString", path));
    },
    ...fileSystem
  };
};
/** @internal */
exports.makeNoop = makeNoop;
const layerNoop = fileSystem => Layer.succeed(tag, makeNoop(fileSystem));
/** @internal */
exports.layerNoop = layerNoop;
const stream = (file, {
  bufferSize = 16,
  bytesToRead: bytesToRead_,
  chunkSize: chunkSize_ = Size(64 * 1024)
} = {}) => {
  const bytesToRead = bytesToRead_ !== undefined ? Size(bytesToRead_) : undefined;
  const chunkSize = Size(chunkSize_);
  function loop(totalBytesRead) {
    if (bytesToRead !== undefined && bytesToRead <= totalBytesRead) {
      return Channel.void;
    }
    const toRead = bytesToRead !== undefined && bytesToRead - totalBytesRead < chunkSize ? bytesToRead - totalBytesRead : chunkSize;
    return Channel.flatMap(file.readAlloc(toRead), Option.match({
      onNone: () => Channel.void,
      onSome: buf => Channel.flatMap(Channel.write(Chunk.of(buf)), _ => loop(totalBytesRead + BigInt(buf.length)))
    }));
  }
  return Stream.bufferChunks(Stream.fromChannel(loop(BigInt(0))), {
    capacity: bufferSize
  });
};
//# sourceMappingURL=fileSystem.js.map