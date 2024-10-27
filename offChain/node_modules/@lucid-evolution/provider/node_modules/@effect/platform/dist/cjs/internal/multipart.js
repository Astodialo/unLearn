"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withMaxParts = exports.withMaxFileSize = exports.withMaxFieldSize = exports.withFieldMimeTypes = exports.toPersisted = exports.schemaPersisted = exports.schemaJson = exports.maxParts = exports.maxFileSize = exports.maxFieldSize = exports.makeConfig = exports.makeChannel = exports.isPersistedFile = exports.isPart = exports.isFile = exports.isField = exports.fieldMimeTypes = exports.TypeId = exports.SingleFileSchema = exports.MultipartError = exports.FilesSchema = exports.FileSchema = exports.ErrorTypeId = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Cause = _interopRequireWildcard(require("effect/Cause"));
var Channel = _interopRequireWildcard(require("effect/Channel"));
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var _Function = require("effect/Function");
var _GlobalValue = require("effect/GlobalValue");
var Inspectable = _interopRequireWildcard(require("effect/Inspectable"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var Queue = _interopRequireWildcard(require("effect/Queue"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
var MP = _interopRequireWildcard(require("multipasta"));
var _Error = require("../Error.js");
var FileSystem = _interopRequireWildcard(require("../FileSystem.js"));
var IncomingMessage = _interopRequireWildcard(require("../HttpIncomingMessage.js"));
var Path = _interopRequireWildcard(require("../Path.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/Multipart");
/** @internal */
const isPart = u => Predicate.hasProperty(u, TypeId);
/** @internal */
exports.isPart = isPart;
const isField = u => isPart(u) && u._tag === "Field";
/** @internal */
exports.isField = isField;
const isFile = u => isPart(u) && u._tag === "File";
/** @internal */
exports.isFile = isFile;
const isPersistedFile = u => Predicate.hasProperty(u, TypeId) && Predicate.isTagged(u, "PersistedFile");
/** @internal */
exports.isPersistedFile = isPersistedFile;
const ErrorTypeId = exports.ErrorTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Multipart/MultipartError");
/** @internal */
class MultipartError extends /*#__PURE__*/(0, _Error.TypeIdError)(ErrorTypeId, "MultipartError") {
  get message() {
    return this.reason;
  }
}
/** @internal */
exports.MultipartError = MultipartError;
const maxParts = exports.maxParts = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/Multipart/maxParts", () => FiberRef.unsafeMake(Option.none()));
/** @internal */
const withMaxParts = exports.withMaxParts = /*#__PURE__*/(0, _Function.dual)(2, (effect, count) => Effect.locally(effect, maxParts, count));
/** @internal */
const maxFieldSize = exports.maxFieldSize = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/Multipart/maxFieldSize", () => FiberRef.unsafeMake(FileSystem.Size(10 * 1024 * 1024)));
/** @internal */
const withMaxFieldSize = exports.withMaxFieldSize = /*#__PURE__*/(0, _Function.dual)(2, (effect, size) => Effect.locally(effect, maxFieldSize, FileSystem.Size(size)));
/** @internal */
const maxFileSize = exports.maxFileSize = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/Multipart/maxFileSize", () => FiberRef.unsafeMake(Option.none()));
/** @internal */
const withMaxFileSize = exports.withMaxFileSize = /*#__PURE__*/(0, _Function.dual)(2, (effect, size) => Effect.locally(effect, maxFileSize, Option.map(size, FileSystem.Size)));
/** @internal */
const fieldMimeTypes = exports.fieldMimeTypes = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/Multipart/fieldMimeTypes", () => FiberRef.unsafeMake(Chunk.make("application/json")));
/** @internal */
const withFieldMimeTypes = exports.withFieldMimeTypes = /*#__PURE__*/(0, _Function.dual)(2, (effect, mimeTypes) => Effect.locally(effect, fieldMimeTypes, Chunk.fromIterable(mimeTypes)));
/** @internal */
const FileSchema = exports.FileSchema = /*#__PURE__*/Schema.declare(isPersistedFile, {
  identifier: "PersistedFile"
});
/** @internal */
const FilesSchema = exports.FilesSchema = /*#__PURE__*/Schema.Array(FileSchema);
/** @internal */
const SingleFileSchema = exports.SingleFileSchema = /*#__PURE__*/Schema.transform( /*#__PURE__*/FilesSchema.pipe( /*#__PURE__*/Schema.itemsCount(1)), FileSchema, {
  strict: true,
  decode: ([file]) => file,
  encode: file => [file]
});
/** @internal */
const schemaPersisted = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return persisted => parse(persisted);
};
/** @internal */
exports.schemaPersisted = schemaPersisted;
const schemaJson = (schema, options) => {
  const fromJson = Schema.parseJson(schema);
  return (0, _Function.dual)(2, (persisted, field) => Effect.map(Schema.decodeUnknown(Schema.Struct({
    [field]: fromJson
  }), options)(persisted), _ => _[field]));
};
/** @internal */
exports.schemaJson = schemaJson;
const makeConfig = headers => Effect.map(Effect.all({
  maxParts: Effect.map(FiberRef.get(maxParts), Option.getOrUndefined),
  maxFieldSize: Effect.map(FiberRef.get(maxFieldSize), Number),
  maxPartSize: Effect.map(FiberRef.get(maxFileSize), (0, _Function.flow)(Option.map(Number), Option.getOrUndefined)),
  maxTotalSize: Effect.map(FiberRef.get(IncomingMessage.maxBodySize), (0, _Function.flow)(Option.map(Number), Option.getOrUndefined)),
  isFile: Effect.map(FiberRef.get(fieldMimeTypes), mimeTypes => {
    if (mimeTypes.length === 0) {
      return undefined;
    }
    return info => !Chunk.some(mimeTypes, _ => info.contentType.includes(_)) && MP.defaultIsFile(info);
  })
}), _ => ({
  ..._,
  headers
}));
/** @internal */
exports.makeConfig = makeConfig;
const makeChannel = (headers, bufferSize = 16) => Channel.acquireUseRelease(Effect.all([makeConfig(headers), Queue.bounded(bufferSize)]), ([config, queue]) => makeFromQueue(config, queue), ([, queue]) => Queue.shutdown(queue));
exports.makeChannel = makeChannel;
const makeFromQueue = (config, queue) => Channel.suspend(() => {
  let error = Option.none();
  let partsBuffer = [];
  let partsFinished = false;
  const input = {
    awaitRead: () => Effect.void,
    emit(element) {
      return Queue.offer(queue, element);
    },
    error(cause) {
      error = Option.some(cause);
      return Queue.offer(queue, null);
    },
    done(_value) {
      return Queue.offer(queue, null);
    }
  };
  const parser = MP.make({
    ...config,
    onField(info, value) {
      partsBuffer.push(new FieldImpl(info.name, info.contentType, MP.decodeField(info, value)));
    },
    onFile(info) {
      let chunks = [];
      let finished = false;
      const take = Channel.suspend(() => {
        if (chunks.length === 0) {
          return finished ? Channel.void : Channel.zipRight(pump, take);
        }
        const chunk = Chunk.unsafeFromArray(chunks);
        chunks = [];
        return finished ? Channel.write(chunk) : Channel.zipRight(Channel.write(chunk), Channel.zipRight(pump, take));
      });
      partsBuffer.push(new FileImpl(info, take));
      return function (chunk) {
        if (chunk === null) {
          finished = true;
        } else {
          chunks.push(chunk);
        }
      };
    },
    onError(error_) {
      error = Option.some(Cause.fail(convertError(error_)));
    },
    onDone() {
      partsFinished = true;
    }
  });
  const pump = Channel.flatMap(Queue.take(queue), chunk => Channel.sync(() => {
    if (chunk === null) {
      parser.end();
    } else {
      Chunk.forEach(chunk, function (buf) {
        parser.write(buf);
      });
    }
  }));
  const takeParts = Channel.zipRight(pump, Channel.suspend(() => {
    if (partsBuffer.length === 0) {
      return Channel.void;
    }
    const parts = Chunk.unsafeFromArray(partsBuffer);
    partsBuffer = [];
    return Channel.write(parts);
  }));
  const partsChannel = Channel.suspend(() => {
    if (error._tag === "Some") {
      return Channel.failCause(error.value);
    } else if (partsFinished) {
      return Channel.void;
    }
    return Channel.zipRight(takeParts, partsChannel);
  });
  return Channel.embedInput(partsChannel, input);
});
function convertError(cause) {
  switch (cause._tag) {
    case "ReachedLimit":
      {
        switch (cause.limit) {
          case "MaxParts":
            {
              return new MultipartError({
                reason: "TooManyParts",
                cause
              });
            }
          case "MaxFieldSize":
            {
              return new MultipartError({
                reason: "FieldTooLarge",
                cause
              });
            }
          case "MaxPartSize":
            {
              return new MultipartError({
                reason: "FileTooLarge",
                cause
              });
            }
          case "MaxTotalSize":
            {
              return new MultipartError({
                reason: "BodyTooLarge",
                cause
              });
            }
        }
      }
    default:
      {
        return new MultipartError({
          reason: "Parse",
          cause
        });
      }
  }
}
class PartBase extends Inspectable.Class {
  [TypeId];
  constructor() {
    super();
    this[TypeId] = TypeId;
  }
}
class FieldImpl extends PartBase {
  key;
  contentType;
  value;
  _tag = "Field";
  constructor(key, contentType, value) {
    super();
    this.key = key;
    this.contentType = contentType;
    this.value = value;
  }
  toJSON() {
    return {
      _id: "@effect/platform/Multipart/Part",
      _tag: "Field",
      key: this.key,
      contentType: this.contentType,
      value: this.value
    };
  }
}
class FileImpl extends PartBase {
  _tag = "File";
  key;
  name;
  contentType;
  content;
  constructor(info, channel) {
    super();
    this.key = info.name;
    this.name = info.filename ?? info.name;
    this.contentType = info.contentType;
    this.content = Stream.fromChannel(channel);
  }
  toJSON() {
    return {
      _id: "@effect/platform/Multipart/Part",
      _tag: "File",
      key: this.key,
      name: this.name,
      contentType: this.contentType
    };
  }
}
const defaultWriteFile = (path, file) => Effect.flatMap(FileSystem.FileSystem, fs => Effect.mapError(Stream.run(file.content, fs.sink(path)), cause => new MultipartError({
  reason: "InternalError",
  cause
})));
/** @internal */
const toPersisted = (stream, writeFile = defaultWriteFile) => (0, _Function.pipe)(Effect.Do, Effect.bind("fs", () => FileSystem.FileSystem), Effect.bind("path", () => Path.Path), Effect.bind("dir", ({
  fs
}) => fs.makeTempDirectoryScoped()), Effect.flatMap(({
  dir,
  path: path_
}) => Stream.runFoldEffect(stream, Object.create(null), (persisted, part) => {
  if (part._tag === "Field") {
    persisted[part.key] = part.value;
    return Effect.succeed(persisted);
  }
  const file = part;
  const path = path_.join(dir, path_.basename(file.name).slice(-128));
  if (!Array.isArray(persisted[part.key])) {
    persisted[part.key] = [];
  }
  ;
  persisted[part.key].push(new PersistedFileImpl(file.key, file.name, file.contentType, path));
  return Effect.as(writeFile(path, file), persisted);
})), Effect.catchTags({
  SystemError: cause => Effect.fail(new MultipartError({
    reason: "InternalError",
    cause
  })),
  BadArgument: cause => Effect.fail(new MultipartError({
    reason: "InternalError",
    cause
  }))
}));
exports.toPersisted = toPersisted;
class PersistedFileImpl extends PartBase {
  key;
  name;
  contentType;
  path;
  _tag = "PersistedFile";
  constructor(key, name, contentType, path) {
    super();
    this.key = key;
    this.name = name;
    this.contentType = contentType;
    this.path = path;
  }
  toJSON() {
    return {
      _id: "@effect/platform/Multipart/Part",
      _tag: "PersistedFile",
      key: this.key,
      name: this.name,
      contentType: this.contentType,
      path: this.path
    };
  }
}
//# sourceMappingURL=multipart.js.map