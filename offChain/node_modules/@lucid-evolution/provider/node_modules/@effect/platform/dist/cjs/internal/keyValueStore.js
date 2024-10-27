"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.prefix = exports.make = exports.layerStorage = exports.layerSchema = exports.layerMemory = exports.layerFileSystem = exports.keyValueStoreTag = exports.TypeId = exports.SchemaStoreTypeId = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Option = _interopRequireWildcard(require("effect/Option"));
var PlatformError = _interopRequireWildcard(require("../Error.js"));
var FileSystem = _interopRequireWildcard(require("../FileSystem.js"));
var Path = _interopRequireWildcard(require("../Path.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/KeyValueStore");
/** @internal */
const keyValueStoreTag = exports.keyValueStoreTag = /*#__PURE__*/Context.GenericTag("@effect/platform/KeyValueStore");
/** @internal */
const make = impl => keyValueStoreTag.of({
  [TypeId]: TypeId,
  has: key => Effect.map(impl.get(key), Option.isSome),
  isEmpty: Effect.map(impl.size, size => size === 0),
  modify: (key, f) => Effect.flatMap(impl.get(key), o => {
    if (Option.isNone(o)) {
      return Effect.succeedNone;
    }
    const newValue = f(o.value);
    return Effect.as(impl.set(key, newValue), Option.some(newValue));
  }),
  forSchema(schema) {
    return makeSchemaStore(this, schema);
  },
  ...impl
});
/** @internal */
exports.make = make;
const prefix = exports.prefix = /*#__PURE__*/(0, _Function.dual)(2, (self, prefix) => ({
  ...self,
  get: key => self.get(`${prefix}${key}`),
  set: (key, value) => self.set(`${prefix}${key}`, value),
  remove: key => self.remove(`${prefix}${key}`),
  has: key => self.has(`${prefix}${key}`),
  modify: (key, f) => self.modify(`${prefix}${key}`, f)
}));
/** @internal */
const SchemaStoreTypeId = exports.SchemaStoreTypeId = /*#__PURE__*/Symbol.for("@effect/platform/KeyValueStore/SchemaStore");
/** @internal */
const makeSchemaStore = (store, schema) => {
  const jsonSchema = Schema.parseJson(schema);
  const parse = Schema.decodeUnknown(jsonSchema);
  const encode = Schema.encode(jsonSchema);
  const get = key => Effect.flatMap(store.get(key), Option.match({
    onNone: () => Effect.succeedNone,
    onSome: value => Effect.asSome(parse(value))
  }));
  const set = (key, value) => Effect.flatMap(encode(value), json => store.set(key, json));
  const modify = (key, f) => Effect.flatMap(get(key), o => {
    if (Option.isNone(o)) {
      return Effect.succeedNone;
    }
    const newValue = f(o.value);
    return Effect.as(set(key, newValue), Option.some(newValue));
  });
  return {
    [SchemaStoreTypeId]: SchemaStoreTypeId,
    get,
    set,
    modify,
    remove: store.remove,
    clear: store.clear,
    size: store.size,
    has: store.has,
    isEmpty: store.isEmpty
  };
};
/** @internal */
const layerMemory = exports.layerMemory = /*#__PURE__*/Layer.sync(keyValueStoreTag, () => {
  const store = new Map();
  return make({
    get: key => Effect.sync(() => Option.fromNullable(store.get(key))),
    set: (key, value) => Effect.sync(() => store.set(key, value)),
    remove: key => Effect.sync(() => store.delete(key)),
    clear: Effect.sync(() => store.clear()),
    size: Effect.sync(() => store.size)
  });
});
/** @internal */
const layerFileSystem = directory => Layer.effect(keyValueStoreTag, Effect.gen(function* (_) {
  const fs = yield* _(FileSystem.FileSystem);
  const path = yield* _(Path.Path);
  const keyPath = key => path.join(directory, encodeURIComponent(key));
  if (!(yield* _(fs.exists(directory)))) {
    yield* _(fs.makeDirectory(directory, {
      recursive: true
    }));
  }
  return make({
    get: key => (0, _Function.pipe)(Effect.map(fs.readFileString(keyPath(key)), Option.some), Effect.catchTag("SystemError", sysError => sysError.reason === "NotFound" ? Effect.succeed(Option.none()) : Effect.fail(sysError))),
    set: (key, value) => fs.writeFileString(keyPath(key), value),
    remove: key => fs.remove(keyPath(key)),
    has: key => fs.exists(keyPath(key)),
    clear: Effect.zipRight(fs.remove(directory, {
      recursive: true
    }), fs.makeDirectory(directory, {
      recursive: true
    })),
    size: Effect.map(fs.readDirectory(directory), files => files.length)
  });
}));
/** @internal */
exports.layerFileSystem = layerFileSystem;
const layerSchema = (schema, tagIdentifier) => {
  const tag = Context.GenericTag(tagIdentifier);
  const layer = Layer.effect(tag, Effect.map(keyValueStoreTag, store => store.forSchema(schema)));
  return {
    tag,
    layer
  };
};
/** @internal */
exports.layerSchema = layerSchema;
const storageError = props => PlatformError.SystemError({
  reason: "PermissionDenied",
  module: "KeyValueStore",
  ...props
});
/** @internal */
const layerStorage = evaluate => Layer.sync(keyValueStoreTag, () => {
  const storage = evaluate();
  return make({
    get: key => Effect.try({
      try: () => Option.fromNullable(storage.getItem(key)),
      catch: () => storageError({
        pathOrDescriptor: key,
        method: "get",
        message: `Unable to get item with key ${key}`
      })
    }),
    set: (key, value) => Effect.try({
      try: () => storage.setItem(key, value),
      catch: () => storageError({
        pathOrDescriptor: key,
        method: "set",
        message: `Unable to set item with key ${key}`
      })
    }),
    remove: key => Effect.try({
      try: () => storage.removeItem(key),
      catch: () => storageError({
        pathOrDescriptor: key,
        method: "remove",
        message: `Unable to remove item with key ${key}`
      })
    }),
    clear: Effect.try({
      try: () => storage.clear(),
      catch: () => storageError({
        pathOrDescriptor: "clear",
        method: "clear",
        message: `Unable to clear storage`
      })
    }),
    size: Effect.try({
      try: () => storage.length,
      catch: () => storageError({
        pathOrDescriptor: "size",
        method: "size",
        message: `Unable to get size`
      })
    })
  });
});
exports.layerStorage = layerStorage;
//# sourceMappingURL=keyValueStore.js.map