"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.prefix = exports.make = exports.layerStorage = exports.layerSchema = exports.layerMemory = exports.layerFileSystem = exports.TypeId = exports.SchemaStoreTypeId = exports.KeyValueStore = void 0;
var internal = _interopRequireWildcard(require("./internal/keyValueStore.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type id
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category tags
 */
const KeyValueStore = exports.KeyValueStore = internal.keyValueStoreTag;
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category combinators
 */
const prefix = exports.prefix = internal.prefix;
/**
 * @since 1.0.0
 * @category layers
 */
const layerMemory = exports.layerMemory = internal.layerMemory;
/**
 * @since 1.0.0
 * @category layers
 */
const layerFileSystem = exports.layerFileSystem = internal.layerFileSystem;
/**
 * @since 1.0.0
 * @category type id
 */
const SchemaStoreTypeId = exports.SchemaStoreTypeId = internal.SchemaStoreTypeId;
/**
 * @since 1.0.0
 * @category layers
 */
const layerSchema = exports.layerSchema = internal.layerSchema;
/**
 * Creates an KeyValueStorage from an instance of the `Storage` api.
 *
 * @since 1.0.0
 * @category layers
 * @see https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API
 */
const layerStorage = exports.layerStorage = internal.layerStorage;
//# sourceMappingURL=KeyValueStore.js.map