"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withMaxParts = exports.withMaxFileSize = exports.withMaxFieldSize = exports.withFieldMimeTypes = exports.toPersisted = exports.schemaPersisted = exports.schemaJson = exports.maxParts = exports.maxFileSize = exports.maxFieldSize = exports.makeConfig = exports.makeChannel = exports.isPersistedFile = exports.isPart = exports.isFile = exports.isField = exports.fieldMimeTypes = exports.TypeId = exports.SingleFileSchema = exports.MultipartError = exports.FilesSchema = exports.FileSchema = exports.ErrorTypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/multipart.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
const isPart = exports.isPart = internal.isPart;
/**
 * @since 1.0.0
 * @category refinements
 */
const isField = exports.isField = internal.isField;
/**
 * @since 1.0.0
 * @category refinements
 */
const isFile = exports.isFile = internal.isFile;
/**
 * @since 1.0.0
 * @category refinements
 */
const isPersistedFile = exports.isPersistedFile = internal.isPersistedFile;
/**
 * @since 1.0.0
 * @category type ids
 */
const ErrorTypeId = exports.ErrorTypeId = internal.ErrorTypeId;
/**
 * @since 1.0.0
 * @category errors
 */
const MultipartError = exports.MultipartError = internal.MultipartError;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const maxParts = exports.maxParts = internal.maxParts;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withMaxParts = exports.withMaxParts = internal.withMaxParts;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const maxFieldSize = exports.maxFieldSize = internal.maxFieldSize;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withMaxFieldSize = exports.withMaxFieldSize = internal.withMaxFieldSize;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const maxFileSize = exports.maxFileSize = internal.maxFileSize;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withMaxFileSize = exports.withMaxFileSize = internal.withMaxFileSize;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const fieldMimeTypes = exports.fieldMimeTypes = internal.fieldMimeTypes;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withFieldMimeTypes = exports.withFieldMimeTypes = internal.withFieldMimeTypes;
/**
 * @since 1.0.0
 * @category schema
 */
const FileSchema = exports.FileSchema = internal.FileSchema;
/**
 * @since 1.0.0
 * @category schema
 */
const FilesSchema = exports.FilesSchema = internal.FilesSchema;
/**
 * @since 1.0.0
 * @category schema
 */
const SingleFileSchema = exports.SingleFileSchema = internal.SingleFileSchema;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaJson = exports.schemaJson = internal.schemaJson;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaPersisted = exports.schemaPersisted = internal.schemaPersisted;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeChannel = exports.makeChannel = internal.makeChannel;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeConfig = exports.makeConfig = internal.makeConfig;
/**
 * @since 1.0.0
 * @category constructors
 */
const toPersisted = exports.toPersisted = internal.toPersisted;
//# sourceMappingURL=Multipart.js.map