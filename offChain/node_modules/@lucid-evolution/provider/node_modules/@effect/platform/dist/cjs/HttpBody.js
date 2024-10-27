"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.urlParams = exports.unsafeJson = exports.uint8Array = exports.text = exports.stream = exports.raw = exports.jsonSchema = exports.json = exports.isHttpBody = exports.formData = exports.fileWeb = exports.fileInfo = exports.file = exports.empty = exports.TypeId = exports.HttpBodyError = exports.ErrorTypeId = void 0;
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var internal = _interopRequireWildcard(require("./internal/httpBody.js"));
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
const isHttpBody = u => Predicate.hasProperty(u, TypeId);
/**
 * @since 1.0.0
 * @category type ids
 */
exports.isHttpBody = isHttpBody;
const ErrorTypeId = exports.ErrorTypeId = internal.ErrorTypeId;
/**
 * @since 1.0.0
 * @category errors
 */
const HttpBodyError = exports.HttpBodyError = internal.HttpBodyError;
/**
 * @since 1.0.0
 * @category constructors
 */
const empty = exports.empty = internal.empty;
/**
 * @since 1.0.0
 * @category constructors
 */
const raw = exports.raw = internal.raw;
/**
 * @since 1.0.0
 * @category constructors
 */
const uint8Array = exports.uint8Array = internal.uint8Array;
/**
 * @since 1.0.0
 * @category constructors
 */
const text = exports.text = internal.text;
/**
 * @since 1.0.0
 * @category constructors
 */
const unsafeJson = exports.unsafeJson = internal.unsafeJson;
/**
 * @since 1.0.0
 * @category constructors
 */
const json = exports.json = internal.json;
/**
 * @since 1.0.0
 * @category constructors
 */
const jsonSchema = exports.jsonSchema = internal.jsonSchema;
/**
 * @since 1.0.0
 * @category constructors
 */
const urlParams = exports.urlParams = internal.urlParams;
/**
 * @since 1.0.0
 * @category constructors
 */
const formData = exports.formData = internal.formData;
/**
 * @since 1.0.0
 * @category constructors
 */
const stream = exports.stream = internal.stream;
/**
 * @since 1.0.0
 * @category constructors
 */
const file = exports.file = internal.file;
/**
 * @since 1.0.0
 * @category constructors
 */
const fileInfo = exports.fileInfo = internal.fileInfo;
/**
 * @since 1.0.0
 * @category constructors
 */
const fileWeb = exports.fileWeb = internal.fileWeb;
//# sourceMappingURL=HttpBody.js.map