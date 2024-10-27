"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.run = exports.makeSerialized = exports.make = exports.layerSerialized = exports.layer = exports.PlatformRunnerTypeId = exports.PlatformRunner = void 0;
var internal = _interopRequireWildcard(require("./internal/workerRunner.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const PlatformRunnerTypeId = exports.PlatformRunnerTypeId = internal.PlatformRunnerTypeId;
/**
 * @since 1.0.0
 * @category tags
 */
const PlatformRunner = exports.PlatformRunner = internal.PlatformRunner;
/**
 * @since 1.0.0
 * @category constructors
 */
const run = exports.run = internal.run;
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category layers
 */
const layer = exports.layer = internal.layer;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeSerialized = exports.makeSerialized = internal.makeSerialized;
/**
 * @since 1.0.0
 * @category layers
 */
const layerSerialized = exports.layerSerialized = internal.layerSerialized;
//# sourceMappingURL=WorkerRunner.js.map