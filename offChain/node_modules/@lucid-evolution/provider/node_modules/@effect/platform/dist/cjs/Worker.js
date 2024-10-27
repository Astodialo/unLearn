"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.makeSerialized = exports.makePoolSerializedLayer = exports.makePoolSerialized = exports.makePoolLayer = exports.makePool = exports.makePlatform = exports.makeManager = exports.layerSpawner = exports.layerManager = exports.WorkerManagerTypeId = exports.WorkerManager = exports.Spawner = exports.PlatformWorkerTypeId = exports.PlatformWorker = void 0;
var internal = _interopRequireWildcard(require("./internal/worker.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const PlatformWorkerTypeId = exports.PlatformWorkerTypeId = internal.PlatformWorkerTypeId;
/**
 * @since 1.0.0
 */
const makePlatform = exports.makePlatform = internal.makePlatform;
/**
 * @since 1.0.0
 * @category tags
 */
const PlatformWorker = exports.PlatformWorker = internal.PlatformWorker;
/**
 * @since 1.0.0
 * @category tags
 */
const Spawner = exports.Spawner = internal.Spawner;
/**
 * @since 1.0.0
 * @category type ids
 */
const WorkerManagerTypeId = exports.WorkerManagerTypeId = internal.WorkerManagerTypeId;
/**
 * @since 1.0.0
 * @category tags
 */
const WorkerManager = exports.WorkerManager = internal.WorkerManager;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeManager = exports.makeManager = internal.makeManager;
/**
 * @since 1.0.0
 * @category layers
 */
const layerManager = exports.layerManager = internal.layerManager;
/**
 * @since 1.0.0
 * @category constructors
 */
const makePool = exports.makePool = internal.makePool;
/**
 * @since 1.0.0
 * @category constructors
 */
const makePoolLayer = exports.makePoolLayer = internal.makePoolLayer;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeSerialized = exports.makeSerialized = internal.makeSerialized;
/**
 * @since 1.0.0
 * @category constructors
 */
const makePoolSerialized = exports.makePoolSerialized = internal.makePoolSerialized;
/**
 * @since 1.0.0
 * @category layers
 */
const makePoolSerializedLayer = exports.makePoolSerializedLayer = internal.makePoolSerializedLayer;
/**
 * @since 1.0.0
 * @category layers
 */
const layerSpawner = exports.layerSpawner = internal.layerSpawner;
//# sourceMappingURL=Worker.js.map