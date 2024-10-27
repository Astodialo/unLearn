"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withTracerPropagation = exports.withTracerDisabledWhen = exports.withFetchOptions = exports.withCookiesRef = exports.transformResponse = exports.transform = exports.tapRequest = exports.tap = exports.scoped = exports.schemaFunction = exports.retry = exports.mapRequestEffect = exports.mapRequest = exports.mapInputRequestEffect = exports.mapInputRequest = exports.mapEffectScoped = exports.mapEffect = exports.map = exports.makeDefault = exports.make = exports.layer = exports.followRedirects = exports.filterStatusOk = exports.filterStatus = exports.filterOrFail = exports.filterOrElse = exports.fetchOk = exports.fetch = exports.currentTracerPropagation = exports.currentTracerDisabledWhen = exports.currentFetchOptions = exports.catchTags = exports.catchTag = exports.catchAll = exports.TypeId = exports.HttpClient = exports.Fetch = void 0;
var internal = _interopRequireWildcard(require("./internal/httpClient.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category tags
 */
const HttpClient = exports.HttpClient = internal.tag;
/**
 * @since 1.0.0
 * @category tags
 */
const Fetch = exports.Fetch = internal.Fetch;
/**
 * @since 1.0.0
 * @category layers
 */
const layer = exports.layer = internal.layer;
/**
 * @since 1.0.0
 * @category constructors
 */
const fetch = exports.fetch = internal.fetch;
/**
 * @since 1.0.0
 * @category constructors
 */
const fetchOk = exports.fetchOk = internal.fetchOk;
/**
 * @since 1.0.0
 * @category error handling
 */
const catchAll = exports.catchAll = internal.catchAll;
/**
 * @since 1.0.0
 * @category error handling
 */
const catchTag = exports.catchTag = internal.catchTag;
/**
 * @since 1.0.0
 * @category error handling
 */
const catchTags = exports.catchTags = internal.catchTags;
/**
 * @since 1.0.0
 * @category filters
 */
const filterOrElse = exports.filterOrElse = internal.filterOrElse;
/**
 * @since 1.0.0
 * @category filters
 */
const filterOrFail = exports.filterOrFail = internal.filterOrFail;
/**
 * @since 1.0.0
 * @category filters
 */
const filterStatus = exports.filterStatus = internal.filterStatus;
/**
 * @since 1.0.0
 * @category filters
 */
const filterStatusOk = exports.filterStatusOk = internal.filterStatusOk;
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category constructors
 */
const makeDefault = exports.makeDefault = internal.makeDefault;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const transform = exports.transform = internal.transform;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const transformResponse = exports.transformResponse = internal.transformResponse;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const map = exports.map = internal.map;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapEffect = exports.mapEffect = internal.mapEffect;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapEffectScoped = exports.mapEffectScoped = internal.mapEffectScoped;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapRequest = exports.mapRequest = internal.mapRequest;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapRequestEffect = exports.mapRequestEffect = internal.mapRequestEffect;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapInputRequest = exports.mapInputRequest = internal.mapInputRequest;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const mapInputRequestEffect = exports.mapInputRequestEffect = internal.mapInputRequestEffect;
/**
 * @since 1.0.0
 * @category error handling
 */
const retry = exports.retry = internal.retry;
/**
 * @since 1.0.0
 * @category resources & finalizers
 */
const scoped = exports.scoped = internal.scoped;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaFunction = exports.schemaFunction = internal.schemaFunction;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const tap = exports.tap = internal.tap;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
const tapRequest = exports.tapRequest = internal.tapRequest;
/**
 * @since 1.0.0
 * @category cookies
 */
const withCookiesRef = exports.withCookiesRef = internal.withCookiesRef;
/**
 * @since 1.0.0
 * @category redirects
 */
const followRedirects = exports.followRedirects = internal.followRedirects;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const currentTracerDisabledWhen = exports.currentTracerDisabledWhen = internal.currentTracerDisabledWhen;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withTracerDisabledWhen = exports.withTracerDisabledWhen = internal.withTracerDisabledWhen;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const currentTracerPropagation = exports.currentTracerPropagation = internal.currentTracerPropagation;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withTracerPropagation = exports.withTracerPropagation = internal.withTracerPropagation;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const currentFetchOptions = exports.currentFetchOptions = internal.currentFetchOptions;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withFetchOptions = exports.withFetchOptions = internal.withFetchOptions;
//# sourceMappingURL=HttpClient.js.map