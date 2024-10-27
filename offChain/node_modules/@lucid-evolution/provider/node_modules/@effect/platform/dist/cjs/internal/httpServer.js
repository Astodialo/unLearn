"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withLogAddress = exports.serverTag = exports.serveEffect = exports.serve = exports.make = exports.logAddress = exports.isServer = exports.formatAddress = exports.addressWith = exports.addressFormattedWith = exports.TypeId = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Layer = _interopRequireWildcard(require("effect/Layer"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpServer");
/** @internal */
const serverTag = exports.serverTag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpServer");
const serverProto = {
  [TypeId]: TypeId
};
/** @internal */
const isServer = u => typeof u === "object" && u !== null && TypeId in u;
/** @internal */
exports.isServer = isServer;
const make = options => Object.assign(Object.create(serverProto), options);
/** @internal */
exports.make = make;
const serve = exports.serve = /*#__PURE__*/(0, _Function.dual)(args => Effect.isEffect(args[0]), (httpApp, middleware) => Layer.scopedDiscard(Effect.flatMap(serverTag, server => server.serve(httpApp, middleware))));
/** @internal */
const serveEffect = exports.serveEffect = /*#__PURE__*/(0, _Function.dual)(args => Effect.isEffect(args[0]), (httpApp, middleware) => Effect.flatMap(serverTag, server => server.serve(httpApp, middleware)));
/** @internal */
const formatAddress = address => {
  switch (address._tag) {
    case "UnixAddress":
      return `unix://${address.path}`;
    case "TcpAddress":
      return `http://${address.hostname}:${address.port}`;
  }
};
/** @internal */
exports.formatAddress = formatAddress;
const addressWith = effect => Effect.flatMap(serverTag, server => effect(server.address));
/** @internal */
exports.addressWith = addressWith;
const addressFormattedWith = effect => Effect.flatMap(serverTag, server => effect(formatAddress(server.address)));
/** @internal */
exports.addressFormattedWith = addressFormattedWith;
const logAddress = exports.logAddress = /*#__PURE__*/addressFormattedWith(_ => Effect.log(`Listening on ${_}`));
/** @internal */
const withLogAddress = layer => Layer.effectDiscard(logAddress).pipe(Layer.provideMerge(layer));
exports.withLogAddress = withLogAddress;
//# sourceMappingURL=httpServer.js.map