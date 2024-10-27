import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import { dual } from "effect/Function";
import * as Layer from "effect/Layer";
/** @internal */
export const TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpServer");
/** @internal */
export const serverTag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpServer");
const serverProto = {
  [TypeId]: TypeId
};
/** @internal */
export const isServer = u => typeof u === "object" && u !== null && TypeId in u;
/** @internal */
export const make = options => Object.assign(Object.create(serverProto), options);
/** @internal */
export const serve = /*#__PURE__*/dual(args => Effect.isEffect(args[0]), (httpApp, middleware) => Layer.scopedDiscard(Effect.flatMap(serverTag, server => server.serve(httpApp, middleware))));
/** @internal */
export const serveEffect = /*#__PURE__*/dual(args => Effect.isEffect(args[0]), (httpApp, middleware) => Effect.flatMap(serverTag, server => server.serve(httpApp, middleware)));
/** @internal */
export const formatAddress = address => {
  switch (address._tag) {
    case "UnixAddress":
      return `unix://${address.path}`;
    case "TcpAddress":
      return `http://${address.hostname}:${address.port}`;
  }
};
/** @internal */
export const addressWith = effect => Effect.flatMap(serverTag, server => effect(server.address));
/** @internal */
export const addressFormattedWith = effect => Effect.flatMap(serverTag, server => effect(formatAddress(server.address)));
/** @internal */
export const logAddress = /*#__PURE__*/addressFormattedWith(_ => Effect.log(`Listening on ${_}`));
/** @internal */
export const withLogAddress = layer => Layer.effectDiscard(logAddress).pipe(Layer.provideMerge(layer));
//# sourceMappingURL=httpServer.js.map