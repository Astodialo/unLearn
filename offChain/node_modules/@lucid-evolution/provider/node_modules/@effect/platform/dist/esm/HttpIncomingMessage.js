import * as Schema from "@effect/schema/Schema";
import * as Effect from "effect/Effect";
import * as FiberRef from "effect/FiberRef";
import { dual } from "effect/Function";
import * as Global from "effect/GlobalValue";
import * as Option from "effect/Option";
import * as FileSystem from "./FileSystem.js";
/**
 * @since 1.0.0
 * @category type ids
 */
export const TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpIncomingMessage");
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaBodyJson = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => Effect.flatMap(self.json, parse);
};
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaBodyJsonScoped = (schema, options) => {
  const decode = schemaBodyJson(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaBodyUrlParams = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => Effect.flatMap(self.urlParamsBody, _ => parse(Object.fromEntries(_)));
};
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaBodyUrlParamsScoped = (schema, options) => {
  const decode = schemaBodyUrlParams(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaHeaders = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => parse(self.headers);
};
/**
 * @since 1.0.0
 * @category schema
 */
export const schemaHeadersScoped = (schema, options) => {
  const decode = schemaHeaders(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export const maxBodySize = /*#__PURE__*/Global.globalValue("@effect/platform/HttpIncomingMessage/maxBodySize", () => FiberRef.unsafeMake(Option.none()));
/**
 * @since 1.0.0
 * @category fiber refs
 */
export const withMaxBodySize = /*#__PURE__*/dual(2, (effect, size) => Effect.locally(effect, maxBodySize, Option.map(size, FileSystem.Size)));
/**
 * @since 1.0.0
 */
export const inspect = (self, that) => {
  const contentType = self.headers["content-type"] ?? "";
  let body;
  if (contentType.includes("application/json")) {
    try {
      body = Effect.runSync(self.json);
    } catch (_) {
      //
    }
  } else if (contentType.includes("text/") || contentType.includes("urlencoded")) {
    try {
      body = Effect.runSync(self.text);
    } catch (_) {
      //
    }
  }
  const obj = {
    ...that,
    headers: self.headers,
    remoteAddress: self.remoteAddress.toJSON()
  };
  if (body !== undefined) {
    obj.body = body;
  }
  return obj;
};
//# sourceMappingURL=HttpIncomingMessage.js.map