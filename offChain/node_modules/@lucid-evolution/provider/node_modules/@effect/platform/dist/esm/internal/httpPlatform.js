import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import { pipe } from "effect/Function";
import * as Etag from "../Etag.js";
import * as FileSystem from "../FileSystem.js";
import * as Headers from "../Headers.js";
/** @internal */
export const TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpPlatform");
/** @internal */
export const tag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpPlatform");
/** @internal */
export const make = impl => Effect.gen(function* (_) {
  const fs = yield* _(FileSystem.FileSystem);
  const etagGen = yield* _(Etag.Generator);
  return tag.of({
    [TypeId]: TypeId,
    fileResponse(path, options) {
      return pipe(Effect.bindTo(fs.stat(path), "info"), Effect.bind("etag", ({
        info
      }) => etagGen.fromFileInfo(info)), Effect.map(({
        etag,
        info
      }) => {
        const start = Number(options?.offset ?? 0);
        const end = options?.bytesToRead !== undefined ? start + Number(options.bytesToRead) : undefined;
        const headers = Headers.set(options?.headers ?? Headers.empty, "etag", Etag.toString(etag));
        if (info.mtime._tag === "Some") {
          ;
          headers["last-modified"] = info.mtime.value.toUTCString();
        }
        const contentLength = end !== undefined ? end - start : Number(info.size) - start;
        return impl.fileResponse(path, options?.status ?? 200, options?.statusText, headers, start, end, contentLength);
      }));
    },
    fileWebResponse(file, options) {
      return Effect.map(etagGen.fromFileWeb(file), etag => {
        const headers = Headers.merge(options?.headers ?? Headers.empty, Headers.unsafeFromRecord({
          etag: Etag.toString(etag),
          "last-modified": new Date(file.lastModified).toUTCString()
        }));
        return impl.fileWebResponse(file, options?.status ?? 200, options?.statusText, headers, options);
      });
    }
  });
});
//# sourceMappingURL=httpPlatform.js.map