"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.tag = exports.make = exports.TypeId = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Etag = _interopRequireWildcard(require("../Etag.js"));
var FileSystem = _interopRequireWildcard(require("../FileSystem.js"));
var Headers = _interopRequireWildcard(require("../Headers.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpPlatform");
/** @internal */
const tag = exports.tag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpPlatform");
/** @internal */
const make = impl => Effect.gen(function* (_) {
  const fs = yield* _(FileSystem.FileSystem);
  const etagGen = yield* _(Etag.Generator);
  return tag.of({
    [TypeId]: TypeId,
    fileResponse(path, options) {
      return (0, _Function.pipe)(Effect.bindTo(fs.stat(path), "info"), Effect.bind("etag", ({
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
exports.make = make;
//# sourceMappingURL=httpPlatform.js.map