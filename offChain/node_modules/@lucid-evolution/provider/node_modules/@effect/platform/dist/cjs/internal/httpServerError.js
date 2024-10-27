"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isServerError = exports.exitResponse = exports.clientAbortFiberId = exports.causeResponseStripped = exports.causeResponse = exports.TypeId = void 0;
var Cause = _interopRequireWildcard(require("effect/Cause"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberId = _interopRequireWildcard(require("effect/FiberId"));
var _GlobalValue = require("effect/GlobalValue");
var Option = _interopRequireWildcard(require("effect/Option"));
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var Respondable = _interopRequireWildcard(require("../HttpServerRespondable.js"));
var internalServerResponse = _interopRequireWildcard(require("./httpServerResponse.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpServerError");
/** @internal */
const isServerError = u => Predicate.hasProperty(u, TypeId);
/** @internal */
exports.isServerError = isServerError;
const clientAbortFiberId = exports.clientAbortFiberId = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/HttpServerError/clientAbortFiberId", () => FiberId.runtime(-499, 0));
/** @internal */
const causeResponse = cause => {
  const [effect, stripped] = Cause.reduce(cause, [Effect.succeed(internalServerError), Cause.empty], (acc, cause) => {
    switch (cause._tag) {
      case "Empty":
        {
          return Option.some(acc);
        }
      case "Fail":
        {
          return Option.some([Respondable.toResponseOrElse(cause.error, internalServerError), cause]);
        }
      case "Die":
        {
          return Option.some([Respondable.toResponseOrElse(cause.defect, internalServerError), cause]);
        }
      case "Interrupt":
        {
          if (acc[1]._tag !== "Empty") {
            return Option.none();
          }
          const response = cause.fiberId === clientAbortFiberId ? clientAbortError : serverAbortError;
          return Option.some([Effect.succeed(response), cause]);
        }
      default:
        {
          return Option.none();
        }
    }
  });
  return Effect.map(effect, response => {
    if (Cause.isEmptyType(stripped)) {
      return [response, Cause.die(response)];
    }
    return [response, Cause.sequential(stripped, Cause.die(response))];
  });
};
/** @internal */
exports.causeResponse = causeResponse;
const causeResponseStripped = cause => {
  let response;
  const stripped = Cause.stripSomeDefects(cause, defect => {
    if (internalServerResponse.isServerResponse(defect)) {
      response = defect;
      return Option.some(Cause.empty);
    }
    return Option.none();
  });
  return [response ?? internalServerError, stripped];
};
exports.causeResponseStripped = causeResponseStripped;
const internalServerError = /*#__PURE__*/internalServerResponse.empty({
  status: 500
});
const clientAbortError = /*#__PURE__*/internalServerResponse.empty({
  status: 499
});
const serverAbortError = /*#__PURE__*/internalServerResponse.empty({
  status: 503
});
/** @internal */
const exitResponse = exit => {
  if (exit._tag === "Success") {
    return exit.value;
  }
  return causeResponseStripped(exit.cause)[0];
};
exports.exitResponse = exitResponse;
//# sourceMappingURL=httpServerError.js.map