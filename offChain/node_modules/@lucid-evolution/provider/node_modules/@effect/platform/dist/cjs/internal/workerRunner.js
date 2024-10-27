"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.run = exports.makeSerialized = exports.make = exports.layerSerialized = exports.layer = exports.PlatformRunnerTypeId = exports.PlatformRunner = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Serializable = _interopRequireWildcard(require("@effect/schema/Serializable"));
var Cause = _interopRequireWildcard(require("effect/Cause"));
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var Either = _interopRequireWildcard(require("effect/Either"));
var Fiber = _interopRequireWildcard(require("effect/Fiber"));
var _Function = require("effect/Function");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Schedule = _interopRequireWildcard(require("effect/Schedule"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
var Transferable = _interopRequireWildcard(require("../Transferable.js"));
var _WorkerError = require("../WorkerError.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const PlatformRunnerTypeId = exports.PlatformRunnerTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Runner/PlatformRunner");
/** @internal */
const PlatformRunner = exports.PlatformRunner = /*#__PURE__*/Context.GenericTag("@effect/platform/Runner/PlatformRunner");
/** @internal */
const run = (process, options) => Effect.gen(function* () {
  const platform = yield* PlatformRunner;
  const backing = yield* platform.start();
  const fiberMap = new Map();
  return yield* backing.run((portId, [id, kind, data, span]) => {
    if (kind === 1) {
      const fiber = fiberMap.get(id);
      if (!fiber) return Effect.void;
      return Fiber.interrupt(fiber);
    }
    return Effect.withFiberRuntime(fiber => {
      fiberMap.set(id, fiber);
      return options?.decode ? options.decode(data) : Effect.succeed(data);
    }).pipe(Effect.flatMap(input => {
      const collector = Transferable.unsafeMakeCollector();
      const stream = process(input);
      let effect = Effect.isEffect(stream) ? Effect.flatMap(stream, out => (0, _Function.pipe)(options?.encodeOutput ? Effect.provideService(options.encodeOutput(input, out), Transferable.Collector, collector) : Effect.succeed(out), Effect.flatMap(payload => backing.send(portId, [id, 0, [payload]], collector.unsafeRead())))) : (0, _Function.pipe)(stream, Stream.runForEachChunk(chunk => {
        if (options?.encodeOutput === undefined) {
          const payload = Chunk.toReadonlyArray(chunk);
          return backing.send(portId, [id, 0, payload]);
        }
        collector.unsafeClear();
        return (0, _Function.pipe)(Effect.forEach(chunk, data => options.encodeOutput(input, data)), Effect.provideService(Transferable.Collector, collector), Effect.flatMap(payload => backing.send(portId, [id, 0, payload], collector.unsafeRead())));
      }), Effect.andThen(backing.send(portId, [id, 1])));
      if (span) {
        effect = Effect.withParentSpan(effect, {
          _tag: "ExternalSpan",
          traceId: span[0],
          spanId: span[1],
          sampled: span[2],
          context: Context.empty()
        });
      }
      return Effect.uninterruptibleMask(restore => restore(effect).pipe(Effect.catchIf(_WorkerError.isWorkerError, error => backing.send(portId, [id, 3, _WorkerError.WorkerError.encodeCause(Cause.fail(error))])), Effect.catchAllCause(cause => Either.match(Cause.failureOrCause(cause), {
        onLeft: error => {
          collector.unsafeClear();
          return (0, _Function.pipe)(options?.encodeError ? Effect.provideService(options.encodeError(input, error), Transferable.Collector, collector) : Effect.succeed(error), Effect.flatMap(payload => backing.send(portId, [id, 2, payload], collector.unsafeRead())), Effect.catchAllCause(cause => backing.send(portId, [id, 3, _WorkerError.WorkerError.encodeCause(cause)])));
        },
        onRight: cause => backing.send(portId, [id, 3, _WorkerError.WorkerError.encodeCause(cause)])
      }))));
    }), Effect.ensuring(Effect.sync(() => fiberMap.delete(id))));
  });
});
/** @internal */
exports.run = run;
const make = (process, options) => Effect.withFiberRuntime(fiber => run(process, options).pipe(Effect.tapErrorCause(Effect.logDebug), Effect.retry(Schedule.spaced(1000)), Effect.annotateLogs({
  package: "@effect/platform-node",
  module: "WorkerRunner"
}), Effect.ensuring(Fiber.interruptAsFork(fiber, fiber.id())), Effect.interruptible, Effect.forkScoped, Effect.asVoid));
/** @internal */
exports.make = make;
const layer = (process, options) => Layer.scopedDiscard(make(process, options));
/** @internal */
exports.layer = layer;
const makeSerialized = (schema, handlers) => Effect.gen(function* (_) {
  const scope = yield* _(Effect.scope);
  let context = Context.empty();
  const parseRequest = Schema.decodeUnknown(schema);
  return yield* _(make(request => {
    const result = handlers[request._tag](request);
    if (Layer.isLayer(result)) {
      return Effect.flatMap(Layer.buildWithScope(result, scope), _ => Effect.sync(() => {
        context = Context.merge(context, _);
      }));
    } else if (Effect.isEffect(result)) {
      return Effect.provide(result, context);
    }
    return Stream.provideContext(result, context);
  }, {
    decode(message) {
      return Effect.mapError(parseRequest(message), cause => new _WorkerError.WorkerError({
        reason: "decode",
        cause
      }));
    },
    encodeError(request, message) {
      return Effect.mapError(Serializable.serializeFailure(request, message), cause => new _WorkerError.WorkerError({
        reason: "encode",
        cause
      }));
    },
    encodeOutput(request, message) {
      return Effect.catchAllCause(Serializable.serializeSuccess(request, message), cause => new _WorkerError.WorkerError({
        reason: "encode",
        cause
      }));
    }
  }));
});
/** @internal */
exports.makeSerialized = makeSerialized;
const layerSerialized = (schema, handlers) => Layer.scopedDiscard(makeSerialized(schema, handlers));
exports.layerSerialized = layerSerialized;
//# sourceMappingURL=workerRunner.js.map