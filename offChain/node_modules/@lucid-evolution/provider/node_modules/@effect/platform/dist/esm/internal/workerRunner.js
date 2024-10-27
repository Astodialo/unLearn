import * as Schema from "@effect/schema/Schema";
import * as Serializable from "@effect/schema/Serializable";
import * as Cause from "effect/Cause";
import * as Chunk from "effect/Chunk";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Either from "effect/Either";
import * as Fiber from "effect/Fiber";
import { pipe } from "effect/Function";
import * as Layer from "effect/Layer";
import * as Schedule from "effect/Schedule";
import * as Stream from "effect/Stream";
import * as Transferable from "../Transferable.js";
import { isWorkerError, WorkerError } from "../WorkerError.js";
/** @internal */
export const PlatformRunnerTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Runner/PlatformRunner");
/** @internal */
export const PlatformRunner = /*#__PURE__*/Context.GenericTag("@effect/platform/Runner/PlatformRunner");
/** @internal */
export const run = (process, options) => Effect.gen(function* () {
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
      let effect = Effect.isEffect(stream) ? Effect.flatMap(stream, out => pipe(options?.encodeOutput ? Effect.provideService(options.encodeOutput(input, out), Transferable.Collector, collector) : Effect.succeed(out), Effect.flatMap(payload => backing.send(portId, [id, 0, [payload]], collector.unsafeRead())))) : pipe(stream, Stream.runForEachChunk(chunk => {
        if (options?.encodeOutput === undefined) {
          const payload = Chunk.toReadonlyArray(chunk);
          return backing.send(portId, [id, 0, payload]);
        }
        collector.unsafeClear();
        return pipe(Effect.forEach(chunk, data => options.encodeOutput(input, data)), Effect.provideService(Transferable.Collector, collector), Effect.flatMap(payload => backing.send(portId, [id, 0, payload], collector.unsafeRead())));
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
      return Effect.uninterruptibleMask(restore => restore(effect).pipe(Effect.catchIf(isWorkerError, error => backing.send(portId, [id, 3, WorkerError.encodeCause(Cause.fail(error))])), Effect.catchAllCause(cause => Either.match(Cause.failureOrCause(cause), {
        onLeft: error => {
          collector.unsafeClear();
          return pipe(options?.encodeError ? Effect.provideService(options.encodeError(input, error), Transferable.Collector, collector) : Effect.succeed(error), Effect.flatMap(payload => backing.send(portId, [id, 2, payload], collector.unsafeRead())), Effect.catchAllCause(cause => backing.send(portId, [id, 3, WorkerError.encodeCause(cause)])));
        },
        onRight: cause => backing.send(portId, [id, 3, WorkerError.encodeCause(cause)])
      }))));
    }), Effect.ensuring(Effect.sync(() => fiberMap.delete(id))));
  });
});
/** @internal */
export const make = (process, options) => Effect.withFiberRuntime(fiber => run(process, options).pipe(Effect.tapErrorCause(Effect.logDebug), Effect.retry(Schedule.spaced(1000)), Effect.annotateLogs({
  package: "@effect/platform-node",
  module: "WorkerRunner"
}), Effect.ensuring(Fiber.interruptAsFork(fiber, fiber.id())), Effect.interruptible, Effect.forkScoped, Effect.asVoid));
/** @internal */
export const layer = (process, options) => Layer.scopedDiscard(make(process, options));
/** @internal */
export const makeSerialized = (schema, handlers) => Effect.gen(function* (_) {
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
      return Effect.mapError(parseRequest(message), cause => new WorkerError({
        reason: "decode",
        cause
      }));
    },
    encodeError(request, message) {
      return Effect.mapError(Serializable.serializeFailure(request, message), cause => new WorkerError({
        reason: "encode",
        cause
      }));
    },
    encodeOutput(request, message) {
      return Effect.catchAllCause(Serializable.serializeSuccess(request, message), cause => new WorkerError({
        reason: "encode",
        cause
      }));
    }
  }));
});
/** @internal */
export const layerSerialized = (schema, handlers) => Layer.scopedDiscard(makeSerialized(schema, handlers));
//# sourceMappingURL=workerRunner.js.map