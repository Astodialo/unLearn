"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toChannelWith = exports.toChannel = exports.makeWebSocketChannel = exports.makeWebSocket = exports.makeChannel = exports.layerWebSocketConstructorGlobal = exports.layerWebSocket = exports.isSocketError = exports.isCloseEvent = exports.fromWebSocket = exports.fromTransformStream = exports.defaultCloseCodeIsError = exports.currentSendQueueCapacity = exports.WebSocketConstructor = exports.WebSocket = exports.TypeId = exports.SocketGenericError = exports.SocketErrorTypeId = exports.SocketCloseError = exports.Socket = exports.CloseEventTypeId = exports.CloseEvent = void 0;
var Cause = _interopRequireWildcard(require("effect/Cause"));
var Channel = _interopRequireWildcard(require("effect/Channel"));
var Chunk = _interopRequireWildcard(require("effect/Chunk"));
var Context = _interopRequireWildcard(require("effect/Context"));
var Deferred = _interopRequireWildcard(require("effect/Deferred"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var ExecutionStrategy = _interopRequireWildcard(require("effect/ExecutionStrategy"));
var Exit = _interopRequireWildcard(require("effect/Exit"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var FiberSet = _interopRequireWildcard(require("effect/FiberSet"));
var _GlobalValue = require("effect/GlobalValue");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var Queue = _interopRequireWildcard(require("effect/Queue"));
var Scope = _interopRequireWildcard(require("effect/Scope"));
var _Error = require("./Error.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/Socket");
/**
 * @since 1.0.0
 * @category tags
 */
const Socket = exports.Socket = /*#__PURE__*/Context.GenericTag("@effect/platform/Socket");
/**
 * @since 1.0.0
 * @category type ids
 */
const CloseEventTypeId = exports.CloseEventTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Socket/CloseEvent");
/**
 * @since 1.0.0
 * @category models
 */
class CloseEvent {
  code;
  reason;
  /**
   * @since 1.0.0
   */
  [CloseEventTypeId];
  constructor(code = 1000, reason) {
    this.code = code;
    this.reason = reason;
    this[CloseEventTypeId] = CloseEventTypeId;
  }
  /**
   * @since 1.0.0
   */
  toString() {
    return this.reason ? `${this.code}: ${this.reason}` : `${this.code}`;
  }
}
/**
 * @since 1.0.0
 * @category refinements
 */
exports.CloseEvent = CloseEvent;
const isCloseEvent = u => Predicate.hasProperty(u, CloseEventTypeId);
/**
 * @since 1.0.0
 * @category type ids
 */
exports.isCloseEvent = isCloseEvent;
const SocketErrorTypeId = exports.SocketErrorTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Socket/SocketError");
/**
 * @since 1.0.0
 * @category refinements
 */
const isSocketError = u => Predicate.hasProperty(u, SocketErrorTypeId);
/**
 * @since 1.0.0
 * @category errors
 */
exports.isSocketError = isSocketError;
class SocketGenericError extends /*#__PURE__*/(0, _Error.TypeIdError)(SocketErrorTypeId, "SocketError") {
  get message() {
    return `An error occurred during ${this.reason}`;
  }
}
/**
 * @since 1.0.0
 * @category errors
 */
exports.SocketGenericError = SocketGenericError;
class SocketCloseError extends /*#__PURE__*/(0, _Error.TypeIdError)(SocketErrorTypeId, "SocketError") {
  /**
   * @since 1.0.0
   */
  static is(u) {
    return isSocketError(u) && u.reason === "Close";
  }
  /**
   * @since 1.0.0
   */
  static isClean(isClean) {
    return function (u) {
      return SocketCloseError.is(u) && isClean(u.code);
    };
  }
  get message() {
    if (this.closeReason) {
      return `${this.reason}: ${this.code}: ${this.closeReason}`;
    }
    return `${this.reason}: ${this.code}`;
  }
}
/**
 * @since 1.0.0
 * @category combinators
 */
exports.SocketCloseError = SocketCloseError;
const toChannel = self => Effect.scope.pipe(Effect.bindTo("scope"), Effect.bind("writeScope", ({
  scope
}) => Scope.fork(scope, ExecutionStrategy.sequential)), Effect.bind("write", ({
  writeScope
}) => Scope.extend(self.writer, writeScope)), Effect.bind("exitQueue", _ => Queue.unbounded()), Effect.let("input", ({
  exitQueue,
  write,
  writeScope
}) => ({
  awaitRead: () => Effect.void,
  emit(chunk) {
    return Effect.catchAllCause(Effect.forEach(chunk, write, {
      discard: true
    }), cause => Queue.offer(exitQueue, Exit.failCause(cause)));
  },
  error(error) {
    return Effect.zipRight(Scope.close(writeScope, Exit.void), Queue.offer(exitQueue, Exit.failCause(error)));
  },
  done() {
    return Scope.close(writeScope, Exit.void);
  }
})), Effect.tap(({
  exitQueue
}) => self.run(data => Queue.offer(exitQueue, Exit.succeed(Chunk.of(data)))).pipe(Effect.zipRight(Effect.failCause(Cause.empty)), Effect.exit, Effect.tap(exit => Queue.offer(exitQueue, exit)), Effect.fork, Effect.interruptible)), Effect.map(({
  exitQueue,
  input
}) => {
  const loop = Channel.flatMap(Queue.take(exitQueue), Exit.match({
    onFailure: cause => Cause.isEmptyType(cause) ? Channel.void : Channel.failCause(cause),
    onSuccess: chunk => Channel.zipRight(Channel.write(chunk), loop)
  }));
  return Channel.embedInput(loop, input);
}), Channel.unwrapScoped);
/**
 * @since 1.0.0
 * @category combinators
 */
exports.toChannel = toChannel;
const toChannelWith = () => self => toChannel(self);
/**
 * @since 1.0.0
 * @category constructors
 */
exports.toChannelWith = toChannelWith;
const makeChannel = () => Channel.unwrap(Effect.map(Socket, toChannelWith()));
/**
 * @since 1.0.0
 */
exports.makeChannel = makeChannel;
const defaultCloseCodeIsError = code => code !== 1000 && code !== 1006;
/**
 * @since 1.0.0
 * @category tags
 */
exports.defaultCloseCodeIsError = defaultCloseCodeIsError;
const WebSocket = exports.WebSocket = /*#__PURE__*/Context.GenericTag("@effect/platform/Socket/WebSocket");
/**
 * @since 1.0.0
 * @category tags
 */
const WebSocketConstructor = exports.WebSocketConstructor = /*#__PURE__*/Context.GenericTag("@effect/platform/Socket/WebSocketConstructor");
/**
 * @since 1.0.0
 * @category layers
 */
const layerWebSocketConstructorGlobal = exports.layerWebSocketConstructorGlobal = /*#__PURE__*/Layer.succeed(WebSocketConstructor, url => new globalThis.WebSocket(url));
/**
 * @since 1.0.0
 * @category constructors
 */
const makeWebSocket = (url, options) => fromWebSocket(Effect.acquireRelease((typeof url === "string" ? Effect.succeed(url) : url).pipe(Effect.flatMap(url => Effect.map(WebSocketConstructor, f => f(url)))), ws => Effect.sync(() => {
  ws.onclose = null;
  ws.onerror = null;
  ws.onmessage = null;
  ws.onopen = null;
  return ws.close();
})), options);
/**
 * @since 1.0.0
 * @category constructors
 */
exports.makeWebSocket = makeWebSocket;
const fromWebSocket = (acquire, options) => Effect.withFiberRuntime(fiber => Effect.map(Queue.dropping(fiber.getFiberRef(currentSendQueueCapacity)), sendQueue => {
  const acquireContext = fiber.getFiberRef(FiberRef.currentContext);
  const closeCodeIsError = options?.closeCodeIsError ?? defaultCloseCodeIsError;
  const runRaw = handler => Effect.scope.pipe(Effect.bindTo("scope"), Effect.bind("ws", ({
    scope
  }) => acquire.pipe(Effect.provide(Context.add(acquireContext, Scope.Scope, scope)))), Effect.bind("fiberSet", _ => FiberSet.make()), Effect.bind("run", ({
    fiberSet,
    ws
  }) => Effect.provideService(FiberSet.runtime(fiberSet)(), WebSocket, ws)), Effect.tap(({
    fiberSet,
    run,
    ws
  }) => {
    let open = false;
    ws.onmessage = event => {
      run(handler(typeof event.data === "string" ? event.data : event.data instanceof Uint8Array ? event.data : new Uint8Array(event.data)));
    };
    ws.onclose = event => {
      Deferred.unsafeDone(fiberSet.deferred, Effect.fail(new SocketCloseError({
        reason: "Close",
        code: event.code,
        closeReason: event.reason
      })));
    };
    ws.onerror = cause => {
      Deferred.unsafeDone(fiberSet.deferred, Effect.fail(new SocketGenericError({
        reason: open ? "Read" : "Open",
        cause
      })));
    };
    if (ws.readyState !== 1) {
      return Effect.async(resume => {
        function onOpen() {
          ws.removeEventListener("open", onOpen);
          resume(Effect.void);
        }
        ws.addEventListener("open", onOpen);
        return Effect.sync(() => {
          ws.removeEventListener("open", onOpen);
        });
      }).pipe(Effect.tap(_ => {
        open = true;
      }), Effect.timeoutFail({
        duration: options?.openTimeout ?? 10000,
        onTimeout: () => new SocketGenericError({
          reason: "OpenTimeout",
          cause: "timeout waiting for \"open\""
        })
      }), Effect.raceFirst(FiberSet.join(fiberSet)));
    }
    open = true;
    return Effect.void;
  }), Effect.tap(({
    fiberSet,
    ws
  }) => Queue.take(sendQueue).pipe(Effect.tap(chunk => isCloseEvent(chunk) ? Effect.failSync(() => {
    ws.close(chunk.code, chunk.reason);
    return new SocketCloseError({
      reason: "Close",
      code: chunk.code,
      closeReason: chunk.reason
    });
  }) : Effect.try({
    try: () => ws.send(chunk),
    catch: cause => new SocketGenericError({
      reason: "Write",
      cause
    })
  })), Effect.forever, FiberSet.run(fiberSet))), Effect.tap(({
    fiberSet
  }) => Effect.catchIf(FiberSet.join(fiberSet), SocketCloseError.isClean(_ => !closeCodeIsError(_)), _ => Effect.void)), Effect.scoped, Effect.interruptible);
  const encoder = new TextEncoder();
  const run = handler => runRaw(data => typeof data === "string" ? handler(encoder.encode(data)) : handler(data));
  const write = chunk => Queue.offer(sendQueue, chunk);
  const writer = Effect.succeed(write);
  return Socket.of({
    [TypeId]: TypeId,
    run,
    runRaw,
    writer
  });
}));
/**
 * @since 1.0.0
 * @category constructors
 */
exports.fromWebSocket = fromWebSocket;
const makeWebSocketChannel = (url, options) => Channel.unwrapScoped(Effect.map(makeWebSocket(url, options), toChannelWith()));
/**
 * @since 1.0.0
 * @category layers
 */
exports.makeWebSocketChannel = makeWebSocketChannel;
const layerWebSocket = (url, options) => Layer.effect(Socket, makeWebSocket(url, options));
/**
 * @since 1.0.0
 * @category fiber refs
 */
exports.layerWebSocket = layerWebSocket;
const currentSendQueueCapacity = exports.currentSendQueueCapacity = /*#__PURE__*/(0, _GlobalValue.globalValue)("@effect/platform/Socket/currentSendQueueCapacity", () => FiberRef.unsafeMake(16));
/**
 * @since 1.0.0
 * @category constructors
 */
const fromTransformStream = (acquire, options) => {
  const EOF = Symbol();
  return Effect.withFiberRuntime(fiber => Effect.map(Queue.dropping(fiber.getFiberRef(currentSendQueueCapacity)), sendQueue => {
    const acquireContext = fiber.getFiberRef(FiberRef.currentContext);
    const closeCodeIsError = options?.closeCodeIsError ?? defaultCloseCodeIsError;
    const runRaw = handler => Effect.scope.pipe(Effect.bindTo("scope"), Effect.bind("stream", ({
      scope
    }) => acquire.pipe(Effect.provide(Context.add(acquireContext, Scope.Scope, scope)))), Effect.bind("reader", ({
      stream
    }) => Effect.acquireRelease(Effect.sync(() => stream.readable.getReader()), reader => Effect.promise(() => reader.cancel()).pipe(Effect.tap(() => {
      reader.releaseLock();
    })))), Effect.bind("writer", ({
      stream
    }) => Effect.acquireRelease(Effect.sync(() => stream.writable.getWriter()), reader => Effect.sync(() => reader.releaseLock()))), Effect.bind("fiberSet", () => FiberSet.make()), Effect.tap(({
      fiberSet,
      writer
    }) => {
      const encoder = new TextEncoder();
      return Queue.take(sendQueue).pipe(Effect.tap(chunk => {
        if (chunk === EOF || isCloseEvent(chunk)) {
          return Effect.zipRight(Effect.promise(() => writer.close()), chunk === EOF ? Effect.interrupt : Effect.fail(new SocketCloseError({
            reason: "Close",
            code: chunk.code,
            closeReason: chunk.reason
          })));
        }
        return Effect.try({
          try: () => {
            if (typeof chunk === "string") {
              writer.write(encoder.encode(chunk));
            } else {
              writer.write(chunk);
            }
          },
          catch: cause => new SocketGenericError({
            reason: "Write",
            cause
          })
        });
      }), Effect.forever, FiberSet.run(fiberSet));
    }), Effect.tap(({
      fiberSet,
      reader
    }) => Effect.tryPromise({
      try: () => reader.read(),
      catch: cause => new SocketGenericError({
        reason: "Read",
        cause
      })
    }).pipe(Effect.tap(result => {
      if (result.done) {
        return Effect.fail(new SocketCloseError({
          reason: "Close",
          code: 1000
        }));
      }
      return handler(result.value);
    }), Effect.forever, FiberSet.run(fiberSet))), Effect.tap(({
      fiberSet
    }) => Effect.catchIf(FiberSet.join(fiberSet), SocketCloseError.isClean(_ => !closeCodeIsError(_)), _ => Effect.void)), Effect.scoped, Effect.interruptible);
    const encoder = new TextEncoder();
    const run = handler => runRaw(data => typeof data === "string" ? handler(encoder.encode(data)) : handler(data));
    const write = chunk => Queue.offer(sendQueue, chunk);
    const writer = Effect.acquireRelease(Effect.succeed(write), () => Queue.offer(sendQueue, EOF));
    return Socket.of({
      [TypeId]: TypeId,
      run,
      runRaw,
      writer
    });
  }));
};
exports.fromTransformStream = fromTransformStream;
//# sourceMappingURL=Socket.js.map