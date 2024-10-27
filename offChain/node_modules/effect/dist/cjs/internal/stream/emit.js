"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.makePush = exports.make = void 0;
var Cause = _interopRequireWildcard(require("../../Cause.js"));
var Chunk = _interopRequireWildcard(require("../../Chunk.js"));
var Effect = _interopRequireWildcard(require("../../Effect.js"));
var Exit = _interopRequireWildcard(require("../../Exit.js"));
var _Function = require("../../Function.js");
var Option = _interopRequireWildcard(require("../../Option.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const make = emit => {
  const ops = {
    chunk(as) {
      return this(Effect.succeed(as));
    },
    die(defect) {
      return this(Effect.die(defect));
    },
    dieMessage(message) {
      return this(Effect.dieMessage(message));
    },
    done(exit) {
      return this(Effect.suspend(() => Exit.mapBoth(exit, {
        onFailure: Option.some,
        onSuccess: Chunk.of
      })));
    },
    end() {
      return this(Effect.fail(Option.none()));
    },
    fail(e) {
      return this(Effect.fail(Option.some(e)));
    },
    fromEffect(effect) {
      return this(Effect.mapBoth(effect, {
        onFailure: Option.some,
        onSuccess: Chunk.of
      }));
    },
    fromEffectChunk(effect) {
      return this((0, _Function.pipe)(effect, Effect.mapError(Option.some)));
    },
    halt(cause) {
      return this(Effect.failCause((0, _Function.pipe)(cause, Cause.map(Option.some))));
    },
    single(value) {
      return this(Effect.succeed(Chunk.of(value)));
    }
  };
  return Object.assign(emit, ops);
};
/** @internal */
exports.make = make;
const makePush = (queue, scheduler) => {
  let finished = false;
  let buffer = [];
  let running = false;
  function array(items) {
    if (finished) return false;
    if (items.length <= 50_000) {
      buffer.push.apply(buffer, items);
    } else {
      for (let i = 0; i < items.length; i++) {
        buffer.push(items[0]);
      }
    }
    if (!running) {
      running = true;
      scheduler.scheduleTask(flush, 0);
    }
    return true;
  }
  function flush() {
    running = false;
    if (buffer.length > 0) {
      queue.unsafeOffer(buffer);
      buffer = [];
    }
  }
  function done(exit) {
    if (finished) return;
    finished = true;
    if (exit._tag === "Success") {
      buffer.push(exit.value);
    }
    flush();
    queue.unsafeOffer(exit._tag === "Success" ? Exit.void : exit);
  }
  return {
    single(value) {
      if (finished) return false;
      buffer.push(value);
      if (!running) {
        running = true;
        scheduler.scheduleTask(flush, 0);
      }
      return true;
    },
    array,
    chunk(chunk) {
      return array(Chunk.toReadonlyArray(chunk));
    },
    done,
    end() {
      if (finished) return;
      finished = true;
      flush();
      queue.unsafeOffer(Exit.void);
    },
    halt(cause) {
      return done(Exit.failCause(cause));
    },
    fail(error) {
      return done(Exit.fail(error));
    },
    die(defect) {
      return done(Exit.die(defect));
    },
    dieMessage(message) {
      return done(Exit.die(new Error(message)));
    }
  };
};
exports.makePush = makePush;
//# sourceMappingURL=emit.js.map