"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.locally = exports.let = exports.isMicroCause = exports.isMicro = exports.isHandle = exports.interruptible = exports.interrupt = exports.ignoreLogged = exports.ignore = exports.getEnvRef = exports.gen = exports.fromOption = exports.fromExitSync = exports.fromExit = exports.fromEither = exports.forkScoped = exports.forkIn = exports.forkDaemon = exports.fork = exports.forever = exports.forEach = exports.flip = exports.flatten = exports.flatMap = exports.filterOrFailCause = exports.filterOrFail = exports.filterMap = exports.filter = exports.failSync = exports.failCauseSync = exports.failCause = exports.fail = exports.exitVoid = exports.exitSucceed = exports.exitIsSuccess = exports.exitIsInterrupt = exports.exitIsFailure = exports.exitIsFail = exports.exitIsDie = exports.exitInterrupt = exports.exitFailCause = exports.exitFail = exports.exitDie = exports.exit = exports.envUnsafeMakeEmpty = exports.envSet = exports.envRefMake = exports.envMutate = exports.envMake = exports.envGet = exports.ensuring = exports.either = exports.die = exports.delay = exports.currentScheduler = exports.currentMaxDepthBeforeYield = exports.currentContext = exports.currentConcurrency = exports.currentAbortSignal = exports.currentAbortController = exports.context = exports.causeWithTrace = exports.causeSquash = exports.causeIsInterrupt = exports.causeIsFail = exports.causeIsDie = exports.causeInterrupt = exports.causeFail = exports.causeDie = exports.catchTag = exports.catchIf = exports.catchCauseIf = exports.catchAllDefect = exports.catchAllCause = exports.catchAll = exports.bindTo = exports.bind = exports.async = exports.asVoid = exports.asSome = exports.as = exports.andThen = exports.all = exports.addFinalizer = exports.acquireUseRelease = exports.acquireRelease = exports.TypeId = exports.TimeoutException = exports.TaggedError = exports.NoSuchElementException = exports.MicroScopeTypeId = exports.MicroScope = exports.MicroSchedulerDefault = exports.MicroCauseTypeId = exports.HandleTypeId = exports.Error = exports.EnvTypeId = exports.EnvRefTypeId = exports.Do = exports.Class = void 0;
exports.zipWith = exports.zip = exports.yieldWithPriority = exports.yieldNow = exports.yieldFlush = exports.withTrace = exports.withConcurrency = exports.when = exports.void = exports.uninterruptibleMask = exports.uninterruptible = exports.tryPromise = exports.try = exports.timeoutOrElse = exports.timeoutOption = exports.timeout = exports.tapErrorCauseIf = exports.tapErrorCause = exports.tapError = exports.tapDefect = exports.tap = exports.sync = exports.suspend = exports.succeedSome = exports.succeedNone = exports.succeed = exports.sleep = exports.serviceOption = exports.service = exports.scoped = exports.scopeUnsafeMake = exports.scopeMake = exports.scope = exports.scheduleWithMaxElapsed = exports.scheduleWithMaxDelay = exports.scheduleUnion = exports.scheduleSpaced = exports.scheduleRecurs = exports.scheduleIntersect = exports.scheduleExponential = exports.scheduleAddDelay = exports.sandbox = exports.runSyncExit = exports.runSync = exports.runSymbol = exports.runPromiseExit = exports.runPromise = exports.runFork = exports.retry = exports.repeatExit = exports.repeat = exports.raceFirst = exports.raceAllFirst = exports.raceAll = exports.race = exports.provideServiceEffect = exports.provideService = exports.provideScope = exports.provideContext = exports.promise = exports.orElseSucceed = exports.orDie = exports.option = exports.onInterrupt = exports.onExitIf = exports.onExit = exports.onError = exports.never = exports.matchEffect = exports.matchCauseEffect = exports.matchCause = exports.match = exports.mapErrorCause = exports.mapError = exports.map = exports.make = void 0;
var Context = _interopRequireWildcard(require("./Context.js"));
var Effectable = _interopRequireWildcard(require("./Effectable.js"));
var Either = _interopRequireWildcard(require("./Either.js"));
var _Function = require("./Function.js");
var _GlobalValue = require("./GlobalValue.js");
var _Inspectable = require("./Inspectable.js");
var doNotation = _interopRequireWildcard(require("./internal/doNotation.js"));
var _effectable = require("./internal/effectable.js");
var _singleShotGen = require("./internal/singleShotGen.js");
var Option = _interopRequireWildcard(require("./Option.js"));
var _Pipeable = require("./Pipeable.js");
var _Predicate = require("./Predicate.js");
var _Utils = require("./Utils.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.4.0
 * @experimental
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("effect/Micro");
/**
 * @since 3.4.0
 * @experimental
 * @category symbols
 */
const runSymbol = exports.runSymbol = /*#__PURE__*/Symbol.for("effect/Micro/runSymbol");
/**
 * @since 3.4.0
 * @experimental
 * @category guards
 */
const isMicro = u => typeof u === "object" && u !== null && TypeId in u;
// ----------------------------------------------------------------------------
// Microable
// ----------------------------------------------------------------------------
exports.isMicro = isMicro;
const MicroProto = {
  ...Effectable.EffectPrototype,
  _op: "Micro",
  [TypeId]: {
    _A: _Function.identity,
    _E: _Function.identity,
    _R: _Function.identity
  },
  [Symbol.iterator]() {
    return new _singleShotGen.SingleShotGen(new _Utils.YieldWrap(this));
  }
};
const MicroBase = /*#__PURE__*/function () {
  function Base() {}
  Base.prototype = MicroProto;
  return Base;
}();
/**
 * @since 3.8.4
 * @experimental
 * @category constructors
 */
class Class extends MicroBase {
  /**
   * @since 3.8.4
   * @experimental
   */
  [runSymbol](env, onExit) {
    this.asMicro()[runSymbol](env, onExit);
  }
}
// ----------------------------------------------------------------------------
// MicroCause
// ----------------------------------------------------------------------------
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.Class = Class;
const MicroCauseTypeId = exports.MicroCauseTypeId = /*#__PURE__*/Symbol.for("effect/Micro/MicroCause");
/**
 * @since 3.6.6
 * @experimental
 * @category guards
 */
const isMicroCause = self => (0, _Predicate.hasProperty)(self, MicroCauseTypeId);
exports.isMicroCause = isMicroCause;
const microCauseVariance = {
  _E: _Function.identity
};
class MicroCauseImpl extends globalThis.Error {
  _tag;
  traces;
  [MicroCauseTypeId];
  constructor(_tag, originalError, traces) {
    const causeName = `MicroCause.${_tag}`;
    let name;
    let message;
    let stack;
    if (originalError instanceof globalThis.Error) {
      name = `(${causeName}) ${originalError.name}`;
      message = originalError.message;
      const messageLines = message.split("\n").length;
      stack = originalError.stack ? `(${causeName}) ${originalError.stack.split("\n").slice(0, messageLines + 3).join("\n")}` : `${name}: ${message}`;
    } else {
      name = causeName;
      message = (0, _Inspectable.toStringUnknown)(originalError, 0);
      stack = `${name}: ${message}`;
    }
    if (traces.length > 0) {
      stack += `\n    ${traces.join("\n    ")}`;
    }
    super(message);
    this._tag = _tag;
    this.traces = traces;
    this[MicroCauseTypeId] = microCauseVariance;
    this.name = name;
    this.stack = stack;
  }
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  }
  toString() {
    return this.stack;
  }
  [_Inspectable.NodeInspectSymbol]() {
    return this.stack;
  }
}
class FailImpl extends MicroCauseImpl {
  error;
  constructor(error, traces = []) {
    super("Fail", error, traces);
    this.error = error;
  }
}
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
const causeFail = (error, traces = []) => new FailImpl(error, traces);
exports.causeFail = causeFail;
class DieImpl extends MicroCauseImpl {
  defect;
  constructor(defect, traces = []) {
    super("Die", defect, traces);
    this.defect = defect;
  }
}
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
const causeDie = (defect, traces = []) => new DieImpl(defect, traces);
exports.causeDie = causeDie;
class InterruptImpl extends MicroCauseImpl {
  constructor(traces = []) {
    super("Interrupt", "interrupted", traces);
  }
}
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
const causeInterrupt = (traces = []) => new InterruptImpl(traces);
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.causeInterrupt = causeInterrupt;
const causeIsFail = self => self._tag === "Fail";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.causeIsFail = causeIsFail;
const causeIsDie = self => self._tag === "Die";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.causeIsDie = causeIsDie;
const causeIsInterrupt = self => self._tag === "Interrupt";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.causeIsInterrupt = causeIsInterrupt;
const causeSquash = self => self._tag === "Fail" ? self.error : self._tag === "Die" ? self.defect : self;
/**
 * @since 3.4.6
 * @experimental
 * @category MicroCause
 */
exports.causeSquash = causeSquash;
const causeWithTrace = exports.causeWithTrace = /*#__PURE__*/(0, _Function.dual)(2, (self, trace) => {
  const traces = [...self.traces, trace];
  switch (self._tag) {
    case "Die":
      return causeDie(self.defect, traces);
    case "Interrupt":
      return causeInterrupt(traces);
    case "Fail":
      return causeFail(self.error, traces);
  }
});
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitInterrupt = exports.exitInterrupt = /*#__PURE__*/Either.left( /*#__PURE__*/causeInterrupt());
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitSucceed = exports.exitSucceed = Either.right;
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitFail = e => Either.left(causeFail(e));
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
exports.exitFail = exitFail;
const exitDie = defect => Either.left(causeDie(defect));
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
exports.exitDie = exitDie;
const exitFailCause = exports.exitFailCause = Either.left;
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitIsSuccess = exports.exitIsSuccess = Either.isRight;
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitIsFailure = exports.exitIsFailure = Either.isLeft;
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
const exitIsInterrupt = self => exitIsFailure(self) && self.left._tag === "Interrupt";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
exports.exitIsInterrupt = exitIsInterrupt;
const exitIsFail = self => exitIsFailure(self) && self.left._tag === "Fail";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
exports.exitIsFail = exitIsFail;
const exitIsDie = self => exitIsFailure(self) && self.left._tag === "Die";
/**
 * @since 3.4.6
 * @experimental
 * @category MicroExit
 */
exports.exitIsDie = exitIsDie;
const exitVoid = exports.exitVoid = /*#__PURE__*/exitSucceed(void 0);
// ----------------------------------------------------------------------------
// env
// ----------------------------------------------------------------------------
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const EnvTypeId = exports.EnvTypeId = /*#__PURE__*/Symbol.for("effect/Micro/Env");
const EnvProto = {
  [EnvTypeId]: {
    _R: _Function.identity
  },
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  }
};
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const envMake = refs => {
  const self = Object.create(EnvProto);
  self.refs = refs;
  return self;
};
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.envMake = envMake;
const envUnsafeMakeEmpty = () => {
  const controller = new AbortController();
  const refs = Object.create(null);
  refs[currentAbortController.key] = controller;
  refs[currentAbortSignal.key] = controller.signal;
  refs[currentScheduler.key] = new MicroSchedulerDefault();
  return envMake(refs);
};
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.envUnsafeMakeEmpty = envUnsafeMakeEmpty;
const envGet = exports.envGet = /*#__PURE__*/(0, _Function.dual)(2, (self, ref) => ref.key in self.refs ? self.refs[ref.key] : ref.initial);
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const envSet = exports.envSet = /*#__PURE__*/(0, _Function.dual)(3, (self, ref, value) => {
  const refs = Object.assign(Object.create(null), self.refs);
  refs[ref.key] = value;
  return envMake(refs);
});
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const envMutate = exports.envMutate = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => envMake(f(Object.assign(Object.create(null), self.refs))));
/**
 * Access the given `Context.Tag` from the environment.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const service = tag => make(function (env, onExit) {
  onExit(exitSucceed(Context.get(envGet(env, currentContext), tag)));
});
/**
 * Access the given `Context.Tag` from the environment, without tracking the
 * dependency at the type level.
 *
 * It will return an `Option` of the service, depending on whether it is
 * available in the environment or not.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.service = service;
const serviceOption = tag => make(function (env, onExit) {
  onExit(exitSucceed(Context.getOption(envGet(env, currentContext), tag)));
});
/**
 * Retrieve the current value of the given `EnvRef`.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.serviceOption = serviceOption;
const getEnvRef = envRef => make((env, onExit) => onExit(Either.right(envGet(env, envRef))));
/**
 * Set the value of the given `EnvRef` for the duration of the effect.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.getEnvRef = getEnvRef;
const locally = exports.locally = /*#__PURE__*/(0, _Function.dual)(3, (self, fiberRef, value) => make((env, onExit) => self[runSymbol](envSet(env, fiberRef, value), onExit)));
/**
 * Access the current `Context` from the environment.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const context = () => getEnvRef(currentContext);
/**
 * Merge the given `Context` with the current context.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.context = context;
const provideContext = exports.provideContext = /*#__PURE__*/(0, _Function.dual)(2, (self, provided) => make(function (env, onExit) {
  const context = envGet(env, currentContext);
  const nextEnv = envSet(env, currentContext, Context.merge(context, provided));
  self[runSymbol](nextEnv, onExit);
}));
/**
 * Add the provided service to the current context.
 *
 * @since 3.4.0
 * @experimental
 * @category environment
 */
const provideService = exports.provideService = /*#__PURE__*/(0, _Function.dual)(3, (self, tag, service) => make(function (env, onExit) {
  const context = envGet(env, currentContext);
  const nextEnv = envSet(env, currentContext, Context.add(context, tag, service));
  self[runSymbol](nextEnv, onExit);
}));
/**
 * Create a service using the provided `Micro` effect, and add it to the
 * current context.
 *
 * @since 3.4.6
 * @experimental
 * @category environment
 */
const provideServiceEffect = exports.provideServiceEffect = /*#__PURE__*/(0, _Function.dual)(3, (self, tag, acquire) => flatMap(acquire, service => provideService(self, tag, service)));
const setImmediate = "setImmediate" in globalThis ? globalThis.setImmediate : f => setTimeout(f, 0);
/**
 * @since 3.5.9
 * @experimental
 * @category scheduler
 */
class MicroSchedulerDefault {
  tasks = [];
  running = false;
  /**
   * @since 3.5.9
   */
  scheduleTask(task, _priority) {
    this.tasks.push(task);
    if (!this.running) {
      this.running = true;
      setImmediate(this.afterScheduled);
    }
  }
  /**
   * @since 3.5.9
   */
  afterScheduled = () => {
    this.running = false;
    this.runTasks();
  };
  /**
   * @since 3.5.9
   */
  runTasks() {
    const tasks = this.tasks;
    this.tasks = [];
    for (let i = 0, len = tasks.length; i < len; i++) {
      tasks[i]();
    }
  }
  /**
   * @since 3.5.9
   */
  shouldYield(_env) {
    return false;
  }
  /**
   * @since 3.5.9
   */
  flush() {
    while (this.tasks.length > 0) {
      this.runTasks();
    }
  }
}
// ========================================================================
// Env refs
// ========================================================================
/**
 * @since 3.4.0
 * @experimental
 * @category environment
 */
exports.MicroSchedulerDefault = MicroSchedulerDefault;
const EnvRefTypeId = exports.EnvRefTypeId = /*#__PURE__*/Symbol.for("effect/Micro/EnvRef");
const EnvRefProto = {
  ...MicroProto,
  [EnvRefTypeId]: EnvRefTypeId,
  [runSymbol](env, onExit) {
    getEnvRef(this)[runSymbol](env, onExit);
  }
};
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const envRefMake = (key, initial) => (0, _GlobalValue.globalValue)(key, () => {
  const self = Object.create(EnvRefProto);
  self.key = key;
  self.initial = initial();
  return self;
});
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
exports.envRefMake = envRefMake;
const currentAbortController = exports.currentAbortController = /*#__PURE__*/envRefMake("effect/Micro/currentAbortController", () => undefined);
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const currentAbortSignal = exports.currentAbortSignal = /*#__PURE__*/envRefMake("effect/Micro/currentAbortSignal", () => undefined);
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const currentContext = exports.currentContext = /*#__PURE__*/envRefMake("effect/Micro/currentContext", () => Context.empty());
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const currentConcurrency = exports.currentConcurrency = /*#__PURE__*/envRefMake("effect/Micro/currentConcurrency", () => "unbounded");
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const currentMaxDepthBeforeYield = exports.currentMaxDepthBeforeYield = /*#__PURE__*/envRefMake("effect/Micro/currentMaxDepthBeforeYield", () => 2048);
const currentInterruptible = /*#__PURE__*/envRefMake("effect/Micro/currentInterruptible", () => true);
/**
 * @since 3.4.0
 * @experimental
 * @category environment refs
 */
const currentScheduler = exports.currentScheduler = /*#__PURE__*/envRefMake("effect/Micro/currentScheduler", () => new MicroSchedulerDefault());
/**
 * If you have a `Micro` that uses `concurrency: "inherit"`, you can use this
 * api to control the concurrency of that `Micro` when it is run.
 *
 * @since 3.4.0
 * @experimental
 * @category environment refs
 * @example
 * import * as Micro from "effect/Micro"
 *
 * Micro.forEach([1, 2, 3], (n) => Micro.succeed(n), {
 *   concurrency: "inherit"
 * }).pipe(
 *   Micro.withConcurrency(2) // use a concurrency of 2
 * )
 */
const withConcurrency = exports.withConcurrency = /*#__PURE__*/(0, _Function.dual)(2, (self, concurrency) => locally(self, currentConcurrency, concurrency));
// ----------------------------------------------------------------------------
// constructors
// ----------------------------------------------------------------------------
const microDepthState = /*#__PURE__*/(0, _GlobalValue.globalValue)("effect/Micro/microDepthState", () => ({
  depth: 0,
  maxDepthBeforeYield: currentMaxDepthBeforeYield.initial
}));
const unsafeMake = run => {
  const self = Object.create(MicroProto);
  self[runSymbol] = run;
  return self;
};
const unsafeMakeOptions = (run, checkAbort) => unsafeMake(function execute(env, onExit) {
  if (checkAbort && env.refs[currentInterruptible.key] !== false && env.refs[currentAbortSignal.key].aborted) {
    return onExit(exitInterrupt);
  }
  microDepthState.depth++;
  if (microDepthState.depth === 1) {
    microDepthState.maxDepthBeforeYield = envGet(env, currentMaxDepthBeforeYield);
  }
  const scheduler = env.refs[currentScheduler.key];
  if (microDepthState.depth >= microDepthState.maxDepthBeforeYield || scheduler.shouldYield(env)) {
    scheduler.scheduleTask(() => execute(env, onExit), 0);
  } else {
    try {
      run(env, onExit);
    } catch (err) {
      onExit(exitDie(err));
    }
  }
  microDepthState.depth--;
});
/**
 * A low-level constructor for creating a `Micro` effect. It takes a function
 * that receives an environment and a callback which should be called with the
 * result of the effect.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const make = run => unsafeMakeOptions(run, true);
/**
 * Converts a `MicroExit` into a `Micro` effect.
 *
 * @since 3.4.6
 * @experimental
 * @category constructors
 */
exports.make = make;
const fromExit = self => make(function (_env, onExit) {
  onExit(self);
});
/**
 * Converts a lazy `MicroExit` into a `Micro` effect.
 *
 * @since 3.4.6
 * @experimental
 * @category constructors
 */
exports.fromExit = fromExit;
const fromExitSync = self => make(function (_env, onExit) {
  onExit(self());
});
/**
 * Creates a `Micro` effect that will succeed with the specified constant value.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.fromExitSync = fromExitSync;
const succeed = a => fromExit(exitSucceed(a));
/**
 * Creates a `Micro` effect that will succeed with `Option.Some` of the value.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.succeed = succeed;
const succeedSome = a => succeed(Option.some(a));
/**
 * Creates a `Micro` effect that will succeed with `Option.None`.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.succeedSome = succeedSome;
const succeedNone = exports.succeedNone = /*#__PURE__*/succeed( /*#__PURE__*/Option.none());
/**
 * Creates a `Micro` effect that will fail with the specified error.
 *
 * This will result in a `CauseFail`, where the error is tracked at the
 * type level.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const fail = e => fromExit(exitFail(e));
/**
 * Creates a `Micro` effect that will fail with the lazily evaluated error.
 *
 * This will result in a `CauseFail`, where the error is tracked at the
 * type level.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.fail = fail;
const failSync = e => make(function (_env, onExit) {
  onExit(exitFail(e()));
});
/**
 * Creates a `Micro` effect that will die with the specified error.
 *
 * This will result in a `CauseDie`, where the error is not tracked at
 * the type level.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.failSync = failSync;
const die = defect => fromExit(exitDie(defect));
/**
 * Creates a `Micro` effect that will fail with the specified `MicroCause`.
 *
 * @since 3.4.6
 * @experimental
 * @category constructors
 */
exports.die = die;
const failCause = cause => fromExit(exitFailCause(cause));
/**
 * Creates a `Micro` effect that will fail with the lazily evaluated `MicroCause`.
 *
 * @since 3.4.6
 * @experimental
 * @category constructors
 */
exports.failCause = failCause;
const failCauseSync = cause => fromExitSync(() => exitFailCause(cause()));
/**
 * Creates a `Micro` effect that will succeed with the lazily evaluated value.
 *
 * If the evaluation of the value throws an error, the effect will fail with
 * `CauseDie`.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.failCauseSync = failCauseSync;
const sync = evaluate => make(function (_env, onExit) {
  onExit(exitSucceed(evaluate()));
});
/**
 * Converts an `Option` into a `Micro` effect, that will fail with
 * `NoSuchElementException` if the option is `None`. Otherwise, it will succeed with the
 * value of the option.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.sync = sync;
const fromOption = option => make(function (_env, onExit) {
  onExit(option._tag === "Some" ? exitSucceed(option.value) : exitFail(new NoSuchElementException({})));
});
/**
 * Converts an `Either` into a `Micro` effect, that will fail with the left side
 * of the either if it is a `Left`. Otherwise, it will succeed with the right
 * side of the either.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.fromOption = fromOption;
const fromEither = either => make(function (_env, onExit) {
  onExit(either._tag === "Right" ? either : exitFail(either.left));
});
/**
 * Lazily creates a `Micro` effect from the given side-effect.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.fromEither = fromEither;
const suspend = evaluate => make(function (env, onExit) {
  evaluate()[runSymbol](env, onExit);
});
exports.suspend = suspend;
const void_ = exports.void = /*#__PURE__*/succeed(void 0);
/**
 * Create a `Micro` effect from an asynchronous computation.
 *
 * You can return a cleanup effect that will be run when the effect is aborted.
 * It is also passed an `AbortSignal` that is triggered when the effect is
 * aborted.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const async = register => make(function (env, onExit) {
  let resumed = false;
  const controller = register.length > 1 ? new AbortController() : undefined;
  const signal = envGet(env, currentAbortSignal);
  let cleanup = undefined;
  function onAbort() {
    if (cleanup) {
      resume(uninterruptible(andThen(cleanup, fromExit(exitInterrupt))));
    } else {
      resume(fromExit(exitInterrupt));
    }
    if (controller !== undefined) {
      controller.abort();
    }
  }
  function resume(effect) {
    if (resumed) {
      return;
    }
    resumed = true;
    signal.removeEventListener("abort", onAbort);
    effect[runSymbol](env, onExit);
  }
  cleanup = controller === undefined ? register(resume) : register(resume, controller.signal);
  if (resumed) return;
  signal.addEventListener("abort", onAbort);
});
exports.async = async;
const try_ = options => make(function (_env, onExit) {
  try {
    onExit(exitSucceed(options.try()));
  } catch (err) {
    onExit(exitFail(options.catch(err)));
  }
});
exports.try = try_;
/**
 * Wrap a `Promise` into a `Micro` effect. Any errors will result in a
 * `CauseDie`.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const promise = evaluate => async(function (resume, signal) {
  evaluate(signal).then(a => resume(succeed(a)), e => resume(die(e)));
});
/**
 * Wrap a `Promise` into a `Micro` effect. Any errors will be caught and
 * converted into a specific error type.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 * @example
 * import { Micro } from "effect"
 *
 * Micro.tryPromise({
 *   try: () => Promise.resolve("success"),
 *   catch: (cause) => new Error("caught", { cause })
 * })
 */
exports.promise = promise;
const tryPromise = options => async(function (resume, signal) {
  try {
    options.try(signal).then(a => resume(succeed(a)), e => resume(fail(options.catch(e))));
  } catch (err) {
    resume(fail(options.catch(err)));
  }
});
/**
 * Pause the execution of the current `Micro` effect, and resume it on the next
 * iteration of the event loop.
 *
 * You can specify a priority for the task, which will determine when it is
 * executed relative to other tasks.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.tryPromise = tryPromise;
const yieldWithPriority = priority => make(function (env, onExit) {
  envGet(env, currentScheduler).scheduleTask(() => onExit(exitVoid), priority);
});
/**
 * Pause the execution of the current `Micro` effect, and resume it on the next
 * iteration of the event loop.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
exports.yieldWithPriority = yieldWithPriority;
const yieldNow = exports.yieldNow = /*#__PURE__*/yieldWithPriority(0);
/**
 * Flush any yielded effects that are waiting to be executed.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const yieldFlush = exports.yieldFlush = /*#__PURE__*/make(function (env, onExit) {
  envGet(env, currentScheduler).flush();
  onExit(exitVoid);
});
/**
 * A `Micro` that will never succeed or fail. It wraps `setInterval` to prevent
 * the Javascript runtime from exiting.
 *
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const never = exports.never = /*#__PURE__*/async(function () {
  const interval = setInterval(_Function.constVoid, 2147483646);
  return sync(() => clearInterval(interval));
});
/**
 * @since 3.4.0
 * @experimental
 * @category constructors
 */
const gen = (...args) => make(function (env, onExit) {
  const iterator = args.length === 1 ? args[0]() : args[1].call(args[0]);
  let running = false;
  let value = undefined;
  function run() {
    running = true;
    try {
      let shouldContinue = true;
      while (shouldContinue) {
        const result = iterator.next(value);
        if (result.done) {
          return onExit(exitSucceed(result.value));
        }
        shouldContinue = false;
        (0, _Utils.yieldWrapGet)(result.value)[runSymbol](env, function (exit) {
          if (exit._tag === "Left") {
            onExit(exit);
          } else {
            shouldContinue = true;
            value = exit.right;
            if (!running) run();
          }
        });
      }
    } catch (err) {
      onExit(exitDie(err));
    }
    running = false;
  }
  run();
});
// ----------------------------------------------------------------------------
// mapping & sequencing
// ----------------------------------------------------------------------------
/**
 * Flattens any nested `Micro` effects, merging the error and requirement types.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
exports.gen = gen;
const flatten = self => make(function (env, onExit) {
  self[runSymbol](env, exit => exit._tag === "Left" ? onExit(exit) : exit.right[runSymbol](env, onExit));
});
/**
 * Transforms the success value of the `Micro` effect with the specified
 * function.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
exports.flatten = flatten;
const map = exports.map = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    onExit(exit._tag === "Left" ? exit : exitSucceed(f(exit.right)));
  });
}));
/**
 * Create a `Micro` effect that will replace the success value of the given
 * effect.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
const as = exports.as = /*#__PURE__*/(0, _Function.dual)(2, (self, value) => map(self, _ => value));
/**
 * Wrap the success value of this `Micro` effect in an `Option.Some`.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
const asSome = self => map(self, Option.some);
/**
 * Map the success value of this `Micro` effect to another `Micro` effect, then
 * flatten the result.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
exports.asSome = asSome;
const flatMap = exports.flatMap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    if (exit._tag === "Left") {
      return onExit(exit);
    }
    f(exit.right)[runSymbol](env, onExit);
  });
}));
/**
 * Swap the error and success types of the `Micro` effect.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
const flip = self => matchEffect(self, {
  onFailure: succeed,
  onSuccess: fail
});
/**
 * A more flexible version of `flatMap`, that combines `map` and `flatMap` into
 * a single api.
 *
 * It also allows you to pass in a `Micro` effect directly, which will be
 * executed after the current effect.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
exports.flip = flip;
const andThen = exports.andThen = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    if (exit._tag === "Left") {
      return onExit(exit);
    } else if (envGet(env, currentAbortSignal).aborted) {
      return onExit(exitInterrupt);
    }
    const value = isMicro(f) ? f : typeof f === "function" ? f(exit.right) : f;
    if (isMicro(value)) {
      value[runSymbol](env, onExit);
    } else {
      onExit(exitSucceed(value));
    }
  });
}));
/**
 * Execute a side effect from the success value of the `Micro` effect.
 *
 * It is similar to the `andThen` api, but the success value is ignored.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
const tap = exports.tap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(function (env, onExit) {
  self[runSymbol](env, function (selfExit) {
    if (selfExit._tag === "Left") {
      return onExit(selfExit);
    } else if (envGet(env, currentAbortSignal).aborted) {
      return onExit(exitInterrupt);
    }
    const value = isMicro(f) ? f : typeof f === "function" ? f(selfExit.right) : f;
    if (isMicro(value)) {
      value[runSymbol](env, function (tapExit) {
        if (tapExit._tag === "Left") {
          return onExit(tapExit);
        }
        onExit(selfExit);
      });
    } else {
      onExit(selfExit);
    }
  });
}));
/**
 * Replace the success value of the `Micro` effect with `void`.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
const asVoid = self => map(self, _ => void 0);
/**
 * Access the `MicroExit` of the given `Micro` effect.
 *
 * @since 3.4.6
 * @experimental
 * @category mapping & sequencing
 */
exports.asVoid = asVoid;
const exit = self => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    onExit(exitSucceed(exit));
  });
});
/**
 * Replace the error type of the given `Micro` with the full `MicroCause` object.
 *
 * @since 3.4.0
 * @experimental
 * @category mapping & sequencing
 */
exports.exit = exit;
const sandbox = self => catchAllCause(self, cause => fail(cause));
exports.sandbox = sandbox;
function forkSignal(env) {
  const controller = new AbortController();
  const parentSignal = envGet(env, currentAbortSignal);
  function onAbort() {
    controller.abort();
    parentSignal.removeEventListener("abort", onAbort);
  }
  parentSignal.addEventListener("abort", onAbort);
  const envWithSignal = envMutate(env, function (refs) {
    refs[currentAbortController.key] = controller;
    refs[currentAbortSignal.key] = controller.signal;
    return refs;
  });
  return [envWithSignal, onAbort];
}
/**
 * Returns an effect that races all the specified effects,
 * yielding the value of the first effect to succeed with a value. Losers of
 * the race will be interrupted immediately
 *
 * @since 3.4.0
 * @experimental
 * @category sequencing
 */
const raceAll = all => make(function (env, onExit) {
  const [envWithSignal, onAbort] = forkSignal(env);
  const effects = Array.from(all);
  let len = effects.length;
  let index = 0;
  let done = 0;
  let exit = undefined;
  const causes = [];
  function onDone(exit_) {
    done++;
    if (exit_._tag === "Right" && exit === undefined) {
      len = index;
      exit = exit_;
      onAbort();
    } else if (exit_._tag === "Left") {
      causes.push(exit_.left);
    }
    if (done >= len) {
      onExit(exit ?? Either.left(causes[0]));
    }
  }
  for (; index < len; index++) {
    effects[index][runSymbol](envWithSignal, onDone);
  }
});
/**
 * Returns an effect that races all the specified effects,
 * yielding the value of the first effect to succeed or fail. Losers of
 * the race will be interrupted immediately
 *
 * @since 3.4.0
 * @experimental
 * @category sequencing
 */
exports.raceAll = raceAll;
const raceAllFirst = all => make(function (env, onExit) {
  const [envWithSignal, onAbort] = forkSignal(env);
  const effects = Array.from(all);
  let len = effects.length;
  let index = 0;
  let done = 0;
  let exit = undefined;
  const causes = [];
  function onDone(exit_) {
    done++;
    if (exit === undefined) {
      len = index;
      exit = exit_;
      onAbort();
    }
    if (done >= len) {
      onExit(exit ?? Either.left(causes[0]));
    }
  }
  for (; index < len; index++) {
    effects[index][runSymbol](envWithSignal, onDone);
  }
});
/**
 * Returns an effect that races two effects, yielding the value of the first
 * effect to succeed. Losers of the race will be interrupted immediately
 *
 * @since 3.4.0
 * @experimental
 * @category sequencing
 */
exports.raceAllFirst = raceAllFirst;
const race = exports.race = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => raceAll([self, that]));
/**
 * Returns an effect that races two effects, yielding the value of the first
 * effect to succeed *or* fail. Losers of the race will be interrupted immediately
 *
 * @since 3.4.0
 * @experimental
 * @category sequencing
 */
const raceFirst = exports.raceFirst = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => raceAllFirst([self, that]));
// ----------------------------------------------------------------------------
// zipping
// ----------------------------------------------------------------------------
/**
 * Combine two `Micro` effects into a single effect that produces a tuple of
 * their results.
 *
 * @since 3.4.0
 * @experimental
 * @category zipping
 */
const zip = exports.zip = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[1]), (self, that, options) => zipWith(self, that, (a, a2) => [a, a2], options));
/**
 * The `Micro.zipWith` function combines two `Micro` effects and allows you to
 * apply a function to the results of the combined effects, transforming them
 * into a single value.
 *
 * @since 3.4.3
 * @experimental
 * @category zipping
 */
const zipWith = exports.zipWith = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[1]), (self, that, f, options) => {
  if (options?.concurrent) {
    // Use `all` exclusively for concurrent cases, as it introduces additional overhead due to the management of concurrency
    return map(all([self, that], {
      concurrency: "unbounded"
    }), ([a, a2]) => f(a, a2));
  }
  return flatMap(self, a => map(that, a2 => f(a, a2)));
});
// ----------------------------------------------------------------------------
// filtering & conditionals
// ----------------------------------------------------------------------------
/**
 * Filter the specified effect with the provided function, failing with specified
 * `MicroCause` if the predicate fails.
 *
 * In addition to the filtering capabilities discussed earlier, you have the option to further
 * refine and narrow down the type of the success channel by providing a
 *
 * @since 3.4.0
 * @experimental
 * @category filtering & conditionals
 */
const filterOrFailCause = exports.filterOrFailCause = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[0]), (self, refinement, orFailWith) => flatMap(self, a => refinement(a) ? succeed(a) : failCause(orFailWith(a))));
/**
 * Filter the specified effect with the provided function, failing with specified
 * error if the predicate fails.
 *
 * In addition to the filtering capabilities discussed earlier, you have the option to further
 * refine and narrow down the type of the success channel by providing a
 *
 * @since 3.4.0
 * @experimental
 * @category filtering & conditionals
 */
const filterOrFail = exports.filterOrFail = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[0]), (self, refinement, orFailWith) => flatMap(self, a => refinement(a) ? succeed(a) : fail(orFailWith(a))));
/**
 * The moral equivalent of `if (p) exp`.
 *
 * @since 3.4.0
 * @experimental
 * @category filtering & conditionals
 */
const when = exports.when = /*#__PURE__*/(0, _Function.dual)(2, (self, condition) => flatMap(isMicro(condition) ? condition : sync(condition), pass => pass ? asSome(self) : succeed(Option.none())));
// ----------------------------------------------------------------------------
// repetition
// ----------------------------------------------------------------------------
/**
 * Repeat the given `Micro` using the provided options.
 *
 * The `while` predicate will be checked after each iteration, and can use the
 * fall `MicroExit` of the effect to determine if the repetition should continue.
 *
 * @since 3.4.6
 * @experimental
 * @category repetition
 */
const repeatExit = exports.repeatExit = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => make(function (env, onExit) {
  const startedAt = options.schedule ? Date.now() : 0;
  let attempt = 0;
  self[runSymbol](env, function loop(exit) {
    if (options.while !== undefined && !options.while(exit)) {
      return onExit(exit);
    } else if (options.times !== undefined && attempt >= options.times) {
      return onExit(exit);
    }
    attempt++;
    let delayEffect = yieldNow;
    if (options.schedule !== undefined) {
      const elapsed = Date.now() - startedAt;
      const duration = options.schedule(attempt, elapsed);
      if (Option.isNone(duration)) {
        return onExit(exit);
      }
      delayEffect = sleep(duration.value);
    }
    delayEffect[runSymbol](env, function (exit) {
      if (exit._tag === "Left") {
        return onExit(exit);
      }
      self[runSymbol](env, loop);
    });
  });
}));
/**
 * Repeat the given `Micro` effect using the provided options. Only successful
 * results will be repeated.
 *
 * @since 3.4.0
 * @experimental
 * @category repetition
 */
const repeat = exports.repeat = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[0]), (self, options) => repeatExit(self, {
  ...options,
  while: exit => exit._tag === "Right" && (options?.while === undefined || options.while(exit.right))
}));
/**
 * Repeat the given `Micro` effect forever, only stopping if the effect fails.
 *
 * @since 3.4.0
 * @experimental
 * @category repetition
 */
const forever = self => repeat(self);
/**
 * Create a `MicroSchedule` that will stop repeating after the specified number
 * of attempts.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
exports.forever = forever;
const scheduleRecurs = n => attempt => attempt <= n ? Option.some(0) : Option.none();
/**
 * Create a `MicroSchedule` that will generate a constant delay.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
exports.scheduleRecurs = scheduleRecurs;
const scheduleSpaced = millis => () => Option.some(millis);
/**
 * Create a `MicroSchedule` that will generate a delay with an exponential backoff.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
exports.scheduleSpaced = scheduleSpaced;
const scheduleExponential = (baseMillis, factor = 2) => attempt => Option.some(Math.pow(factor, attempt) * baseMillis);
/**
 * Returns a new `MicroSchedule` with an added calculated delay to each delay
 * returned by this schedule.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
exports.scheduleExponential = scheduleExponential;
const scheduleAddDelay = exports.scheduleAddDelay = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => (attempt, elapsed) => Option.map(self(attempt, elapsed), duration => duration + f()));
/**
 * Transform a `MicroSchedule` to one that will have a delay that will never exceed
 * the specified maximum.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
const scheduleWithMaxDelay = exports.scheduleWithMaxDelay = /*#__PURE__*/(0, _Function.dual)(2, (self, max) => (attempt, elapsed) => Option.map(self(attempt, elapsed), duration => Math.min(duration, max)));
/**
 * Transform a `MicroSchedule` to one that will stop repeating after the specified
 * amount of time.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
const scheduleWithMaxElapsed = exports.scheduleWithMaxElapsed = /*#__PURE__*/(0, _Function.dual)(2, (self, max) => (attempt, elapsed) => elapsed < max ? self(attempt, elapsed) : Option.none());
/**
 * Combines two `MicroSchedule`s, by recurring if either schedule wants to
 * recur, using the minimum of the two durations between recurrences.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
const scheduleUnion = exports.scheduleUnion = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => (attempt, elapsed) => Option.zipWith(self(attempt, elapsed), that(attempt, elapsed), (d1, d2) => Math.min(d1, d2)));
/**
 * Combines two `MicroSchedule`s, by recurring only if both schedules want to
 * recur, using the maximum of the two durations between recurrences.
 *
 * @since 3.4.6
 * @experimental
 * @category scheduling
 */
const scheduleIntersect = exports.scheduleIntersect = /*#__PURE__*/(0, _Function.dual)(2, (self, that) => (attempt, elapsed) => Option.zipWith(self(attempt, elapsed), that(attempt, elapsed), (d1, d2) => Math.max(d1, d2)));
// ----------------------------------------------------------------------------
// error handling
// ----------------------------------------------------------------------------
/**
 * Catch the full `MicroCause` object of the given `Micro` effect, allowing you to
 * recover from any kind of cause.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const catchAllCause = exports.catchAllCause = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchCauseIf(self, _Function.constTrue, f));
/**
 * Selectively catch a `MicroCause` object of the given `Micro` effect,
 * using the provided predicate to determine if the failure should be caught.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const catchCauseIf = exports.catchCauseIf = /*#__PURE__*/(0, _Function.dual)(3, (self, predicate, f) => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    if (exit._tag === "Right" || !predicate(exit.left)) {
      onExit(exit);
    } else {
      f(exit.left)[runSymbol](env, onExit);
    }
  });
}));
/**
 * Catch the error of the given `Micro` effect, allowing you to recover from it.
 *
 * It only catches expected (`MicroCause.Fail`) errors.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const catchAll = exports.catchAll = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchAllCause(self, cause => causeIsFail(cause) ? f(cause.error) : failCause(cause)));
/**
 * Catch any unexpected errors of the given `Micro` effect, allowing you to recover from them.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const catchAllDefect = exports.catchAllDefect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchCauseIf(self, causeIsDie, die => f(die.defect)));
/**
 * Perform a side effect using the full `MicroCause` object of the given `Micro`.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const tapErrorCause = exports.tapErrorCause = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => tapErrorCauseIf(self, _Function.constTrue, f));
/**
 * Perform a side effect using if a `MicroCause` object matches the specified
 * predicate.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const tapErrorCauseIf = exports.tapErrorCauseIf = /*#__PURE__*/(0, _Function.dual)(3, (self, refinement, f) => catchCauseIf(self, refinement, cause => andThen(f(cause), failCause(cause))));
/**
 * Perform a side effect from expected errors of the given `Micro`.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const tapError = exports.tapError = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => tapErrorCauseIf(self, causeIsFail, fail => f(fail.error)));
/**
 * Perform a side effect from unexpected errors of the given `Micro`.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const tapDefect = exports.tapDefect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => tapErrorCauseIf(self, causeIsDie, die => f(die.defect)));
/**
 * Catch any expected errors that match the specified predicate.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const catchIf = exports.catchIf = /*#__PURE__*/(0, _Function.dual)(3, (self, predicate, f) => catchCauseIf(self, f => causeIsFail(f) && predicate(f.error), fail => f(fail.error)));
/**
 * Recovers from the specified tagged error.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const catchTag = exports.catchTag = /*#__PURE__*/(0, _Function.dual)(3, (self, k, f) => catchIf(self, (0, _Predicate.isTagged)(k), f));
/**
 * Transform the full `MicroCause` object of the given `Micro` effect.
 *
 * @since 3.4.6
 * @experimental
 * @category error handling
 */
const mapErrorCause = exports.mapErrorCause = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchAllCause(self, cause => failCause(f(cause))));
/**
 * Transform any expected errors of the given `Micro` effect.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const mapError = exports.mapError = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchAll(self, error => fail(f(error))));
/**
 * Elevate any expected errors of the given `Micro` effect to unexpected errors,
 * resulting in an error type of `never`.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const orDie = self => catchAll(self, die);
/**
 * Recover from all errors by succeeding with the given value.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
exports.orDie = orDie;
const orElseSucceed = exports.orElseSucceed = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => catchAll(self, _ => sync(f)));
/**
 * Ignore any expected errors of the given `Micro` effect, returning `void`.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const ignore = self => matchEffect(self, {
  onFailure: _ => void_,
  onSuccess: _ => void_
});
/**
 * Ignore any expected errors of the given `Micro` effect, returning `void`.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
exports.ignore = ignore;
const ignoreLogged = self => matchEffect(self, {
  // eslint-disable-next-line no-console
  onFailure: error => sync(() => console.error(error)),
  onSuccess: _ => void_
});
/**
 * Replace the success value of the given `Micro` effect with an `Option`,
 * wrapping the success value in `Some` and returning `None` if the effect fails
 * with an expected error.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
exports.ignoreLogged = ignoreLogged;
const option = self => match(self, {
  onFailure: _ => Option.none(),
  onSuccess: Option.some
});
/**
 * Replace the success value of the given `Micro` effect with an `Either`,
 * wrapping the success value in `Right` and wrapping any expected errors with
 * a `Left`.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
exports.option = option;
const either = self => match(self, {
  onFailure: Either.left,
  onSuccess: Either.right
});
/**
 * Retry the given `Micro` effect using the provided options.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
exports.either = either;
const retry = exports.retry = /*#__PURE__*/(0, _Function.dual)(args => isMicro(args[0]), (self, options) => repeatExit(self, {
  ...options,
  while: exit => exit._tag === "Left" && exit.left._tag === "Fail" && (options?.while === undefined || options.while(exit.left.error))
}));
/**
 * Add a stack trace to any failures that occur in the effect. The trace will be
 * added to the `traces` field of the `MicroCause` object.
 *
 * @since 3.4.0
 * @experimental
 * @category error handling
 */
const withTrace = function () {
  const prevLimit = globalThis.Error.stackTraceLimit;
  globalThis.Error.stackTraceLimit = 2;
  const error = new globalThis.Error();
  globalThis.Error.stackTraceLimit = prevLimit;
  function generate(name, cause) {
    const stack = error.stack;
    if (!stack) {
      return cause;
    }
    const line = stack.split("\n")[2]?.trim().replace(/^at /, "");
    if (!line) {
      return cause;
    }
    const lineMatch = line.match(/\((.*)\)$/);
    return causeWithTrace(cause, `at ${name} (${lineMatch ? lineMatch[1] : line})`);
  }
  const f = name => self => unsafeMakeOptions(function (env, onExit) {
    self[runSymbol](env, function (exit) {
      onExit(exit._tag === "Left" ? Either.left(generate(name, exit.left)) : exit);
    });
  }, false);
  if (arguments.length === 2) {
    return f(arguments[1])(arguments[0]);
  }
  return f(arguments[0]);
};
// ----------------------------------------------------------------------------
// pattern matching
// ----------------------------------------------------------------------------
/**
 * @since 3.4.6
 * @experimental
 * @category pattern matching
 */
exports.withTrace = withTrace;
const matchCauseEffect = exports.matchCauseEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => make(function (env, onExit) {
  self[runSymbol](env, function (exit) {
    try {
      const next = exit._tag === "Left" ? options.onFailure(exit.left) : options.onSuccess(exit.right);
      next[runSymbol](env, onExit);
    } catch (err) {
      onExit(exitDie(err));
    }
  });
}));
/**
 * @since 3.4.6
 * @experimental
 * @category pattern matching
 */
const matchCause = exports.matchCause = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => matchCauseEffect(self, {
  onFailure: cause => sync(() => options.onFailure(cause)),
  onSuccess: value => sync(() => options.onSuccess(value))
}));
/**
 * @since 3.4.6
 * @experimental
 * @category pattern matching
 */
const matchEffect = exports.matchEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => matchCauseEffect(self, {
  onFailure: cause => cause._tag === "Fail" ? options.onFailure(cause.error) : failCause(cause),
  onSuccess: options.onSuccess
}));
/**
 * @since 3.4.0
 * @experimental
 * @category pattern matching
 */
const match = exports.match = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => matchEffect(self, {
  onFailure: error => sync(() => options.onFailure(error)),
  onSuccess: value => sync(() => options.onSuccess(value))
}));
// ----------------------------------------------------------------------------
// delays & timeouts
// ----------------------------------------------------------------------------
/**
 * Create a `Micro` effect that will sleep for the specified duration.
 *
 * @since 3.4.0
 * @experimental
 * @category delays & timeouts
 */
const sleep = millis => async(function (resume) {
  const timeout = setTimeout(function () {
    resume(void_);
  }, millis);
  return sync(() => {
    return clearTimeout(timeout);
  });
});
/**
 * Returns an effect that will delay the execution of this effect by the
 * specified duration.
 *
 * @since 3.4.0
 * @experimental
 * @category delays & timeouts
 */
exports.sleep = sleep;
const delay = exports.delay = /*#__PURE__*/(0, _Function.dual)(2, (self, millis) => andThen(sleep(millis), self));
/**
 * Returns an effect that will timeout this effect, that will execute the
 * fallback effect if the timeout elapses before the effect has produced a value.
 *
 * If the timeout elapses, the running effect will be safely interrupted.
 *
 * @since 3.4.0
 * @experimental
 * @category delays & timeouts
 */
const timeoutOrElse = exports.timeoutOrElse = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => raceFirst(self, andThen(interruptible(sleep(options.duration)), options.onTimeout)));
/**
 * Returns an effect that will timeout this effect, that will fail with a
 * `TimeoutException` if the timeout elapses before the effect has produced a
 * value.
 *
 * If the timeout elapses, the running effect will be safely interrupted.
 *
 * @since 3.4.0
 * @experimental
 * @category delays & timeouts
 */
const timeout = exports.timeout = /*#__PURE__*/(0, _Function.dual)(2, (self, millis) => timeoutOrElse(self, {
  duration: millis,
  onTimeout: () => fail(new TimeoutException())
}));
/**
 * Returns an effect that will timeout this effect, succeeding with a `None`
 * if the timeout elapses before the effect has produced a value; and `Some` of
 * the produced value otherwise.
 *
 * If the timeout elapses, the running effect will be safely interrupted.
 *
 * @since 3.4.0
 * @experimental
 * @category delays & timeouts
 */
const timeoutOption = exports.timeoutOption = /*#__PURE__*/(0, _Function.dual)(2, (self, millis) => raceFirst(asSome(self), as(interruptible(sleep(millis)), Option.none())));
// ----------------------------------------------------------------------------
// resources & finalization
// ----------------------------------------------------------------------------
/**
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const MicroScopeTypeId = exports.MicroScopeTypeId = /*#__PURE__*/Symbol.for("effect/Micro/MicroScope");
/**
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const MicroScope = exports.MicroScope = /*#__PURE__*/Context.GenericTag("effect/Micro/MicroScope");
class MicroScopeImpl {
  [MicroScopeTypeId];
  state = {
    _tag: "Open",
    finalizers: /*#__PURE__*/new Set()
  };
  constructor() {
    this[MicroScopeTypeId] = MicroScopeTypeId;
  }
  unsafeAddFinalizer(finalizer) {
    if (this.state._tag === "Open") {
      this.state.finalizers.add(finalizer);
    }
  }
  addFinalizer(finalizer) {
    return suspend(() => {
      if (this.state._tag === "Open") {
        this.state.finalizers.add(finalizer);
        return void_;
      }
      return finalizer(this.state.exit);
    });
  }
  unsafeRemoveFinalizer(finalizer) {
    if (this.state._tag === "Open") {
      this.state.finalizers.delete(finalizer);
    }
  }
  close(microExit) {
    return suspend(() => {
      if (this.state._tag === "Open") {
        const finalizers = Array.from(this.state.finalizers).reverse();
        this.state = {
          _tag: "Closed",
          exit: microExit
        };
        return flatMap(forEach(finalizers, finalizer => exit(finalizer(microExit))), exits => asVoid(fromExit(Either.all(exits))));
      }
      return void_;
    });
  }
  get fork() {
    return sync(() => {
      const newScope = new MicroScopeImpl();
      if (this.state._tag === "Closed") {
        newScope.state = this.state;
        return newScope;
      }
      function fin(exit) {
        return newScope.close(exit);
      }
      this.state.finalizers.add(fin);
      newScope.unsafeAddFinalizer(_ => sync(() => this.unsafeRemoveFinalizer(fin)));
      return newScope;
    });
  }
}
/**
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const scopeMake = exports.scopeMake = /*#__PURE__*/sync(() => new MicroScopeImpl());
/**
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const scopeUnsafeMake = () => new MicroScopeImpl();
/**
 * Access the current `MicroScope`.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
exports.scopeUnsafeMake = scopeUnsafeMake;
const scope = exports.scope = /*#__PURE__*/service(MicroScope);
/**
 * Provide a `MicroScope` to an effect.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const provideScope = exports.provideScope = /*#__PURE__*/(0, _Function.dual)(2, (self, scope) => provideService(self, MicroScope, scope));
/**
 * Provide a `MicroScope` to the given effect, closing it after the effect has
 * finished executing.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const scoped = self => suspend(function () {
  const scope = new MicroScopeImpl();
  return onExit(provideService(self, MicroScope, scope), exit => scope.close(exit));
});
/**
 * Create a resource with a cleanup `Micro` effect, ensuring the cleanup is
 * executed when the `MicroScope` is closed.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
exports.scoped = scoped;
const acquireRelease = (acquire, release) => uninterruptible(flatMap(scope, scope => tap(acquire, a => scope.addFinalizer(exit => release(a, exit)))));
/**
 * Add a finalizer to the current `MicroScope`.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
exports.acquireRelease = acquireRelease;
const addFinalizer = finalizer => flatMap(scope, scope => scope.addFinalizer(finalizer));
/**
 * When the `Micro` effect is completed, run the given finalizer effect with the
 * `MicroExit` of the executed effect.
 *
 * @since 3.4.6
 * @experimental
 * @category resources & finalization
 */
exports.addFinalizer = addFinalizer;
const onExit = exports.onExit = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => onExitIf(self, _Function.constTrue, f));
/**
 * When the `Micro` effect is completed, run the given finalizer effect if it
 * matches the specified predicate.
 *
 * @since 3.4.6
 * @experimental
 * @category resources & finalization
 */
const onExitIf = exports.onExitIf = /*#__PURE__*/(0, _Function.dual)(3, (self, refinement, f) => uninterruptibleMask(restore => make(function (env, onExit) {
  restore(self)[runSymbol](env, function (exit) {
    if (!refinement(exit)) {
      return onExit(exit);
    }
    f(exit)[runSymbol](env, function (finalizerExit) {
      if (finalizerExit._tag === "Left") {
        return onExit(finalizerExit);
      }
      onExit(exit);
    });
  });
})));
/**
 * Regardless of the result of the this `Micro` effect, run the finalizer effect.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const ensuring = exports.ensuring = /*#__PURE__*/(0, _Function.dual)(2, (self, finalizer) => onExit(self, _ => finalizer));
/**
 * When the `Micro` effect fails, run the given finalizer effect with the
 * `MicroCause` of the executed effect.
 *
 * @since 3.4.6
 * @experimental
 * @category resources & finalization
 */
const onError = exports.onError = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => onExitIf(self, exitIsFailure, exit => f(exit.left)));
/**
 * If this `Micro` effect is aborted, run the finalizer effect.
 *
 * @since 3.4.6
 * @experimental
 * @category resources & finalization
 */
const onInterrupt = exports.onInterrupt = /*#__PURE__*/(0, _Function.dual)(2, (self, finalizer) => onExitIf(self, exitIsInterrupt, _ => finalizer));
/**
 * Acquire a resource, use it, and then release the resource when the `use`
 * effect has completed.
 *
 * @since 3.4.0
 * @experimental
 * @category resources & finalization
 */
const acquireUseRelease = (acquire, use, release) => uninterruptibleMask(restore => flatMap(acquire, a => flatMap(exit(restore(use(a))), exit => andThen(release(a, exit), fromExit(exit)))));
// ----------------------------------------------------------------------------
// interruption
// ----------------------------------------------------------------------------
/**
 * Abort the current `Micro` effect.
 *
 * @since 3.4.6
 * @experimental
 * @category interruption
 */
exports.acquireUseRelease = acquireUseRelease;
const interrupt = exports.interrupt = /*#__PURE__*/make(function (env, onExit) {
  const controller = envGet(env, currentAbortController);
  controller.abort();
  onExit(exitInterrupt);
});
/**
 * Wrap the given `Micro` effect in an uninterruptible region, preventing the
 * effect from being aborted.
 *
 * @since 3.4.0
 * @experimental
 * @category interruption
 */
const uninterruptible = self => unsafeMakeOptions(function (env, onExit) {
  const nextEnv = envMutate(env, function (env) {
    env[currentInterruptible.key] = false;
    env[currentAbortSignal.key] = new AbortController().signal;
    return env;
  });
  self[runSymbol](nextEnv, onExit);
}, false);
/**
 * Wrap the given `Micro` effect in an uninterruptible region, preventing the
 * effect from being aborted.
 *
 * You can use the `restore` function to restore a `Micro` effect to the
 * interruptibility state before the `uninterruptibleMask` was applied.
 *
 * @since 3.4.0
 * @experimental
 * @category interruption
 * @example
 * import * as Micro from "effect/Micro"
 *
 * Micro.uninterruptibleMask((restore) =>
 *   Micro.sleep(1000).pipe( // uninterruptible
 *     Micro.andThen(restore(Micro.sleep(1000))) // interruptible
 *   )
 * )
 */
exports.uninterruptible = uninterruptible;
const uninterruptibleMask = f => unsafeMakeOptions((env, onExit) => {
  const isInterruptible = envGet(env, currentInterruptible);
  const effect = isInterruptible ? f(interruptible) : f(_Function.identity);
  const nextEnv = isInterruptible ? envMutate(env, function (env) {
    env[currentInterruptible.key] = false;
    env[currentAbortSignal.key] = new AbortController().signal;
    return env;
  }) : env;
  effect[runSymbol](nextEnv, onExit);
}, false);
/**
 * Wrap the given `Micro` effect in an interruptible region, allowing the effect
 * to be aborted.
 *
 * @since 3.4.0
 * @experimental
 * @category interruption
 */
exports.uninterruptibleMask = uninterruptibleMask;
const interruptible = self => make((env, onExit) => {
  const isInterruptible = envGet(env, currentInterruptible);
  let newEnv = env;
  if (!isInterruptible) {
    const controller = envGet(env, currentAbortController);
    newEnv = envMutate(env, function (env) {
      env[currentInterruptible.key] = true;
      env[currentAbortSignal.key] = controller.signal;
      return env;
    });
  }
  self[runSymbol](newEnv, onExit);
});
/**
 * Runs all the provided effects in sequence respecting the structure provided in input.
 *
 * Supports multiple arguments, a single argument tuple / array or record / struct.
 *
 * @since 3.4.0
 * @experimental
 * @category collecting & elements
 */
exports.interruptible = interruptible;
const all = (arg, options) => {
  if (Array.isArray(arg) || (0, _Predicate.isIterable)(arg)) {
    return forEach(arg, _Function.identity, options);
  } else if (options?.discard) {
    return forEach(Object.values(arg), _Function.identity, options);
  }
  return suspend(() => {
    const out = {};
    return as(forEach(Object.entries(arg), ([key, effect]) => map(effect, value => {
      out[key] = value;
    }), {
      discard: true,
      concurrency: options?.concurrency
    }), out);
  });
};
/**
 * For each element of the provided iterable, run the effect and collect the results.
 *
 * If the `discard` option is set to `true`, the results will be discarded and
 * the effect will return `void`.
 *
 * The `concurrency` option can be set to control how many effects are run in
 * parallel. By default, the effects are run sequentially.
 *
 * @since 3.4.0
 * @experimental
 * @category collecting & elements
 */
exports.all = all;
const forEach = (iterable, f, options) => make(function (env, onExit) {
  const concurrencyOption = options?.concurrency === "inherit" ? envGet(env, currentConcurrency) : options?.concurrency ?? 1;
  const concurrency = concurrencyOption === "unbounded" ? Number.POSITIVE_INFINITY : Math.max(1, concurrencyOption);
  // abort
  const [envWithSignal, onAbort] = forkSignal(env);
  // iterate
  let result = undefined;
  const items = Array.from(iterable);
  let length = items.length;
  if (length === 0) {
    return onExit(Either.right(options?.discard ? undefined : []));
  }
  const out = options?.discard ? undefined : new Array(length);
  let index = 0;
  let inProgress = 0;
  let doneCount = 0;
  let pumping = false;
  function pump() {
    pumping = true;
    while (inProgress < concurrency && index < length) {
      const currentIndex = index;
      const item = items[currentIndex];
      index++;
      inProgress++;
      try {
        f(item, currentIndex)[runSymbol](envWithSignal, function (exit) {
          if (exit._tag === "Left") {
            if (result === undefined) {
              result = exit;
              length = index;
              onAbort();
            }
          } else if (out !== undefined) {
            out[currentIndex] = exit.right;
          }
          doneCount++;
          inProgress--;
          if (doneCount === length) {
            onExit(result ?? Either.right(out));
          } else if (!pumping && inProgress < concurrency) {
            pump();
          }
        });
      } catch (err) {
        result = exitDie(err);
        length = index;
        onAbort();
      }
    }
    pumping = false;
  }
  pump();
});
/**
 * Effectfully filter the elements of the provided iterable.
 *
 * Use the `concurrency` option to control how many elements are processed in parallel.
 *
 * @since 3.4.0
 * @experimental
 * @category collecting & elements
 */
exports.forEach = forEach;
const filter = (iterable, f, options) => filterMap(iterable, a => map(f(a), pass => {
  pass = options?.negate ? !pass : pass;
  return pass ? Option.some(a) : Option.none();
}), options);
/**
 * Effectfully filter the elements of the provided iterable.
 *
 * Use the `concurrency` option to control how many elements are processed in parallel.
 *
 * @since 3.4.0
 * @experimental
 * @category collecting & elements
 */
exports.filter = filter;
const filterMap = (iterable, f, options) => suspend(() => {
  const out = [];
  return as(forEach(iterable, a => map(f(a), o => {
    if (o._tag === "Some") {
      out.push(o.value);
    }
  }), {
    discard: true,
    concurrency: options?.concurrency
  }), out);
});
// ----------------------------------------------------------------------------
// do notation
// ----------------------------------------------------------------------------
/**
 * Start a do notation block.
 *
 * @since 3.4.0
 * @experimental
 * @category do notation
 */
exports.filterMap = filterMap;
const Do = exports.Do = /*#__PURE__*/succeed({});
/**
 * Bind the success value of this `Micro` effect to the provided name.
 *
 * @since 3.4.0
 * @experimental
 * @category do notation
 */
const bindTo = exports.bindTo = /*#__PURE__*/doNotation.bindTo(map);
/**
 * Bind the success value of this `Micro` effect to the provided name.
 *
 * @since 3.4.0
 * @experimental
 * @category do notation
 */
const bind = exports.bind = /*#__PURE__*/doNotation.bind(map, flatMap);
const let_ = exports.let = /*#__PURE__*/doNotation.let_(map);
// ----------------------------------------------------------------------------
// handle & forking
// ----------------------------------------------------------------------------
/**
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
const HandleTypeId = exports.HandleTypeId = /*#__PURE__*/Symbol.for("effect/Micro/Handle");
/**
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
const isHandle = u => typeof u === "object" && u !== null && HandleTypeId in u;
exports.isHandle = isHandle;
class HandleImpl extends Class {
  parentSignal;
  [HandleTypeId];
  observers = /*#__PURE__*/new Set();
  _exit = undefined;
  _controller;
  isRoot;
  constructor(parentSignal, controller) {
    super();
    this.parentSignal = parentSignal;
    this[HandleTypeId] = HandleTypeId;
    this.isRoot = controller !== undefined;
    this._controller = controller ?? new AbortController();
    if (!this.isRoot) {
      parentSignal.addEventListener("abort", this.unsafeInterrupt);
    }
  }
  unsafePoll() {
    return this._exit ?? null;
  }
  unsafeInterrupt = () => {
    this._controller.abort();
  };
  emit(exit) {
    if (this._exit) {
      return;
    }
    this._exit = exit;
    if (!this.isRoot) {
      this.parentSignal.removeEventListener("abort", this.unsafeInterrupt);
    }
    this.observers.forEach(observer => observer(exit));
    this.observers.clear();
  }
  addObserver(observer) {
    if (this._exit) {
      return observer(this._exit);
    }
    this.observers.add(observer);
  }
  removeObserver(observer) {
    this.observers.delete(observer);
  }
  get await() {
    return suspend(() => {
      if (this._exit) {
        return succeed(this._exit);
      }
      return async(resume => {
        function observer(exit) {
          resume(succeed(exit));
        }
        this.addObserver(observer);
        return sync(() => {
          this.removeObserver(observer);
        });
      });
    });
  }
  get join() {
    return flatMap(this.await, fromExit);
  }
  get interrupt() {
    return suspend(() => {
      this.unsafeInterrupt();
      return this.await;
    });
  }
  asMicro() {
    return this.join;
  }
}
/**
 * Run the `Micro` effect in a new `Handle` that can be awaited, joined, or
 * aborted.
 *
 * When the parent `Micro` finishes, this `Micro` will be aborted.
 *
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
const fork = self => make(function (env, onExit) {
  const signal = envGet(env, currentAbortSignal);
  const handle = new HandleImpl(signal);
  const nextEnv = envMutate(env, map => {
    map[currentAbortController.key] = handle._controller;
    map[currentAbortSignal.key] = handle._controller.signal;
    return map;
  });
  envGet(env, currentScheduler).scheduleTask(() => {
    self[runSymbol](nextEnv, exit => {
      handle.emit(exit);
    });
  }, 0);
  onExit(Either.right(handle));
});
/**
 * Run the `Micro` effect in a new `Handle` that can be awaited, joined, or
 * aborted.
 *
 * It will not be aborted when the parent `Micro` finishes.
 *
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
exports.fork = fork;
const forkDaemon = self => make(function (env, onExit) {
  const controller = new AbortController();
  const handle = new HandleImpl(controller.signal, controller);
  const nextEnv = envMutate(env, map => {
    map[currentAbortController.key] = controller;
    map[currentAbortSignal.key] = controller.signal;
    return map;
  });
  envGet(env, currentScheduler).scheduleTask(() => {
    self[runSymbol](nextEnv, exit => {
      handle.emit(exit);
    });
  }, 0);
  onExit(Either.right(handle));
});
/**
 * Run the `Micro` effect in a new `Handle` that can be awaited, joined, or
 * aborted.
 *
 * The lifetime of the handle will be attached to the provided `MicroScope`.
 *
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
exports.forkDaemon = forkDaemon;
const forkIn = exports.forkIn = /*#__PURE__*/(0, _Function.dual)(2, (self, scope) => uninterruptibleMask(restore => flatMap(scope.fork, scope => tap(restore(forkDaemon(onExit(self, exit => scope.close(exit)))), fiber => scope.addFinalizer(_ => asVoid(fiber.interrupt))))));
/**
 * Run the `Micro` effect in a new `Handle` that can be awaited, joined, or
 * aborted.
 *
 * The lifetime of the handle will be attached to the current `MicroScope`.
 *
 * @since 3.4.0
 * @experimental
 * @category handle & forking
 */
const forkScoped = self => flatMap(scope, scope => forkIn(self, scope));
// ----------------------------------------------------------------------------
// execution
// ----------------------------------------------------------------------------
/**
 * Execute the `Micro` effect and return a `Handle` that can be awaited, joined,
 * or aborted.
 *
 * You can listen for the result by adding an observer using the handle's
 * `addObserver` method.
 *
 * @since 3.4.0
 * @experimental
 * @category execution
 * @example
 * import * as Micro from "effect/Micro"
 *
 * const handle = Micro.succeed(42).pipe(
 *   Micro.delay(1000),
 *   Micro.runFork
 * )
 *
 * handle.addObserver((exit) => {
 *   console.log(exit)
 * })
 */
exports.forkScoped = forkScoped;
const runFork = (effect, options) => {
  const controller = new AbortController();
  const refs = Object.create(null);
  refs[currentAbortController.key] = controller;
  refs[currentAbortSignal.key] = controller.signal;
  refs[currentScheduler.key] = options?.scheduler ?? new MicroSchedulerDefault();
  const env = envMake(refs);
  const handle = new HandleImpl(controller.signal, controller);
  effect[runSymbol](envSet(env, currentAbortSignal, handle._controller.signal), exit => {
    handle.emit(exit);
    if (options?.signal) {
      options.signal.removeEventListener("abort", handle.unsafeInterrupt);
    }
  });
  if (options?.signal) {
    if (options.signal.aborted) {
      handle.unsafeInterrupt();
    } else {
      options.signal.addEventListener("abort", handle.unsafeInterrupt, {
        once: true
      });
    }
  }
  return handle;
};
/**
 * Execute the `Micro` effect and return a `Promise` that resolves with the
 * `MicroExit` of the computation.
 *
 * @since 3.4.6
 * @experimental
 * @category execution
 */
exports.runFork = runFork;
const runPromiseExit = (effect, options) => new Promise((resolve, _reject) => {
  const handle = runFork(effect, options);
  handle.addObserver(resolve);
});
/**
 * Execute the `Micro` effect and return a `Promise` that resolves with the
 * successful value of the computation.
 *
 * @since 3.4.0
 * @experimental
 * @category execution
 */
exports.runPromiseExit = runPromiseExit;
const runPromise = (effect, options) => runPromiseExit(effect, options).then(exit => {
  if (exit._tag === "Left") {
    throw exit.left;
  }
  return exit.right;
});
/**
 * Attempt to execute the `Micro` effect synchronously and return the `MicroExit`.
 *
 * If any asynchronous effects are encountered, the function will return a
 * `CauseDie` containing the `Handle`.
 *
 * @since 3.4.6
 * @experimental
 * @category execution
 */
exports.runPromise = runPromise;
const runSyncExit = effect => {
  const scheduler = new MicroSchedulerDefault();
  const handle = runFork(effect, {
    scheduler
  });
  scheduler.flush();
  const exit = handle.unsafePoll();
  if (exit === null) {
    return exitDie(handle);
  }
  return exit;
};
/**
 * Attempt to execute the `Micro` effect synchronously and return the success
 * value.
 *
 * @since 3.4.0
 * @experimental
 * @category execution
 */
exports.runSyncExit = runSyncExit;
const runSync = effect => {
  const exit = runSyncExit(effect);
  if (exit._tag === "Left") {
    throw exit.left;
  }
  return exit.right;
};
exports.runSync = runSync;
const YieldableError = /*#__PURE__*/function () {
  class YieldableError extends globalThis.Error {
    [runSymbol](_env, onExit) {
      onExit(exitFail(this));
    }
    toString() {
      return this.message ? `${this.name}: ${this.message}` : this.name;
    }
    toJSON() {
      return {
        ...this
      };
    }
    [_Inspectable.NodeInspectSymbol]() {
      const stack = this.stack;
      if (stack) {
        return `${this.toString()}\n${stack.split("\n").slice(1).join("\n")}`;
      }
      return this.toString();
    }
  }
  Object.assign(YieldableError.prototype, MicroProto, _effectable.StructuralPrototype);
  return YieldableError;
}();
/**
 * @since 3.4.0
 * @experimental
 * @category errors
 */
const Error = exports.Error = /*#__PURE__*/function () {
  return class extends YieldableError {
    constructor(args) {
      super();
      if (args) {
        Object.assign(this, args);
      }
    }
  };
}();
/**
 * @since 3.4.0
 * @experimental
 * @category errors
 */
const TaggedError = tag => {
  class Base extends Error {
    _tag = tag;
  }
  ;
  Base.prototype.name = tag;
  return Base;
};
/**
 * Represents a checked exception which occurs when an expected element was
 * unable to be found.
 *
 * @since 3.4.4
 * @experimental
 * @category errors
 */
exports.TaggedError = TaggedError;
class NoSuchElementException extends /*#__PURE__*/TaggedError("NoSuchElementException") {}
/**
 * Represents a checked exception which occurs when a timeout occurs.
 *
 * @since 3.4.4
 * @experimental
 * @category errors
 */
exports.NoSuchElementException = NoSuchElementException;
class TimeoutException extends /*#__PURE__*/TaggedError("TimeoutException") {}
exports.TimeoutException = TimeoutException;
//# sourceMappingURL=Micro.js.map