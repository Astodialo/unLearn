import * as Context from "../Context.js";
import * as Duration from "../Duration.js";
import * as Effectable from "../Effectable.js";
import { dual, identity } from "../Function.js";
import * as Iterable from "../Iterable.js";
import * as Option from "../Option.js";
import { pipeArguments } from "../Pipeable.js";
import { hasProperty } from "../Predicate.js";
import * as coreEffect from "./core-effect.js";
import * as core from "./core.js";
import * as defaultServices from "./defaultServices.js";
import * as circular from "./effect/circular.js";
import * as fiberRuntime from "./fiberRuntime.js";
import * as internalQueue from "./queue.js";
/** @internal */
export const PoolTypeId = /*#__PURE__*/Symbol.for("effect/Pool");
const poolVariance = {
  /* c8 ignore next */
  _E: _ => _,
  /* c8 ignore next */
  _A: _ => _
};
/** @internal */
export const isPool = u => hasProperty(u, PoolTypeId);
/** @internal */
export const makeWith = options => core.uninterruptibleMask(restore => core.flatMap(core.context(), context => {
  const scope = Context.get(context, fiberRuntime.scopeTag);
  const acquire = core.mapInputContext(options.acquire, input => Context.merge(context, input));
  const pool = new PoolImpl(acquire, options.concurrency ?? 1, options.min, options.max, options.strategy, Math.min(Math.max(options.targetUtilization ?? 1, 0.1), 1));
  const initialize = core.tap(fiberRuntime.forkDaemon(restore(pool.resize)), fiber => scope.addFinalizer(() => core.interruptFiber(fiber)));
  const runStrategy = core.tap(fiberRuntime.forkDaemon(restore(options.strategy.run(pool))), fiber => scope.addFinalizer(() => core.interruptFiber(fiber)));
  return core.succeed(pool).pipe(core.zipLeft(scope.addFinalizer(() => pool.shutdown)), core.zipLeft(initialize), core.zipLeft(runStrategy));
}));
/** @internal */
export const make = options => makeWith({
  ...options,
  min: options.size,
  max: options.size,
  strategy: strategyNoop()
});
/** @internal */
export const makeWithTTL = options => core.flatMap(options.timeToLiveStrategy === "creation" ? strategyCreationTTL(options.timeToLive) : strategyUsageTTL(options.timeToLive), strategy => makeWith({
  ...options,
  strategy
}));
/** @internal */
export const get = self => self.get;
/** @internal */
export const invalidate = /*#__PURE__*/dual(2, (self, item) => self.invalidate(item));
class PoolImpl extends Effectable.Class {
  acquire;
  concurrency;
  minSize;
  maxSize;
  strategy;
  targetUtilization;
  [PoolTypeId];
  isShuttingDown = false;
  semaphore;
  items = /*#__PURE__*/new Set();
  available = /*#__PURE__*/new Set();
  invalidated = /*#__PURE__*/new Set();
  waiters = 0;
  constructor(acquire, concurrency, minSize, maxSize, strategy, targetUtilization) {
    super();
    this.acquire = acquire;
    this.concurrency = concurrency;
    this.minSize = minSize;
    this.maxSize = maxSize;
    this.strategy = strategy;
    this.targetUtilization = targetUtilization;
    this[PoolTypeId] = poolVariance;
    this.semaphore = circular.unsafeMakeSemaphore(concurrency * maxSize);
  }
  allocate = /*#__PURE__*/core.acquireUseRelease( /*#__PURE__*/fiberRuntime.scopeMake(), scope => this.acquire.pipe(fiberRuntime.scopeExtend(scope), core.exit, core.flatMap(exit => {
    const item = {
      exit,
      finalizer: core.catchAllCause(scope.close(exit), reportUnhandledError),
      refCount: 0,
      disableReclaim: false
    };
    this.items.add(item);
    this.available.add(item);
    return core.as(exit._tag === "Success" ? this.strategy.onAcquire(item) : core.zipRight(item.finalizer, this.strategy.onAcquire(item)), item);
  })), (scope, exit) => exit._tag === "Failure" ? scope.close(exit) : core.void);
  get currentUsage() {
    let count = this.waiters;
    for (const item of this.items) {
      count += item.refCount;
    }
    return count;
  }
  get targetSize() {
    if (this.isShuttingDown) return 0;
    const utilization = this.currentUsage / this.targetUtilization;
    const target = Math.ceil(utilization / this.concurrency);
    return Math.min(Math.max(this.minSize, target), this.maxSize);
  }
  get activeSize() {
    return this.items.size - this.invalidated.size;
  }
  resizeLoop = /*#__PURE__*/core.suspend(() => {
    if (this.activeSize >= this.targetSize) {
      return core.void;
    }
    return this.strategy.reclaim(this).pipe(core.flatMap(Option.match({
      onNone: () => this.allocate,
      onSome: core.succeed
    })), core.zipRight(this.resizeLoop));
  });
  resizeSemaphore = /*#__PURE__*/circular.unsafeMakeSemaphore(1);
  resize = /*#__PURE__*/this.resizeSemaphore.withPermits(1)(this.resizeLoop);
  getPoolItem = /*#__PURE__*/core.uninterruptibleMask(restore => restore(this.semaphore.take(1)).pipe(core.zipRight(fiberRuntime.scopeTag), core.flatMap(scope => core.suspend(() => {
    this.waiters++;
    if (this.isShuttingDown) {
      return core.interrupt;
    } else if (this.targetSize > this.activeSize) {
      return core.zipRight(restore(this.resize), core.sync(() => Iterable.unsafeHead(this.available)));
    }
    return core.succeed(Iterable.unsafeHead(this.available));
  }).pipe(fiberRuntime.ensuring(core.sync(() => this.waiters--)), core.tap(item => {
    if (item.exit._tag === "Failure") {
      this.items.delete(item);
      this.invalidated.delete(item);
      this.available.delete(item);
      return this.semaphore.release(1);
    }
    item.refCount++;
    this.available.delete(item);
    if (item.refCount < this.concurrency) {
      this.available.add(item);
    }
    return scope.addFinalizer(() => core.zipRight(core.suspend(() => {
      item.refCount--;
      if (this.invalidated.has(item)) {
        return this.invalidatePoolItem(item);
      }
      this.available.add(item);
      return core.void;
    }), this.semaphore.release(1)));
  }), core.onInterrupt(() => this.semaphore.release(1))))));
  commit() {
    return this.get;
  }
  get = /*#__PURE__*/core.flatMap( /*#__PURE__*/core.suspend(() => this.isShuttingDown ? core.interrupt : this.getPoolItem), _ => _.exit);
  invalidate(item) {
    return core.suspend(() => {
      if (this.isShuttingDown) return core.void;
      for (const poolItem of this.items) {
        if (poolItem.exit._tag === "Success" && poolItem.exit.value === item) {
          poolItem.disableReclaim = true;
          return core.uninterruptible(this.invalidatePoolItem(poolItem));
        }
      }
      return core.void;
    });
  }
  invalidatePoolItem(poolItem) {
    return core.suspend(() => {
      if (!this.items.has(poolItem)) {
        return core.void;
      } else if (poolItem.refCount === 0) {
        this.items.delete(poolItem);
        this.available.delete(poolItem);
        this.invalidated.delete(poolItem);
        return core.zipRight(poolItem.finalizer, this.resize);
      }
      this.invalidated.add(poolItem);
      this.available.delete(poolItem);
      return core.void;
    });
  }
  get shutdown() {
    return core.suspend(() => {
      if (this.isShuttingDown) return core.void;
      this.isShuttingDown = true;
      const size = this.items.size;
      const semaphore = circular.unsafeMakeSemaphore(size);
      return core.forEachSequentialDiscard(this.items, item => {
        if (item.refCount > 0) {
          item.finalizer = core.zipLeft(item.finalizer, semaphore.release(1));
          this.invalidated.add(item);
          return semaphore.take(1);
        }
        this.items.delete(item);
        this.available.delete(item);
        this.invalidated.delete(item);
        return item.finalizer;
      }).pipe(core.zipRight(this.semaphore.releaseAll), core.zipRight(semaphore.take(size)));
    });
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
}
const strategyNoop = () => ({
  run: _ => core.void,
  onAcquire: _ => core.void,
  reclaim: _ => coreEffect.succeedNone
});
const strategyCreationTTL = ttl => defaultServices.clockWith(clock => core.map(internalQueue.unbounded(), queue => {
  const ttlMillis = Duration.toMillis(ttl);
  const creationTimes = new WeakMap();
  return identity({
    run: pool => {
      const process = item => core.suspend(() => {
        if (!pool.items.has(item) || pool.invalidated.has(item)) {
          return core.void;
        }
        const now = clock.unsafeCurrentTimeMillis();
        const created = creationTimes.get(item);
        const remaining = ttlMillis - (now - created);
        return remaining > 0 ? coreEffect.delay(process(item), remaining) : pool.invalidatePoolItem(item);
      });
      return queue.take.pipe(core.tap(process), coreEffect.forever);
    },
    onAcquire: item => core.suspend(() => {
      creationTimes.set(item, clock.unsafeCurrentTimeMillis());
      return queue.offer(item);
    }),
    reclaim: _ => coreEffect.succeedNone
  });
}));
const strategyUsageTTL = ttl => core.map(internalQueue.unbounded(), queue => {
  return identity({
    run: pool => {
      const process = core.suspend(() => {
        const excess = pool.activeSize - pool.targetSize;
        if (excess <= 0) return core.void;
        return queue.take.pipe(core.tap(item => pool.invalidatePoolItem(item)), core.zipRight(process));
      });
      return process.pipe(coreEffect.delay(ttl), coreEffect.forever);
    },
    onAcquire: item => queue.offer(item),
    reclaim(pool) {
      return core.suspend(() => {
        if (pool.invalidated.size === 0) {
          return coreEffect.succeedNone;
        }
        const item = Iterable.head(Iterable.filter(pool.invalidated, item => !item.disableReclaim));
        if (item._tag === "None") {
          return coreEffect.succeedNone;
        }
        pool.invalidated.delete(item.value);
        if (item.value.refCount < pool.concurrency) {
          pool.available.add(item.value);
        }
        return core.as(queue.offer(item.value), item);
      });
    }
  });
});
const reportUnhandledError = cause => core.withFiberRuntime(fiber => {
  const unhandledLogLevel = fiber.getFiberRef(core.currentUnhandledErrorLogLevel);
  if (unhandledLogLevel._tag === "Some") {
    fiber.log("Unhandled error in pool finalizer", cause, unhandledLogLevel);
  }
  return core.void;
});
//# sourceMappingURL=pool.js.map