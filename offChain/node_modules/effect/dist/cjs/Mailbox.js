"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toStream = exports.toChannel = exports.make = exports.isReadonlyMailbox = exports.isMailbox = exports.into = exports.TypeId = exports.ReadonlyTypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/mailbox.js"));
var _Predicate = require("./Predicate.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.8.0
 * @experimental
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 3.8.0
 * @experimental
 * @category type ids
 */
const ReadonlyTypeId = exports.ReadonlyTypeId = internal.ReadonlyTypeId;
/**
 * @since 3.8.0
 * @experimental
 * @category guards
 */
const isMailbox = u => (0, _Predicate.hasProperty)(u, TypeId);
/**
 * @since 3.8.0
 * @experimental
 * @category guards
 */
exports.isMailbox = isMailbox;
const isReadonlyMailbox = u => (0, _Predicate.hasProperty)(u, ReadonlyTypeId);
/**
 * A `Mailbox` is a queue that can be signaled to be done or failed.
 *
 * @since 3.8.0
 * @experimental
 * @category constructors
 * @example
 * import { Effect, Mailbox } from "effect"
 *
 * Effect.gen(function*() {
 *   const mailbox = yield* Mailbox.make<number, string>()
 *
 *   // add messages to the mailbox
 *   yield* mailbox.offer(1)
 *   yield* mailbox.offer(2)
 *   yield* mailbox.offerAll([3, 4, 5])
 *
 *   // take messages from the mailbox
 *   const [messages, done] = yield* mailbox.takeAll
 *   assert.deepStrictEqual(messages, [1, 2, 3, 4, 5])
 *   assert.strictEqual(done, false)
 *
 *   // signal that the mailbox is done
 *   yield* mailbox.end
 *   const [messages2, done2] = yield* mailbox.takeAll
 *   assert.deepStrictEqual(messages2, [])
 *   assert.strictEqual(done2, true)
 *
 *   // signal that the mailbox has failed
 *   yield* mailbox.fail("boom")
 * })
 */
exports.isReadonlyMailbox = isReadonlyMailbox;
const make = exports.make = internal.make;
/**
 * Run an `Effect` into a `Mailbox`, where success ends the mailbox and failure
 * fails the mailbox.
 *
 * @since 3.8.0
 * @experimental
 * @category combinators
 */
const into = exports.into = internal.into;
/**
 * Create a `Channel` from a `Mailbox`.
 *
 * @since 3.8.0
 * @experimental
 * @category conversions
 */
const toChannel = exports.toChannel = internal.toChannel;
/**
 * Create a `Stream` from a `Mailbox`.
 *
 * @since 3.8.0
 * @experimental
 * @category conversions
 */
const toStream = exports.toStream = internal.toStream;
//# sourceMappingURL=Mailbox.js.map