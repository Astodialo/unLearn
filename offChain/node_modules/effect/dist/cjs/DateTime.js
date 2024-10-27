"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.zonedOffsetIso = exports.zonedOffset = exports.zoneUnsafeMakeNamed = exports.zoneToString = exports.zoneMakeOffset = exports.zoneMakeNamedEffect = exports.zoneMakeNamed = exports.zoneMakeLocal = exports.zoneFromString = exports.withDateUtc = exports.withDate = exports.withCurrentZoneOffset = exports.withCurrentZoneNamed = exports.withCurrentZoneLocal = exports.withCurrentZone = exports.unsafeSetZoneNamed = exports.unsafeNow = exports.unsafeMakeZoned = exports.unsafeMake = exports.unsafeIsPast = exports.unsafeIsFuture = exports.unsafeFromDate = exports.toPartsUtc = exports.toParts = exports.toEpochMillis = exports.toDateUtc = exports.toDate = exports.subtractDuration = exports.subtract = exports.startOf = exports.setZoneOffset = exports.setZoneNamed = exports.setZoneCurrent = exports.setZone = exports.setPartsUtc = exports.setParts = exports.removeTime = exports.nowInCurrentZone = exports.now = exports.nearest = exports.mutateUtc = exports.mutate = exports.min = exports.max = exports.match = exports.mapEpochMillis = exports.makeZonedFromString = exports.makeZoned = exports.make = exports.lessThanOrEqualTo = exports.lessThan = exports.layerCurrentZoneOffset = exports.layerCurrentZoneNamed = exports.layerCurrentZoneLocal = exports.layerCurrentZone = exports.isZoned = exports.isUtc = exports.isTimeZoneOffset = exports.isTimeZoneNamed = exports.isTimeZone = exports.isPast = exports.isFuture = exports.isDateTime = exports.greaterThanOrEqualTo = exports.greaterThan = exports.getPartUtc = exports.getPart = exports.formatUtc = exports.formatLocal = exports.formatIsoZoned = exports.formatIsoOffset = exports.formatIsoDateUtc = exports.formatIsoDate = exports.formatIso = exports.formatIntl = exports.format = exports.endOf = exports.distanceDurationEither = exports.distanceDuration = exports.distance = exports.clamp = exports.between = exports.addDuration = exports.add = exports.TypeId = exports.TimeZoneTypeId = exports.Order = exports.Equivalence = exports.CurrentTimeZone = void 0;
var _Cause = require("./Cause.js");
var Clock = _interopRequireWildcard(require("./Clock.js"));
var Context = _interopRequireWildcard(require("./Context.js"));
var Duration = _interopRequireWildcard(require("./Duration.js"));
var Effect = _interopRequireWildcard(require("./Effect.js"));
var Either = _interopRequireWildcard(require("./Either.js"));
var Equal = _interopRequireWildcard(require("./Equal.js"));
var Equivalence_ = _interopRequireWildcard(require("./Equivalence.js"));
var _Function = require("./Function.js");
var _GlobalValue = require("./GlobalValue.js");
var Hash = _interopRequireWildcard(require("./Hash.js"));
var Inspectable = _interopRequireWildcard(require("./Inspectable.js"));
var Layer = _interopRequireWildcard(require("./Layer.js"));
var Option = _interopRequireWildcard(require("./Option.js"));
var order = _interopRequireWildcard(require("./Order.js"));
var _Pipeable = require("./Pipeable.js");
var Predicate = _interopRequireWildcard(require("./Predicate.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.6.0
 */

/**
 * @since 3.6.0
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("effect/DateTime");
/**
 * @since 3.6.0
 * @category type ids
 */
const TimeZoneTypeId = exports.TimeZoneTypeId = /*#__PURE__*/Symbol.for("effect/DateTime/TimeZone");
const Proto = {
  [TypeId]: TypeId,
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  },
  [Inspectable.NodeInspectSymbol]() {
    return this.toString();
  },
  toJSON() {
    return toDateUtc(this).toJSON();
  }
};
const ProtoUtc = {
  ...Proto,
  _tag: "Utc",
  [Hash.symbol]() {
    return Hash.cached(this, Hash.number(this.epochMillis));
  },
  [Equal.symbol](that) {
    return isDateTime(that) && that._tag === "Utc" && this.epochMillis === that.epochMillis;
  },
  toString() {
    return `DateTime.Utc(${toDateUtc(this).toJSON()})`;
  }
};
const ProtoZoned = {
  ...Proto,
  _tag: "Zoned",
  [Hash.symbol]() {
    return (0, _Function.pipe)(Hash.number(this.epochMillis), Hash.combine(Hash.hash(this.zone)), Hash.cached(this));
  },
  [Equal.symbol](that) {
    return isDateTime(that) && that._tag === "Zoned" && this.epochMillis === that.epochMillis && Equal.equals(this.zone, that.zone);
  },
  toString() {
    return `DateTime.Zoned(${formatIsoZoned(this)})`;
  }
};
const ProtoTimeZone = {
  [TimeZoneTypeId]: TimeZoneTypeId,
  [Inspectable.NodeInspectSymbol]() {
    return this.toString();
  }
};
const ProtoTimeZoneNamed = {
  ...ProtoTimeZone,
  _tag: "Named",
  [Hash.symbol]() {
    return Hash.cached(this, Hash.string(`Named:${this.id}`));
  },
  [Equal.symbol](that) {
    return isTimeZone(that) && that._tag === "Named" && this.id === that.id;
  },
  toString() {
    return `TimeZone.Named(${this.id})`;
  },
  toJSON() {
    return {
      _id: "TimeZone",
      _tag: "Named",
      id: this.id
    };
  }
};
const ProtoTimeZoneOffset = {
  ...ProtoTimeZone,
  _tag: "Offset",
  [Hash.symbol]() {
    return Hash.cached(this, Hash.string(`Offset:${this.offset}`));
  },
  [Equal.symbol](that) {
    return isTimeZone(that) && that._tag === "Offset" && this.offset === that.offset;
  },
  toString() {
    return `TimeZone.Offset(${offsetToString(this.offset)})`;
  },
  toJSON() {
    return {
      _id: "TimeZone",
      _tag: "Offset",
      offset: this.offset
    };
  }
};
const makeZonedProto = (epochMillis, zone, partsUtc) => {
  const self = Object.create(ProtoZoned);
  self.epochMillis = epochMillis;
  self.zone = zone;
  self.partsUtc = partsUtc;
  return self;
};
// =============================================================================
// guards
// =============================================================================
/**
 * @since 3.6.0
 * @category guards
 */
const isDateTime = u => Predicate.hasProperty(u, TypeId);
exports.isDateTime = isDateTime;
const isDateTimeArgs = args => isDateTime(args[0]);
/**
 * @since 3.6.0
 * @category guards
 */
const isTimeZone = u => Predicate.hasProperty(u, TimeZoneTypeId);
/**
 * @since 3.6.0
 * @category guards
 */
exports.isTimeZone = isTimeZone;
const isTimeZoneOffset = u => isTimeZone(u) && u._tag === "Offset";
/**
 * @since 3.6.0
 * @category guards
 */
exports.isTimeZoneOffset = isTimeZoneOffset;
const isTimeZoneNamed = u => isTimeZone(u) && u._tag === "Named";
/**
 * @since 3.6.0
 * @category guards
 */
exports.isTimeZoneNamed = isTimeZoneNamed;
const isUtc = self => self._tag === "Utc";
/**
 * @since 3.6.0
 * @category guards
 */
exports.isUtc = isUtc;
const isZoned = self => self._tag === "Zoned";
// =============================================================================
// instances
// =============================================================================
/**
 * @since 3.6.0
 * @category instances
 */
exports.isZoned = isZoned;
const Equivalence = exports.Equivalence = /*#__PURE__*/Equivalence_.make((a, b) => a.epochMillis === b.epochMillis);
/**
 * @since 3.6.0
 * @category instances
 */
const Order = exports.Order = /*#__PURE__*/order.make((self, that) => self.epochMillis < that.epochMillis ? -1 : self.epochMillis > that.epochMillis ? 1 : 0);
/**
 * @since 3.6.0
 */
const clamp = exports.clamp = /*#__PURE__*/order.clamp(Order);
// =============================================================================
// constructors
// =============================================================================
const makeUtc = epochMillis => {
  const self = Object.create(ProtoUtc);
  self.epochMillis = epochMillis;
  return self;
};
/**
 * Create a `DateTime` from a `Date`.
 *
 * If the `Date` is invalid, an `IllegalArgumentException` will be thrown.
 *
 * @since 3.6.0
 * @category constructors
 */
const unsafeFromDate = date => {
  const epochMillis = date.getTime();
  if (Number.isNaN(epochMillis)) {
    throw new _Cause.IllegalArgumentException("Invalid date");
  }
  return makeUtc(epochMillis);
};
/**
 * Create a `DateTime` from one of the following:
 *
 * - A `DateTime`
 * - A `Date` instance (invalid dates will throw an `IllegalArgumentException`)
 * - The `number` of milliseconds since the Unix epoch
 * - An object with the parts of a date
 * - A `string` that can be parsed by `Date.parse`
 *
 * @since 3.6.0
 * @category constructors
 * @example
 * import { DateTime } from "effect"
 *
 * // from Date
 * DateTime.unsafeMake(new Date())
 *
 * // from parts
 * DateTime.unsafeMake({ year: 2024 })
 *
 * // from string
 * DateTime.unsafeMake("2024-01-01")
 */
exports.unsafeFromDate = unsafeFromDate;
const unsafeMake = input => {
  if (isDateTime(input)) {
    return input;
  } else if (input instanceof Date) {
    return unsafeFromDate(input);
  } else if (typeof input === "object") {
    const date = new Date(0);
    setPartsDate(date, input);
    return unsafeFromDate(date);
  }
  return unsafeFromDate(new Date(input));
};
/**
 * Create a `DateTime.Zoned` using `DateTime.unsafeMake` and a time zone.
 *
 * The input is treated as UTC and then the time zone is attached, unless
 * `adjustForTimeZone` is set to `true`. In that case, the input is treated as
 * already in the time zone.
 *
 * @since 3.6.0
 * @category constructors
 * @example
 * import { DateTime } from "effect"
 *
 * DateTime.unsafeMakeZoned(new Date(), { timeZone: "Europe/London" })
 */
exports.unsafeMake = unsafeMake;
const unsafeMakeZoned = (input, options) => {
  const self = unsafeMake(input);
  let zone;
  if (isTimeZone(options.timeZone)) {
    zone = options.timeZone;
  } else if (typeof options.timeZone === "number") {
    zone = zoneMakeOffset(options.timeZone);
  } else {
    const parsedZone = zoneFromString(options.timeZone);
    if (Option.isNone(parsedZone)) {
      throw new _Cause.IllegalArgumentException(`Invalid time zone: ${options.timeZone}`);
    }
    zone = parsedZone.value;
  }
  if (options.adjustForTimeZone !== true) {
    return makeZonedProto(self.epochMillis, zone, self.partsUtc);
  }
  return makeZonedFromAdjusted(self.epochMillis, zone);
};
/**
 * Create a `DateTime.Zoned` using `DateTime.make` and a time zone.
 *
 * The input is treated as UTC and then the time zone is attached.
 *
 * If the date time input or time zone is invalid, `None` will be returned.
 *
 * @since 3.6.0
 * @category constructors
 * @example
 * import { DateTime } from "effect"
 *
 * DateTime.makeZoned(new Date(), { timeZone: "Europe/London" })
 */
exports.unsafeMakeZoned = unsafeMakeZoned;
const makeZoned = exports.makeZoned = /*#__PURE__*/Option.liftThrowable(unsafeMakeZoned);
/**
 * Create a `DateTime` from one of the following:
 *
 * - A `DateTime`
 * - A `Date` instance (invalid dates will throw an `IllegalArgumentException`)
 * - The `number` of milliseconds since the Unix epoch
 * - An object with the parts of a date
 * - A `string` that can be parsed by `Date.parse`
 *
 * If the input is invalid, `None` will be returned.
 *
 * @since 3.6.0
 * @category constructors
 * @example
 * import { DateTime } from "effect"
 *
 * // from Date
 * DateTime.make(new Date())
 *
 * // from parts
 * DateTime.make({ year: 2024 })
 *
 * // from string
 * DateTime.make("2024-01-01")
 */
const make = exports.make = /*#__PURE__*/Option.liftThrowable(unsafeMake);
const zonedStringRegex = /^(.{17,35})\[(.+)\]$/;
/**
 * Create a `DateTime.Zoned` from a string.
 *
 * It uses the format: `YYYY-MM-DDTHH:mm:ss.sss+HH:MM[Time/Zone]`.
 *
 * @since 3.6.0
 * @category constructors
 */
const makeZonedFromString = input => {
  const match = zonedStringRegex.exec(input);
  if (match === null) {
    const offset = parseOffset(input);
    return offset !== null ? makeZoned(input, {
      timeZone: offset
    }) : Option.none();
  }
  const [, isoString, timeZone] = match;
  return makeZoned(isoString, {
    timeZone
  });
};
/**
 * Get the current time using the `Clock` service and convert it to a `DateTime`.
 *
 * @since 3.6.0
 * @category constructors
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 * })
 */
exports.makeZonedFromString = makeZonedFromString;
const now = exports.now = /*#__PURE__*/Effect.map(Clock.currentTimeMillis, makeUtc);
/**
 * Get the current time using `Date.now`.
 *
 * @since 3.6.0
 * @category constructors
 */
const unsafeNow = () => makeUtc(Date.now());
// =============================================================================
// time zones
// =============================================================================
/**
 * Set the time zone of a `DateTime`, returning a new `DateTime.Zoned`.
 *
 * @since 3.6.0
 * @category time zones
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   const zone = DateTime.zoneUnsafeMakeNamed("Europe/London")
 *
 *   // set the time zone
 *   const zoned: DateTime.Zoned = DateTime.setZone(now, zone)
 * })
 */
exports.unsafeNow = unsafeNow;
const setZone = exports.setZone = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, zone, options) => options?.adjustForTimeZone === true ? makeZonedFromAdjusted(self.epochMillis, zone) : makeZonedProto(self.epochMillis, zone, self.partsUtc));
/**
 * Add a fixed offset time zone to a `DateTime`.
 *
 * The offset is in milliseconds.
 *
 * @since 3.6.0
 * @category time zones
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *
 *   // set the offset time zone in milliseconds
 *   const zoned: DateTime.Zoned = DateTime.setZoneOffset(now, 3 * 60 * 60 * 1000)
 * })
 */
const setZoneOffset = exports.setZoneOffset = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, offset, options) => setZone(self, zoneMakeOffset(offset), options));
const validZoneCache = /*#__PURE__*/(0, _GlobalValue.globalValue)("effect/DateTime/validZoneCache", () => new Map());
const formatOptions = {
  day: "numeric",
  month: "numeric",
  year: "numeric",
  hour: "numeric",
  minute: "numeric",
  second: "numeric",
  timeZoneName: "longOffset",
  fractionalSecondDigits: 3,
  hourCycle: "h23"
};
const zoneMakeIntl = format => {
  const zoneId = format.resolvedOptions().timeZone;
  if (validZoneCache.has(zoneId)) {
    return validZoneCache.get(zoneId);
  }
  const zone = Object.create(ProtoTimeZoneNamed);
  zone.id = zoneId;
  zone.format = format;
  validZoneCache.set(zoneId, zone);
  return zone;
};
/**
 * Attempt to create a named time zone from a IANA time zone identifier.
 *
 * If the time zone is invalid, an `IllegalArgumentException` will be thrown.
 *
 * @since 3.6.0
 * @category time zones
 */
const zoneUnsafeMakeNamed = zoneId => {
  if (validZoneCache.has(zoneId)) {
    return validZoneCache.get(zoneId);
  }
  try {
    return zoneMakeIntl(new Intl.DateTimeFormat("en-US", {
      ...formatOptions,
      timeZone: zoneId
    }));
  } catch (_) {
    throw new _Cause.IllegalArgumentException(`Invalid time zone: ${zoneId}`);
  }
};
/**
 * Create a fixed offset time zone.
 *
 * @since 3.6.0
 * @category time zones
 */
exports.zoneUnsafeMakeNamed = zoneUnsafeMakeNamed;
const zoneMakeOffset = offset => {
  const zone = Object.create(ProtoTimeZoneOffset);
  zone.offset = offset;
  return zone;
};
/**
 * Create a named time zone from a IANA time zone identifier. If the time zone
 * is invalid, `None` will be returned.
 *
 * @since 3.6.0
 * @category time zones
 */
exports.zoneMakeOffset = zoneMakeOffset;
const zoneMakeNamed = exports.zoneMakeNamed = /*#__PURE__*/Option.liftThrowable(zoneUnsafeMakeNamed);
/**
 * Create a named time zone from a IANA time zone identifier. If the time zone
 * is invalid, it will fail with an `IllegalArgumentException`.
 *
 * @since 3.6.0
 * @category time zones
 */
const zoneMakeNamedEffect = zoneId => Effect.try({
  try: () => zoneUnsafeMakeNamed(zoneId),
  catch: e => e
});
/**
 * Create a named time zone from the system's local time zone.
 *
 * @since 3.6.0
 * @category time zones
 */
exports.zoneMakeNamedEffect = zoneMakeNamedEffect;
const zoneMakeLocal = () => zoneMakeIntl(new Intl.DateTimeFormat("en-US", formatOptions));
exports.zoneMakeLocal = zoneMakeLocal;
const offsetZoneRegex = /^(?:GMT|[+-])/;
/**
 * Try parse a TimeZone from a string
 *
 * @since 3.6.0
 * @category time zones
 */
const zoneFromString = zone => {
  if (offsetZoneRegex.test(zone)) {
    const offset = parseOffset(zone);
    return offset === null ? Option.none() : Option.some(zoneMakeOffset(offset));
  }
  return zoneMakeNamed(zone);
};
/**
 * Format a `TimeZone` as a string.
 *
 * @since 3.6.0
 * @category time zones
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * // Outputs "+03:00"
 * DateTime.zoneToString(DateTime.zoneMakeOffset(3 * 60 * 60 * 1000))
 *
 * // Outputs "Europe/London"
 * DateTime.zoneToString(DateTime.zoneUnsafeMakeNamed("Europe/London"))
 */
exports.zoneFromString = zoneFromString;
const zoneToString = self => {
  if (self._tag === "Offset") {
    return offsetToString(self.offset);
  }
  return self.id;
};
/**
 * Set the time zone of a `DateTime` from an IANA time zone identifier. If the
 * time zone is invalid, `None` will be returned.
 *
 * @since 3.6.0
 * @category time zones
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   // set the time zone, returns an Option
 *   DateTime.setZoneNamed(now, "Europe/London")
 * })
 */
exports.zoneToString = zoneToString;
const setZoneNamed = exports.setZoneNamed = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, zoneId, options) => Option.map(zoneMakeNamed(zoneId), zone => setZone(self, zone, options)));
/**
 * Set the time zone of a `DateTime` from an IANA time zone identifier. If the
 * time zone is invalid, an `IllegalArgumentException` will be thrown.
 *
 * @since 3.6.0
 * @category time zones
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   // set the time zone
 *   DateTime.unsafeSetZoneNamed(now, "Europe/London")
 * })
 */
const unsafeSetZoneNamed = exports.unsafeSetZoneNamed = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, zoneId, options) => setZone(self, zoneUnsafeMakeNamed(zoneId), options));
// =============================================================================
// comparisons
// =============================================================================
/**
 * Calulate the difference between two `DateTime` values, returning the number
 * of milliseconds the `other` DateTime is from `self`.
 *
 * If `other` is *after* `self`, the result will be a positive number.
 *
 * @since 3.6.0
 * @category comparisons
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   const other = DateTime.add(now, { minutes: 1 })
 *
 *   // returns 60000
 *   DateTime.distance(now, other)
 * })
 */
const distance = exports.distance = /*#__PURE__*/(0, _Function.dual)(2, (self, other) => toEpochMillis(other) - toEpochMillis(self));
/**
 * Calulate the difference between two `DateTime` values.
 *
 * If the `other` DateTime is before `self`, the result will be a negative
 * `Duration`, returned as a `Left`.
 *
 * If the `other` DateTime is after `self`, the result will be a positive
 * `Duration`, returned as a `Right`.
 *
 * @since 3.6.0
 * @category comparisons
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   const other = DateTime.add(now, { minutes: 1 })
 *
 *   // returns Either.right(Duration.minutes(1))
 *   DateTime.distanceDurationEither(now, other)
 *
 *   // returns Either.left(Duration.minutes(1))
 *   DateTime.distanceDurationEither(other, now)
 * })
 */
const distanceDurationEither = exports.distanceDurationEither = /*#__PURE__*/(0, _Function.dual)(2, (self, other) => {
  const diffMillis = distance(self, other);
  return diffMillis > 0 ? Either.right(Duration.millis(diffMillis)) : Either.left(Duration.millis(-diffMillis));
});
/**
 * Calulate the distance between two `DateTime` values.
 *
 * @since 3.6.0
 * @category comparisons
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *   const other = DateTime.add(now, { minutes: 1 })
 *
 *   // returns Duration.minutes(1)
 *   DateTime.distanceDuration(now, other)
 * })
 */
const distanceDuration = exports.distanceDuration = /*#__PURE__*/(0, _Function.dual)(2, (self, other) => Duration.millis(Math.abs(distance(self, other))));
/**
 * @since 3.6.0
 * @category comparisons
 */
const min = exports.min = /*#__PURE__*/order.min(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const max = exports.max = /*#__PURE__*/order.max(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const greaterThan = exports.greaterThan = /*#__PURE__*/order.greaterThan(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const greaterThanOrEqualTo = exports.greaterThanOrEqualTo = /*#__PURE__*/order.greaterThanOrEqualTo(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const lessThan = exports.lessThan = /*#__PURE__*/order.lessThan(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const lessThanOrEqualTo = exports.lessThanOrEqualTo = /*#__PURE__*/order.lessThanOrEqualTo(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const between = exports.between = /*#__PURE__*/order.between(Order);
/**
 * @since 3.6.0
 * @category comparisons
 */
const isFuture = self => Effect.map(now, lessThan(self));
/**
 * @since 3.6.0
 * @category comparisons
 */
exports.isFuture = isFuture;
const unsafeIsFuture = self => lessThan(unsafeNow(), self);
/**
 * @since 3.6.0
 * @category comparisons
 */
exports.unsafeIsFuture = unsafeIsFuture;
const isPast = self => Effect.map(now, greaterThan(self));
/**
 * @since 3.6.0
 * @category comparisons
 */
exports.isPast = isPast;
const unsafeIsPast = self => greaterThan(unsafeNow(), self);
// =============================================================================
// conversions
// =============================================================================
/**
 * Get the UTC `Date` of a `DateTime`.
 *
 * @since 3.6.0
 * @category conversions
 */
exports.unsafeIsPast = unsafeIsPast;
const toDateUtc = self => new Date(self.epochMillis);
/**
 * Convert a `DateTime` to a `Date`, applying the time zone first.
 *
 * @since 3.6.0
 * @category conversions
 */
exports.toDateUtc = toDateUtc;
const toDate = self => {
  if (self._tag === "Utc") {
    return new Date(self.epochMillis);
  } else if (self.zone._tag === "Offset") {
    return new Date(self.epochMillis + self.zone.offset);
  } else if (self.adjustedEpochMillis !== undefined) {
    return new Date(self.adjustedEpochMillis);
  }
  const parts = self.zone.format.formatToParts(self.epochMillis).filter(_ => _.type !== "literal");
  const date = new Date(0);
  date.setUTCFullYear(Number(parts[2].value), Number(parts[0].value) - 1, Number(parts[1].value));
  date.setUTCHours(Number(parts[3].value), Number(parts[4].value), Number(parts[5].value), Number(parts[6].value));
  self.adjustedEpochMillis = date.getTime();
  return date;
};
/**
 * Calculate the time zone offset of a `DateTime.Zoned` in milliseconds.
 *
 * @since 3.6.0
 * @category conversions
 */
exports.toDate = toDate;
const zonedOffset = self => {
  const date = toDate(self);
  return date.getTime() - toEpochMillis(self);
};
exports.zonedOffset = zonedOffset;
const offsetToString = offset => {
  const abs = Math.abs(offset);
  const hours = Math.floor(abs / (60 * 60 * 1000));
  const minutes = Math.round(abs % (60 * 60 * 1000) / (60 * 1000));
  return `${offset < 0 ? "-" : "+"}${String(hours).padStart(2, "0")}:${String(minutes).padStart(2, "0")}`;
};
/**
 * Calculate the time zone offset of a `DateTime` in milliseconds.
 *
 * The offset is formatted as "±HH:MM".
 *
 * @since 3.6.0
 * @category conversions
 */
const zonedOffsetIso = self => offsetToString(zonedOffset(self));
/**
 * Get the milliseconds since the Unix epoch of a `DateTime`.
 *
 * @since 3.6.0
 * @category conversions
 */
exports.zonedOffsetIso = zonedOffsetIso;
const toEpochMillis = self => self.epochMillis;
/**
 * Remove the time aspect of a `DateTime`, first adjusting for the time
 * zone. It will return a `DateTime.Utc` only containing the date.
 *
 * @since 3.6.0
 * @category conversions
 * @example
 * import { DateTime } from "effect"
 *
 * // returns "2024-01-01T00:00:00Z"
 * DateTime.unsafeMakeZoned("2024-01-01T05:00:00Z", {
 *   timeZone: "Pacific/Auckland",
 *   adjustForTimeZone: true
 * }).pipe(
 *   DateTime.removeTime,
 *   DateTime.formatIso
 * )
 */
exports.toEpochMillis = toEpochMillis;
const removeTime = self => withDate(self, date => {
  date.setUTCHours(0, 0, 0, 0);
  return makeUtc(date.getTime());
});
// =============================================================================
// parts
// =============================================================================
exports.removeTime = removeTime;
const dateToParts = date => ({
  millis: date.getUTCMilliseconds(),
  seconds: date.getUTCSeconds(),
  minutes: date.getUTCMinutes(),
  hours: date.getUTCHours(),
  day: date.getUTCDate(),
  weekDay: date.getUTCDay(),
  month: date.getUTCMonth() + 1,
  year: date.getUTCFullYear()
});
/**
 * Get the different parts of a `DateTime` as an object.
 *
 * The parts will be time zone adjusted.
 *
 * @since 3.6.0
 * @category parts
 */
const toParts = self => {
  if (self._tag === "Utc") {
    return toPartsUtc(self);
  } else if (self.partsAdjusted !== undefined) {
    return self.partsAdjusted;
  }
  self.partsAdjusted = withDate(self, dateToParts);
  return self.partsAdjusted;
};
/**
 * Get the different parts of a `DateTime` as an object.
 *
 * The parts will be in UTC.
 *
 * @since 3.6.0
 * @category parts
 */
exports.toParts = toParts;
const toPartsUtc = self => {
  if (self.partsUtc !== undefined) {
    return self.partsUtc;
  }
  self.partsUtc = withDateUtc(self, dateToParts);
  return self.partsUtc;
};
/**
 * Get a part of a `DateTime` as a number.
 *
 * The part will be in the UTC time zone.
 *
 * @since 3.6.0
 * @category parts
 * @example
 * import { DateTime } from "effect"
 *
 * const now = DateTime.unsafeMake({ year: 2024 })
 * const year = DateTime.getPartUtc(now, "year")
 * assert.strictEqual(year, 2024)
 */
exports.toPartsUtc = toPartsUtc;
const getPartUtc = exports.getPartUtc = /*#__PURE__*/(0, _Function.dual)(2, (self, part) => toPartsUtc(self)[part]);
/**
 * Get a part of a `DateTime` as a number.
 *
 * The part will be time zone adjusted.
 *
 * @since 3.6.0
 * @category parts
 * @example
 * import { DateTime } from "effect"
 *
 * const now = DateTime.unsafeMakeZoned({ year: 2024 }, { timeZone: "Europe/London" })
 * const year = DateTime.getPart(now, "year")
 * assert.strictEqual(year, 2024)
 */
const getPart = exports.getPart = /*#__PURE__*/(0, _Function.dual)(2, (self, part) => toParts(self)[part]);
const setPartsDate = (date, parts) => {
  if (parts.year !== undefined) {
    date.setUTCFullYear(parts.year);
  }
  if (parts.month !== undefined) {
    date.setUTCMonth(parts.month - 1);
  }
  if (parts.day !== undefined) {
    date.setUTCDate(parts.day);
  }
  if (parts.weekDay !== undefined) {
    const diff = parts.weekDay - date.getUTCDay();
    date.setUTCDate(date.getUTCDate() + diff);
  }
  if (parts.hours !== undefined) {
    date.setUTCHours(parts.hours);
  }
  if (parts.minutes !== undefined) {
    date.setUTCMinutes(parts.minutes);
  }
  if (parts.seconds !== undefined) {
    date.setUTCSeconds(parts.seconds);
  }
  if (parts.millis !== undefined) {
    date.setUTCMilliseconds(parts.millis);
  }
};
/**
 * Set the different parts of a `DateTime` as an object.
 *
 * The Date will be time zone adjusted.
 *
 * @since 3.6.0
 * @category parts
 */
const setParts = exports.setParts = /*#__PURE__*/(0, _Function.dual)(2, (self, parts) => mutate(self, date => setPartsDate(date, parts)));
/**
 * Set the different parts of a `DateTime` as an object.
 *
 * @since 3.6.0
 * @category parts
 */
const setPartsUtc = exports.setPartsUtc = /*#__PURE__*/(0, _Function.dual)(2, (self, parts) => mutateUtc(self, date => setPartsDate(date, parts)));
// =============================================================================
// current time zone
// =============================================================================
/**
 * @since 3.6.0
 * @category current time zone
 */
class CurrentTimeZone extends /*#__PURE__*/Context.Tag("effect/DateTime/CurrentTimeZone")() {}
/**
 * Set the time zone of a `DateTime` to the current time zone, which is
 * determined by the `CurrentTimeZone` service.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.now
 *
 *   // set the time zone to "Europe/London"
 *   const zoned = yield* DateTime.setZoneCurrent(now)
 * }).pipe(DateTime.withCurrentZoneNamed("Europe/London"))
 */
exports.CurrentTimeZone = CurrentTimeZone;
const setZoneCurrent = self => Effect.map(CurrentTimeZone, zone => setZone(self, zone));
/**
 * Provide the `CurrentTimeZone` to an effect.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * const zone = DateTime.zoneUnsafeMakeNamed("Europe/London")
 *
 * Effect.gen(function* () {
 *   const now = yield* DateTime.nowInCurrentZone
 * }).pipe(DateTime.withCurrentZone(zone))
 */
exports.setZoneCurrent = setZoneCurrent;
const withCurrentZone = exports.withCurrentZone = /*#__PURE__*/(0, _Function.dual)(2, (effect, zone) => Effect.provideService(effect, CurrentTimeZone, zone));
/**
 * Provide the `CurrentTimeZone` to an effect, using the system's local time
 * zone.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   // will use the system's local time zone
 *   const now = yield* DateTime.nowInCurrentZone
 * }).pipe(DateTime.withCurrentZoneLocal)
 */
const withCurrentZoneLocal = effect => Effect.provideServiceEffect(effect, CurrentTimeZone, Effect.sync(zoneMakeLocal));
/**
 * Provide the `CurrentTimeZone` to an effect, using a offset.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   // will use the system's local time zone
 *   const now = yield* DateTime.nowInCurrentZone
 * }).pipe(DateTime.withCurrentZoneOffset(3 * 60 * 60 * 1000))
 */
exports.withCurrentZoneLocal = withCurrentZoneLocal;
const withCurrentZoneOffset = exports.withCurrentZoneOffset = /*#__PURE__*/(0, _Function.dual)(2, (effect, offset) => Effect.provideService(effect, CurrentTimeZone, zoneMakeOffset(offset)));
/**
 * Provide the `CurrentTimeZone` to an effect using an IANA time zone
 * identifier.
 *
 * If the time zone is invalid, it will fail with an `IllegalArgumentException`.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   // will use the "Europe/London" time zone
 *   const now = yield* DateTime.nowInCurrentZone
 * }).pipe(DateTime.withCurrentZoneNamed("Europe/London"))
 */
const withCurrentZoneNamed = exports.withCurrentZoneNamed = /*#__PURE__*/(0, _Function.dual)(2, (effect, zone) => Effect.provideServiceEffect(effect, CurrentTimeZone, zoneMakeNamedEffect(zone)));
/**
 * Get the current time as a `DateTime.Zoned`, using the `CurrentTimeZone`.
 *
 * @since 3.6.0
 * @category current time zone
 * @example
 * import { DateTime, Effect } from "effect"
 *
 * Effect.gen(function* () {
 *   // will use the "Europe/London" time zone
 *   const now = yield* DateTime.nowInCurrentZone
 * }).pipe(DateTime.withCurrentZoneNamed("Europe/London"))
 */
const nowInCurrentZone = exports.nowInCurrentZone = /*#__PURE__*/Effect.flatMap(now, setZoneCurrent);
/**
 * Create a Layer from the given time zone.
 *
 * @since 3.6.0
 * @category current time zone
 */
const layerCurrentZone = zone => Layer.succeed(CurrentTimeZone, zone);
/**
 * Create a Layer from the given time zone offset.
 *
 * @since 3.6.0
 * @category current time zone
 */
exports.layerCurrentZone = layerCurrentZone;
const layerCurrentZoneOffset = offset => Layer.succeed(CurrentTimeZone, zoneMakeOffset(offset));
/**
 * Create a Layer from the given IANA time zone identifier.
 *
 * @since 3.6.0
 * @category current time zone
 */
exports.layerCurrentZoneOffset = layerCurrentZoneOffset;
const layerCurrentZoneNamed = zoneId => Layer.effect(CurrentTimeZone, zoneMakeNamedEffect(zoneId));
/**
 * Create a Layer from the systems local time zone.
 *
 * @since 3.6.0
 * @category current time zone
 */
exports.layerCurrentZoneNamed = layerCurrentZoneNamed;
const layerCurrentZoneLocal = exports.layerCurrentZoneLocal = /*#__PURE__*/Layer.sync(CurrentTimeZone, zoneMakeLocal);
// =============================================================================
// mapping
// =============================================================================
const makeZonedFromAdjusted = (adjustedMillis, zone) => {
  const offset = zone._tag === "Offset" ? zone.offset : calculateNamedOffset(adjustedMillis, zone);
  return makeZonedProto(adjustedMillis - offset, zone);
};
const offsetRegex = /([+-])(\d{2}):(\d{2})$/;
const parseOffset = offset => {
  const match = offsetRegex.exec(offset);
  if (match === null) {
    return null;
  }
  const [, sign, hours, minutes] = match;
  return (sign === "+" ? 1 : -1) * (Number(hours) * 60 + Number(minutes)) * 60 * 1000;
};
const calculateNamedOffset = (adjustedMillis, zone) => {
  const offset = zone.format.formatToParts(adjustedMillis).find(_ => _.type === "timeZoneName")?.value ?? "";
  if (offset === "GMT") {
    return 0;
  }
  const result = parseOffset(offset);
  if (result === null) {
    // fallback to using the adjusted date
    return zonedOffset(makeZonedProto(adjustedMillis, zone));
  }
  return result;
};
/**
 * Modify a `DateTime` by applying a function to a cloned `Date` instance.
 *
 * The `Date` will first have the time zone applied if possible, and then be
 * converted back to a `DateTime` within the same time zone.
 *
 * @since 3.6.0
 * @category mapping
 */
const mutate = exports.mutate = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => {
  if (self._tag === "Utc") {
    const date = toDateUtc(self);
    f(date);
    return makeUtc(date.getTime());
  }
  const adjustedDate = toDate(self);
  const newAdjustedDate = new Date(adjustedDate.getTime());
  f(newAdjustedDate);
  return makeZonedFromAdjusted(newAdjustedDate.getTime(), self.zone);
});
/**
 * Modify a `DateTime` by applying a function to a cloned UTC `Date` instance.
 *
 * @since 3.6.0
 * @category mapping
 */
const mutateUtc = exports.mutateUtc = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => mapEpochMillis(self, millis => {
  const date = new Date(millis);
  f(date);
  return date.getTime();
}));
/**
 * Transform a `DateTime` by applying a function to the number of milliseconds
 * since the Unix epoch.
 *
 * @since 3.6.0
 * @category mapping
 * @example
 * import { DateTime } from "effect"
 *
 * // add 10 milliseconds
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.mapEpochMillis((millis) => millis + 10)
 * )
 */
const mapEpochMillis = exports.mapEpochMillis = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => {
  const millis = f(toEpochMillis(self));
  return self._tag === "Utc" ? makeUtc(millis) : makeZonedProto(millis, self.zone);
});
/**
 * Using the time zone adjusted `Date`, apply a function to the `Date` and
 * return the result.
 *
 * @since 3.6.0
 * @category mapping
 * @example
 * import { DateTime } from "effect"
 *
 * // get the time zone adjusted date in milliseconds
 * DateTime.unsafeMakeZoned(0, { timeZone: "Europe/London" }).pipe(
 *   DateTime.withDate((date) => date.getTime())
 * )
 */
const withDate = exports.withDate = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => f(toDate(self)));
/**
 * Using the time zone adjusted `Date`, apply a function to the `Date` and
 * return the result.
 *
 * @since 3.6.0
 * @category mapping
 * @example
 * import { DateTime } from "effect"
 *
 * // get the date in milliseconds
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.withDateUtc((date) => date.getTime())
 * )
 */
const withDateUtc = exports.withDateUtc = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => f(toDateUtc(self)));
/**
 * @since 3.6.0
 * @category mapping
 */
const match = exports.match = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => self._tag === "Utc" ? options.onUtc(self) : options.onZoned(self));
// =============================================================================
// math
// =============================================================================
/**
 * Add the given `Duration` to a `DateTime`.
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // add 5 minutes
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.addDuration("5 minutes")
 * )
 */
const addDuration = exports.addDuration = /*#__PURE__*/(0, _Function.dual)(2, (self, duration) => mapEpochMillis(self, millis => millis + Duration.toMillis(duration)));
/**
 * Subtract the given `Duration` from a `DateTime`.
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // subtract 5 minutes
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.subtractDuration("5 minutes")
 * )
 */
const subtractDuration = exports.subtractDuration = /*#__PURE__*/(0, _Function.dual)(2, (self, duration) => mapEpochMillis(self, millis => millis - Duration.toMillis(duration)));
const addMillis = (date, amount) => {
  date.setTime(date.getTime() + amount);
};
/**
 * Add the given `amount` of `unit`'s to a `DateTime`.
 *
 * The time zone is taken into account when adding days, weeks, months, and
 * years.
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // add 5 minutes
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.add({ minutes: 5 })
 * )
 */
const add = exports.add = /*#__PURE__*/(0, _Function.dual)(2, (self, parts) => mutate(self, date => {
  if (parts.millis) {
    addMillis(date, parts.millis);
  }
  if (parts.seconds) {
    addMillis(date, parts.seconds * 1000);
  }
  if (parts.minutes) {
    addMillis(date, parts.minutes * 60 * 1000);
  }
  if (parts.hours) {
    addMillis(date, parts.hours * 60 * 60 * 1000);
  }
  if (parts.days) {
    date.setUTCDate(date.getUTCDate() + parts.days);
  }
  if (parts.weeks) {
    date.setUTCDate(date.getUTCDate() + parts.weeks * 7);
  }
  if (parts.months) {
    const day = date.getUTCDate();
    date.setUTCMonth(date.getUTCMonth() + parts.months + 1, 0);
    if (day < date.getUTCDate()) {
      date.setUTCDate(day);
    }
  }
  if (parts.years) {
    const day = date.getUTCDate();
    const month = date.getUTCMonth();
    date.setUTCFullYear(date.getUTCFullYear() + parts.years, month + 1, 0);
    if (day < date.getUTCDate()) {
      date.setUTCDate(day);
    }
  }
}));
/**
 * Subtract the given `amount` of `unit`'s from a `DateTime`.
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // subtract 5 minutes
 * DateTime.unsafeMake(0).pipe(
 *   DateTime.subtract({ minutes: 5 })
 * )
 */
const subtract = exports.subtract = /*#__PURE__*/(0, _Function.dual)(2, (self, parts) => {
  const newParts = {};
  for (const key in parts) {
    newParts[key] = -1 * parts[key];
  }
  return add(self, newParts);
});
function startOfDate(date, part, options) {
  switch (part) {
    case "second":
      {
        date.setUTCMilliseconds(0);
        break;
      }
    case "minute":
      {
        date.setUTCSeconds(0, 0);
        break;
      }
    case "hour":
      {
        date.setUTCMinutes(0, 0, 0);
        break;
      }
    case "day":
      {
        date.setUTCHours(0, 0, 0, 0);
        break;
      }
    case "week":
      {
        const weekStartsOn = options?.weekStartsOn ?? 0;
        const day = date.getUTCDay();
        const diff = (day - weekStartsOn + 7) % 7;
        date.setUTCDate(date.getUTCDate() - diff);
        date.setUTCHours(0, 0, 0, 0);
        break;
      }
    case "month":
      {
        date.setUTCDate(1);
        date.setUTCHours(0, 0, 0, 0);
        break;
      }
    case "year":
      {
        date.setUTCMonth(0, 1);
        date.setUTCHours(0, 0, 0, 0);
        break;
      }
  }
}
/**
 * Converts a `DateTime` to the start of the given `part`.
 *
 * If the part is `week`, the `weekStartsOn` option can be used to specify the
 * day of the week that the week starts on. The default is 0 (Sunday).
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // returns "2024-01-01T00:00:00Z"
 * DateTime.unsafeMake("2024-01-01T12:00:00Z").pipe(
 *   DateTime.startOf("day"),
 *   DateTime.formatIso
 * )
 */
const startOf = exports.startOf = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, part, options) => mutate(self, date => startOfDate(date, part, options)));
function endOfDate(date, part, options) {
  switch (part) {
    case "second":
      {
        date.setUTCMilliseconds(999);
        break;
      }
    case "minute":
      {
        date.setUTCSeconds(59, 999);
        break;
      }
    case "hour":
      {
        date.setUTCMinutes(59, 59, 999);
        break;
      }
    case "day":
      {
        date.setUTCHours(23, 59, 59, 999);
        break;
      }
    case "week":
      {
        const weekStartsOn = options?.weekStartsOn ?? 0;
        const day = date.getUTCDay();
        const diff = (day - weekStartsOn + 7) % 7;
        date.setUTCDate(date.getUTCDate() - diff + 6);
        date.setUTCHours(23, 59, 59, 999);
        break;
      }
    case "month":
      {
        date.setUTCMonth(date.getUTCMonth() + 1, 0);
        date.setUTCHours(23, 59, 59, 999);
        break;
      }
    case "year":
      {
        date.setUTCMonth(11, 31);
        date.setUTCHours(23, 59, 59, 999);
        break;
      }
  }
}
/**
 * Converts a `DateTime` to the end of the given `part`.
 *
 * If the part is `week`, the `weekStartsOn` option can be used to specify the
 * day of the week that the week starts on. The default is 0 (Sunday).
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // returns "2024-01-01T23:59:59.999Z"
 * DateTime.unsafeMake("2024-01-01T12:00:00Z").pipe(
 *   DateTime.endOf("day"),
 *   DateTime.formatIso
 * )
 */
const endOf = exports.endOf = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, part, options) => mutate(self, date => endOfDate(date, part, options)));
/**
 * Converts a `DateTime` to the nearest given `part`.
 *
 * If the part is `week`, the `weekStartsOn` option can be used to specify the
 * day of the week that the week starts on. The default is 0 (Sunday).
 *
 * @since 3.6.0
 * @category math
 * @example
 * import { DateTime } from "effect"
 *
 * // returns "2024-01-02T00:00:00Z"
 * DateTime.unsafeMake("2024-01-01T12:01:00Z").pipe(
 *   DateTime.nearest("day"),
 *   DateTime.formatIso
 * )
 */
const nearest = exports.nearest = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, part, options) => mutate(self, date => {
  if (part === "milli") return;
  const millis = date.getTime();
  const start = new Date(millis);
  startOfDate(start, part, options);
  const startMillis = start.getTime();
  const end = new Date(millis);
  endOfDate(end, part, options);
  const endMillis = end.getTime() + 1;
  const diffStart = millis - startMillis;
  const diffEnd = endMillis - millis;
  if (diffStart < diffEnd) {
    date.setTime(startMillis);
  } else {
    date.setTime(endMillis);
  }
}));
// =============================================================================
// formatting
// =============================================================================
const intlTimeZone = self => {
  if (self._tag === "Named") {
    return self.id;
  }
  return offsetToString(self.offset);
};
/**
 * Format a `DateTime` as a string using the `DateTimeFormat` API.
 *
 * The `timeZone` option is set to the offset of the time zone.
 *
 * Note: On Node versions < 22, fixed "Offset" zones will set the time zone to
 * "UTC" and use the adjusted `Date`.
 *
 * @since 3.6.0
 * @category formatting
 */
const format = exports.format = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, options) => {
  try {
    return new Intl.DateTimeFormat(options?.locale, {
      timeZone: self._tag === "Utc" ? "UTC" : intlTimeZone(self.zone),
      ...options
    }).format(self.epochMillis);
  } catch (_) {
    return new Intl.DateTimeFormat(options?.locale, {
      timeZone: "UTC",
      ...options
    }).format(toDate(self));
  }
});
/**
 * Format a `DateTime` as a string using the `DateTimeFormat` API.
 *
 * It will use the system's local time zone & locale.
 *
 * @since 3.6.0
 * @category formatting
 */
const formatLocal = exports.formatLocal = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, options) => new Intl.DateTimeFormat(options?.locale, options).format(self.epochMillis));
/**
 * Format a `DateTime` as a string using the `DateTimeFormat` API.
 *
 * This forces the time zone to be UTC.
 *
 * @since 3.6.0
 * @category formatting
 */
const formatUtc = exports.formatUtc = /*#__PURE__*/(0, _Function.dual)(isDateTimeArgs, (self, options) => new Intl.DateTimeFormat(options?.locale, {
  ...options,
  timeZone: "UTC"
}).format(self.epochMillis));
/**
 * Format a `DateTime` as a string using the `DateTimeFormat` API.
 *
 * @since 3.6.0
 * @category formatting
 */
const formatIntl = exports.formatIntl = /*#__PURE__*/(0, _Function.dual)(2, (self, format) => format.format(self.epochMillis));
/**
 * Format a `DateTime` as a UTC ISO string.
 *
 * @since 3.6.0
 * @category formatting
 */
const formatIso = self => toDateUtc(self).toISOString();
/**
 * Format a `DateTime` as a time zone adjusted ISO date string.
 *
 * @since 3.6.0
 * @category formatting
 */
exports.formatIso = formatIso;
const formatIsoDate = self => toDate(self).toISOString().slice(0, 10);
/**
 * Format a `DateTime` as a UTC ISO date string.
 *
 * @since 3.6.0
 * @category formatting
 */
exports.formatIsoDate = formatIsoDate;
const formatIsoDateUtc = self => toDateUtc(self).toISOString().slice(0, 10);
/**
 * Format a `DateTime.Zoned` as a ISO string with an offset.
 *
 * @since 3.6.0
 * @category formatting
 */
exports.formatIsoDateUtc = formatIsoDateUtc;
const formatIsoOffset = self => {
  const date = toDate(self);
  return self._tag === "Utc" ? date.toISOString() : `${date.toISOString().slice(0, -1)}${zonedOffsetIso(self)}`;
};
/**
 * Format a `DateTime.Zoned` as a string.
 *
 * It uses the format: `YYYY-MM-DDTHH:mm:ss.sss+HH:MM[Time/Zone]`.
 *
 * @since 3.6.0
 * @category formatting
 */
exports.formatIsoOffset = formatIsoOffset;
const formatIsoZoned = self => self.zone._tag === "Offset" ? formatIsoOffset(self) : `${formatIsoOffset(self)}[${self.zone.id}]`;
exports.formatIsoZoned = formatIsoZoned;
//# sourceMappingURL=DateTime.js.map