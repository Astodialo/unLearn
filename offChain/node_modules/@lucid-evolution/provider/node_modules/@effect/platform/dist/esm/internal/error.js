import * as Data from "effect/Data";
/** @internal */
export const PlatformErrorTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Error/PlatformErrorTypeId");
const make = tag => props => Data.struct({
  [PlatformErrorTypeId]: PlatformErrorTypeId,
  _tag: tag,
  ...props
});
/** @internal */
export const badArgument = /*#__PURE__*/make("BadArgument");
/** @internal */
export const systemError = /*#__PURE__*/make("SystemError");
//# sourceMappingURL=error.js.map