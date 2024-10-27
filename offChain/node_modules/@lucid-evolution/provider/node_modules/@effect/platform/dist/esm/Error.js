import * as Data from "effect/Data";
import * as Predicate from "effect/Predicate";
import * as internal from "./internal/error.js";
/**
 * @since 1.0.0
 * @category type id
 */
export const PlatformErrorTypeId = internal.PlatformErrorTypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
export const isPlatformError = u => Predicate.hasProperty(u, PlatformErrorTypeId);
/**
 * @since 1.0.0
 * @category error
 */
export const TypeIdError = (typeId, tag) => {
  class Base extends Data.Error {
    _tag = tag;
  }
  ;
  Base.prototype[typeId] = typeId;
  Base.prototype.name = tag;
  return Base;
};
/**
 * @since 1.0.0
 * @category error
 */
export const BadArgument = internal.badArgument;
/**
 * @since 1.0.0
 * @category error
 */
export const SystemError = internal.systemError;
//# sourceMappingURL=Error.js.map