/**
 * @since 1.0.0
 */
import * as Cause from "effect/Cause";
import * as Exit from "effect/Exit";
/**
 * @category teardown
 * @since 1.0.0
 */
export const defaultTeardown = (exit, onExit) => {
  onExit(Exit.isFailure(exit) && !Cause.isInterruptedOnly(exit.cause) ? 1 : 0);
};
//# sourceMappingURL=Runtime.js.map