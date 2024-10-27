/**
 * @since 0.67.0
 */
import * as array_ from "effect/Array";
import * as Effect from "effect/Effect";
import * as util_ from "./internal/util.js";
import * as TreeFormatter from "./TreeFormatter.js";
/**
 * @category formatting
 * @since 0.67.0
 */
export const formatIssue = issue => go(issue);
/**
 * @category formatting
 * @since 0.67.0
 */
export const formatIssueSync = issue => Effect.runSync(formatIssue(issue));
/**
 * @category formatting
 * @since 0.67.0
 */
export const formatError = error => formatIssue(error.issue);
/**
 * @category formatting
 * @since 0.67.0
 */
export const formatErrorSync = error => formatIssueSync(error.issue);
const succeed = issue => Effect.succeed([issue]);
const getArray = (issue, path, onFailure) => Effect.matchEffect(TreeFormatter.getMessage(issue), {
  onFailure,
  onSuccess: message => succeed({
    _tag: issue._tag,
    path,
    message
  })
});
const go = (e, path = []) => {
  const _tag = e._tag;
  switch (_tag) {
    case "Type":
      return Effect.map(TreeFormatter.formatTypeMessage(e), message => [{
        _tag,
        path,
        message
      }]);
    case "Forbidden":
      return succeed({
        _tag,
        path,
        message: TreeFormatter.formatForbiddenMessage(e)
      });
    case "Unexpected":
      return succeed({
        _tag,
        path,
        message: TreeFormatter.formatUnexpectedMessage(e)
      });
    case "Missing":
      return Effect.map(TreeFormatter.formatMissingMessage(e), message => [{
        _tag,
        path,
        message
      }]);
    case "Pointer":
      return go(e.issue, path.concat(e.path));
    case "Composite":
      return getArray(e, path, () => util_.isNonEmpty(e.issues) ? Effect.map(Effect.forEach(e.issues, issue => go(issue, path)), array_.flatten) : go(e.issues, path));
    case "Refinement":
    case "Transformation":
      return getArray(e, path, () => go(e.issue, path));
  }
};
//# sourceMappingURL=ArrayFormatter.js.map