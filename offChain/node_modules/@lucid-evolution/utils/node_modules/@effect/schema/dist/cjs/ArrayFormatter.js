"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.formatIssueSync = exports.formatIssue = exports.formatErrorSync = exports.formatError = void 0;
var array_ = _interopRequireWildcard(require("effect/Array"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var util_ = _interopRequireWildcard(require("./internal/util.js"));
var TreeFormatter = _interopRequireWildcard(require("./TreeFormatter.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 0.67.0
 */

/**
 * @category formatting
 * @since 0.67.0
 */
const formatIssue = issue => go(issue);
/**
 * @category formatting
 * @since 0.67.0
 */
exports.formatIssue = formatIssue;
const formatIssueSync = issue => Effect.runSync(formatIssue(issue));
/**
 * @category formatting
 * @since 0.67.0
 */
exports.formatIssueSync = formatIssueSync;
const formatError = error => formatIssue(error.issue);
/**
 * @category formatting
 * @since 0.67.0
 */
exports.formatError = formatError;
const formatErrorSync = error => formatIssueSync(error.issue);
exports.formatErrorSync = formatErrorSync;
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