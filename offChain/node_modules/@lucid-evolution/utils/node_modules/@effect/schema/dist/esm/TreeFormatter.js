/**
 * @since 0.67.0
 */
import * as Effect from "effect/Effect";
import * as Option from "effect/Option";
import * as Predicate from "effect/Predicate";
import * as AST from "./AST.js";
import * as util_ from "./internal/util.js";
const make = (value, forest = []) => ({
  value,
  forest
});
/**
 * @category formatting
 * @since 0.67.0
 */
export const formatIssue = issue => Effect.map(go(issue), tree => drawTree(tree));
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
const drawTree = tree => tree.value + draw("\n", tree.forest);
const draw = (indentation, forest) => {
  let r = "";
  const len = forest.length;
  let tree;
  for (let i = 0; i < len; i++) {
    tree = forest[i];
    const isLast = i === len - 1;
    r += indentation + (isLast ? "└" : "├") + "─ " + tree.value;
    r += draw(indentation + (len > 1 && !isLast ? "│  " : "   "), tree.forest);
  }
  return r;
};
const formatTransformationKind = kind => {
  switch (kind) {
    case "Encoded":
      return "Encoded side transformation failure";
    case "Transformation":
      return "Transformation process failure";
    case "Type":
      return "Type side transformation failure";
  }
};
const formatRefinementKind = kind => {
  switch (kind) {
    case "From":
      return "From side refinement failure";
    case "Predicate":
      return "Predicate refinement failure";
  }
};
const getAnnotated = issue => "ast" in issue ? Option.some(issue.ast) : Option.none();
const getCurrentMessage = issue => getAnnotated(issue).pipe(Option.flatMap(AST.getMessageAnnotation), Effect.flatMap(annotation => {
  const out = annotation(issue);
  return Predicate.isString(out) ? Effect.succeed({
    message: out,
    override: false
  }) : Effect.isEffect(out) ? Effect.map(out, message => ({
    message,
    override: false
  })) : Predicate.isString(out.message) ? Effect.succeed({
    message: out.message,
    override: out.override
  }) : Effect.map(out.message, message => ({
    message,
    override: out.override
  }));
}));
const createParseIssueGuard = tag => issue => issue._tag === tag;
const isComposite = /*#__PURE__*/createParseIssueGuard("Composite");
const isRefinement = /*#__PURE__*/createParseIssueGuard("Refinement");
const isTransformation = /*#__PURE__*/createParseIssueGuard("Transformation");
/** @internal */
export const getMessage = issue => getCurrentMessage(issue).pipe(Effect.flatMap(currentMessage => {
  const useInnerMessage = !currentMessage.override && (isComposite(issue) || isRefinement(issue) && issue.kind === "From" || isTransformation(issue) && issue.kind !== "Transformation");
  return useInnerMessage ? isTransformation(issue) || isRefinement(issue) ? getMessage(issue.issue) : Option.none() : Effect.succeed(currentMessage.message);
}));
const getParseIssueTitleAnnotation = issue => getAnnotated(issue).pipe(Option.flatMap(AST.getParseIssueTitleAnnotation), Option.filterMap(annotation => Option.fromNullable(annotation(issue))));
/** @internal */
export const formatTypeMessage = e => getMessage(e).pipe(Effect.orElse(() => getParseIssueTitleAnnotation(e)), Effect.catchAll(() => Effect.succeed(e.message ?? `Expected ${String(e.ast)}, actual ${util_.formatUnknown(e.actual)}`)));
const getParseIssueTitle = issue => Option.getOrElse(getParseIssueTitleAnnotation(issue), () => String(issue.ast));
/** @internal */
export const formatForbiddenMessage = e => e.message ?? "is forbidden";
/** @internal */
export const formatUnexpectedMessage = e => e.message ?? "is unexpected";
/** @internal */
export const formatMissingMessage = e => AST.getMissingMessageAnnotation(e.ast).pipe(Effect.flatMap(annotation => {
  const out = annotation();
  return Predicate.isString(out) ? Effect.succeed(out) : out;
}), Effect.catchAll(() => Effect.succeed(e.message ?? "is missing")));
const getTree = (issue, onFailure) => Effect.matchEffect(getMessage(issue), {
  onFailure,
  onSuccess: message => Effect.succeed(make(message))
});
const go = e => {
  switch (e._tag) {
    case "Type":
      return Effect.map(formatTypeMessage(e), make);
    case "Forbidden":
      return Effect.succeed(make(getParseIssueTitle(e), [make(formatForbiddenMessage(e))]));
    case "Unexpected":
      return Effect.succeed(make(formatUnexpectedMessage(e)));
    case "Missing":
      return Effect.map(formatMissingMessage(e), make);
    case "Transformation":
      return getTree(e, () => Effect.map(go(e.issue), tree => make(getParseIssueTitle(e), [make(formatTransformationKind(e.kind), [tree])])));
    case "Refinement":
      return getTree(e, () => Effect.map(go(e.issue), tree => make(getParseIssueTitle(e), [make(formatRefinementKind(e.kind), [tree])])));
    case "Pointer":
      return Effect.map(go(e.issue), tree => make(util_.formatPath(e.path), [tree]));
    case "Composite":
      {
        const parseIssueTitle = getParseIssueTitle(e);
        return getTree(e, () => util_.isNonEmpty(e.issues) ? Effect.map(Effect.forEach(e.issues, go), forest => make(parseIssueTitle, forest)) : Effect.map(go(e.issues), tree => make(parseIssueTitle, [tree])));
      }
  }
};
//# sourceMappingURL=TreeFormatter.js.map