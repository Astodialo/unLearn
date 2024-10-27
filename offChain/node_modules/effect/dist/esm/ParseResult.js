/**
 * @since 3.10.0
 */
import * as array_ from "./Array.js";
import { TaggedError } from "./Data.js";
import * as Effect from "./Effect.js";
import * as Either from "./Either.js";
import { dual } from "./Function.js";
import { globalValue } from "./GlobalValue.js";
import * as Inspectable from "./Inspectable.js";
import * as util_ from "./internal/schema/util.js";
import * as Option from "./Option.js";
import * as Predicate from "./Predicate.js";
import * as AST from "./SchemaAST.js";
/**
 * @category model
 * @since 3.10.0
 */
export class Pointer {
  path;
  actual;
  issue;
  /**
   * @since 3.10.0
   */
  _tag = "Pointer";
  constructor(path, actual, issue) {
    this.path = path;
    this.actual = actual;
    this.issue = issue;
  }
}
/**
 * Error that occurs when an unexpected key or index is present.
 *
 * @category model
 * @since 3.10.0
 */
export class Unexpected {
  actual;
  message;
  /**
   * @since 3.10.0
   */
  _tag = "Unexpected";
  constructor(actual,
  /**
   * @since 3.10.0
   */
  message) {
    this.actual = actual;
    this.message = message;
  }
}
/**
 * Error that occurs when a required key or index is missing.
 *
 * @category model
 * @since 3.10.0
 */
export class Missing {
  ast;
  message;
  /**
   * @since 3.10.0
   */
  _tag = "Missing";
  /**
   * @since 3.10.0
   */
  actual = undefined;
  constructor(
  /**
   * @since 3.10.0
   */
  ast,
  /**
   * @since 3.10.0
   */
  message) {
    this.ast = ast;
    this.message = message;
  }
}
/**
 * Error that contains multiple issues.
 *
 * @category model
 * @since 3.10.0
 */
export class Composite {
  ast;
  actual;
  issues;
  output;
  /**
   * @since 3.10.0
   */
  _tag = "Composite";
  constructor(ast, actual, issues, output) {
    this.ast = ast;
    this.actual = actual;
    this.issues = issues;
    this.output = output;
  }
}
/**
 * Error that occurs when a refinement has an error.
 *
 * @category model
 * @since 3.10.0
 */
export class Refinement {
  ast;
  actual;
  kind;
  issue;
  /**
   * @since 3.10.0
   */
  _tag = "Refinement";
  constructor(ast, actual, kind, issue) {
    this.ast = ast;
    this.actual = actual;
    this.kind = kind;
    this.issue = issue;
  }
}
/**
 * Error that occurs when a transformation has an error.
 *
 * @category model
 * @since 3.10.0
 */
export class Transformation {
  ast;
  actual;
  kind;
  issue;
  /**
   * @since 3.10.0
   */
  _tag = "Transformation";
  constructor(ast, actual, kind, issue) {
    this.ast = ast;
    this.actual = actual;
    this.kind = kind;
    this.issue = issue;
  }
}
/**
 * The `Type` variant of the `ParseIssue` type represents an error that occurs when the `actual` value is not of the expected type.
 * The `ast` field specifies the expected type, and the `actual` field contains the value that caused the error.
 *
 * @category model
 * @since 3.10.0
 */
export class Type {
  ast;
  actual;
  message;
  /**
   * @since 3.10.0
   */
  _tag = "Type";
  constructor(ast, actual, message) {
    this.ast = ast;
    this.actual = actual;
    this.message = message;
  }
}
/**
 * The `Forbidden` variant of the `ParseIssue` type represents a forbidden operation, such as when encountering an Effect that is not allowed to execute (e.g., using `runSync`).
 *
 * @category model
 * @since 3.10.0
 */
export class Forbidden {
  ast;
  actual;
  message;
  /**
   * @since 3.10.0
   */
  _tag = "Forbidden";
  constructor(ast, actual, message) {
    this.ast = ast;
    this.actual = actual;
    this.message = message;
  }
}
/**
 * @category type id
 * @since 3.10.0
 */
export const ParseErrorTypeId = /*#__PURE__*/Symbol.for("effect/Schema/ParseErrorTypeId");
/**
 * @since 3.10.0
 */
export const isParseError = u => Predicate.hasProperty(u, ParseErrorTypeId);
/**
 * @since 3.10.0
 */
export class ParseError extends /*#__PURE__*/TaggedError("ParseError") {
  /**
   * @since 3.10.0
   */
  [ParseErrorTypeId] = ParseErrorTypeId;
  get message() {
    return this.toString();
  }
  /**
   * @since 3.10.0
   */
  toString() {
    return TreeFormatter.formatIssueSync(this.issue);
  }
  /**
   * @since 3.10.0
   */
  toJSON() {
    return {
      _id: "ParseError",
      message: this.toString()
    };
  }
  /**
   * @since 3.10.0
   */
  [Inspectable.NodeInspectSymbol]() {
    return this.toJSON();
  }
}
/**
 * @category constructors
 * @since 3.10.0
 */
export const parseError = issue => new ParseError({
  issue
});
/**
 * @category constructors
 * @since 3.10.0
 */
export const succeed = Either.right;
/**
 * @category constructors
 * @since 3.10.0
 */
export const fail = Either.left;
const _try = Either.try;
export {
/**
 * @category constructors
 * @since 3.10.0
 */
_try as try };
/**
 * @category constructors
 * @since 3.10.0
 */
export const fromOption = Either.fromOption;
/**
 * @category optimisation
 * @since 3.10.0
 */
export const flatMap = /*#__PURE__*/dual(2, (self, f) => {
  const s = self;
  if (s["_tag"] === "Left") {
    return s;
  }
  if (s["_tag"] === "Right") {
    return f(s.right);
  }
  return Effect.flatMap(self, f);
});
/**
 * @category optimisation
 * @since 3.10.0
 */
export const map = /*#__PURE__*/dual(2, (self, f) => {
  const s = self;
  if (s["_tag"] === "Left") {
    return s;
  }
  if (s["_tag"] === "Right") {
    return Either.right(f(s.right));
  }
  return Effect.map(self, f);
});
/**
 * @category optimisation
 * @since 3.10.0
 */
export const mapError = /*#__PURE__*/dual(2, (self, f) => {
  const s = self;
  if (s["_tag"] === "Left") {
    return Either.left(f(s.left));
  }
  if (s["_tag"] === "Right") {
    return s;
  }
  return Effect.mapError(self, f);
});
/**
 * @category optimisation
 * @since 3.10.0
 */
export const eitherOrUndefined = self => {
  const s = self;
  if (s["_tag"] === "Left" || s["_tag"] === "Right") {
    return s;
  }
};
/**
 * @category optimisation
 * @since 3.10.0
 */
export const mapBoth = /*#__PURE__*/dual(2, (self, options) => {
  const s = self;
  if (s["_tag"] === "Left") {
    return Either.left(options.onFailure(s.left));
  }
  if (s["_tag"] === "Right") {
    return Either.right(options.onSuccess(s.right));
  }
  return Effect.mapBoth(self, options);
});
/**
 * @category optimisation
 * @since 3.10.0
 */
export const orElse = /*#__PURE__*/dual(2, (self, f) => {
  const s = self;
  if (s["_tag"] === "Left") {
    return f(s.left);
  }
  if (s["_tag"] === "Right") {
    return s;
  }
  return Effect.catchAll(self, f);
});
/** @internal */
export const mergeInternalOptions = (options, overrideOptions) => {
  if (overrideOptions === undefined || Predicate.isNumber(overrideOptions)) {
    return options;
  }
  if (options === undefined) {
    return overrideOptions;
  }
  return {
    ...options,
    ...overrideOptions
  };
};
const getEither = (ast, isDecoding, options) => {
  const parser = goMemo(ast, isDecoding);
  return (u, overrideOptions) => parser(u, mergeInternalOptions(options, overrideOptions));
};
const getSync = (ast, isDecoding, options) => {
  const parser = getEither(ast, isDecoding, options);
  return (input, overrideOptions) => Either.getOrThrowWith(parser(input, overrideOptions), parseError);
};
const getOption = (ast, isDecoding, options) => {
  const parser = getEither(ast, isDecoding, options);
  return (input, overrideOptions) => Option.getRight(parser(input, overrideOptions));
};
const getEffect = (ast, isDecoding, options) => {
  const parser = goMemo(ast, isDecoding);
  return (input, overrideOptions) => parser(input, {
    ...mergeInternalOptions(options, overrideOptions),
    isEffectAllowed: true
  });
};
/**
 * @throws `ParseError`
 * @category decoding
 * @since 3.10.0
 */
export const decodeUnknownSync = (schema, options) => getSync(schema.ast, true, options);
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeUnknownOption = (schema, options) => getOption(schema.ast, true, options);
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeUnknownEither = (schema, options) => getEither(schema.ast, true, options);
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeUnknownPromise = (schema, options) => {
  const parser = decodeUnknown(schema, options);
  return (u, overrideOptions) => Effect.runPromise(parser(u, overrideOptions));
};
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeUnknown = (schema, options) => getEffect(schema.ast, true, options);
/**
 * @throws `ParseError`
 * @category encoding
 * @since 3.10.0
 */
export const encodeUnknownSync = (schema, options) => getSync(schema.ast, false, options);
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeUnknownOption = (schema, options) => getOption(schema.ast, false, options);
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeUnknownEither = (schema, options) => getEither(schema.ast, false, options);
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeUnknownPromise = (schema, options) => {
  const parser = encodeUnknown(schema, options);
  return (u, overrideOptions) => Effect.runPromise(parser(u, overrideOptions));
};
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeUnknown = (schema, options) => getEffect(schema.ast, false, options);
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeSync = decodeUnknownSync;
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeOption = decodeUnknownOption;
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodeEither = decodeUnknownEither;
/**
 * @category decoding
 * @since 3.10.0
 */
export const decodePromise = decodeUnknownPromise;
/**
 * @category decoding
 * @since 3.10.0
 */
export const decode = decodeUnknown;
/**
 * @throws `ParseError`
 * @category validation
 * @since 3.10.0
 */
export const validateSync = (schema, options) => getSync(AST.typeAST(schema.ast), true, options);
/**
 * @category validation
 * @since 3.10.0
 */
export const validateOption = (schema, options) => getOption(AST.typeAST(schema.ast), true, options);
/**
 * @category validation
 * @since 3.10.0
 */
export const validateEither = (schema, options) => getEither(AST.typeAST(schema.ast), true, options);
/**
 * @category validation
 * @since 3.10.0
 */
export const validatePromise = (schema, options) => {
  const parser = validate(schema, options);
  return (u, overrideOptions) => Effect.runPromise(parser(u, overrideOptions));
};
/**
 * @category validation
 * @since 3.10.0
 */
export const validate = (schema, options) => getEffect(AST.typeAST(schema.ast), true, options);
/**
 * By default the option `exact` is set to `true`.
 *
 * @category validation
 * @since 3.10.0
 */
export const is = (schema, options) => {
  const parser = goMemo(AST.typeAST(schema.ast), true);
  return (u, overrideOptions) => Either.isRight(parser(u, {
    exact: true,
    ...mergeInternalOptions(options, overrideOptions)
  }));
};
/**
 * By default the option `exact` is set to `true`.
 *
 * @throws `ParseError`
 * @category validation
 * @since 3.10.0
 */
export const asserts = (schema, options) => {
  const parser = goMemo(AST.typeAST(schema.ast), true);
  return (u, overrideOptions) => {
    const result = parser(u, {
      exact: true,
      ...mergeInternalOptions(options, overrideOptions)
    });
    if (Either.isLeft(result)) {
      throw parseError(result.left);
    }
  };
};
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeSync = encodeUnknownSync;
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeOption = encodeUnknownOption;
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodeEither = encodeUnknownEither;
/**
 * @category encoding
 * @since 3.10.0
 */
export const encodePromise = encodeUnknownPromise;
/**
 * @category encoding
 * @since 3.10.0
 */
export const encode = encodeUnknown;
const decodeMemoMap = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("effect/Schema/Parser/decodeMemoMap"), () => new WeakMap());
const encodeMemoMap = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("effect/Schema/Parser/encodeMemoMap"), () => new WeakMap());
const goMemo = (ast, isDecoding) => {
  const memoMap = isDecoding ? decodeMemoMap : encodeMemoMap;
  const memo = memoMap.get(ast);
  if (memo) {
    return memo;
  }
  const raw = go(ast, isDecoding);
  const parseOptionsAnnotation = AST.getParseOptionsAnnotation(ast);
  const parserWithOptions = Option.isSome(parseOptionsAnnotation) ? (i, options) => raw(i, mergeInternalOptions(options, parseOptionsAnnotation.value)) : raw;
  const decodingFallbackAnnotation = AST.getDecodingFallbackAnnotation(ast);
  const parser = isDecoding && Option.isSome(decodingFallbackAnnotation) ? (i, options) => handleForbidden(orElse(parserWithOptions(i, options), decodingFallbackAnnotation.value), ast, i, options) : parserWithOptions;
  memoMap.set(ast, parser);
  return parser;
};
const getConcurrency = ast => Option.getOrUndefined(AST.getConcurrencyAnnotation(ast));
const getBatching = ast => Option.getOrUndefined(AST.getBatchingAnnotation(ast));
const go = (ast, isDecoding) => {
  switch (ast._tag) {
    case "Refinement":
      {
        if (isDecoding) {
          const from = goMemo(ast.from, true);
          return (i, options) => {
            options = options ?? AST.defaultParseOption;
            const allErrors = options?.errors === "all";
            const result = flatMap(orElse(from(i, options), ef => {
              const issue = new Refinement(ast, i, "From", ef);
              if (allErrors && AST.hasStableFilter(ast)) {
                return Option.match(ast.filter(i, options, ast), {
                  onNone: () => Either.left(issue),
                  onSome: ep => Either.left(new Composite(ast, i, [issue, new Refinement(ast, i, "Predicate", ep)]))
                });
              }
              return Either.left(issue);
            }), a => Option.match(ast.filter(a, options, ast), {
              onNone: () => Either.right(a),
              onSome: ep => Either.left(new Refinement(ast, i, "Predicate", ep))
            }));
            return handleForbidden(result, ast, i, options);
          };
        } else {
          const from = goMemo(AST.typeAST(ast), true);
          const to = goMemo(dropRightRefinement(ast.from), false);
          return (i, options) => handleForbidden(flatMap(from(i, options), a => to(a, options)), ast, i, options);
        }
      }
    case "Transformation":
      {
        const transform = getFinalTransformation(ast.transformation, isDecoding);
        const from = isDecoding ? goMemo(ast.from, true) : goMemo(ast.to, false);
        const to = isDecoding ? goMemo(ast.to, true) : goMemo(ast.from, false);
        return (i, options) => handleForbidden(flatMap(mapError(from(i, options), e => new Transformation(ast, i, isDecoding ? "Encoded" : "Type", e)), a => flatMap(mapError(transform(a, options ?? AST.defaultParseOption, ast, i), e => new Transformation(ast, i, "Transformation", e)), i2 => mapError(to(i2, options), e => new Transformation(ast, i, isDecoding ? "Type" : "Encoded", e)))), ast, i, options);
      }
    case "Declaration":
      {
        const parse = isDecoding ? ast.decodeUnknown(...ast.typeParameters) : ast.encodeUnknown(...ast.typeParameters);
        return (i, options) => handleForbidden(parse(i, options ?? AST.defaultParseOption, ast), ast, i, options);
      }
    case "Literal":
      return fromRefinement(ast, u => u === ast.literal);
    case "UniqueSymbol":
      return fromRefinement(ast, u => u === ast.symbol);
    case "UndefinedKeyword":
      return fromRefinement(ast, Predicate.isUndefined);
    case "NeverKeyword":
      return fromRefinement(ast, Predicate.isNever);
    case "UnknownKeyword":
    case "AnyKeyword":
    case "VoidKeyword":
      return Either.right;
    case "StringKeyword":
      return fromRefinement(ast, Predicate.isString);
    case "NumberKeyword":
      return fromRefinement(ast, Predicate.isNumber);
    case "BooleanKeyword":
      return fromRefinement(ast, Predicate.isBoolean);
    case "BigIntKeyword":
      return fromRefinement(ast, Predicate.isBigInt);
    case "SymbolKeyword":
      return fromRefinement(ast, Predicate.isSymbol);
    case "ObjectKeyword":
      return fromRefinement(ast, Predicate.isObject);
    case "Enums":
      return fromRefinement(ast, u => ast.enums.some(([_, value]) => value === u));
    case "TemplateLiteral":
      {
        const regex = AST.getTemplateLiteralRegExp(ast);
        return fromRefinement(ast, u => Predicate.isString(u) && regex.test(u));
      }
    case "TupleType":
      {
        const elements = ast.elements.map(e => goMemo(e.type, isDecoding));
        const rest = ast.rest.map(annotatedAST => goMemo(annotatedAST.type, isDecoding));
        let requiredTypes = ast.elements.filter(e => !e.isOptional);
        if (ast.rest.length > 0) {
          requiredTypes = requiredTypes.concat(ast.rest.slice(1));
        }
        const requiredLen = requiredTypes.length;
        const expectedIndexes = ast.elements.length > 0 ? ast.elements.map((_, i) => i).join(" | ") : "never";
        const concurrency = getConcurrency(ast);
        const batching = getBatching(ast);
        return (input, options) => {
          if (!array_.isArray(input)) {
            return Either.left(new Type(ast, input));
          }
          const allErrors = options?.errors === "all";
          const es = [];
          let stepKey = 0;
          const output = [];
          // ---------------------------------------------
          // handle missing indexes
          // ---------------------------------------------
          const len = input.length;
          for (let i = len; i <= requiredLen - 1; i++) {
            const e = new Pointer(i, input, new Missing(requiredTypes[i - len]));
            if (allErrors) {
              es.push([stepKey++, e]);
              continue;
            } else {
              return Either.left(new Composite(ast, input, e, output));
            }
          }
          // ---------------------------------------------
          // handle excess indexes
          // ---------------------------------------------
          if (ast.rest.length === 0) {
            for (let i = ast.elements.length; i <= len - 1; i++) {
              const e = new Pointer(i, input, new Unexpected(input[i], `is unexpected, expected: ${expectedIndexes}`));
              if (allErrors) {
                es.push([stepKey++, e]);
                continue;
              } else {
                return Either.left(new Composite(ast, input, e, output));
              }
            }
          }
          let i = 0;
          let queue = undefined;
          // ---------------------------------------------
          // handle elements
          // ---------------------------------------------
          for (; i < elements.length; i++) {
            if (len < i + 1) {
              if (ast.elements[i].isOptional) {
                // the input element is missing
                continue;
              }
            } else {
              const parser = elements[i];
              const te = parser(input[i], options);
              const eu = eitherOrUndefined(te);
              if (eu) {
                if (Either.isLeft(eu)) {
                  // the input element is present but is not valid
                  const e = new Pointer(i, input, eu.left);
                  if (allErrors) {
                    es.push([stepKey++, e]);
                    continue;
                  } else {
                    return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                  }
                }
                output.push([stepKey++, eu.right]);
              } else {
                const nk = stepKey++;
                const index = i;
                if (!queue) {
                  queue = [];
                }
                queue.push(({
                  es,
                  output
                }) => Effect.flatMap(Effect.either(te), t => {
                  if (Either.isLeft(t)) {
                    // the input element is present but is not valid
                    const e = new Pointer(index, input, t.left);
                    if (allErrors) {
                      es.push([nk, e]);
                      return Effect.void;
                    } else {
                      return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                    }
                  }
                  output.push([nk, t.right]);
                  return Effect.void;
                }));
              }
            }
          }
          // ---------------------------------------------
          // handle rest element
          // ---------------------------------------------
          if (array_.isNonEmptyReadonlyArray(rest)) {
            const [head, ...tail] = rest;
            for (; i < len - tail.length; i++) {
              const te = head(input[i], options);
              const eu = eitherOrUndefined(te);
              if (eu) {
                if (Either.isLeft(eu)) {
                  const e = new Pointer(i, input, eu.left);
                  if (allErrors) {
                    es.push([stepKey++, e]);
                    continue;
                  } else {
                    return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                  }
                } else {
                  output.push([stepKey++, eu.right]);
                }
              } else {
                const nk = stepKey++;
                const index = i;
                if (!queue) {
                  queue = [];
                }
                queue.push(({
                  es,
                  output
                }) => Effect.flatMap(Effect.either(te), t => {
                  if (Either.isLeft(t)) {
                    const e = new Pointer(index, input, t.left);
                    if (allErrors) {
                      es.push([nk, e]);
                      return Effect.void;
                    } else {
                      return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                    }
                  } else {
                    output.push([nk, t.right]);
                    return Effect.void;
                  }
                }));
              }
            }
            // ---------------------------------------------
            // handle post rest elements
            // ---------------------------------------------
            for (let j = 0; j < tail.length; j++) {
              i += j;
              if (len < i + 1) {
                continue;
              } else {
                const te = tail[j](input[i], options);
                const eu = eitherOrUndefined(te);
                if (eu) {
                  if (Either.isLeft(eu)) {
                    // the input element is present but is not valid
                    const e = new Pointer(i, input, eu.left);
                    if (allErrors) {
                      es.push([stepKey++, e]);
                      continue;
                    } else {
                      return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                    }
                  }
                  output.push([stepKey++, eu.right]);
                } else {
                  const nk = stepKey++;
                  const index = i;
                  if (!queue) {
                    queue = [];
                  }
                  queue.push(({
                    es,
                    output
                  }) => Effect.flatMap(Effect.either(te), t => {
                    if (Either.isLeft(t)) {
                      // the input element is present but is not valid
                      const e = new Pointer(index, input, t.left);
                      if (allErrors) {
                        es.push([nk, e]);
                        return Effect.void;
                      } else {
                        return Either.left(new Composite(ast, input, e, sortByIndex(output)));
                      }
                    }
                    output.push([nk, t.right]);
                    return Effect.void;
                  }));
                }
              }
            }
          }
          // ---------------------------------------------
          // compute result
          // ---------------------------------------------
          const computeResult = ({
            es,
            output
          }) => array_.isNonEmptyArray(es) ? Either.left(new Composite(ast, input, sortByIndex(es), sortByIndex(output))) : Either.right(sortByIndex(output));
          if (queue && queue.length > 0) {
            const cqueue = queue;
            return Effect.suspend(() => {
              const state = {
                es: array_.copy(es),
                output: array_.copy(output)
              };
              return Effect.flatMap(Effect.forEach(cqueue, f => f(state), {
                concurrency,
                batching,
                discard: true
              }), () => computeResult(state));
            });
          }
          return computeResult({
            output,
            es
          });
        };
      }
    case "TypeLiteral":
      {
        if (ast.propertySignatures.length === 0 && ast.indexSignatures.length === 0) {
          return fromRefinement(ast, Predicate.isNotNullable);
        }
        const propertySignatures = [];
        const expectedKeysMap = {};
        const expectedKeys = [];
        for (const ps of ast.propertySignatures) {
          propertySignatures.push([goMemo(ps.type, isDecoding), ps]);
          expectedKeysMap[ps.name] = null;
          expectedKeys.push(ps.name);
        }
        const indexSignatures = ast.indexSignatures.map(is => [goMemo(is.parameter, isDecoding), goMemo(is.type, isDecoding), is.parameter]);
        const expectedAST = AST.Union.make(ast.indexSignatures.map(is => is.parameter).concat(expectedKeys.map(key => Predicate.isSymbol(key) ? new AST.UniqueSymbol(key) : new AST.Literal(key))));
        const expected = goMemo(expectedAST, isDecoding);
        const concurrency = getConcurrency(ast);
        const batching = getBatching(ast);
        return (input, options) => {
          if (!Predicate.isRecord(input)) {
            return Either.left(new Type(ast, input));
          }
          const allErrors = options?.errors === "all";
          const es = [];
          let stepKey = 0;
          // ---------------------------------------------
          // handle excess properties
          // ---------------------------------------------
          const onExcessPropertyError = options?.onExcessProperty === "error";
          const onExcessPropertyPreserve = options?.onExcessProperty === "preserve";
          const output = {};
          let inputKeys;
          if (onExcessPropertyError || onExcessPropertyPreserve) {
            inputKeys = util_.ownKeys(input);
            for (const key of inputKeys) {
              const eu = eitherOrUndefined(expected(key, options));
              if (Either.isLeft(eu)) {
                // key is unexpected
                if (onExcessPropertyError) {
                  const e = new Pointer(key, input, new Unexpected(input[key], `is unexpected, expected: ${String(expectedAST)}`));
                  if (allErrors) {
                    es.push([stepKey++, e]);
                    continue;
                  } else {
                    return Either.left(new Composite(ast, input, e, output));
                  }
                } else {
                  // preserve key
                  output[key] = input[key];
                }
              }
            }
          }
          let queue = undefined;
          const isExact = options?.exact === true;
          for (let i = 0; i < propertySignatures.length; i++) {
            const ps = propertySignatures[i][1];
            const name = ps.name;
            const hasKey = Object.prototype.hasOwnProperty.call(input, name);
            if (!hasKey) {
              if (ps.isOptional) {
                continue;
              } else if (isExact) {
                const e = new Pointer(name, input, new Missing(ps));
                if (allErrors) {
                  es.push([stepKey++, e]);
                  continue;
                } else {
                  return Either.left(new Composite(ast, input, e, output));
                }
              }
            }
            const parser = propertySignatures[i][0];
            const te = parser(input[name], options);
            const eu = eitherOrUndefined(te);
            if (eu) {
              if (Either.isLeft(eu)) {
                const e = new Pointer(name, input, hasKey ? eu.left : new Missing(ps));
                if (allErrors) {
                  es.push([stepKey++, e]);
                  continue;
                } else {
                  return Either.left(new Composite(ast, input, e, output));
                }
              }
              output[name] = eu.right;
            } else {
              const nk = stepKey++;
              const index = name;
              if (!queue) {
                queue = [];
              }
              queue.push(({
                es,
                output
              }) => Effect.flatMap(Effect.either(te), t => {
                if (Either.isLeft(t)) {
                  const e = new Pointer(index, input, hasKey ? t.left : new Missing(ps));
                  if (allErrors) {
                    es.push([nk, e]);
                    return Effect.void;
                  } else {
                    return Either.left(new Composite(ast, input, e, output));
                  }
                }
                output[index] = t.right;
                return Effect.void;
              }));
            }
          }
          // ---------------------------------------------
          // handle index signatures
          // ---------------------------------------------
          for (let i = 0; i < indexSignatures.length; i++) {
            const indexSignature = indexSignatures[i];
            const parameter = indexSignature[0];
            const type = indexSignature[1];
            const keys = util_.getKeysForIndexSignature(input, indexSignature[2]);
            for (const key of keys) {
              // ---------------------------------------------
              // handle keys
              // ---------------------------------------------
              const keu = eitherOrUndefined(parameter(key, options));
              if (keu && Either.isRight(keu)) {
                // ---------------------------------------------
                // handle values
                // ---------------------------------------------
                const vpr = type(input[key], options);
                const veu = eitherOrUndefined(vpr);
                if (veu) {
                  if (Either.isLeft(veu)) {
                    const e = new Pointer(key, input, veu.left);
                    if (allErrors) {
                      es.push([stepKey++, e]);
                      continue;
                    } else {
                      return Either.left(new Composite(ast, input, e, output));
                    }
                  } else {
                    if (!Object.prototype.hasOwnProperty.call(expectedKeysMap, key)) {
                      output[key] = veu.right;
                    }
                  }
                } else {
                  const nk = stepKey++;
                  const index = key;
                  if (!queue) {
                    queue = [];
                  }
                  queue.push(({
                    es,
                    output
                  }) => Effect.flatMap(Effect.either(vpr), tv => {
                    if (Either.isLeft(tv)) {
                      const e = new Pointer(index, input, tv.left);
                      if (allErrors) {
                        es.push([nk, e]);
                        return Effect.void;
                      } else {
                        return Either.left(new Composite(ast, input, e, output));
                      }
                    } else {
                      if (!Object.prototype.hasOwnProperty.call(expectedKeysMap, key)) {
                        output[key] = tv.right;
                      }
                      return Effect.void;
                    }
                  }));
                }
              }
            }
          }
          // ---------------------------------------------
          // compute result
          // ---------------------------------------------
          const computeResult = ({
            es,
            output
          }) => {
            if (array_.isNonEmptyArray(es)) {
              return Either.left(new Composite(ast, input, sortByIndex(es), output));
            }
            if (options?.propertyOrder === "original") {
              // preserve input keys order
              const keys = inputKeys || util_.ownKeys(input);
              for (const name of expectedKeys) {
                if (keys.indexOf(name) === -1) {
                  keys.push(name);
                }
              }
              const out = {};
              for (const key of keys) {
                if (Object.prototype.hasOwnProperty.call(output, key)) {
                  out[key] = output[key];
                }
              }
              return Either.right(out);
            }
            return Either.right(output);
          };
          if (queue && queue.length > 0) {
            const cqueue = queue;
            return Effect.suspend(() => {
              const state = {
                es: array_.copy(es),
                output: Object.assign({}, output)
              };
              return Effect.flatMap(Effect.forEach(cqueue, f => f(state), {
                concurrency,
                batching,
                discard: true
              }), () => computeResult(state));
            });
          }
          return computeResult({
            es,
            output
          });
        };
      }
    case "Union":
      {
        const searchTree = getSearchTree(ast.types, isDecoding);
        const ownKeys = util_.ownKeys(searchTree.keys);
        const len = ownKeys.length;
        const map = new Map();
        for (let i = 0; i < ast.types.length; i++) {
          map.set(ast.types[i], goMemo(ast.types[i], isDecoding));
        }
        const concurrency = getConcurrency(ast) ?? 1;
        const batching = getBatching(ast);
        return (input, options) => {
          const es = [];
          let stepKey = 0;
          let candidates = [];
          if (len > 0) {
            if (Predicate.isRecordOrArray(input)) {
              for (let i = 0; i < len; i++) {
                const name = ownKeys[i];
                const buckets = searchTree.keys[name].buckets;
                // for each property that should contain a literal, check if the input contains that property
                if (Object.prototype.hasOwnProperty.call(input, name)) {
                  const literal = String(input[name]);
                  // check that the value obtained from the input for the property corresponds to an existing bucket
                  if (Object.prototype.hasOwnProperty.call(buckets, literal)) {
                    // retrive the minimal set of candidates for decoding
                    candidates = candidates.concat(buckets[literal]);
                  } else {
                    const literals = AST.Union.make(searchTree.keys[name].literals);
                    es.push([stepKey++, new Composite(new AST.TypeLiteral([new AST.PropertySignature(name, literals, false, true)], []), input, new Pointer(name, input, new Type(literals, input[name])))]);
                  }
                } else {
                  const literals = AST.Union.make(searchTree.keys[name].literals);
                  const fakeps = new AST.PropertySignature(name, literals, false, true);
                  es.push([stepKey++, new Composite(new AST.TypeLiteral([fakeps], []), input, new Pointer(name, input, new Missing(fakeps)))]);
                }
              }
            } else {
              es.push([stepKey++, new Type(ast, input)]);
            }
          }
          if (searchTree.otherwise.length > 0) {
            candidates = candidates.concat(searchTree.otherwise);
          }
          let queue = undefined;
          for (let i = 0; i < candidates.length; i++) {
            const candidate = candidates[i];
            const pr = map.get(candidate)(input, options);
            // the members of a union are ordered based on which one should be decoded first,
            // therefore if one member has added a task, all subsequent members must
            // also add a task to the queue even if they are synchronous
            const eu = !queue || queue.length === 0 ? eitherOrUndefined(pr) : undefined;
            if (eu) {
              if (Either.isRight(eu)) {
                return eu;
              } else {
                es.push([stepKey++, eu.left]);
              }
            } else {
              const nk = stepKey++;
              if (!queue) {
                queue = [];
              }
              queue.push(state => Effect.suspend(() => {
                if ("finalResult" in state) {
                  return Effect.void;
                } else {
                  return Effect.flatMap(Effect.either(pr), t => {
                    if (Either.isRight(t)) {
                      state.finalResult = t;
                    } else {
                      state.es.push([nk, t.left]);
                    }
                    return Effect.void;
                  });
                }
              }));
            }
          }
          // ---------------------------------------------
          // compute result
          // ---------------------------------------------
          const computeResult = es => array_.isNonEmptyArray(es) ? es.length === 1 && es[0][1]._tag === "Type" ? Either.left(es[0][1]) : Either.left(new Composite(ast, input, sortByIndex(es))) :
          // this should never happen
          Either.left(new Type(ast, input));
          if (queue && queue.length > 0) {
            const cqueue = queue;
            return Effect.suspend(() => {
              const state = {
                es: array_.copy(es)
              };
              return Effect.flatMap(Effect.forEach(cqueue, f => f(state), {
                concurrency,
                batching,
                discard: true
              }), () => {
                if ("finalResult" in state) {
                  return state.finalResult;
                }
                return computeResult(state.es);
              });
            });
          }
          return computeResult(es);
        };
      }
    case "Suspend":
      {
        const get = util_.memoizeThunk(() => goMemo(AST.annotations(ast.f(), ast.annotations), isDecoding));
        return (a, options) => get()(a, options);
      }
  }
};
const fromRefinement = (ast, refinement) => u => refinement(u) ? Either.right(u) : Either.left(new Type(ast, u));
/** @internal */
export const getLiterals = (ast, isDecoding) => {
  switch (ast._tag) {
    case "Declaration":
      {
        const annotation = AST.getSurrogateAnnotation(ast);
        if (Option.isSome(annotation)) {
          return getLiterals(annotation.value, isDecoding);
        }
        break;
      }
    case "TypeLiteral":
      {
        const out = [];
        for (let i = 0; i < ast.propertySignatures.length; i++) {
          const propertySignature = ast.propertySignatures[i];
          const type = isDecoding ? AST.encodedAST(propertySignature.type) : AST.typeAST(propertySignature.type);
          if (AST.isLiteral(type) && !propertySignature.isOptional) {
            out.push([propertySignature.name, type]);
          }
        }
        return out;
      }
    case "TupleType":
      {
        const out = [];
        for (let i = 0; i < ast.elements.length; i++) {
          const element = ast.elements[i];
          const type = isDecoding ? AST.encodedAST(element.type) : AST.typeAST(element.type);
          if (AST.isLiteral(type) && !element.isOptional) {
            out.push([i, type]);
          }
        }
        return out;
      }
    case "Refinement":
      return getLiterals(ast.from, isDecoding);
    case "Suspend":
      return getLiterals(ast.f(), isDecoding);
    case "Transformation":
      return getLiterals(isDecoding ? ast.from : ast.to, isDecoding);
  }
  return [];
};
/**
 * The purpose of the algorithm is to narrow down the pool of possible candidates for decoding as much as possible.
 *
 * This function separates the schemas into two groups, `keys` and `otherwise`:
 *
 * - `keys`: the schema has at least one property with a literal value
 * - `otherwise`: the schema has no properties with a literal value
 *
 * If a schema has at least one property with a literal value, so it ends up in `keys`, first a namespace is created for
 * the name of the property containing the literal, and then within this namespace a "bucket" is created for the literal
 * value in which to store all the schemas that have the same property and literal value.
 *
 * @internal
 */
export const getSearchTree = (members, isDecoding) => {
  const keys = {};
  const otherwise = [];
  for (let i = 0; i < members.length; i++) {
    const member = members[i];
    const tags = getLiterals(member, isDecoding);
    if (tags.length > 0) {
      for (let j = 0; j < tags.length; j++) {
        const [key, literal] = tags[j];
        const hash = String(literal.literal);
        keys[key] = keys[key] || {
          buckets: {},
          literals: []
        };
        const buckets = keys[key].buckets;
        if (Object.prototype.hasOwnProperty.call(buckets, hash)) {
          if (j < tags.length - 1) {
            continue;
          }
          buckets[hash].push(member);
          keys[key].literals.push(literal);
        } else {
          buckets[hash] = [member];
          keys[key].literals.push(literal);
          break;
        }
      }
    } else {
      otherwise.push(member);
    }
  }
  return {
    keys,
    otherwise
  };
};
const dropRightRefinement = ast => AST.isRefinement(ast) ? dropRightRefinement(ast.from) : ast;
const handleForbidden = (effect, ast, actual, options) => {
  const eu = eitherOrUndefined(effect);
  if (eu) {
    return eu;
  }
  if (options?.isEffectAllowed === true) {
    return effect;
  }
  try {
    return Effect.runSync(Effect.either(effect));
  } catch (e) {
    return Either.left(new Forbidden(ast, actual, "cannot be be resolved synchronously, this is caused by using runSync on an effect that performs async work"));
  }
};
const compare = ([a], [b]) => a > b ? 1 : a < b ? -1 : 0;
function sortByIndex(es) {
  return es.sort(compare).map(t => t[1]);
}
// -------------------------------------------------------------------------------------
// transformations interpreter
// -------------------------------------------------------------------------------------
/** @internal */
export const getFinalTransformation = (transformation, isDecoding) => {
  switch (transformation._tag) {
    case "FinalTransformation":
      return isDecoding ? transformation.decode : transformation.encode;
    case "ComposeTransformation":
      return Either.right;
    case "TypeLiteralTransformation":
      return input => {
        let out = Either.right(input);
        // ---------------------------------------------
        // handle property signature transformations
        // ---------------------------------------------
        for (const pst of transformation.propertySignatureTransformations) {
          const [from, to] = isDecoding ? [pst.from, pst.to] : [pst.to, pst.from];
          const transformation = isDecoding ? pst.decode : pst.encode;
          const f = input => {
            const o = transformation(Object.prototype.hasOwnProperty.call(input, from) ? Option.some(input[from]) : Option.none());
            delete input[from];
            if (Option.isSome(o)) {
              input[to] = o.value;
            }
            return input;
          };
          out = map(out, f);
        }
        return out;
      };
  }
};
const makeTree = (value, forest = []) => ({
  value,
  forest
});
/**
 * @category formatting
 * @since 3.10.0
 */
export const TreeFormatter = {
  formatIssue: issue => Effect.map(formatTree(issue), drawTree),
  formatIssueSync: issue => Effect.runSync(TreeFormatter.formatIssue(issue)),
  formatError: error => TreeFormatter.formatIssue(error.issue),
  formatErrorSync: error => TreeFormatter.formatIssueSync(error.issue)
};
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
/**
 * Returns `true` if the value is a `Composite`.
 *
 * @category guards
 * @since 3.10.0
 */
export const isComposite = /*#__PURE__*/createParseIssueGuard("Composite");
const isRefinement = /*#__PURE__*/createParseIssueGuard("Refinement");
const isTransformation = /*#__PURE__*/createParseIssueGuard("Transformation");
const getMessage = issue => getCurrentMessage(issue).pipe(Effect.flatMap(currentMessage => {
  const useInnerMessage = !currentMessage.override && (isComposite(issue) || isRefinement(issue) && issue.kind === "From" || isTransformation(issue) && issue.kind !== "Transformation");
  return useInnerMessage ? isTransformation(issue) || isRefinement(issue) ? getMessage(issue.issue) : Option.none() : Effect.succeed(currentMessage.message);
}));
const getParseIssueTitleAnnotation = issue => getAnnotated(issue).pipe(Option.flatMap(AST.getParseIssueTitleAnnotation), Option.filterMap(annotation => Option.fromNullable(annotation(issue))));
const formatTypeMessage = e => getMessage(e).pipe(Effect.orElse(() => getParseIssueTitleAnnotation(e)), Effect.catchAll(() => Effect.succeed(e.message ?? `Expected ${String(e.ast)}, actual ${util_.formatUnknown(e.actual)}`)));
const getParseIssueTitle = issue => Option.getOrElse(getParseIssueTitleAnnotation(issue), () => String(issue.ast));
const formatForbiddenMessage = e => e.message ?? "is forbidden";
const formatUnexpectedMessage = e => e.message ?? "is unexpected";
const formatMissingMessage = e => AST.getMissingMessageAnnotation(e.ast).pipe(Effect.flatMap(annotation => {
  const out = annotation();
  return Predicate.isString(out) ? Effect.succeed(out) : out;
}), Effect.catchAll(() => Effect.succeed(e.message ?? "is missing")));
const getTree = (issue, onFailure) => Effect.matchEffect(getMessage(issue), {
  onFailure,
  onSuccess: message => Effect.succeed(makeTree(message))
});
const formatTree = e => {
  switch (e._tag) {
    case "Type":
      return Effect.map(formatTypeMessage(e), makeTree);
    case "Forbidden":
      return Effect.succeed(makeTree(getParseIssueTitle(e), [makeTree(formatForbiddenMessage(e))]));
    case "Unexpected":
      return Effect.succeed(makeTree(formatUnexpectedMessage(e)));
    case "Missing":
      return Effect.map(formatMissingMessage(e), makeTree);
    case "Transformation":
      return getTree(e, () => Effect.map(formatTree(e.issue), tree => makeTree(getParseIssueTitle(e), [makeTree(formatTransformationKind(e.kind), [tree])])));
    case "Refinement":
      return getTree(e, () => Effect.map(formatTree(e.issue), tree => makeTree(getParseIssueTitle(e), [makeTree(formatRefinementKind(e.kind), [tree])])));
    case "Pointer":
      return Effect.map(formatTree(e.issue), tree => makeTree(util_.formatPath(e.path), [tree]));
    case "Composite":
      {
        const parseIssueTitle = getParseIssueTitle(e);
        return getTree(e, () => util_.isNonEmpty(e.issues) ? Effect.map(Effect.forEach(e.issues, formatTree), forest => makeTree(parseIssueTitle, forest)) : Effect.map(formatTree(e.issues), tree => makeTree(parseIssueTitle, [tree])));
      }
  }
};
/**
 * @category formatting
 * @since 3.10.0
 */
export const ArrayFormatter = {
  formatIssue: issue => formatArray(issue),
  formatIssueSync: issue => Effect.runSync(ArrayFormatter.formatIssue(issue)),
  formatError: error => ArrayFormatter.formatIssue(error.issue),
  formatErrorSync: error => ArrayFormatter.formatIssueSync(error.issue)
};
const succeedArrayFormatterIssue = issue => Effect.succeed([issue]);
const getArray = (issue, path, onFailure) => Effect.matchEffect(getMessage(issue), {
  onFailure,
  onSuccess: message => succeedArrayFormatterIssue({
    _tag: issue._tag,
    path,
    message
  })
});
const formatArray = (e, path = []) => {
  const _tag = e._tag;
  switch (_tag) {
    case "Type":
      return Effect.map(formatTypeMessage(e), message => [{
        _tag,
        path,
        message
      }]);
    case "Forbidden":
      return succeedArrayFormatterIssue({
        _tag,
        path,
        message: formatForbiddenMessage(e)
      });
    case "Unexpected":
      return succeedArrayFormatterIssue({
        _tag,
        path,
        message: formatUnexpectedMessage(e)
      });
    case "Missing":
      return Effect.map(formatMissingMessage(e), message => [{
        _tag,
        path,
        message
      }]);
    case "Pointer":
      return formatArray(e.issue, path.concat(e.path));
    case "Composite":
      return getArray(e, path, () => util_.isNonEmpty(e.issues) ? Effect.map(Effect.forEach(e.issues, issue => formatArray(issue, path)), array_.flatten) : formatArray(e.issues, path));
    case "Refinement":
    case "Transformation":
      return getArray(e, path, () => formatArray(e.issue, path));
  }
};
//# sourceMappingURL=ParseResult.js.map