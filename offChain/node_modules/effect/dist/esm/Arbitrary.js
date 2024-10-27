/**
 * @since 3.10.0
 */
import * as Arr from "./Array.js";
import * as FastCheck from "./FastCheck.js";
import * as errors_ from "./internal/schema/errors.js";
import * as filters_ from "./internal/schema/filters.js";
import * as util_ from "./internal/schema/util.js";
import * as Option from "./Option.js";
import * as Predicate from "./Predicate.js";
import * as AST from "./SchemaAST.js";
/**
 * Returns a LazyArbitrary for the `A` type of the provided schema.
 *
 * @category arbitrary
 * @since 3.10.0
 */
export const makeLazy = schema => go(schema.ast, {
  maxDepth: 2
}, []);
/**
 * Returns a fast-check Arbitrary for the `A` type of the provided schema.
 *
 * @category arbitrary
 * @since 3.10.0
 */
export const make = schema => makeLazy(schema)(FastCheck);
const getArbitraryAnnotation = /*#__PURE__*/AST.getAnnotation(AST.ArbitraryAnnotationId);
const getRefinementFromArbitrary = (ast, ctx, path) => {
  const constraints = combineConstraints(ctx.constraints, getConstraints(ast));
  return go(ast.from, constraints ? {
    ...ctx,
    constraints
  } : ctx, path);
};
const getSuspendedContext = (ctx, ast) => {
  if (ctx.depthIdentifier !== undefined) {
    return ctx;
  }
  const depthIdentifier = AST.getIdentifierAnnotation(ast).pipe(Option.orElse(() => AST.getIdentifierAnnotation(ast.f())), Option.getOrElse(() => "SuspendDefaultDepthIdentifier"));
  return {
    ...ctx,
    depthIdentifier
  };
};
const getSuspendedArray = (fc, depthIdentifier, maxDepth, item, constraints) => {
  let minLength = 1;
  let maxLength = 2;
  if (constraints && constraints.minLength !== undefined && constraints.minLength > minLength) {
    minLength = constraints.minLength;
    if (minLength > maxLength) {
      maxLength = minLength;
    }
  }
  return fc.oneof({
    maxDepth,
    depthIdentifier
  }, fc.constant([]), fc.array(item, {
    minLength,
    maxLength
  }));
};
const go = (ast, ctx, path) => {
  const hook = getArbitraryAnnotation(ast);
  if (Option.isSome(hook)) {
    switch (ast._tag) {
      case "Declaration":
        return hook.value(...ast.typeParameters.map(p => go(p, ctx, path)), ctx);
      case "Refinement":
        return hook.value(getRefinementFromArbitrary(ast, ctx, path), ctx);
      default:
        return hook.value(ctx);
    }
  }
  switch (ast._tag) {
    case "Declaration":
      {
        throw new Error(errors_.getArbitraryMissingAnnotationErrorMessage(path, ast));
      }
    case "Literal":
      return fc => fc.constant(ast.literal);
    case "UniqueSymbol":
      return fc => fc.constant(ast.symbol);
    case "UndefinedKeyword":
      return fc => fc.constant(undefined);
    case "NeverKeyword":
      return () => {
        throw new Error(errors_.getArbitraryUnsupportedErrorMessage(path, ast));
      };
    case "UnknownKeyword":
    case "AnyKeyword":
    case "VoidKeyword":
      return fc => fc.anything();
    case "StringKeyword":
      return fc => {
        if (ctx.constraints) {
          switch (ctx.constraints._tag) {
            case "StringConstraints":
              return fc.string(ctx.constraints.constraints);
          }
        }
        return fc.string();
      };
    case "NumberKeyword":
      return fc => {
        if (ctx.constraints) {
          switch (ctx.constraints._tag) {
            case "NumberConstraints":
              return fc.float(ctx.constraints.constraints);
            case "IntegerConstraints":
              return fc.integer(ctx.constraints.constraints);
          }
        }
        return fc.float();
      };
    case "BooleanKeyword":
      return fc => fc.boolean();
    case "BigIntKeyword":
      return fc => {
        if (ctx.constraints) {
          switch (ctx.constraints._tag) {
            case "BigIntConstraints":
              return fc.bigInt(ctx.constraints.constraints);
          }
        }
        return fc.bigInt();
      };
    case "SymbolKeyword":
      return fc => fc.string().map(s => Symbol.for(s));
    case "ObjectKeyword":
      return fc => fc.oneof(fc.object(), fc.array(fc.anything()));
    case "TemplateLiteral":
      {
        return fc => {
          const string = fc.string({
            maxLength: 5
          });
          const number = fc.float({
            noDefaultInfinity: true
          }).filter(n => !Number.isNaN(n));
          const components = [fc.constant(ast.head)];
          for (const span of ast.spans) {
            if (AST.isStringKeyword(span.type)) {
              components.push(string);
            } else {
              components.push(number);
            }
            components.push(fc.constant(span.literal));
          }
          return fc.tuple(...components).map(spans => spans.join(""));
        };
      }
    case "TupleType":
      {
        const elements = [];
        let hasOptionals = false;
        let i = 0;
        for (const element of ast.elements) {
          elements.push(go(element.type, ctx, path.concat(i++)));
          if (element.isOptional) {
            hasOptionals = true;
          }
        }
        const rest = ast.rest.map(annotatedAST => go(annotatedAST.type, ctx, path));
        return fc => {
          // ---------------------------------------------
          // handle elements
          // ---------------------------------------------
          let output = fc.tuple(...elements.map(arb => arb(fc)));
          if (hasOptionals) {
            const indexes = fc.tuple(...ast.elements.map(element => element.isOptional ? fc.boolean() : fc.constant(true)));
            output = output.chain(tuple => indexes.map(booleans => {
              for (const [i, b] of booleans.reverse().entries()) {
                if (!b) {
                  tuple.splice(booleans.length - i, 1);
                }
              }
              return tuple;
            }));
          }
          // ---------------------------------------------
          // handle rest element
          // ---------------------------------------------
          if (Arr.isNonEmptyReadonlyArray(rest)) {
            const [head, ...tail] = rest;
            const item = head(fc);
            const constraints = ctx.constraints && ctx.constraints._tag === "ArrayConstraints" ? ctx.constraints.constraints : undefined;
            output = output.chain(as => {
              return (ctx.depthIdentifier !== undefined ? getSuspendedArray(fc, ctx.depthIdentifier, ctx.maxDepth, item, constraints) : fc.array(item, constraints)).map(rest => [...as, ...rest]);
            });
            // ---------------------------------------------
            // handle post rest elements
            // ---------------------------------------------
            for (let j = 0; j < tail.length; j++) {
              output = output.chain(as => tail[j](fc).map(a => [...as, a]));
            }
          }
          return output;
        };
      }
    case "TypeLiteral":
      {
        const propertySignaturesTypes = ast.propertySignatures.map(ps => go(ps.type, ctx, path.concat(ps.name)));
        const indexSignatures = ast.indexSignatures.map(is => [go(is.parameter, ctx, path), go(is.type, ctx, path)]);
        return fc => {
          const arbs = {};
          const requiredKeys = [];
          // ---------------------------------------------
          // handle property signatures
          // ---------------------------------------------
          for (let i = 0; i < propertySignaturesTypes.length; i++) {
            const ps = ast.propertySignatures[i];
            const name = ps.name;
            if (!ps.isOptional) {
              requiredKeys.push(name);
            }
            arbs[name] = propertySignaturesTypes[i](fc);
          }
          let output = fc.record(arbs, {
            requiredKeys
          });
          // ---------------------------------------------
          // handle index signatures
          // ---------------------------------------------
          for (let i = 0; i < indexSignatures.length; i++) {
            const key = indexSignatures[i][0](fc);
            const value = indexSignatures[i][1](fc);
            output = output.chain(o => {
              const item = fc.tuple(key, value);
              const arr = ctx.depthIdentifier !== undefined ? getSuspendedArray(fc, ctx.depthIdentifier, ctx.maxDepth, item) : fc.array(item);
              return arr.map(tuples => ({
                ...Object.fromEntries(tuples),
                ...o
              }));
            });
          }
          return output;
        };
      }
    case "Union":
      {
        const types = ast.types.map(member => go(member, ctx, path));
        return fc => fc.oneof(...types.map(arb => arb(fc)));
      }
    case "Enums":
      {
        if (ast.enums.length === 0) {
          throw new Error(errors_.getArbitraryEmptyEnumErrorMessage(path));
        }
        return fc => fc.oneof(...ast.enums.map(([_, value]) => fc.constant(value)));
      }
    case "Refinement":
      {
        const from = getRefinementFromArbitrary(ast, ctx, path);
        return fc => from(fc).filter(a => Option.isNone(ast.filter(a, AST.defaultParseOption, ast)));
      }
    case "Suspend":
      {
        const get = util_.memoizeThunk(() => {
          return go(ast.f(), getSuspendedContext(ctx, ast), path);
        });
        return fc => fc.constant(null).chain(() => get()(fc));
      }
    case "Transformation":
      return go(ast.to, ctx, path);
  }
};
/** @internal */
export class NumberConstraints {
  _tag = "NumberConstraints";
  constraints;
  constructor(options) {
    this.constraints = {};
    if (Predicate.isNumber(options.min)) {
      this.constraints.min = Math.fround(options.min);
    }
    if (Predicate.isNumber(options.max)) {
      this.constraints.max = Math.fround(options.max);
    }
    if (Predicate.isBoolean(options.noNaN)) {
      this.constraints.noNaN = options.noNaN;
    }
    if (Predicate.isBoolean(options.noDefaultInfinity)) {
      this.constraints.noDefaultInfinity = options.noDefaultInfinity;
    }
  }
}
/** @internal */
export class StringConstraints {
  _tag = "StringConstraints";
  constraints;
  constructor(options) {
    this.constraints = {};
    if (Predicate.isNumber(options.minLength)) {
      this.constraints.minLength = options.minLength;
    }
    if (Predicate.isNumber(options.maxLength)) {
      this.constraints.maxLength = options.maxLength;
    }
  }
}
/** @internal */
export class IntegerConstraints {
  _tag = "IntegerConstraints";
  constraints;
  constructor(options) {
    this.constraints = {};
    if (Predicate.isNumber(options.min)) {
      this.constraints.min = options.min;
    }
    if (Predicate.isNumber(options.max)) {
      this.constraints.max = options.max;
    }
  }
}
/** @internal */
export class ArrayConstraints {
  _tag = "ArrayConstraints";
  constraints;
  constructor(options) {
    this.constraints = {};
    if (Predicate.isNumber(options.minLength)) {
      this.constraints.minLength = options.minLength;
    }
    if (Predicate.isNumber(options.maxLength)) {
      this.constraints.maxLength = options.maxLength;
    }
  }
}
/** @internal */
export class BigIntConstraints {
  _tag = "BigIntConstraints";
  constraints;
  constructor(options) {
    this.constraints = {};
    if (Predicate.isBigInt(options.min)) {
      this.constraints.min = options.min;
    }
    if (Predicate.isBigInt(options.max)) {
      this.constraints.max = options.max;
    }
  }
}
/** @internal */
export const getConstraints = ast => {
  const TypeAnnotationId = ast.annotations[AST.SchemaIdAnnotationId];
  const jsonSchema = ast.annotations[AST.JSONSchemaAnnotationId];
  switch (TypeAnnotationId) {
    // int
    case filters_.IntSchemaId:
      return new IntegerConstraints({});
    // number
    case filters_.GreaterThanSchemaId:
    case filters_.GreaterThanOrEqualToSchemaId:
    case filters_.LessThanSchemaId:
    case filters_.LessThanOrEqualToSchemaId:
    case filters_.BetweenSchemaId:
      return new NumberConstraints({
        min: jsonSchema.exclusiveMinimum ?? jsonSchema.minimum,
        max: jsonSchema.exclusiveMaximum ?? jsonSchema.maximum
      });
    // bigint
    case filters_.GreaterThanBigintSchemaId:
    case filters_.GreaterThanOrEqualToBigIntSchemaId:
    case filters_.LessThanBigIntSchemaId:
    case filters_.LessThanOrEqualToBigIntSchemaId:
    case filters_.BetweenBigintSchemaId:
      {
        const constraints = ast.annotations[TypeAnnotationId];
        return new BigIntConstraints(constraints);
      }
    // string
    case filters_.MinLengthSchemaId:
    case filters_.MaxLengthSchemaId:
    case filters_.LengthSchemaId:
      return new StringConstraints(jsonSchema);
    // array
    case filters_.MinItemsSchemaId:
    case filters_.MaxItemsSchemaId:
    case filters_.ItemsCountSchemaId:
      return new ArrayConstraints({
        minLength: jsonSchema.minItems,
        maxLength: jsonSchema.maxItems
      });
  }
};
/** @internal */
export const combineConstraints = (c1, c2) => {
  if (c1 === undefined) {
    return c2;
  }
  if (c2 === undefined) {
    return c1;
  }
  switch (c1._tag) {
    case "ArrayConstraints":
      {
        switch (c2._tag) {
          case "ArrayConstraints":
            return new ArrayConstraints({
              minLength: getMax(c1.constraints.minLength, c2.constraints.minLength),
              maxLength: getMin(c1.constraints.maxLength, c2.constraints.maxLength)
            });
        }
        break;
      }
    case "NumberConstraints":
      {
        switch (c2._tag) {
          case "NumberConstraints":
            return new NumberConstraints({
              min: getMax(c1.constraints.min, c2.constraints.min),
              max: getMin(c1.constraints.max, c2.constraints.max),
              noNaN: getOr(c1.constraints.noNaN, c2.constraints.noNaN),
              noDefaultInfinity: getOr(c1.constraints.noDefaultInfinity, c2.constraints.noDefaultInfinity)
            });
          case "IntegerConstraints":
            return new IntegerConstraints({
              min: getMax(c1.constraints.min, c2.constraints.min),
              max: getMin(c1.constraints.max, c2.constraints.max)
            });
        }
        break;
      }
    case "BigIntConstraints":
      {
        switch (c2._tag) {
          case "BigIntConstraints":
            return new BigIntConstraints({
              min: getMax(c1.constraints.min, c2.constraints.min),
              max: getMin(c1.constraints.max, c2.constraints.max)
            });
        }
        break;
      }
    case "StringConstraints":
      {
        switch (c2._tag) {
          case "StringConstraints":
            return new StringConstraints({
              minLength: getMax(c1.constraints.minLength, c2.constraints.minLength),
              maxLength: getMin(c1.constraints.maxLength, c2.constraints.maxLength)
            });
        }
        break;
      }
    case "IntegerConstraints":
      {
        switch (c2._tag) {
          case "NumberConstraints":
          case "IntegerConstraints":
            {
              return new IntegerConstraints({
                min: getMax(c1.constraints.min, c2.constraints.min),
                max: getMin(c1.constraints.max, c2.constraints.max)
              });
            }
        }
        break;
      }
  }
};
const getOr = (a, b) => {
  return a === undefined ? b : b === undefined ? a : a || b;
};
function getMax(n1, n2) {
  return n1 === undefined ? n2 : n2 === undefined ? n1 : n1 <= n2 ? n2 : n1;
}
function getMin(n1, n2) {
  return n1 === undefined ? n2 : n2 === undefined ? n1 : n1 <= n2 ? n1 : n2;
}
//# sourceMappingURL=Arbitrary.js.map