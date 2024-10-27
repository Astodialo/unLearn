"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.pretty = exports.match = exports.make = exports.PrettyHookId = void 0;
var Arr = _interopRequireWildcard(require("effect/Array"));
var Option = _interopRequireWildcard(require("effect/Option"));
var AST = _interopRequireWildcard(require("./AST.js"));
var errors_ = _interopRequireWildcard(require("./internal/errors.js"));
var util_ = _interopRequireWildcard(require("./internal/util.js"));
var ParseResult = _interopRequireWildcard(require("./ParseResult.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 0.67.0
 */

/**
 * @category hooks
 * @since 0.67.0
 */
const PrettyHookId = exports.PrettyHookId = /*#__PURE__*/Symbol.for("@effect/schema/PrettyHookId");
/**
 * @category annotations
 * @since 0.67.0
 */
const pretty = handler => self => self.annotations({
  [PrettyHookId]: handler
});
/**
 * @category prettify
 * @since 0.67.0
 */
exports.pretty = pretty;
const make = schema => compile(schema.ast, []);
exports.make = make;
const getHook = /*#__PURE__*/AST.getAnnotation(PrettyHookId);
const getMatcher = defaultPretty => ast => Option.match(getHook(ast), {
  onNone: () => defaultPretty,
  onSome: handler => handler()
});
const toString = /*#__PURE__*/getMatcher(a => String(a));
const stringify = /*#__PURE__*/getMatcher(a => JSON.stringify(a));
const formatUnknown = /*#__PURE__*/getMatcher(util_.formatUnknown);
/**
 * @since 0.67.0
 */
const match = exports.match = {
  "Declaration": (ast, go, path) => {
    const hook = getHook(ast);
    if (Option.isSome(hook)) {
      return hook.value(...ast.typeParameters.map(tp => go(tp, path)));
    }
    throw new Error(errors_.getPrettyMissingAnnotationErrorMessage(path, ast));
  },
  "VoidKeyword": /*#__PURE__*/getMatcher(() => "void(0)"),
  "NeverKeyword": /*#__PURE__*/getMatcher(() => {
    throw new Error(errors_.getPrettyNeverErrorMessage);
  }),
  "Literal": /*#__PURE__*/getMatcher(literal => typeof literal === "bigint" ? `${String(literal)}n` : JSON.stringify(literal)),
  "SymbolKeyword": toString,
  "UniqueSymbol": toString,
  "TemplateLiteral": stringify,
  "UndefinedKeyword": toString,
  "UnknownKeyword": formatUnknown,
  "AnyKeyword": formatUnknown,
  "ObjectKeyword": formatUnknown,
  "StringKeyword": stringify,
  "NumberKeyword": toString,
  "BooleanKeyword": toString,
  "BigIntKeyword": /*#__PURE__*/getMatcher(a => `${String(a)}n`),
  "Enums": stringify,
  "TupleType": (ast, go, path) => {
    const hook = getHook(ast);
    if (Option.isSome(hook)) {
      return hook.value();
    }
    const elements = ast.elements.map((e, i) => go(e.type, path.concat(i)));
    const rest = ast.rest.map(annotatedAST => go(annotatedAST.type, path));
    return input => {
      const output = [];
      let i = 0;
      // ---------------------------------------------
      // handle elements
      // ---------------------------------------------
      for (; i < elements.length; i++) {
        if (input.length < i + 1) {
          if (ast.elements[i].isOptional) {
            continue;
          }
        } else {
          output.push(elements[i](input[i]));
        }
      }
      // ---------------------------------------------
      // handle rest element
      // ---------------------------------------------
      if (Arr.isNonEmptyReadonlyArray(rest)) {
        const [head, ...tail] = rest;
        for (; i < input.length - tail.length; i++) {
          output.push(head(input[i]));
        }
        // ---------------------------------------------
        // handle post rest elements
        // ---------------------------------------------
        for (let j = 0; j < tail.length; j++) {
          i += j;
          output.push(tail[j](input[i]));
        }
      }
      return "[" + output.join(", ") + "]";
    };
  },
  "TypeLiteral": (ast, go, path) => {
    const hook = getHook(ast);
    if (Option.isSome(hook)) {
      return hook.value();
    }
    const propertySignaturesTypes = ast.propertySignatures.map(ps => go(ps.type, path.concat(ps.name)));
    const indexSignatureTypes = ast.indexSignatures.map(is => go(is.type, path));
    const expectedKeys = {};
    for (let i = 0; i < propertySignaturesTypes.length; i++) {
      expectedKeys[ast.propertySignatures[i].name] = null;
    }
    return input => {
      const output = [];
      // ---------------------------------------------
      // handle property signatures
      // ---------------------------------------------
      for (let i = 0; i < propertySignaturesTypes.length; i++) {
        const ps = ast.propertySignatures[i];
        const name = ps.name;
        if (ps.isOptional && !Object.prototype.hasOwnProperty.call(input, name)) {
          continue;
        }
        output.push(`${util_.formatPropertyKey(name)}: ${propertySignaturesTypes[i](input[name])}`);
      }
      // ---------------------------------------------
      // handle index signatures
      // ---------------------------------------------
      if (indexSignatureTypes.length > 0) {
        for (let i = 0; i < indexSignatureTypes.length; i++) {
          const type = indexSignatureTypes[i];
          const keys = util_.getKeysForIndexSignature(input, ast.indexSignatures[i].parameter);
          for (const key of keys) {
            if (Object.prototype.hasOwnProperty.call(expectedKeys, key)) {
              continue;
            }
            output.push(`${util_.formatPropertyKey(key)}: ${type(input[key])}`);
          }
        }
      }
      return Arr.isNonEmptyReadonlyArray(output) ? "{ " + output.join(", ") + " }" : "{}";
    };
  },
  "Union": (ast, go, path) => {
    const hook = getHook(ast);
    if (Option.isSome(hook)) {
      return hook.value();
    }
    const types = ast.types.map(ast => [ParseResult.is({
      ast
    }), go(ast, path)]);
    return a => {
      const index = types.findIndex(([is]) => is(a));
      if (index === -1) {
        throw new Error(errors_.getPrettyNoMatchingSchemaErrorMessage(a, path, ast));
      }
      return types[index][1](a);
    };
  },
  "Suspend": (ast, go, path) => {
    return Option.match(getHook(ast), {
      onNone: () => {
        const get = util_.memoizeThunk(() => go(ast.f(), path));
        return a => get()(a);
      },
      onSome: handler => handler()
    });
  },
  "Refinement": (ast, go, path) => {
    return Option.match(getHook(ast), {
      onNone: () => go(ast.from, path),
      onSome: handler => handler()
    });
  },
  "Transformation": (ast, go, path) => {
    return Option.match(getHook(ast), {
      onNone: () => go(ast.to, path),
      onSome: handler => handler()
    });
  }
};
const compile = /*#__PURE__*/AST.getCompiler(match);
//# sourceMappingURL=Pretty.js.map