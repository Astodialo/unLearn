import * as Context from "effect/Context";
/** @internal */
export const GeneratorTypeId = /*#__PURE__*/Symbol.for("@effect/platform/Etag/Generator");
/** @internal */
export const tag = /*#__PURE__*/Context.GenericTag("@effect/platform/Etag/Generator");
/** @internal */
export const toString = self => {
  switch (self._tag) {
    case "Weak":
      return `W/"${self.value}"`;
    case "Strong":
      return `"${self.value}"`;
  }
};
//# sourceMappingURL=etag.js.map