"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.hasBody = void 0;
/**
 * @since 1.0.0
 */
const hasBody = method => method !== "GET" && method !== "HEAD";
exports.hasBody = hasBody;
//# sourceMappingURL=HttpMethod.js.map