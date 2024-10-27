/**
 * @since 0.67.0
 */
import * as Effect from "effect/Effect";
import type * as ParseResult from "./ParseResult.js";
/**
 * @category model
 * @since 0.67.0
 */
export interface Issue {
    readonly _tag: ParseResult.ParseIssue["_tag"];
    readonly path: ReadonlyArray<PropertyKey>;
    readonly message: string;
}
/**
 * @category formatting
 * @since 0.67.0
 */
export declare const formatIssue: (issue: ParseResult.ParseIssue) => Effect.Effect<Array<Issue>>;
/**
 * @category formatting
 * @since 0.67.0
 */
export declare const formatIssueSync: (issue: ParseResult.ParseIssue) => Array<Issue>;
/**
 * @category formatting
 * @since 0.67.0
 */
export declare const formatError: (error: ParseResult.ParseError) => Effect.Effect<Array<Issue>>;
/**
 * @category formatting
 * @since 0.67.0
 */
export declare const formatErrorSync: (error: ParseResult.ParseError) => Array<Issue>;
//# sourceMappingURL=ArrayFormatter.d.ts.map