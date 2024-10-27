/**
 * @since 1.0.0
 */
import type * as Cause from "effect/Cause";
import type { Simplify } from "effect/Types";
/**
 * @since 1.0.0
 * @category type id
 */
export declare const PlatformErrorTypeId: unique symbol;
/**
 * @since 1.0.0
 * @category type id
 */
export type PlatformErrorTypeId = typeof PlatformErrorTypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
export declare const isPlatformError: (u: unknown) => u is PlatformError;
/**
 * @since 1.0.0
 * @category error
 */
export type PlatformError = BadArgument | SystemError;
/**
 * @since 1.0.0
 * @category error
 */
export declare const TypeIdError: <const TypeId extends symbol, const Tag extends string>(typeId: TypeId, tag: Tag) => new <A extends Record<string, any>>(args: Simplify<A>) => Cause.YieldableError & Record<TypeId, TypeId> & {
    readonly _tag: Tag;
} & Readonly<A>;
/**
 * @since 1.0.0
 */
export declare namespace PlatformError {
    /**
     * @since 1.0.0
     * @category models
     */
    interface Base {
        readonly [PlatformErrorTypeId]: typeof PlatformErrorTypeId;
        readonly _tag: string;
        readonly module: "Clipboard" | "Command" | "FileSystem" | "KeyValueStore" | "Path" | "Stream" | "Terminal";
        readonly method: string;
        readonly message: string;
    }
    /**
     * @since 1.0.0
     */
    type ProvidedFields = PlatformErrorTypeId | "_tag";
}
/**
 * @since 1.0.0
 * @category error
 */
export interface BadArgument extends PlatformError.Base {
    readonly _tag: "BadArgument";
}
/**
 * @since 1.0.0
 * @category error
 */
export declare const BadArgument: (props: Omit<BadArgument, PlatformError.ProvidedFields>) => BadArgument;
/**
 * @since 1.0.0
 * @category model
 */
export type SystemErrorReason = "AlreadyExists" | "BadResource" | "Busy" | "InvalidData" | "NotFound" | "PermissionDenied" | "TimedOut" | "UnexpectedEof" | "Unknown" | "WouldBlock" | "WriteZero";
/**
 * @since 1.0.0
 * @category models
 */
export interface SystemError extends PlatformError.Base {
    readonly _tag: "SystemError";
    readonly reason: SystemErrorReason;
    readonly syscall?: string | undefined;
    readonly pathOrDescriptor: string | number;
}
/**
 * @since 1.0.0
 * @category error
 */
export declare const SystemError: (props: Omit<SystemError, PlatformError.ProvidedFields>) => SystemError;
//# sourceMappingURL=Error.d.ts.map