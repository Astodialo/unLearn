import * as internal from "./internal/platformLogger.js";
/**
 * Create a Logger from another string Logger that writes to the specified file.
 *
 * @since 1.0.0
 * @example
 * import { PlatformLogger } from "@effect/platform"
 * import { NodeFileSystem, NodeRuntime } from "@effect/platform-node"
 * import { Effect, Layer, Logger } from "effect"
 *
 * const fileLogger = Logger.logfmtLogger.pipe(
 *   PlatformLogger.toFile("/tmp/log.txt")
 * )
 * const LoggerLive = Logger.replaceScoped(Logger.defaultLogger, fileLogger).pipe(
 *   Layer.provide(NodeFileSystem.layer)
 * )
 *
 * Effect.log("a").pipe(
 *   Effect.zipRight(Effect.log("b")),
 *   Effect.zipRight(Effect.log("c")),
 *   Effect.provide(LoggerLive),
 *   NodeRuntime.runMain
 * )
 */
export const toFile = internal.toFile;
//# sourceMappingURL=PlatformLogger.js.map