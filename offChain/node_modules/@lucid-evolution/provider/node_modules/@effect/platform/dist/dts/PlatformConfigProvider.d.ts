import * as ConfigProvider from "effect/ConfigProvider";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as FileSystem from "./FileSystem.js";
import * as Path from "./Path.js";
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const fromFileTree: (options?: {
    readonly rootDirectory?: string;
}) => Effect.Effect<ConfigProvider.ConfigProvider, never, Path.Path | FileSystem.FileSystem>;
/**
 * Add the file tree ConfigProvider to the environment, as a fallback to the current ConfigProvider.
 *
 * @since 1.0.0
 * @category layers
 */
export declare const layerFileTreeAdd: (options?: {
    readonly rootDirectory?: string;
}) => Layer.Layer<never, never, Path.Path | FileSystem.FileSystem>;
/**
 * Add the file tree ConfigProvider to the environment, replacing the current ConfigProvider.
 *
 * @since 1.0.0
 * @category layers
 */
export declare const layerFileTree: (options?: {
    readonly rootDirectory?: string;
}) => Layer.Layer<never, never, Path.Path | FileSystem.FileSystem>;
//# sourceMappingURL=PlatformConfigProvider.d.ts.map