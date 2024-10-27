import * as Effect from "effect/Effect";
import { dual } from "effect/Function";
import * as Logger from "effect/Logger";
import * as FileSystem from "../FileSystem.js";
/** @internal */
export const toFile = /*#__PURE__*/dual(args => Logger.isLogger(args[0]), (self, path, options) => Effect.gen(function* (_) {
  const fs = yield* _(FileSystem.FileSystem);
  const logFile = yield* _(fs.open(path, {
    flag: "a+",
    ...options
  }));
  const encoder = new TextEncoder();
  return yield* _(Logger.batched(self, options?.batchWindow ?? 1000, output => Effect.ignore(logFile.write(encoder.encode(output.join("\n") + "\n")))));
}));
//# sourceMappingURL=platformLogger.js.map