"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MultipastaStream = exports.FileStream = void 0;
Object.defineProperty(exports, "decodeField", {
  enumerable: true,
  get: function () {
    return MP.decodeField;
  }
});
exports.make = void 0;
var MP = /*#__PURE__*/_interopRequireWildcard( /*#__PURE__*/require("./index.js"));
var _nodeStream = /*#__PURE__*/require("node:stream");
function _getRequireWildcardCache(e) {
  if ("function" != typeof WeakMap) return null;
  var r = new WeakMap(),
    t = new WeakMap();
  return (_getRequireWildcardCache = function (e) {
    return e ? t : r;
  })(e);
}
function _interopRequireWildcard(e, r) {
  if (!r && e && e.__esModule) return e;
  if (null === e || "object" != typeof e && "function" != typeof e) return {
    default: e
  };
  var t = _getRequireWildcardCache(r);
  if (t && t.has(e)) return t.get(e);
  var n = {
      __proto__: null
    },
    a = Object.defineProperty && Object.getOwnPropertyDescriptor;
  for (var u in e) if ("default" !== u && Object.prototype.hasOwnProperty.call(e, u)) {
    var i = a ? Object.getOwnPropertyDescriptor(e, u) : null;
    i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u];
  }
  return n.default = e, t && t.set(e, n), n;
}
class MultipastaStream extends _nodeStream.Duplex {
  _parser;
  _canWrite = true;
  _writeCallback;
  constructor(config) {
    super({
      readableObjectMode: true
    });
    this._parser = MP.make({
      ...config,
      onField: (info, value) => {
        const field = {
          _tag: "Field",
          info,
          value
        };
        this.push(field);
        this.emit("field", field);
      },
      onFile: info => {
        const file = new FileStream(info, this);
        this.push(file);
        this.emit("file", file);
        return chunk => {
          this._canWrite = file.push(chunk);
          if (chunk === null && !this._canWrite) {
            this._resume();
          }
        };
      },
      onError: error => {
        this.emit("error", error);
      },
      onDone: () => {
        this.push(null);
      }
    });
  }
  _resume() {
    this._canWrite = true;
    if (this._writeCallback !== undefined) {
      const callback = this._writeCallback;
      this._writeCallback = undefined;
      callback();
    }
  }
  _read(_size) {}
  _write(chunk, encoding, callback) {
    this._parser.write(chunk instanceof Uint8Array ? chunk : Buffer.from(chunk, encoding));
    if (this._canWrite) {
      callback();
    } else {
      this._writeCallback = callback;
    }
  }
  _final(callback) {
    this._parser.end();
    callback();
  }
}
exports.MultipastaStream = MultipastaStream;
const make = config => new MultipastaStream(config);
exports.make = make;
class FileStream extends _nodeStream.Readable {
  info;
  _parent;
  _tag = "File";
  filename;
  constructor(info, _parent) {
    super();
    this.info = info;
    this._parent = _parent;
    this.filename = info.filename;
  }
  _read(_size) {
    if (this._parent._canWrite === false) {
      this._parent._resume();
    }
  }
}
exports.FileStream = FileStream;
//# sourceMappingURL=node.js.map