// Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
//
// This file is part of dromozoa.
//
// dromozoa is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// dromozoa is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.
//
// You should have received a copy of the GNU General Public License
// and a copy of the GCC Runtime Library Exception along with
// dromozoa.  If not, see <http://www.gnu.org/licenses/>.

import * as fs from "fs";

globalThis.fs = fs;

class LuaError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "LuaError";
    this.msg = msg;
  }
}

class LuaFunction {
  constructor(fn) {
    this.fn = fn;
  }
}

class LuaTable {
  constructor() {
    this.map = new Map();
    this.n = 0;
    // this.metatable;
  }
}

const type_impl = {
  undefined: "nil",
  number: "number",
  string: "string",
  boolean: "boolean",
  symbol: "userdata",
  bigint: "userdata",
  function: "userdata",
};

const type = v => {
  const t = type_impl[typeof v];
  if (t !== undefined) {
    return t;
  } else if (v instanceof LuaFunction) {
    return "function";
  } else if (v instanceof LuaTable) {
    return "table";
  } else {
    return "userdata";
  }
};

const getmetafield = (t, ev) => {
  if (t instanceof LuaTable) {
    const metatable = t.metatable;
    if (metatable !== undefined) {
      return metatable.map.get(ev);
    }
  } else if (typeof t === "string") {
    return D.string_metatable.map.get(ev);
  }
};

const checknumber = (v, msg) => {
  const t = typeof v;
  if (t === "number") {
    return v;
  } else if (t === "string") {
    v = +v;
    if (Number.isNaN(v)) {
      throw new LuaError(msg + " (number expected, got string)");
    }
    return v;
  } else {
    throw new LuaError(msg + " (number expected, got " + type(v) + ")");
  }
};

const OP_SETTABLE = (t, k, v) => {
  if (t instanceof LuaTable) {
    if (t.map.get(k) === undefined) {
      if (t.metatable !== undefined) {
        const metafield = t.metatable.map.get("__newindex");
        if (metafield !== undefined) {
          if (metafield instanceof LuaTable) {
            OP_SETTABLE(metafield, k, v);
          } else {
            OP_CALL(metafield, [ t, k, v ])[0];
          }
          return;
        }
      }
    }
    if (v !== undefined) {
      if (t.n !== undefined && Number.isInteger(k) && k > t.n && k !== ++t.n) {
        t.n = undefined;
      }
      t.map.set(k, v);
    } else {
      if (t.n !== undefined && Number.isInteger(k) && k !== t.n--) {
        t.n = undefined;
      }
      t.map.delete(k);
    }
  } else {
    if (v !== undefined) {
      t[k] = v;
    } else {
      delete t[k];
    }
  }
};

const OP_GETTABLE = (t, k) => {
  if (t instanceof LuaTable) {
    const v = t.map.get(k);
    if (v !== undefined) {
      return v;
    }
    if (t.metatable !== undefined) {
      const metafield = t.metatable.map.get("__index");
      if (metafield !== undefined) {
        if (metafield instanceof LuaTable) {
          return OP_GETTABLE(metafield, k);
        } else {
          return OP_CALL(metafield, [ t, k ])[0];
        }
      }
    }
  } else if (typeof t === "string") {
    return OP_GETTABLE(D.string_metatable.map.get("__index"), k);
  } else {
    return t[k];
  }
};

// TODO LuaFunction, LuaTable自体を調整する
const OP_CALL = (f, args) => {
  if (f instanceof LuaFunction) {
    return f.fn(...args);
  } else if (f instanceof LuaTable) {
    return OP_CALL(getmetafield(f, "__call"), [ f, ...args ]);
  } else {
    return [ f.apply(undefined, args) ];
  }
};

const OP_SELF = (t, k, args) => {
  const f = OP_GETTABLE(t, k);
  if (f instanceof LuaFunction) {
    return f.fn(t, ...args);
  } else if (f instanceof LuaTable) {
    return OP_CALL(getmetafield(f, "__call"), [ f, t, ...args ]);
  } else {
    return [ f.apply(t, args) ];
  }
};

const OP_SETLIST = (t, r) => {
  let i = 0;
  for (; i < r.length; ++i) {
    const v = r[i];
    if (v !== undefined) {
      t.map.set(i + 1, v);
    } else {
      break;
    }
  }
  t.n = i;
  for (; i < r.length; ++i) {
    const v = r[i];
    if (v !== undefined) {
      t.map.set(i + 1, v);
    }
  }
  return t;
};

const OP_LEN = t => {
  if (t instanceof LuaTable) {
    if (t.n === undefined) {
      let i = 1;
      for (; t.map.get(i) !== undefined; ++i) {}
      t.n = i - 1;
    }
    return t.n;
  } else if (typeof t === "string") {
    return D.string_len(t);
  } else {
    return t.length;
  }
};

const D = {
  string_metatable: new LuaTable(),
  getmetafield: getmetafield,

  native_new: (f, ...args) => new f(...args),
  native_function: (f) => (...args) => OP_CALL(f, args)[0],

  array_pack: (...args) => args,
  array_unpack: new LuaFunction(args => args),

  array_from: (t, m, n) => {
    const r = [];
    for (let i = m; i <= n; ++i) {
      r[i - m] = OP_GETTABLE(t, i);
    }
    return r;
  },

  table_entries:a=>a.map.entries(),
  string_replace:(a,...b)=>a.replace(...b),
  arg:[],
};

const E = new LuaTable();

const P = new LuaTable();
OP_SETTABLE(P, "preload", new LuaTable());
OP_SETTABLE(P, "loaded", new LuaTable());
OP_SETTABLE(E, "package", P);

OP_SETTABLE(E, "dromozoa", D);
OP_SETTABLE(E, "globalThis", globalThis);
OP_SETTABLE(E, "type", type);

OP_SETTABLE(E, "error", msg => {
  throw new LuaError(msg);
});

OP_SETTABLE(E, "getmetatable", t => {
  if (t instanceof LuaTable) {
    const metatable = t.metatable;
    if (metatable !== undefined) {
      const metafield = metatable.map.get("__metatable");
      if (metafield !== undefined) {
        return metafield;
      }
    }
    return metatable;
  } else if (typeof t === "string") {
    return D.string_metatable;
  }
});

OP_SETTABLE(E, "setmetatable", (t, metatable) => {
  if (getmetafield(t, "__metatable") !== undefined) {
    throw new LuaError("cannot change a protected metatable");
  }
  t.metatable = metatable;
  return t;
});

OP_SETTABLE(E, "select", new LuaFunction((k, ...args) => {
  if (k === "#") {
    return [ args.length ];
  } else {
    return args.slice(k - 1);
  }
}));

OP_SETTABLE(E, "pcall", new LuaFunction((f, ...args) => {
  try {
    return [ true, ...OP_CALL(f, args) ];
  } catch (e) {
    if (e instanceof LuaError) {
      return [ false, e.msg ];
    } else {
      return [ false, e.toString() ];
    }
  }
}));
