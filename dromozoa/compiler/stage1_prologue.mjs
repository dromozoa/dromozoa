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

//-------------------------------------------------------------------------

const type_impl = {
  undefined: "nil",
  number: "number",
  string: "string",
  boolean: "boolean",
  symbol: "userdata",
  bigint: "userdata",
};

const type = v => {
  const t = type_impl[typeof v];
  if (t) {
    return t;
  } else if (v.LuaTable) {
    return "table";
  } else if (v.LuaFunction) {
    return "function";
  } else {
    return "userdata";
  }
};

//-------------------------------------------------------------------------

class LuaError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "LuaError";
    this.msg = msg;
  }

  LuaError() {}
}

class LuaTable extends Map {
  constructor() {
    super();
    this.n = 0;
    this.metatable = undefined;
  }

  LuaTable() {}
}

const LuaFunction = f => {
  f.LuaFunction = true;
  return f;
};

//-------------------------------------------------------------------------

const OP_GETTABLE = (t, k) => {
  if (t.LuaTable) {
    const v = t.get(k);
    if (v !== undefined || !t.metatable) {
      return v;
    }
    const metafield = t.metatable.get("__index");
    if (metafield === undefined) {
      return undefined;
    }
    if (metafield.LuaTable) {
      return OP_GETTABLE(metafield, k);
    } else {
      return metafield(t, k)[0];
    }
  } else if (typeof t === "string") {
    return D.string_metatable.get("__index").get(k);
  } else {
    return t[k];
  }
};

const OP_SETLIST = (t, r) => {
  const n = r.length;
  for (let i = 1; i <= n; ++i) {
    const v = r[i - 1];
    if (v !== undefined) {
      t.set(i, v);
    } else {
      t.delete(i);
    }
  }
  if (t.n !== undefined && t.n < n) {
    t.n = n;
  }
};

const OP_LEN = t => {
  if (t.LuaTable) {
    if (t.n === undefined) {
      let i = 1;
      for (; t.get(i) !== undefined; ++i) {}
      t.n = i - 1;
    }
    return t.n;
  } else if (typeof t === "string") {
    return E.get("string").get("len")(t)[0];
  } else {
    return t.length;
  }
};

const OP_CHECKNUMBER = (v, msg) => {
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

//-------------------------------------------------------------------------

const D = {
  string_metatable: new LuaTable(),

  getmetafield: (t, ev) => {
    if (t.LuaTable) {
      if (!t.metatable) {
        return undefined;
      }
      return t.metatable.get(ev);
    } else if (typeof t === "string") {
      return D.string_metatable.get(ev);
    }
  },

  native_new: (f, ...args) => {
    return new f(...args);
  },

  native_function: f => {
    return (...args) => {
      return f(...args)[0];
    };
  },

  native_call: (t, k, ...args) => {
    return t[k].apply(t, args);
  },

  array_pack: (...args) => {
    return args;
  },

  array_unpack: LuaFunction(r => {
    return r;
  }),

  array_from: (t, m, n) => {
    const r = [];
    for (let i = m; i <= n; ++i) {
      r[i - m] = OP_GETTABLE(t, i);
    }
    return r;
  },
};

//-------------------------------------------------------------------------

const E = new LuaTable();
E.set("globalThis", globalThis);
E.set("type", type);
E.set("dromozoa", D);

{
  const P = new LuaTable();
  P.set("preload", new LuaTable());
  P.set("loaded", new LuaTable());
  E.set("package", P);
}

{
  const A = new LuaTable();
  for (let i = 0; i < process.argv.length; ++i) {
    A.set(i - 1, process.argv[i]);
  }
  A.n = process.argv.length - 2;
  E.set("arg", A);
}
const A = process.argv.slice(2);

//-------------------------------------------------------------------------

E.set("error", msg => {
  throw new LuaError(msg);
});

E.set("getmetatable", t => {
  if (t.LuaTable) {
    if (!t.metatable) {
      return undefined;
    }
    const metafield = t.metatable.get("__metatable");
    if (metafield === undefined) {
      return t.metatable;
    }
    return metafield;
  } else if (typeof t === "string") {
    return D.string_metatable;
  }
});

E.set("setmetatable", (t, metatable) => {
  if (t.metatable && t.metatable.get("__metatable") !== undefined) {
    throw new LuaError("cannot change a protected metatable");
  }
  t.metatable = metatable;
  return t;
});

E.set("select", LuaFunction((k, ...args) => {
  if (k === "#") {
    return [ args.length ];
  } else {
    return args.slice(k - 1);
  }
}));

E.set("pcall", LuaFunction((f, ...args) => {
  try {
    return [ true, ...f(...args) ];
  } catch (e) {
    if (e.LuaError) {
      return [ false, e.msg ];
    } else {
      return [ false, e.toString() ];
    }
  }
}));

//-------------------------------------------------------------------------

const S0 = [];
const S1 = [undefined];
