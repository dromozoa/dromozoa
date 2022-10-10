return "import * as fs from \"fs\";\nglobalThis.fs=fs;\nclass LuaError extends Error {\n  constructor(msg) {\n    super(msg);\n    this.name = \"LuaError\";\n    this.msg = msg;\n  }\n}\nclass LuaFunction {\n  constructor(fn) {\n    this.fn = fn;\n  }\n}\nclass LuaTable {\n  constructor() {\n    this.map = new Map();\n    this.n = 0;\n  }\n}\nconst type_impl = {\n  undefined: \"nil\",\n  number: \"number\",\n  string: \"string\",\n  boolean: \"boolean\",\n  symbol: \"userdata\",\n  bigint: \"userdata\",\n  function: \"userdata\",\n};\nconst type = v => {\n  const t = type_impl[typeof v];\n  if (t !== undefined) {\n    return t;\n  } else if (v instanceof LuaFunction) {\n    return \"function\";\n  } else if (v instanceof LuaTable) {\n    return \"table\";\n  } else {\n    return \"userdata\";\n  }\n};\nconst getmetafield = (t, ev) => {\n  if (t instanceof LuaTable) {\n    const metatable = t.metatable;\n    if (metatable !== undefined) {\n      return metatable.map.get(ev);\n    }\n  } else if (typeof t === \"string\") {\n    return D.string_metatable.map.get(ev);\n  }\n};\nconst checknumber = (v, msg) => {\n  const t = typeof v;\n  if (t === \"number\") {\n    return v;\n  } else if (t === \"string\") {\n    v = +v;\n    if (Number.isNaN(v)) {\n      throw new LuaError(msg + \" (number expected, got string)\");\n    }\n    return v;\n  } else {\n    throw new LuaError(msg + \" (number expected, got \" + type(v) + \")\");\n  }\n};\nconst OP_SETTABLE = (t, k, v) => {\n  if (t instanceof LuaTable) {\n    if (t.map.get(k) === undefined) {\n      if (t.metatable !== undefined) {\n        const metafield = t.metatable.map.get(\"__newindex\");\n        if (metafield !== undefined) {\n          if (metafield instanceof LuaTable) {\n            OP_SETTABLE(metafield, k, v);\n          } else {\n            OP_CALL(metafield, [ t, k, v ])[0];\n          }\n          return;\n        }\n      }\n    }\n    if (v !== undefined) {\n      if (t.n !== undefined && Number.isInteger(k) && k > t.n && k !== ++t.n) {\n        t.n = undefined;\n      }\n      t.map.set(k, v);\n    } else {\n      if (t.n !== undefined && Number.isInteger(k) && k !== t.n--) {\n        t.n = undefined;\n      }\n      t.map.delete(k);\n    }\n  } else {\n    if (v !== undefined) {\n      t[k] = v;\n    } else {\n      delete t[k];\n    }\n  }\n};\nconst OP_GETTABLE = (t, k) => {\n  if (t instanceof LuaTable) {\n    const v = t.map.get(k);\n    if (v !== undefined) {\n      return v;\n    }\n    if (t.metatable !== undefined) {\n      const metafield = t.metatable.map.get(\"__index\");\n      if (metafield !== undefined) {\n        if (metafield instanceof LuaTable) {\n          return OP_GETTABLE(metafield, k);\n        } else {\n          return OP_CALL(metafield, [ t, k ])[0];\n        }\n      }\n    }\n  } else if (typeof t === \"string\") {\n    return OP_GETTABLE(D.string_metatable.map.get(\"__index\"), k);\n  } else {\n    return t[k];\n  }\n};\nconst OP_CALL = (f, args) => {\n  if (f instanceof LuaFunction) {\n    return f.fn(...args);\n  } else if (f instanceof LuaTable) {\n    return OP_CALL(getmetafield(f, \"__call\"), [ f, ...args ]);\n  } else {\n    return [ f.apply(undefined, args) ];\n  }\n};\nconst OP_SELF = (t, k, args) => {\n  const f = OP_GETTABLE(t, k);\n  if (f instanceof LuaFunction) {\n    return f.fn(t, ...args);\n  } else if (f instanceof LuaTable) {\n    return OP_CALL(getmetafield(f, \"__call\"), [ f, t, ...args ]);\n  } else {\n    return [ f.apply(t, args) ];\n  }\n};\nconst OP_SETLIST = (t, r) => {\n  let i = 0;\n  for (; i < r.length; ++i) {\n    const v = r[i];\n    if (v !== undefined) {\n      t.map.set(i + 1, v);\n    } else {\n      break;\n    }\n  }\n  t.n = i;\n  for (; i < r.length; ++i) {\n    const v = r[i];\n    if (v !== undefined) {\n      t.map.set(i + 1, v);\n    }\n  }\n  return t;\n};\nconst OP_LEN = t => {\n  if (t instanceof LuaTable) {\n    if (t.n === undefined) {\n      let i = 1;\n      for (; t.map.get(i) !== undefined; ++i) {}\n      t.n = i - 1;\n    }\n    return t.n;\n  } else if (typeof t === \"string\") {\n    return D.string_len(t);\n  } else {\n    return t.length;\n  }\n};\nconst D = {\n  string_metatable: new LuaTable(),\n  getmetafield: getmetafield,\n  native_new: (f, ...args) => new f(...args),\n  native_function: (f) => (...args) => OP_CALL(f, args)[0],\n  array_pack: (...args) => args,\n  array_unpack: new LuaFunction(args => args),\n  array_from: (t, m, n) => {\n    const r = [];\n    for (let i = m; i <= n; ++i) {\n      r[i - m] = OP_GETTABLE(t, i);\n    }\n    return r;\n  },\n  table_entries:a=>a.map.entries(),\n  string_replace:(a,...b)=>a.replace(...b),\n  arg:[],\n};\nconst E = new LuaTable();\nconst P = new LuaTable();\nP.map.set(\"preload\", new LuaTable()).set(\"loaded\", new LuaTable());\nE.map.set(\"package\", P);\nE.map.set(\"dromozoa\", D);\nE.map.set(\"globalThis\", globalThis);\nE.map.set(\"type\", type);\nE.map.set(\"error\", msg => {\n  throw new LuaError(msg);\n});\nE.map.set(\"getmetatable\", t => {\n  if (t instanceof LuaTable) {\n    const metatable = t.metatable;\n    if (metatable !== undefined) {\n      const metafield = metatable.map.get(\"__metatable\");\n      if (metafield !== undefined) {\n        return metafield;\n      }\n    }\n    return metatable;\n  } else if (typeof t === \"string\") {\n    return D.string_metatable;\n  }\n});\nE.map.set(\"setmetatable\", (t, metatable) => {\n  if (getmetafield(t, \"__metatable\") !== undefined) {\n    throw new LuaError(\"cannot change a protected metatable\");\n  }\n  t.metatable = metatable;\n  return t;\n});\nE.map.set(\"select\", new LuaFunction((k, ...args) => {\n  if (k === \"#\") {\n    return [ args.length ];\n  } else {\n    return args.slice(k - 1);\n  }\n}));\nE.map.set(\"pcall\", new LuaFunction((f, ...args) => {\n  try {\n    return [ true, ...OP_CALL(f, args) ];\n  } catch (e) {\n    if (e instanceof LuaError) {\n      return [ false, e.msg ];\n    } else {\n      return [ false, e.toString() ];\n    }\n  }\n}));\n"
