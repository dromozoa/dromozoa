return "import * as fs from \"fs\";\nglobalThis.fs = fs;\nconst type_impl = {\n  undefined: \"nil\",\n  number: \"number\",\n  string: \"string\",\n  boolean: \"boolean\",\n  symbol: \"userdata\",\n  bigint: \"userdata\",\n};\nconst type = v => {\n  const t = type_impl[typeof v];\n  if (t) {\n    return t;\n  } else if (v.LuaTable) {\n    return \"table\";\n  } else if (v.LuaFunction) {\n    return \"function\";\n  } else {\n    return \"userdata\";\n  }\n};\nclass LuaError extends Error {\n  constructor(msg) {\n    super(msg);\n    this.name = \"LuaError\";\n    this.msg = msg;\n  }\n  LuaError() {}\n}\nclass LuaTable extends Map {\n  constructor() {\n    super();\n    this.n = 0;\n    this.metatable = undefined;\n  }\n  LuaTable() {}\n}\nconst OP_NEWTABLE = () => {\n  return new LuaTable();\n};\nconst OP_NEWFUNCTION = f => {\n  f.LuaFunction = true;\n  return f;\n};\nconst OP_SETTABLE = (t, k, v) => {\n  if (t.LuaTable) {\n    if (t.get(k) === undefined && t.metatable) {\n      const metafield = t.metatable.get(\"__newindex\");\n      if (metafield !== undefined) {\n        if (metafield.LuaTable) {\n          OP_SETTABLE(metafield, k, v);\n        } else {\n          OP_CALL(metafield, [t, k, v]);\n        }\n        return;\n      }\n    }\n    if (v !== undefined) {\n      if (t.n !== undefined && Number.isInteger(k) && k > t.n && k !== ++t.n) {\n        t.n = undefined;\n      }\n      t.set(k, v);\n    } else {\n      if (t.n !== undefined && Number.isInteger(k) && k !== t.n--) {\n        t.n = undefined;\n      }\n      t.delete(k);\n    }\n  } else {\n    if (v !== undefined) {\n      t[k] = v;\n    } else {\n      delete t[k];\n    }\n  }\n};\nconst OP_GETTABLE = (t, k) => {\n  if (t.LuaTable) {\n    const v = t.get(k);\n    if (v !== undefined || !t.metatable) {\n      return v;\n    }\n    const metafield = t.metatable.get(\"__index\");\n    if (metafield === undefined) {\n      return undefined;\n    }\n    if (metafield.LuaTable) {\n      return OP_GETTABLE(metafield, k);\n    } else {\n      return OP_CALL(metafield, [t, k])[0];\n    }\n  } else if (typeof t === \"string\") {\n    return OP_GETTABLE(D.string_metatable.get(\"__index\"), k);\n  } else {\n    return t[k];\n  }\n};\nconst OP_CALL = (f, args) => {\n  if (f.LuaFunction) {\n    return f(...args);\n  } else if (f.LuaTable) {\n    return OP_CALL(f.metatable.get(\"__call\"), [f, ...args]);\n  } else {\n    return [ f(...args) ];\n  }\n};\nconst OP_SELF = (t, k, args) => {\n  const f = OP_GETTABLE(t, k);\n  if (f.LuaFunction) {\n    return f(t, ...args);\n  } else if (f.LuaTable) {\n    return OP_CALL(f.metatable.get(\"__call\"), [f, t, ...args]);\n  } else {\n    return [ f.apply(t, args) ];\n  }\n};\nconst OP_SETLIST = (t, r) => {\n  for (let i = 0; i < r.length; ++i) {\n    OP_SETTABLE(t, i + 1, r[i]);\n  }\n};\nconst OP_LEN = t => {\n  if (t.LuaTable) {\n    if (t.n === undefined) {\n      let i = 1;\n      for (; t.get(i) !== undefined; ++i) {}\n      t.n = i - 1;\n    }\n    return t.n;\n  } else if (typeof t === \"string\") {\n    return D.string_len(t);\n  } else {\n    return t.length;\n  }\n};\nconst OP_CLOSE = t => {\n  if (t !== undefined) {\n    OP_CALL(t.metatable.get(\"__close\"), [t]);\n  }\n};\nconst OP_CHECKNUMBER = (v, msg) => {\n  const t = typeof v;\n  if (t === \"number\") {\n    return v;\n  } else if (t === \"string\") {\n    v = +v;\n    if (Number.isNaN(v)) {\n      throw new LuaError(msg + \" (number expected, got string)\");\n    }\n    return v;\n  } else {\n    throw new LuaError(msg + \" (number expected, got \" + type(v) + \")\");\n  }\n};\nconst D = {\n  string_metatable: OP_NEWTABLE(),\n  arg: [],\n  getmetafield: (t, ev) => {\n    if (t.LuaTable) {\n      if (!t.metatable) {\n        return undefined;\n      }\n      return t.metatable.get(ev);\n    } else if (typeof t === \"string\") {\n      return D.string_metatable.get(ev);\n    }\n  },\n  native_new: (f, ...args) => {\n    return new f(...args);\n  },\n  native_function: f => {\n    return (...args) => {\n      return f(...args)[0];\n    };\n  },\n  native_call: (t, k, ...args) => {\n    return t[k].apply(t, args);\n  },\n  array_pack: (...args) => {\n    return args;\n  },\n  array_unpack: OP_NEWFUNCTION(r => {\n    return r;\n  }),\n  array_from: (t, m, n) => {\n    const r = [];\n    for (let i = m; i <= n; ++i) {\n      r[i - m] = OP_GETTABLE(t, i);\n    }\n    return r;\n  },\n};\nconst E = OP_NEWTABLE();\nOP_SETTABLE(E, \"globalThis\", globalThis);\nOP_SETTABLE(E, \"type\", type);\nOP_SETTABLE(E, \"dromozoa\", D);\nconst P = OP_NEWTABLE();\nOP_SETTABLE(P, \"preload\", OP_NEWTABLE());\nOP_SETTABLE(P, \"loaded\", OP_NEWTABLE());\nOP_SETTABLE(E, \"package\", P);\nOP_SETTABLE(E, \"error\", msg => {\n  throw new LuaError(msg);\n});\nOP_SETTABLE(E, \"getmetatable\", t => {\n  if (t.LuaTable) {\n    if (!t.metatable) {\n      return undefined;\n    }\n    const metafield = t.metatable.get(\"__metatable\");\n    if (metafield !== undefined) {\n      return metafield;\n    }\n    return t.metatable;\n  } else if (typeof t === \"string\") {\n    return D.string_metatable;\n  }\n});\nOP_SETTABLE(E, \"setmetatable\", (t, metatable) => {\n  if (t.metatable && t.metatable.get(\"__metatable\") !== undefined) {\n    throw new LuaError(\"cannot change a protected metatable\");\n  }\n  t.metatable = metatable;\n  return t;\n});\nOP_SETTABLE(E, \"select\", OP_NEWFUNCTION((k, ...args) => {\n  if (k === \"#\") {\n    return [ args.length ];\n  } else {\n    return args.slice(k - 1);\n  }\n}));\nOP_SETTABLE(E, \"pcall\", OP_NEWFUNCTION((f, ...args) => {\n  try {\n    return [ true, ...OP_CALL(f, args) ];\n  } catch (e) {\n    if (e.LuaError) {\n      return [ false, e.msg ];\n    } else {\n      return [ false, e.toString() ];\n    }\n  }\n}));\n"
