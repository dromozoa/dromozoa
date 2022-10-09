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

/*
 * /* */

import * as fs from "fs";
globalThis.fs=fs;
class LuaError extends Error{constructor(msg){super(msg);this.name="LuaError";this.msg=msg;}}

class LuaFunction{constructor(fn){this.fn=fn;}}
class LuaTable{constructor(){this.map=new Map();this.n=0}}
const D={
type_impl:{undefined:"nil",number:"number",string:"string",boolean:"boolean"},
type:a=>{const t=D.type_impl[typeof a];return t!==undefined?t:a instanceof LuaFunction?"function":a instanceof LuaTable?"table":"userdata";},
error:a=>{throw new LuaError(a);},
checknumber:(a,b)=>{const t=D.type(a),v=t==="number"||t==="string"?Number(a):NaN;if (Number.isNaN(v))D.error(b+" (number expected, got "+t+")");return v;},
rawget:(a,b)=>a instanceof LuaTable?a.map.get(b):a[b],
rawset:(a,b,c)=>{
  if(a instanceof LuaTable)
    if(c===undefined){if(a.n!==undefined&&Number.isInteger(b)&&b!==a.n--)a.n=undefined;a.map.delete(b);}
    else{if(a.n!==undefined&&Number.isInteger(b)&&b>a.n&&b!==++a.n)a.n=undefined;a.map.set(b,c);}
  else
    if(c===undefined)delete a[b];
    else a[b]=c;
  return a;
},
rawlen:a=>{
  if(a instanceof LuaTable){let n=a.n;if(n!==undefined)return n;for(n=1;a.map.get(n)!==undefined;++n);return a.n=--n;}
  else return typeof a==="string"?D.string_len(a):a.length;
},
getmetatable:a=>{const m=typeof a==="string"?D.string_metatable:a.metatable;if(m===undefined)return m;const f=D.rawget(m,"__metatable");return f===undefined?m:f;},
getmetafield:(a,b)=>{const m=typeof a==="string"?D.string_metatable:a.metatable;if(m!==undefined)return D.rawget(m,b);},
setmetatable:(a,b)=>{if(D.getmetafield(a,"__metatable")!==undefined)D.error("cannot change a protected metatable");a.metatable=b;return a;},
select:new LuaFunction((a,...b)=>a==="#"?[b.length]:b.slice(a-1)),
native:(a)=>(...b)=>D.OP_CALL(a,b)[0],
array_pack:(...a)=>a,
array_unpack:new LuaFunction(a=>a),
array_from:(a,b,c)=>{let v=[];for(let i=b;i<=c;++i)v[i-b]=D.OP_GETTABLE(a,i);return v;},
newuserdata:(a,...b)=>new a(...b),
entries:a=>a.map.entries(),
replace:(a,...b)=>a.replace(...b),
arg:[],
OP_ADD:(a,b)=>Number(a)+Number(b),
OP_SUB:(a,b)=>a-b,
OP_MUL:(a,b)=>a*b,
OP_DIV:(a,b)=>a/b,
OP_IDIV:(a,b)=>Math.floor(a/b),
OP_MOD:(a,b)=>(a%b+b)%b,
OP_POW:(a,b)=>Math.pow(a,b),
OP_BAND:(a,b)=>a&b,
OP_BXOR:(a,b)=>a^b,
OP_BOR:(a,b)=>a|b,
OP_SHR:(a,b)=>a>>>b,
OP_SHL:(a,b)=>a<<b,
OP_CONCAT:(a,b)=>String(a)+String(b),
OP_LT:(a,b)=>a<b,
OP_LE:(a,b)=>a<=b,
OP_GT:(a,b)=>a>b,
OP_GE:(a,b)=>a>=b,
OP_EQ:(a,b)=>a===b,
OP_NE:(a,b)=>a!==b,
OP_UNM:a=>-a,
OP_NOT:a=>a===undefined||a===false,
OP_LEN:a=>D.rawlen(a),
OP_BNOT:a=>~a,
OP_SETTABLE:(a,b,c)=>{
  if(D.rawget(a,b)===undefined){const f=D.getmetafield(a,"__newindex");if(f!==undefined){if(f instanceof LuaTable)D.OP_SETTABLE(f,b,c);else D.OP_CALL(f,[a,b,c]);return a;}}
  return D.rawset(a,b,c);
},
OP_GETTABLE:(a,b)=>{
  if(typeof a!=="string"){const v=D.rawget(a,b);if(v!==undefined)return v;}
  const f=D.getmetafield(a,"__index");if(f!==undefined)return f instanceof LuaTable?D.OP_GETTABLE(f,b):D.OP_CALL(f,[a,b])[0];
},
OP_NEWTABLE:()=>new LuaTable(),
OP_CALL:(a,b)=>a instanceof LuaFunction?a.fn(...b):a instanceof LuaTable?D.OP_CALL(D.getmetafield(a,"__call"),[a,...b]):[a.apply(undefined,b)],
OP_SELF:(a,b,c)=>{const f=D.OP_GETTABLE(a,b);return f instanceof LuaFunction?f.fn(a,...c):f instanceof LuaTable?D.OP_CALL(D.getmetafield(f,"__call"),[f,a,...c]):[f.apply(a,c)];},
OP_CLOSE:(a)=>{if(a!==undefined)D.OP_CALL(D.getmetafield(a,"__close"),[a]);},
OP_ADJUST:(a,b)=>{if(a.length<b)a[b-1]=undefined;else a.splice(b);},
OP_SETLIST:(a,b)=>{for(let i=0;i<b.length;++i)D.rawset(a,i+1,b[i]);},
};
const E=new LuaTable();
D.OP_SETTABLE(E,"dromozoa",D);
D.OP_SETTABLE(E,"globalThis",globalThis);
D.OP_SETTABLE(E,"package",D.OP_SETTABLE(D.OP_SETTABLE(D.OP_NEWTABLE(),"preload",D.OP_NEWTABLE()),"loaded",D.OP_NEWTABLE()));
D.OP_SETTABLE(E,"type",D.type);
D.OP_SETTABLE(E,"error",D.error);
D.OP_SETTABLE(E,"getmetatable",D.getmetatable);
D.OP_SETTABLE(E,"setmetatable",D.setmetatable);
D.OP_SETTABLE(E,"select",D.select);
D.OP_SETTABLE(E,"pcall",new LuaFunction((a,...b)=>{try{return[true,...D.OP_CALL(a,b)];}catch(e){return[false,e instanceof LuaError?e.msg:e.toString()];}}));

