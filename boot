#! /usr/bin/env lua

local json = require "dromozoa.commons.json"

local function lexer(source)


end

-- 最小限のPrattパーサを書いてみる。
-- まずは式だけとして、文はどうするのがよいか。
local function parser(tokens)
  -- null denotation
  local NUD = {
    ["INTEGER"] = function (parser, token)
      return {
        type = token[1];
        value = token[2];
      }
    end;
  }

  -- left denotation
  local LED = {
    ["+"] = {
      lbp = 10;
      rbp = 10;
      action = function (parser, left, token)
        local right = parser:parse(10)
        return {
          type = "binop";
          op = "+";
          left, right;
        }
      end;
    };

    ["-"] = {
      lbp = 10;
      rbp = 10;
      action = function (parser, left, token)
        local right = parser:parse(10)
        return {
          type = "binop";
          op = "-";
          left, right;
        }
      end;
    };

    ["*"] = {
      lbp = 20;
      rbp = 20;
      action = function (parser, left, token)
        local right = parser:parse(20)
        return {
          type = "binop";
          op = "*";
          left, right;
        }
      end;
    };
  }

  local index = 1
  local parser = {}

  function parser:peek()
    return tokens[index]
  end

  function parser:next()
    local token = self:peek()
    index = index + 1
    return token
  end

  function parser:parse(rbp)
    local token = self:next()
    local nud = assert(NUD[token[1]])
    local left = nud(self, token)

    while true do
      local look = self:peek()
      local inf = LED[look[1]]
      if not inf or inf.lbp <= rbp then
        break
      end
      local op_token = self:next()
      left = inf.action(self, left, op_token)
    end

    return left
  end

  return parser:parse(0)
end

-- local tokens = lexer "12+34*56-78"
local tokens = {
  { "INTEGER", 12 },
  { "+" },
  { "INTEGER", 34 },
  { "*" },
  { "INTEGER", 56 },
  { "-" },
  { "INTEGER", 78 },
  { "eof" },
}
local result = parser(tokens)
print(json.encode(result, { pretty = true }))
