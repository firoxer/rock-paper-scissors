-- From https://fennel-lang.org/

package.preload["fennel.repl"] = package.preload["fennel.repl"] or function(...)
  local utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local compiler = require("fennel.compiler")
  local specials = require("fennel.specials")
  local view = require("fennel.view")
  local unpack = (table.unpack or _G.unpack)
  local function default_read_chunk(parser_state)
    local function _620_()
      if (0 < parser_state["stack-size"]) then
        return ".."
      else
        return ">> "
      end
    end
    io.write(_620_())
    io.flush()
    local input = io.read()
    return (input and (input .. "\n"))
  end
  local function default_on_values(xs)
    io.write(table.concat(xs, "\9"))
    return io.write("\n")
  end
  local function default_on_error(errtype, err, lua_source)
    local function _622_()
      local _621_ = errtype
      if (_621_ == "Lua Compile") then
        return ("Bad code generated - likely a bug with the compiler:\n" .. "--- Generated Lua Start ---\n" .. lua_source .. "--- Generated Lua End ---\n")
      elseif (_621_ == "Runtime") then
        return (compiler.traceback(tostring(err), 4) .. "\n")
      elseif true then
        local _ = _621_
        return ("%s error: %s\n"):format(errtype, tostring(err))
      else
        return nil
      end
    end
    return io.write(_622_())
  end
  local save_source = " ___replLocals___['%s'] = %s"
  local function splice_save_locals(env, lua_source, scope)
    local spliced_source = {}
    local bind = "local %s = ___replLocals___['%s']"
    for line in lua_source:gmatch("([^\n]+)\n?") do
      table.insert(spliced_source, line)
    end
    for name in pairs(env.___replLocals___) do
      table.insert(spliced_source, 1, bind:format(name, name))
    end
    if ((1 < #spliced_source) and (spliced_source[#spliced_source]):match("^ *return .*$")) then
      for _, name in pairs(scope.manglings) do
        table.insert(spliced_source, #spliced_source, save_source:format(name, name))
      end
    else
    end
    return table.concat(spliced_source, "\n")
  end
  local function completer(env, scope, text)
    local max_items = 2000
    local seen = {}
    local matches = {}
    local input_fragment = text:gsub(".*[%s)(]+", "")
    local stop_looking_3f = false
    local function add_partials(input, tbl, prefix)
      local scope_first_3f = ((tbl == env) or (tbl == env.___replLocals___))
      local tbl_14_auto = matches
      local i_15_auto = #tbl_14_auto
      local function _625_()
        if scope_first_3f then
          return scope.manglings
        else
          return tbl
        end
      end
      for k, is_mangled in utils.allpairs(_625_()) do
        if (max_items <= #matches) then break end
        local val_16_auto
        do
          local lookup_k
          if scope_first_3f then
            lookup_k = is_mangled
          else
            lookup_k = k
          end
          if ((type(k) == "string") and (input == k:sub(0, #input)) and not seen[k] and ((":" ~= prefix:sub(-1)) or ("function" == type(tbl[lookup_k])))) then
            seen[k] = true
            val_16_auto = (prefix .. k)
          else
            val_16_auto = nil
          end
        end
        if (nil ~= val_16_auto) then
          i_15_auto = (i_15_auto + 1)
          do end (tbl_14_auto)[i_15_auto] = val_16_auto
        else
        end
      end
      return tbl_14_auto
    end
    local function descend(input, tbl, prefix, add_matches, method_3f)
      local splitter
      if method_3f then
        splitter = "^([^:]+):(.*)"
      else
        splitter = "^([^.]+)%.(.*)"
      end
      local head, tail = input:match(splitter)
      local raw_head = (scope.manglings[head] or head)
      if (type(tbl[raw_head]) == "table") then
        stop_looking_3f = true
        if method_3f then
          return add_partials(tail, tbl[raw_head], (prefix .. head .. ":"))
        else
          return add_matches(tail, tbl[raw_head], (prefix .. head))
        end
      else
        return nil
      end
    end
    local function add_matches(input, tbl, prefix)
      local prefix0
      if prefix then
        prefix0 = (prefix .. ".")
      else
        prefix0 = ""
      end
      if (not input:find("%.") and input:find(":")) then
        return descend(input, tbl, prefix0, add_matches, true)
      elseif not input:find("%.") then
        return add_partials(input, tbl, prefix0)
      else
        return descend(input, tbl, prefix0, add_matches, false)
      end
    end
    for _, source in ipairs({scope.specials, scope.macros, (env.___replLocals___ or {}), env, env._G}) do
      if stop_looking_3f then break end
      add_matches(input_fragment, source)
    end
    return matches
  end
  local commands = {}
  local function command_3f(input)
    return input:match("^%s*,")
  end
  local function command_docs()
    local _634_
    do
      local tbl_14_auto = {}
      local i_15_auto = #tbl_14_auto
      for name, f in pairs(commands) do
        local val_16_auto = ("  ,%s - %s"):format(name, ((compiler.metadata):get(f, "fnl/docstring") or "undocumented"))
        if (nil ~= val_16_auto) then
          i_15_auto = (i_15_auto + 1)
          do end (tbl_14_auto)[i_15_auto] = val_16_auto
        else
        end
      end
      _634_ = tbl_14_auto
    end
    return table.concat(_634_, "\n")
  end
  commands.help = function(_, _0, on_values)
    return on_values({("Welcome to Fennel.\nThis is the REPL where you can enter code to be evaluated.\nYou can also run these repl commands:\n\n" .. command_docs() .. "\n  ,exit - Leave the repl.\n\nUse ,doc something to see descriptions for individual macros and special forms.\n\nFor more information about the language, see https://fennel-lang.org/reference")})
  end
  do end (compiler.metadata):set(commands.help, "fnl/docstring", "Show this message.")
  local function reload(module_name, env, on_values, on_error)
    local _636_, _637_ = pcall(specials["load-code"]("return require(...)", env), module_name)
    if ((_636_ == true) and (nil ~= _637_)) then
      local old = _637_
      local _
      package.loaded[module_name] = nil
      _ = nil
      local ok, new = pcall(require, module_name)
      local new0
      if not ok then
        on_values({new})
        new0 = old
      else
        new0 = new
      end
      specials["macro-loaded"][module_name] = nil
      if ((type(old) == "table") and (type(new0) == "table")) then
        for k, v in pairs(new0) do
          old[k] = v
        end
        for k in pairs(old) do
          if (nil == (new0)[k]) then
            old[k] = nil
          else
          end
        end
        package.loaded[module_name] = old
      else
      end
      return on_values({"ok"})
    elseif ((_636_ == false) and (nil ~= _637_)) then
      local msg = _637_
      if (specials["macro-loaded"])[module_name] then
        specials["macro-loaded"][module_name] = nil
        return nil
      else
        local function _642_()
          local _641_ = msg:gsub("\n.*", "")
          return _641_
        end
        return on_error("Runtime", _642_())
      end
    else
      return nil
    end
  end
  local function run_command(read, on_error, f)
    local _645_, _646_, _647_ = pcall(read)
    if ((_645_ == true) and (_646_ == true) and (nil ~= _647_)) then
      local val = _647_
      return f(val)
    elseif (_645_ == false) then
      return on_error("Parse", "Couldn't parse input.")
    else
      return nil
    end
  end
  commands.reload = function(env, read, on_values, on_error)
    local function _649_(_241)
      return reload(tostring(_241), env, on_values, on_error)
    end
    return run_command(read, on_error, _649_)
  end
  do end (compiler.metadata):set(commands.reload, "fnl/docstring", "Reload the specified module.")
  commands.reset = function(env, _, on_values)
    env.___replLocals___ = {}
    return on_values({"ok"})
  end
  do end (compiler.metadata):set(commands.reset, "fnl/docstring", "Erase all repl-local scope.")
  commands.complete = function(env, read, on_values, on_error, scope, chars)
    local function _650_()
      return on_values(completer(env, scope, string.char(unpack(chars)):gsub(",complete +", ""):sub(1, -2)))
    end
    return run_command(read, on_error, _650_)
  end
  do end (compiler.metadata):set(commands.complete, "fnl/docstring", "Print all possible completions for a given input symbol.")
  local function apropos_2a(pattern, tbl, prefix, seen, names)
    for name, subtbl in pairs(tbl) do
      if (("string" == type(name)) and (package ~= subtbl)) then
        local _651_ = type(subtbl)
        if (_651_ == "function") then
          if ((prefix .. name)):match(pattern) then
            table.insert(names, (prefix .. name))
          else
          end
        elseif (_651_ == "table") then
          if not seen[subtbl] then
            local _654_
            do
              local _653_ = seen
              _653_[subtbl] = true
              _654_ = _653_
            end
            apropos_2a(pattern, subtbl, (prefix .. name:gsub("%.", "/") .. "."), _654_, names)
          else
          end
        else
        end
      else
      end
    end
    return names
  end
  local function apropos(pattern)
    local names = apropos_2a(pattern, package.loaded, "", {}, {})
    local tbl_14_auto = {}
    local i_15_auto = #tbl_14_auto
    for _, name in ipairs(names) do
      local val_16_auto = name:gsub("^_G%.", "")
      if (nil ~= val_16_auto) then
        i_15_auto = (i_15_auto + 1)
        do end (tbl_14_auto)[i_15_auto] = val_16_auto
      else
      end
    end
    return tbl_14_auto
  end
  commands.apropos = function(_env, read, on_values, on_error, _scope)
    local function _659_(_241)
      return on_values(apropos(tostring(_241)))
    end
    return run_command(read, on_error, _659_)
  end
  do end (compiler.metadata):set(commands.apropos, "fnl/docstring", "Print all functions matching a pattern in all loaded modules.")
  local function apropos_follow_path(path)
    local paths
    do
      local tbl_14_auto = {}
      local i_15_auto = #tbl_14_auto
      for p in path:gmatch("[^%.]+") do
        local val_16_auto = p
        if (nil ~= val_16_auto) then
          i_15_auto = (i_15_auto + 1)
          do end (tbl_14_auto)[i_15_auto] = val_16_auto
        else
        end
      end
      paths = tbl_14_auto
    end
    local tgt = package.loaded
    for _, path0 in ipairs(paths) do
      if (nil == tgt) then break end
      local _662_
      do
        local _661_ = path0:gsub("%/", ".")
        _662_ = _661_
      end
      tgt = tgt[_662_]
    end
    return tgt
  end
  local function apropos_doc(pattern)
    local tbl_14_auto = {}
    local i_15_auto = #tbl_14_auto
    for _, path in ipairs(apropos(".*")) do
      local val_16_auto
      do
        local tgt = apropos_follow_path(path)
        if ("function" == type(tgt)) then
          local _663_ = (compiler.metadata):get(tgt, "fnl/docstring")
          if (nil ~= _663_) then
            local docstr = _663_
            val_16_auto = (docstr:match(pattern) and path)
          else
            val_16_auto = nil
          end
        else
          val_16_auto = nil
        end
      end
      if (nil ~= val_16_auto) then
        i_15_auto = (i_15_auto + 1)
        do end (tbl_14_auto)[i_15_auto] = val_16_auto
      else
      end
    end
    return tbl_14_auto
  end
  commands["apropos-doc"] = function(_env, read, on_values, on_error, _scope)
    local function _667_(_241)
      return on_values(apropos_doc(tostring(_241)))
    end
    return run_command(read, on_error, _667_)
  end
  do end (compiler.metadata):set(commands["apropos-doc"], "fnl/docstring", "Print all functions that match the pattern in their docs")
  local function apropos_show_docs(on_values, pattern)
    for _, path in ipairs(apropos(pattern)) do
      local tgt = apropos_follow_path(path)
      if (("function" == type(tgt)) and (compiler.metadata):get(tgt, "fnl/docstring")) then
        on_values(specials.doc(tgt, path))
        on_values()
      else
      end
    end
    return nil
  end
  commands["apropos-show-docs"] = function(_env, read, on_values, on_error)
    local function _669_(_241)
      return apropos_show_docs(on_values, tostring(_241))
    end
    return run_command(read, on_error, _669_)
  end
  do end (compiler.metadata):set(commands["apropos-show-docs"], "fnl/docstring", "Print all documentations matching a pattern in function name")
  local function resolve(identifier, _670_, scope)
    local _arg_671_ = _670_
    local ___replLocals___ = _arg_671_["___replLocals___"]
    local env = _arg_671_
    local e
    local function _672_(_241, _242)
      return (___replLocals___[_242] or env[_242])
    end
    e = setmetatable({}, {__index = _672_})
    local _673_, _674_ = pcall(compiler["compile-string"], tostring(identifier), {scope = scope})
    if ((_673_ == true) and (nil ~= _674_)) then
      local code = _674_
      return specials["load-code"](code, e)()
    else
      return nil
    end
  end
  commands.find = function(env, read, on_values, on_error, scope)
    local function _676_(_241)
      local _677_
      do
        local _678_ = utils["sym?"](_241)
        if (nil ~= _678_) then
          local _679_ = resolve(_678_, env, scope)
          if (nil ~= _679_) then
            _677_ = debug.getinfo(_679_)
          else
            _677_ = _679_
          end
        else
          _677_ = _678_
        end
      end
      if ((_G.type(_677_) == "table") and ((_677_).what == "Lua") and (nil ~= (_677_).source) and (nil ~= (_677_).linedefined) and (nil ~= (_677_).short_src)) then
        local source = (_677_).source
        local line = (_677_).linedefined
        local src = (_677_).short_src
        local fnlsrc
        do
          local t_682_ = compiler.sourcemap
          if (nil ~= t_682_) then
            t_682_ = (t_682_)[source]
          else
          end
          if (nil ~= t_682_) then
            t_682_ = (t_682_)[line]
          else
          end
          if (nil ~= t_682_) then
            t_682_ = (t_682_)[2]
          else
          end
          fnlsrc = t_682_
        end
        return on_values({string.format("%s:%s", src, (fnlsrc or line))})
      elseif (_677_ == nil) then
        return on_error("Repl", "Unknown value")
      elseif true then
        local _ = _677_
        return on_error("Repl", "No source info")
      else
        return nil
      end
    end
    return run_command(read, on_error, _676_)
  end
  do end (compiler.metadata):set(commands.find, "fnl/docstring", "Print the filename and line number for a given function")
  commands.doc = function(env, read, on_values, on_error, scope)
    local function _687_(_241)
      local name = tostring(_241)
      local path = (utils["multi-sym?"](name) or {name})
      local ok_3f, target = nil, nil
      local function _688_()
        return (utils["get-in"](scope.specials, path) or utils["get-in"](scope.macros, path) or resolve(name, env, scope))
      end
      ok_3f, target = pcall(_688_)
      if ok_3f then
        return on_values({specials.doc(target, name)})
      else
        return on_error("Repl", "Could not resolve value for docstring lookup")
      end
    end
    return run_command(read, on_error, _687_)
  end
  do end (compiler.metadata):set(commands.doc, "fnl/docstring", "Print the docstring and arglist for a function, macro, or special form.")
  commands.compile = function(env, read, on_values, on_error, scope)
    local function _690_(_241)
      local allowedGlobals = specials["current-global-names"](env)
      local ok_3f, result = pcall(compiler.compile, _241, {env = env, scope = scope, allowedGlobals = allowedGlobals})
      if ok_3f then
        return on_values({result})
      else
        return on_error("Repl", ("Error compiling expression: " .. result))
      end
    end
    return run_command(read, on_error, _690_)
  end
  do end (compiler.metadata):set(commands.compile, "fnl/docstring", "compiles the expression into lua and prints the result.")
  local function load_plugin_commands(plugins)
    for _, plugin in ipairs((plugins or {})) do
      for name, f in pairs(plugin) do
        local _692_ = name:match("^repl%-command%-(.*)")
        if (nil ~= _692_) then
          local cmd_name = _692_
          commands[cmd_name] = (commands[cmd_name] or f)
        else
        end
      end
    end
    return nil
  end
  local function run_command_loop(input, read, loop, env, on_values, on_error, scope, chars)
    local command_name = input:match(",([^%s/]+)")
    do
      local _694_ = commands[command_name]
      if (nil ~= _694_) then
        local command = _694_
        command(env, read, on_values, on_error, scope, chars)
      elseif true then
        local _ = _694_
        if ("exit" ~= command_name) then
          on_values({"Unknown command", command_name})
        else
        end
      else
      end
    end
    if ("exit" ~= command_name) then
      return loop()
    else
      return nil
    end
  end
  local function try_readline_21(opts, ok, readline)
    if ok then
      if readline.set_readline_name then
        readline.set_readline_name("fennel")
      else
      end
      do
        local rl_opts
        do
          local tbl_11_auto = {keeplines = 1000, histfile = ""}
          for k, v in pairs(readline.set_options({})) do
            local _699_, _700_ = k, v
            if ((nil ~= _699_) and (nil ~= _700_)) then
              local k_12_auto = _699_
              local v_13_auto = _700_
              tbl_11_auto[k_12_auto] = v_13_auto
            else
            end
          end
          rl_opts = tbl_11_auto
        end
        readline.set_options(rl_opts)
      end
      opts.readChunk = function(parser_state)
        local prompt
        if (0 < parser_state["stack-size"]) then
          prompt = ".. "
        else
          prompt = ">> "
        end
        local str = readline.readline(prompt)
        if str then
          return (str .. "\n")
        else
          return nil
        end
      end
      local completer0 = nil
      opts.registerCompleter = function(repl_completer)
        completer0 = repl_completer
        return nil
      end
      local function repl_completer(text, from, to)
        if completer0 then
          readline.set_completion_append_character("")
          return completer0(text:sub(from, to))
        else
          return {}
        end
      end
      readline.set_complete_function(repl_completer)
      return readline
    else
      return nil
    end
  end
  local function should_use_readline_3f(opts)
    return (("dumb" ~= os.getenv("TERM")) and not opts.readChunk and not opts.registerCompleter)
  end
  local function repl(_3foptions)
    local old_root_options = utils.root.options
    local opts = ((_3foptions and utils.copy(_3foptions)) or {})
    local readline = (should_use_readline_3f(opts) and try_readline_21(opts, pcall(require, "readline")))
    local env = specials["wrap-env"]((opts.env or rawget(_G, "_ENV") or _G))
    local save_locals_3f = (opts.saveLocals ~= false)
    local read_chunk = (opts.readChunk or default_read_chunk)
    local on_values = (opts.onValues or default_on_values)
    local on_error = (opts.onError or default_on_error)
    local pp = (opts.pp or view)
    local byte_stream, clear_stream = parser.granulate(read_chunk)
    local chars = {}
    local read, reset = nil, nil
    local function _706_(parser_state)
      local c = byte_stream(parser_state)
      table.insert(chars, c)
      return c
    end
    read, reset = parser.parser(_706_)
    opts.env, opts.scope = env, compiler["make-scope"]()
    opts.useMetadata = (opts.useMetadata ~= false)
    if (opts.allowedGlobals == nil) then
      opts.allowedGlobals = specials["current-global-names"](env)
    else
    end
    if opts.registerCompleter then
      local function _710_()
        local _708_ = env
        local _709_ = opts.scope
        local function _711_(...)
          return completer(_708_, _709_, ...)
        end
        return _711_
      end
      opts.registerCompleter(_710_())
    else
    end
    load_plugin_commands(opts.plugins)
    if save_locals_3f then
      local function newindex(t, k, v)
        if opts.scope.unmanglings[k] then
          return rawset(t, k, v)
        else
          return nil
        end
      end
      env.___replLocals___ = setmetatable({}, {__newindex = newindex})
    else
    end
    local function print_values(...)
      local vals = {...}
      local out = {}
      env._, env.__ = vals[1], vals
      for i = 1, select("#", ...) do
        table.insert(out, pp(vals[i]))
      end
      return on_values(out)
    end
    local function loop()
      for k in pairs(chars) do
        chars[k] = nil
      end
      reset()
      local ok, not_eof_3f, x = pcall(read)
      local src_string = string.char(unpack(chars))
      if not ok then
        on_error("Parse", not_eof_3f)
        clear_stream()
        return loop()
      elseif command_3f(src_string) then
        return run_command_loop(src_string, read, loop, env, on_values, on_error, opts.scope, chars)
      else
        if not_eof_3f then
          do
            local _715_, _716_ = nil, nil
            local function _718_()
              local _717_ = opts
              _717_["source"] = src_string
              return _717_
            end
            _715_, _716_ = pcall(compiler.compile, x, _718_())
            if ((_715_ == false) and (nil ~= _716_)) then
              local msg = _716_
              clear_stream()
              on_error("Compile", msg)
            elseif ((_715_ == true) and (nil ~= _716_)) then
              local src = _716_
              local src0
              if save_locals_3f then
                src0 = splice_save_locals(env, src, opts.scope)
              else
                src0 = src
              end
              local _720_, _721_ = pcall(specials["load-code"], src0, env)
              if ((_720_ == false) and (nil ~= _721_)) then
                local msg = _721_
                clear_stream()
                on_error("Lua Compile", msg, src0)
              elseif (true and (nil ~= _721_)) then
                local _ = _720_
                local chunk = _721_
                local function _722_()
                  return print_values(chunk())
                end
                local function _723_()
                  local function _724_(...)
                    return on_error("Runtime", ...)
                  end
                  return _724_
                end
                xpcall(_722_, _723_())
              else
              end
            else
            end
          end
          utils.root.options = old_root_options
          return loop()
        else
          return nil
        end
      end
    end
    loop()
    if readline then
      return readline.save_history()
    else
      return nil
    end
  end
  return repl
end
package.preload["fennel.specials"] = package.preload["fennel.specials"] or function(...)
  local utils = require("fennel.utils")
  local view = require("fennel.view")
  local parser = require("fennel.parser")
  local compiler = require("fennel.compiler")
  local unpack = (table.unpack or _G.unpack)
  local SPECIALS = compiler.scopes.global.specials
  local function wrap_env(env)
    local function _415_(_, key)
      if utils["string?"](key) then
        return env[compiler["global-unmangling"](key)]
      else
        return env[key]
      end
    end
    local function _417_(_, key, value)
      if utils["string?"](key) then
        env[compiler["global-unmangling"](key)] = value
        return nil
      else
        env[key] = value
        return nil
      end
    end
    local function _419_()
      local function putenv(k, v)
        local _420_
        if utils["string?"](k) then
          _420_ = compiler["global-unmangling"](k)
        else
          _420_ = k
        end
        return _420_, v
      end
      return next, utils.kvmap(env, putenv), nil
    end
    return setmetatable({}, {__index = _415_, __newindex = _417_, __pairs = _419_})
  end
  local function current_global_names(_3fenv)
    local mt
    do
      local _422_ = getmetatable(_3fenv)
      if ((_G.type(_422_) == "table") and (nil ~= (_422_).__pairs)) then
        local mtpairs = (_422_).__pairs
        local tbl_11_auto = {}
        for k, v in mtpairs(_3fenv) do
          local _423_, _424_ = k, v
          if ((nil ~= _423_) and (nil ~= _424_)) then
            local k_12_auto = _423_
            local v_13_auto = _424_
            tbl_11_auto[k_12_auto] = v_13_auto
          else
          end
        end
        mt = tbl_11_auto
      elseif (_422_ == nil) then
        mt = (_3fenv or _G)
      else
        mt = nil
      end
    end
    return (mt and utils.kvmap(mt, compiler["global-unmangling"]))
  end
  local function load_code(code, _3fenv, _3ffilename)
    local env = (_3fenv or rawget(_G, "_ENV") or _G)
    local _427_, _428_ = rawget(_G, "setfenv"), rawget(_G, "loadstring")
    if ((nil ~= _427_) and (nil ~= _428_)) then
      local setfenv = _427_
      local loadstring = _428_
      local f = assert(loadstring(code, _3ffilename))
      local _429_ = f
      setfenv(_429_, env)
      return _429_
    elseif true then
      local _ = _427_
      return assert(load(code, _3ffilename, "t", env))
    else
      return nil
    end
  end
  local function doc_2a(tgt, name)
    if not tgt then
      return (name .. " not found")
    else
      local docstring = (((compiler.metadata):get(tgt, "fnl/docstring") or "#<undocumented>")):gsub("\n$", ""):gsub("\n", "\n  ")
      local mt = getmetatable(tgt)
      if ((type(tgt) == "function") or ((type(mt) == "table") and (type(mt.__call) == "function"))) then
        local arglist = table.concat(((compiler.metadata):get(tgt, "fnl/arglist") or {"#<unknown-arguments>"}), " ")
        local _431_
        if (0 < #arglist) then
          _431_ = " "
        else
          _431_ = ""
        end
        return string.format("(%s%s%s)\n  %s", name, _431_, arglist, docstring)
      else
        return string.format("%s\n  %s", name, docstring)
      end
    end
  end
  local function doc_special(name, arglist, docstring, body_form_3f)
    compiler.metadata[SPECIALS[name]] = {["fnl/arglist"] = arglist, ["fnl/docstring"] = docstring, ["fnl/body-form?"] = body_form_3f}
    return nil
  end
  local function compile_do(ast, scope, parent, _3fstart)
    local start = (_3fstart or 2)
    local len = #ast
    local sub_scope = compiler["make-scope"](scope)
    for i = start, len do
      compiler.compile1(ast[i], sub_scope, parent, {nval = 0})
    end
    return nil
  end
  SPECIALS["do"] = function(ast, scope, parent, opts, _3fstart, _3fchunk, _3fsub_scope, _3fpre_syms)
    local start = (_3fstart or 2)
    local sub_scope = (_3fsub_scope or compiler["make-scope"](scope))
    local chunk = (_3fchunk or {})
    local len = #ast
    local retexprs = {returned = true}
    local function compile_body(outer_target, outer_tail, outer_retexprs)
      if (len < start) then
        compiler.compile1(nil, sub_scope, chunk, {tail = outer_tail, target = outer_target})
      else
        for i = start, len do
          local subopts = {nval = (((i ~= len) and 0) or opts.nval), tail = (((i == len) and outer_tail) or nil), target = (((i == len) and outer_target) or nil)}
          local _ = utils["propagate-options"](opts, subopts)
          local subexprs = compiler.compile1(ast[i], sub_scope, chunk, subopts)
          if (i ~= len) then
            compiler["keep-side-effects"](subexprs, parent, nil, ast[i])
          else
          end
        end
      end
      compiler.emit(parent, chunk, ast)
      compiler.emit(parent, "end", ast)
      utils.hook("do", ast, sub_scope)
      return (outer_retexprs or retexprs)
    end
    if (opts.target or (opts.nval == 0) or opts.tail) then
      compiler.emit(parent, "do", ast)
      return compile_body(opts.target, opts.tail)
    elseif opts.nval then
      local syms = {}
      for i = 1, opts.nval do
        local s = ((_3fpre_syms and (_3fpre_syms)[i]) or compiler.gensym(scope))
        do end (syms)[i] = s
        retexprs[i] = utils.expr(s, "sym")
      end
      local outer_target = table.concat(syms, ", ")
      compiler.emit(parent, string.format("local %s", outer_target), ast)
      compiler.emit(parent, "do", ast)
      return compile_body(outer_target, opts.tail)
    else
      local fname = compiler.gensym(scope)
      local fargs
      if scope.vararg then
        fargs = "..."
      else
        fargs = ""
      end
      compiler.emit(parent, string.format("local function %s(%s)", fname, fargs), ast)
      return compile_body(nil, true, utils.expr((fname .. "(" .. fargs .. ")"), "statement"))
    end
  end
  doc_special("do", {"..."}, "Evaluate multiple forms; return last value.", true)
  SPECIALS.values = function(ast, scope, parent)
    local len = #ast
    local exprs = {}
    for i = 2, len do
      local subexprs = compiler.compile1(ast[i], scope, parent, {nval = ((i ~= len) and 1)})
      table.insert(exprs, subexprs[1])
      if (i == len) then
        for j = 2, #subexprs do
          table.insert(exprs, subexprs[j])
        end
      else
      end
    end
    return exprs
  end
  doc_special("values", {"..."}, "Return multiple values from a function. Must be in tail position.")
  local function deep_tostring(x, key_3f)
    if utils["list?"](x) then
      local _440_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, v in ipairs(x) do
          local val_16_auto = deep_tostring(v)
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _440_ = tbl_14_auto
      end
      return ("(" .. table.concat(_440_, " ") .. ")")
    elseif utils["sequence?"](x) then
      local _442_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, v in ipairs(x) do
          local val_16_auto = deep_tostring(v)
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _442_ = tbl_14_auto
      end
      return ("[" .. table.concat(_442_, " ") .. "]")
    elseif utils["table?"](x) then
      local _444_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for k, v in utils.stablepairs(x) do
          local val_16_auto = (deep_tostring(k, true) .. " " .. deep_tostring(v))
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _444_ = tbl_14_auto
      end
      return ("{" .. table.concat(_444_, " ") .. "}")
    elseif (key_3f and utils["string?"](x) and x:find("^[-%w?\\^_!$%&*+./@:|<=>]+$")) then
      return (":" .. x)
    elseif utils["string?"](x) then
      return string.format("%q", x):gsub("\\\"", "\\\\\""):gsub("\"", "\\\"")
    else
      return tostring(x)
    end
  end
  local function set_fn_metadata(arg_list, docstring, parent, fn_name)
    if utils.root.options.useMetadata then
      local args
      local function _447_(_241)
        return ("\"%s\""):format(deep_tostring(_241))
      end
      args = utils.map(arg_list, _447_)
      local meta_fields = {"\"fnl/arglist\"", ("{" .. table.concat(args, ", ") .. "}")}
      if docstring then
        table.insert(meta_fields, "\"fnl/docstring\"")
        table.insert(meta_fields, ("\"" .. docstring:gsub("%s+$", ""):gsub("\\", "\\\\"):gsub("\n", "\\n"):gsub("\"", "\\\"") .. "\""))
      else
      end
      local meta_str = ("require(\"%s\").metadata"):format((utils.root.options.moduleName or "fennel"))
      return compiler.emit(parent, ("pcall(function() %s:setall(%s, %s) end)"):format(meta_str, fn_name, table.concat(meta_fields, ", ")))
    else
      return nil
    end
  end
  local function get_fn_name(ast, scope, fn_name, multi)
    if (fn_name and (fn_name[1] ~= "nil")) then
      local _450_
      if not multi then
        _450_ = compiler["declare-local"](fn_name, {}, scope, ast)
      else
        _450_ = (compiler["symbol-to-expression"](fn_name, scope))[1]
      end
      return _450_, not multi, 3
    else
      return nil, true, 2
    end
  end
  local function compile_named_fn(ast, f_scope, f_chunk, parent, index, fn_name, local_3f, arg_name_list, f_metadata)
    for i = (index + 1), #ast do
      compiler.compile1(ast[i], f_scope, f_chunk, {nval = (((i ~= #ast) and 0) or nil), tail = (i == #ast)})
    end
    local _453_
    if local_3f then
      _453_ = "local function %s(%s)"
    else
      _453_ = "%s = function(%s)"
    end
    compiler.emit(parent, string.format(_453_, fn_name, table.concat(arg_name_list, ", ")), ast)
    compiler.emit(parent, f_chunk, ast)
    compiler.emit(parent, "end", ast)
    set_fn_metadata(f_metadata["fnl/arglist"], f_metadata["fnl/docstring"], parent, fn_name)
    utils.hook("fn", ast, f_scope)
    return utils.expr(fn_name, "sym")
  end
  local function compile_anonymous_fn(ast, f_scope, f_chunk, parent, index, arg_name_list, f_metadata, scope)
    local fn_name = compiler.gensym(scope)
    return compile_named_fn(ast, f_scope, f_chunk, parent, index, fn_name, true, arg_name_list, f_metadata)
  end
  local function get_function_metadata(ast, arg_list, index)
    local f_metadata = {["fnl/arglist"] = arg_list}
    local index_2a = (index + 1)
    local expr = ast[index_2a]
    if (utils["string?"](expr) and (index_2a < #ast)) then
      local _456_
      do
        local _455_ = f_metadata
        _455_["fnl/docstring"] = expr
        _456_ = _455_
      end
      return _456_, index_2a
    elseif (utils["table?"](expr) and (index_2a < #ast)) then
      local _457_
      do
        local tbl_11_auto = f_metadata
        for k, v in pairs(expr) do
          local _458_, _459_ = k, v
          if ((nil ~= _458_) and (nil ~= _459_)) then
            local k_12_auto = _458_
            local v_13_auto = _459_
            tbl_11_auto[k_12_auto] = v_13_auto
          else
          end
        end
        _457_ = tbl_11_auto
      end
      return _457_, index_2a
    else
      return f_metadata, index
    end
  end
  SPECIALS.fn = function(ast, scope, parent)
    local f_scope
    do
      local _462_ = compiler["make-scope"](scope)
      do end (_462_)["vararg"] = false
      f_scope = _462_
    end
    local f_chunk = {}
    local fn_sym = utils["sym?"](ast[2])
    local multi = (fn_sym and utils["multi-sym?"](fn_sym[1]))
    local fn_name, local_3f, index = get_fn_name(ast, scope, fn_sym, multi)
    local arg_list = compiler.assert(utils["table?"](ast[index]), "expected parameters table", ast)
    compiler.assert((not multi or not multi["multi-sym-method-call"]), ("unexpected multi symbol " .. tostring(fn_name)), fn_sym)
    local function get_arg_name(arg)
      if utils["varg?"](arg) then
        compiler.assert((arg == arg_list[#arg_list]), "expected vararg as last parameter", ast)
        f_scope.vararg = true
        return "..."
      elseif (utils["sym?"](arg) and (tostring(arg) ~= "nil") and not utils["multi-sym?"](tostring(arg))) then
        return compiler["declare-local"](arg, {}, f_scope, ast)
      elseif utils["table?"](arg) then
        local raw = utils.sym(compiler.gensym(scope))
        local declared = compiler["declare-local"](raw, {}, f_scope, ast)
        compiler.destructure(arg, raw, ast, f_scope, f_chunk, {declaration = true, nomulti = true, symtype = "arg"})
        return declared
      else
        return compiler.assert(false, ("expected symbol for function parameter: %s"):format(tostring(arg)), ast[index])
      end
    end
    local arg_name_list = utils.map(arg_list, get_arg_name)
    local f_metadata, index0 = get_function_metadata(ast, arg_list, index)
    if fn_name then
      return compile_named_fn(ast, f_scope, f_chunk, parent, index0, fn_name, local_3f, arg_name_list, f_metadata)
    else
      return compile_anonymous_fn(ast, f_scope, f_chunk, parent, index0, arg_name_list, f_metadata, scope)
    end
  end
  doc_special("fn", {"name?", "args", "docstring?", "..."}, "Function syntax. May optionally include a name and docstring or a metadata table.\nIf a name is provided, the function will be bound in the current scope.\nWhen called with the wrong number of args, excess args will be discarded\nand lacking args will be nil, use lambda for arity-checked functions.", true)
  SPECIALS.lua = function(ast, _, parent)
    compiler.assert(((#ast == 2) or (#ast == 3)), "expected 1 or 2 arguments", ast)
    local _466_
    do
      local _465_ = utils["sym?"](ast[2])
      if (nil ~= _465_) then
        _466_ = tostring(_465_)
      else
        _466_ = _465_
      end
    end
    if ("nil" ~= _466_) then
      table.insert(parent, {ast = ast, leaf = tostring(ast[2])})
    else
    end
    local _470_
    do
      local _469_ = utils["sym?"](ast[3])
      if (nil ~= _469_) then
        _470_ = tostring(_469_)
      else
        _470_ = _469_
      end
    end
    if ("nil" ~= _470_) then
      return tostring(ast[3])
    else
      return nil
    end
  end
  local function dot(ast, scope, parent)
    compiler.assert((1 < #ast), "expected table argument", ast)
    local len = #ast
    local _let_473_ = compiler.compile1(ast[2], scope, parent, {nval = 1})
    local lhs = _let_473_[1]
    if (len == 2) then
      return tostring(lhs)
    else
      local indices = {}
      for i = 3, len do
        local index = ast[i]
        if (utils["string?"](index) and utils["valid-lua-identifier?"](index)) then
          table.insert(indices, ("." .. index))
        else
          local _let_474_ = compiler.compile1(index, scope, parent, {nval = 1})
          local index0 = _let_474_[1]
          table.insert(indices, ("[" .. tostring(index0) .. "]"))
        end
      end
      if (tostring(lhs):find("[{\"0-9]") or ("nil" == tostring(lhs))) then
        return ("(" .. tostring(lhs) .. ")" .. table.concat(indices))
      else
        return (tostring(lhs) .. table.concat(indices))
      end
    end
  end
  SPECIALS["."] = dot
  doc_special(".", {"tbl", "key1", "..."}, "Look up key1 in tbl table. If more args are provided, do a nested lookup.")
  SPECIALS.global = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceglobal = true, nomulti = true, symtype = "global"})
    return nil
  end
  doc_special("global", {"name", "val"}, "Set name as a global with val.")
  SPECIALS.set = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    compiler.destructure(ast[2], ast[3], ast, scope, parent, {noundef = true, symtype = "set"})
    return nil
  end
  doc_special("set", {"name", "val"}, "Set a local variable to a new value. Only works on locals using var.")
  local function set_forcibly_21_2a(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceset = true, symtype = "set"})
    return nil
  end
  SPECIALS["set-forcibly!"] = set_forcibly_21_2a
  local function local_2a(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, nomulti = true, symtype = "local"})
    return nil
  end
  SPECIALS["local"] = local_2a
  doc_special("local", {"name", "val"}, "Introduce new top-level immutable local.")
  SPECIALS.var = function(ast, scope, parent)
    compiler.assert((#ast == 3), "expected name and value", ast)
    compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, isvar = true, nomulti = true, symtype = "var"})
    return nil
  end
  doc_special("var", {"name", "val"}, "Introduce new mutable local.")
  local function kv_3f(t)
    local _478_
    do
      local tbl_14_auto = {}
      local i_15_auto = #tbl_14_auto
      for k in pairs(t) do
        local val_16_auto
        if ("number" ~= type(k)) then
          val_16_auto = k
        else
          val_16_auto = nil
        end
        if (nil ~= val_16_auto) then
          i_15_auto = (i_15_auto + 1)
          do end (tbl_14_auto)[i_15_auto] = val_16_auto
        else
        end
      end
      _478_ = tbl_14_auto
    end
    return (_478_)[1]
  end
  SPECIALS.let = function(ast, scope, parent, opts)
    local bindings = ast[2]
    local pre_syms = {}
    compiler.assert((utils["table?"](bindings) and not kv_3f(bindings)), "expected binding sequence", bindings)
    compiler.assert(((#bindings % 2) == 0), "expected even number of name/value bindings", ast[2])
    compiler.assert((3 <= #ast), "expected body expression", ast[1])
    for _ = 1, (opts.nval or 0) do
      table.insert(pre_syms, compiler.gensym(scope))
    end
    local sub_scope = compiler["make-scope"](scope)
    local sub_chunk = {}
    for i = 1, #bindings, 2 do
      compiler.destructure(bindings[i], bindings[(i + 1)], ast, sub_scope, sub_chunk, {declaration = true, nomulti = true, symtype = "let"})
    end
    return SPECIALS["do"](ast, scope, parent, opts, 3, sub_chunk, sub_scope, pre_syms)
  end
  doc_special("let", {"[name1 val1 ... nameN valN]", "..."}, "Introduces a new scope in which a given set of local bindings are used.", true)
  local function get_prev_line(parent)
    if ("table" == type(parent)) then
      return get_prev_line((parent.leaf or parent[#parent]))
    else
      return (parent or "")
    end
  end
  local function disambiguate_3f(rootstr, parent)
    local function _483_()
      local _482_ = get_prev_line(parent)
      if (nil ~= _482_) then
        local prev_line = _482_
        return prev_line:match("%)$")
      else
        return nil
      end
    end
    return (rootstr:match("^{") or _483_())
  end
  SPECIALS.tset = function(ast, scope, parent)
    compiler.assert((3 < #ast), "expected table, key, and value arguments", ast)
    local root = (compiler.compile1(ast[2], scope, parent, {nval = 1}))[1]
    local keys = {}
    for i = 3, (#ast - 1) do
      local _let_485_ = compiler.compile1(ast[i], scope, parent, {nval = 1})
      local key = _let_485_[1]
      table.insert(keys, tostring(key))
    end
    local value = (compiler.compile1(ast[#ast], scope, parent, {nval = 1}))[1]
    local rootstr = tostring(root)
    local fmtstr
    if disambiguate_3f(rootstr, parent) then
      fmtstr = "do end (%s)[%s] = %s"
    else
      fmtstr = "%s[%s] = %s"
    end
    return compiler.emit(parent, fmtstr:format(rootstr, table.concat(keys, "]["), tostring(value)), ast)
  end
  doc_special("tset", {"tbl", "key1", "...", "keyN", "val"}, "Set the value of a table field. Can take additional keys to set\nnested values, but all parents must contain an existing table.")
  local function calculate_target(scope, opts)
    if not (opts.tail or opts.target or opts.nval) then
      return "iife", true, nil
    elseif (opts.nval and (opts.nval ~= 0) and not opts.target) then
      local accum = {}
      local target_exprs = {}
      for i = 1, opts.nval do
        local s = compiler.gensym(scope)
        do end (accum)[i] = s
        target_exprs[i] = utils.expr(s, "sym")
      end
      return "target", opts.tail, table.concat(accum, ", "), target_exprs
    else
      return "none", opts.tail, opts.target
    end
  end
  local function if_2a(ast, scope, parent, opts)
    compiler.assert((2 < #ast), "expected condition and body", ast)
    local do_scope = compiler["make-scope"](scope)
    local branches = {}
    local wrapper, inner_tail, inner_target, target_exprs = calculate_target(scope, opts)
    local body_opts = {nval = opts.nval, tail = inner_tail, target = inner_target}
    local function compile_body(i)
      local chunk = {}
      local cscope = compiler["make-scope"](do_scope)
      compiler["keep-side-effects"](compiler.compile1(ast[i], cscope, chunk, body_opts), chunk, nil, ast[i])
      return {chunk = chunk, scope = cscope}
    end
    if (1 == (#ast % 2)) then
      table.insert(ast, utils.sym("nil"))
    else
    end
    for i = 2, (#ast - 1), 2 do
      local condchunk = {}
      local res = compiler.compile1(ast[i], do_scope, condchunk, {nval = 1})
      local cond = res[1]
      local branch = compile_body((i + 1))
      branch.cond = cond
      branch.condchunk = condchunk
      branch.nested = ((i ~= 2) and (next(condchunk, nil) == nil))
      table.insert(branches, branch)
    end
    local else_branch = compile_body(#ast)
    local s = compiler.gensym(scope)
    local buffer = {}
    local last_buffer = buffer
    for i = 1, #branches do
      local branch = branches[i]
      local fstr
      if not branch.nested then
        fstr = "if %s then"
      else
        fstr = "elseif %s then"
      end
      local cond = tostring(branch.cond)
      local cond_line = fstr:format(cond)
      if branch.nested then
        compiler.emit(last_buffer, branch.condchunk, ast)
      else
        for _, v in ipairs(branch.condchunk) do
          compiler.emit(last_buffer, v, ast)
        end
      end
      compiler.emit(last_buffer, cond_line, ast)
      compiler.emit(last_buffer, branch.chunk, ast)
      if (i == #branches) then
        compiler.emit(last_buffer, "else", ast)
        compiler.emit(last_buffer, else_branch.chunk, ast)
        compiler.emit(last_buffer, "end", ast)
      elseif not (branches[(i + 1)]).nested then
        local next_buffer = {}
        compiler.emit(last_buffer, "else", ast)
        compiler.emit(last_buffer, next_buffer, ast)
        compiler.emit(last_buffer, "end", ast)
        last_buffer = next_buffer
      else
      end
    end
    if (wrapper == "iife") then
      local iifeargs = ((scope.vararg and "...") or "")
      compiler.emit(parent, ("local function %s(%s)"):format(tostring(s), iifeargs), ast)
      compiler.emit(parent, buffer, ast)
      compiler.emit(parent, "end", ast)
      return utils.expr(("%s(%s)"):format(tostring(s), iifeargs), "statement")
    elseif (wrapper == "none") then
      for i = 1, #buffer do
        compiler.emit(parent, buffer[i], ast)
      end
      return {returned = true}
    else
      compiler.emit(parent, ("local %s"):format(inner_target), ast)
      for i = 1, #buffer do
        compiler.emit(parent, buffer[i], ast)
      end
      return target_exprs
    end
  end
  SPECIALS["if"] = if_2a
  doc_special("if", {"cond1", "body1", "...", "condN", "bodyN"}, "Conditional form.\nTakes any number of condition/body pairs and evaluates the first body where\nthe condition evaluates to truthy. Similar to cond in other lisps.")
  local function remove_until_condition(bindings)
    local last_item = bindings[(#bindings - 1)]
    if ((utils["sym?"](last_item) and (tostring(last_item) == "&until")) or ("until" == last_item)) then
      table.remove(bindings, (#bindings - 1))
      return table.remove(bindings)
    else
      return nil
    end
  end
  local function compile_until(condition, scope, chunk)
    if condition then
      local _let_494_ = compiler.compile1(condition, scope, chunk, {nval = 1})
      local condition_lua = _let_494_[1]
      return compiler.emit(chunk, ("if %s then break end"):format(tostring(condition_lua)), utils.expr(condition, "expression"))
    else
      return nil
    end
  end
  SPECIALS.each = function(ast, scope, parent)
    compiler.assert((3 <= #ast), "expected body expression", ast[1])
    local binding = compiler.assert(utils["table?"](ast[2]), "expected binding table", ast)
    local _ = compiler.assert((2 <= #binding), "expected binding and iterator", binding)
    local until_condition = remove_until_condition(binding)
    local iter = table.remove(binding, #binding)
    local destructures = {}
    local new_manglings = {}
    local sub_scope = compiler["make-scope"](scope)
    local function destructure_binding(v)
      compiler.assert(not utils["string?"](v), ("unexpected iterator clause " .. tostring(v)), binding)
      if utils["sym?"](v) then
        return compiler["declare-local"](v, {}, sub_scope, ast, new_manglings)
      else
        local raw = utils.sym(compiler.gensym(sub_scope))
        do end (destructures)[raw] = v
        return compiler["declare-local"](raw, {}, sub_scope, ast)
      end
    end
    local bind_vars = utils.map(binding, destructure_binding)
    local vals = compiler.compile1(iter, scope, parent)
    local val_names = utils.map(vals, tostring)
    local chunk = {}
    compiler.emit(parent, ("for %s in %s do"):format(table.concat(bind_vars, ", "), table.concat(val_names, ", ")), ast)
    for raw, args in utils.stablepairs(destructures) do
      compiler.destructure(args, raw, ast, sub_scope, chunk, {declaration = true, nomulti = true, symtype = "each"})
    end
    compiler["apply-manglings"](sub_scope, new_manglings, ast)
    compile_until(until_condition, sub_scope, chunk)
    compile_do(ast, sub_scope, chunk, 3)
    compiler.emit(parent, chunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  doc_special("each", {"[key value (iterator)]", "..."}, "Runs the body once for each set of values provided by the given iterator.\nMost commonly used with ipairs for sequential tables or pairs for  undefined\norder, but can be used with any iterator.", true)
  local function while_2a(ast, scope, parent)
    local len1 = #parent
    local condition = (compiler.compile1(ast[2], scope, parent, {nval = 1}))[1]
    local len2 = #parent
    local sub_chunk = {}
    if (len1 ~= len2) then
      for i = (len1 + 1), len2 do
        table.insert(sub_chunk, parent[i])
        do end (parent)[i] = nil
      end
      compiler.emit(parent, "while true do", ast)
      compiler.emit(sub_chunk, ("if not %s then break end"):format(condition[1]), ast)
    else
      compiler.emit(parent, ("while " .. tostring(condition) .. " do"), ast)
    end
    compile_do(ast, compiler["make-scope"](scope), sub_chunk, 3)
    compiler.emit(parent, sub_chunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  SPECIALS["while"] = while_2a
  doc_special("while", {"condition", "..."}, "The classic while loop. Evaluates body until a condition is non-truthy.", true)
  local function for_2a(ast, scope, parent)
    local ranges = compiler.assert(utils["table?"](ast[2]), "expected binding table", ast)
    local until_condition = remove_until_condition(ast[2])
    local binding_sym = table.remove(ast[2], 1)
    local sub_scope = compiler["make-scope"](scope)
    local range_args = {}
    local chunk = {}
    compiler.assert(utils["sym?"](binding_sym), ("unable to bind %s %s"):format(type(binding_sym), tostring(binding_sym)), ast[2])
    compiler.assert((3 <= #ast), "expected body expression", ast[1])
    compiler.assert((#ranges <= 3), "unexpected arguments", ranges[4])
    for i = 1, math.min(#ranges, 3) do
      range_args[i] = tostring((compiler.compile1(ranges[i], scope, parent, {nval = 1}))[1])
    end
    compiler.emit(parent, ("for %s = %s do"):format(compiler["declare-local"](binding_sym, {}, sub_scope, ast), table.concat(range_args, ", ")), ast)
    compile_until(until_condition, sub_scope, chunk)
    compile_do(ast, sub_scope, chunk, 3)
    compiler.emit(parent, chunk, ast)
    return compiler.emit(parent, "end", ast)
  end
  SPECIALS["for"] = for_2a
  doc_special("for", {"[index start stop step?]", "..."}, "Numeric loop construct.\nEvaluates body once for each value between start and stop (inclusive).", true)
  local function native_method_call(ast, _scope, _parent, target, args)
    local _let_498_ = ast
    local _ = _let_498_[1]
    local _0 = _let_498_[2]
    local method_string = _let_498_[3]
    local call_string
    if ((target.type == "literal") or (target.type == "varg") or (target.type == "expression")) then
      call_string = "(%s):%s(%s)"
    else
      call_string = "%s:%s(%s)"
    end
    return utils.expr(string.format(call_string, tostring(target), method_string, table.concat(args, ", ")), "statement")
  end
  local function nonnative_method_call(ast, scope, parent, target, args)
    local method_string = tostring((compiler.compile1(ast[3], scope, parent, {nval = 1}))[1])
    local args0 = {tostring(target), unpack(args)}
    return utils.expr(string.format("%s[%s](%s)", tostring(target), method_string, table.concat(args0, ", ")), "statement")
  end
  local function double_eval_protected_method_call(ast, scope, parent, target, args)
    local method_string = tostring((compiler.compile1(ast[3], scope, parent, {nval = 1}))[1])
    local call = "(function(tgt, m, ...) return tgt[m](tgt, ...) end)(%s, %s)"
    table.insert(args, 1, method_string)
    return utils.expr(string.format(call, tostring(target), table.concat(args, ", ")), "statement")
  end
  local function method_call(ast, scope, parent)
    compiler.assert((2 < #ast), "expected at least 2 arguments", ast)
    local _let_500_ = compiler.compile1(ast[2], scope, parent, {nval = 1})
    local target = _let_500_[1]
    local args = {}
    for i = 4, #ast do
      local subexprs
      local _501_
      if (i ~= #ast) then
        _501_ = 1
      else
        _501_ = nil
      end
      subexprs = compiler.compile1(ast[i], scope, parent, {nval = _501_})
      utils.map(subexprs, tostring, args)
    end
    if (utils["string?"](ast[3]) and utils["valid-lua-identifier?"](ast[3])) then
      return native_method_call(ast, scope, parent, target, args)
    elseif (target.type == "sym") then
      return nonnative_method_call(ast, scope, parent, target, args)
    else
      return double_eval_protected_method_call(ast, scope, parent, target, args)
    end
  end
  SPECIALS[":"] = method_call
  doc_special(":", {"tbl", "method-name", "..."}, "Call the named method on tbl with the provided args.\nMethod name doesn't have to be known at compile-time; if it is, use\n(tbl:method-name ...) instead.")
  SPECIALS.comment = function(ast, _, parent)
    local els = {}
    for i = 2, #ast do
      table.insert(els, view(ast[i], {["one-line?"] = true}))
    end
    return compiler.emit(parent, ("--[[ " .. table.concat(els, " ") .. " ]]"), ast)
  end
  doc_special("comment", {"..."}, "Comment which will be emitted in Lua output.", true)
  local function hashfn_max_used(f_scope, i, max)
    local max0
    if f_scope.symmeta[("$" .. i)].used then
      max0 = i
    else
      max0 = max
    end
    if (i < 9) then
      return hashfn_max_used(f_scope, (i + 1), max0)
    else
      return max0
    end
  end
  SPECIALS.hashfn = function(ast, scope, parent)
    compiler.assert((#ast == 2), "expected one argument", ast)
    local f_scope
    do
      local _506_ = compiler["make-scope"](scope)
      do end (_506_)["vararg"] = false
      _506_["hashfn"] = true
      f_scope = _506_
    end
    local f_chunk = {}
    local name = compiler.gensym(scope)
    local symbol = utils.sym(name)
    local args = {}
    compiler["declare-local"](symbol, {}, scope, ast)
    for i = 1, 9 do
      args[i] = compiler["declare-local"](utils.sym(("$" .. i)), {}, f_scope, ast)
    end
    local function walker(idx, node, parent_node)
      if (utils["sym?"](node) and (tostring(node) == "$...")) then
        parent_node[idx] = utils.varg()
        f_scope.vararg = true
        return nil
      else
        return (utils["list?"](node) or utils["table?"](node))
      end
    end
    utils["walk-tree"](ast[2], walker)
    compiler.compile1(ast[2], f_scope, f_chunk, {tail = true})
    local max_used = hashfn_max_used(f_scope, 1, 0)
    if f_scope.vararg then
      compiler.assert((max_used == 0), "$ and $... in hashfn are mutually exclusive", ast)
    else
    end
    local arg_str
    if f_scope.vararg then
      arg_str = tostring(utils.varg())
    else
      arg_str = table.concat(args, ", ", 1, max_used)
    end
    compiler.emit(parent, string.format("local function %s(%s)", name, arg_str), ast)
    compiler.emit(parent, f_chunk, ast)
    compiler.emit(parent, "end", ast)
    return utils.expr(name, "sym")
  end
  doc_special("hashfn", {"..."}, "Function literal shorthand; args are either $... OR $1, $2, etc.")
  local function maybe_short_circuit_protect(ast, i, name, _510_)
    local _arg_511_ = _510_
    local mac = _arg_511_["macros"]
    local call = (utils["list?"](ast) and tostring(ast[1]))
    if ((("or" == name) or ("and" == name)) and (1 < i) and (mac[call] or ("set" == call) or ("tset" == call) or ("global" == call))) then
      return utils.list(utils.sym("do"), ast)
    else
      return ast
    end
  end
  local function arithmetic_special(name, zero_arity, unary_prefix, ast, scope, parent)
    local len = #ast
    local operands = {}
    local padded_op = (" " .. name .. " ")
    for i = 2, len do
      local subast = maybe_short_circuit_protect(ast[i], i, name, scope)
      local subexprs = compiler.compile1(subast, scope, parent)
      if (i == len) then
        utils.map(subexprs, tostring, operands)
      else
        table.insert(operands, tostring(subexprs[1]))
      end
    end
    local _514_ = #operands
    if (_514_ == 0) then
      local _516_
      do
        local _515_ = zero_arity
        compiler.assert(_515_, "Expected more than 0 arguments", ast)
        _516_ = _515_
      end
      return utils.expr(_516_, "literal")
    elseif (_514_ == 1) then
      if unary_prefix then
        return ("(" .. unary_prefix .. padded_op .. operands[1] .. ")")
      else
        return operands[1]
      end
    elseif true then
      local _ = _514_
      return ("(" .. table.concat(operands, padded_op) .. ")")
    else
      return nil
    end
  end
  local function define_arithmetic_special(name, zero_arity, unary_prefix, _3flua_name)
    local _522_
    do
      local _519_ = (_3flua_name or name)
      local _520_ = zero_arity
      local _521_ = unary_prefix
      local function _523_(...)
        return arithmetic_special(_519_, _520_, _521_, ...)
      end
      _522_ = _523_
    end
    SPECIALS[name] = _522_
    return doc_special(name, {"a", "b", "..."}, "Arithmetic operator; works the same as Lua but accepts more arguments.")
  end
  define_arithmetic_special("+", "0")
  define_arithmetic_special("..", "''")
  define_arithmetic_special("^")
  define_arithmetic_special("-", nil, "")
  define_arithmetic_special("*", "1")
  define_arithmetic_special("%")
  define_arithmetic_special("/", nil, "1")
  define_arithmetic_special("//", nil, "1")
  SPECIALS["or"] = function(ast, scope, parent)
    return arithmetic_special("or", "false", nil, ast, scope, parent)
  end
  SPECIALS["and"] = function(ast, scope, parent)
    return arithmetic_special("and", "true", nil, ast, scope, parent)
  end
  doc_special("and", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
  doc_special("or", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
  local function bitop_special(native_name, lib_name, zero_arity, unary_prefix, ast, scope, parent)
    if (#ast == 1) then
      return compiler.assert(zero_arity, "Expected more than 0 arguments.", ast)
    else
      local len = #ast
      local operands = {}
      local padded_native_name = (" " .. native_name .. " ")
      local prefixed_lib_name = ("bit." .. lib_name)
      for i = 2, len do
        local subexprs
        local _524_
        if (i ~= len) then
          _524_ = 1
        else
          _524_ = nil
        end
        subexprs = compiler.compile1(ast[i], scope, parent, {nval = _524_})
        utils.map(subexprs, tostring, operands)
      end
      if (#operands == 1) then
        if utils.root.options.useBitLib then
          return (prefixed_lib_name .. "(" .. unary_prefix .. ", " .. operands[1] .. ")")
        else
          return ("(" .. unary_prefix .. padded_native_name .. operands[1] .. ")")
        end
      else
        if utils.root.options.useBitLib then
          return (prefixed_lib_name .. "(" .. table.concat(operands, ", ") .. ")")
        else
          return ("(" .. table.concat(operands, padded_native_name) .. ")")
        end
      end
    end
  end
  local function define_bitop_special(name, zero_arity, unary_prefix, native)
    local _534_
    do
      local _530_ = native
      local _531_ = name
      local _532_ = zero_arity
      local _533_ = unary_prefix
      local function _535_(...)
        return bitop_special(_530_, _531_, _532_, _533_, ...)
      end
      _534_ = _535_
    end
    SPECIALS[name] = _534_
    return nil
  end
  define_bitop_special("lshift", nil, "1", "<<")
  define_bitop_special("rshift", nil, "1", ">>")
  define_bitop_special("band", "0", "0", "&")
  define_bitop_special("bor", "0", "0", "|")
  define_bitop_special("bxor", "0", "0", "~")
  doc_special("lshift", {"x", "n"}, "Bitwise logical left shift of x by n bits.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  doc_special("rshift", {"x", "n"}, "Bitwise logical right shift of x by n bits.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  doc_special("band", {"x1", "x2", "..."}, "Bitwise AND of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  doc_special("bor", {"x1", "x2", "..."}, "Bitwise OR of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  doc_special("bxor", {"x1", "x2", "..."}, "Bitwise XOR of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  doc_special("..", {"a", "b", "..."}, "String concatenation operator; works the same as Lua but accepts more arguments.")
  local function native_comparator(op, _536_, scope, parent)
    local _arg_537_ = _536_
    local _ = _arg_537_[1]
    local lhs_ast = _arg_537_[2]
    local rhs_ast = _arg_537_[3]
    local _let_538_ = compiler.compile1(lhs_ast, scope, parent, {nval = 1})
    local lhs = _let_538_[1]
    local _let_539_ = compiler.compile1(rhs_ast, scope, parent, {nval = 1})
    local rhs = _let_539_[1]
    return string.format("(%s %s %s)", tostring(lhs), op, tostring(rhs))
  end
  local function double_eval_protected_comparator(op, chain_op, ast, scope, parent)
    local arglist = {}
    local comparisons = {}
    local vals = {}
    local chain = string.format(" %s ", (chain_op or "and"))
    for i = 2, #ast do
      table.insert(arglist, tostring(compiler.gensym(scope)))
      table.insert(vals, tostring((compiler.compile1(ast[i], scope, parent, {nval = 1}))[1]))
    end
    for i = 1, (#arglist - 1) do
      table.insert(comparisons, string.format("(%s %s %s)", arglist[i], op, arglist[(i + 1)]))
    end
    return string.format("(function(%s) return %s end)(%s)", table.concat(arglist, ","), table.concat(comparisons, chain), table.concat(vals, ","))
  end
  local function define_comparator_special(name, _3flua_op, _3fchain_op)
    do
      local op = (_3flua_op or name)
      local function opfn(ast, scope, parent)
        compiler.assert((2 < #ast), "expected at least two arguments", ast)
        if (3 == #ast) then
          return native_comparator(op, ast, scope, parent)
        else
          return double_eval_protected_comparator(op, _3fchain_op, ast, scope, parent)
        end
      end
      SPECIALS[name] = opfn
    end
    return doc_special(name, {"a", "b", "..."}, "Comparison operator; works the same as Lua but accepts more arguments.")
  end
  define_comparator_special(">")
  define_comparator_special("<")
  define_comparator_special(">=")
  define_comparator_special("<=")
  define_comparator_special("=", "==")
  define_comparator_special("not=", "~=", "or")
  local function define_unary_special(op, _3frealop)
    local function opfn(ast, scope, parent)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local tail = compiler.compile1(ast[2], scope, parent, {nval = 1})
      return ((_3frealop or op) .. tostring(tail[1]))
    end
    SPECIALS[op] = opfn
    return nil
  end
  define_unary_special("not", "not ")
  doc_special("not", {"x"}, "Logical operator; works the same as Lua.")
  define_unary_special("bnot", "~")
  doc_special("bnot", {"x"}, "Bitwise negation; only works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
  define_unary_special("length", "#")
  doc_special("length", {"x"}, "Returns the length of a table or string.")
  do end (SPECIALS)["~="] = SPECIALS["not="]
  SPECIALS["#"] = SPECIALS.length
  SPECIALS.quote = function(ast, scope, parent)
    compiler.assert((#ast == 2), "expected one argument", ast)
    local runtime, this_scope = true, scope
    while this_scope do
      this_scope = this_scope.parent
      if (this_scope == compiler.scopes.compiler) then
        runtime = false
      else
      end
    end
    return compiler["do-quote"](ast[2], scope, parent, runtime)
  end
  doc_special("quote", {"x"}, "Quasiquote the following form. Only works in macro/compiler scope.")
  local macro_loaded = {}
  local function safe_getmetatable(tbl)
    local mt = getmetatable(tbl)
    assert((mt ~= getmetatable("")), "Illegal metatable access!")
    return mt
  end
  local safe_require = nil
  local function safe_compiler_env()
    local _543_
    do
      local _542_ = rawget(_G, "utf8")
      if (nil ~= _542_) then
        _543_ = utils.copy(_542_)
      else
        _543_ = _542_
      end
    end
    return {table = utils.copy(table), math = utils.copy(math), string = utils.copy(string), pairs = utils.stablepairs, ipairs = ipairs, select = select, tostring = tostring, tonumber = tonumber, bit = rawget(_G, "bit"), pcall = pcall, xpcall = xpcall, next = next, print = print, type = type, assert = assert, error = error, setmetatable = setmetatable, getmetatable = safe_getmetatable, require = safe_require, rawlen = rawget(_G, "rawlen"), rawget = rawget, rawset = rawset, rawequal = rawequal, _VERSION = _VERSION, utf8 = _543_}
  end
  local function combined_mt_pairs(env)
    local combined = {}
    local _let_545_ = getmetatable(env)
    local __index = _let_545_["__index"]
    if ("table" == type(__index)) then
      for k, v in pairs(__index) do
        combined[k] = v
      end
    else
    end
    for k, v in next, env, nil do
      combined[k] = v
    end
    return next, combined, nil
  end
  local function make_compiler_env(ast, scope, parent, _3fopts)
    local provided
    do
      local _547_ = (_3fopts or utils.root.options)
      if ((_G.type(_547_) == "table") and ((_547_)["compiler-env"] == "strict")) then
        provided = safe_compiler_env()
      elseif ((_G.type(_547_) == "table") and (nil ~= (_547_).compilerEnv)) then
        local compilerEnv = (_547_).compilerEnv
        provided = compilerEnv
      elseif ((_G.type(_547_) == "table") and (nil ~= (_547_)["compiler-env"])) then
        local compiler_env = (_547_)["compiler-env"]
        provided = compiler_env
      elseif true then
        local _ = _547_
        provided = safe_compiler_env(false)
      else
        provided = nil
      end
    end
    local env
    local function _549_(base)
      return utils.sym(compiler.gensym((compiler.scopes.macro or scope), base))
    end
    local function _550_()
      return compiler.scopes.macro
    end
    local function _551_(symbol)
      compiler.assert(compiler.scopes.macro, "must call from macro", ast)
      return compiler.scopes.macro.manglings[tostring(symbol)]
    end
    local function _552_(form)
      compiler.assert(compiler.scopes.macro, "must call from macro", ast)
      return compiler.macroexpand(form, compiler.scopes.macro)
    end
    env = {_AST = ast, _CHUNK = parent, _IS_COMPILER = true, _SCOPE = scope, _SPECIALS = compiler.scopes.global.specials, _VARARG = utils.varg(), ["macro-loaded"] = macro_loaded, unpack = unpack, ["assert-compile"] = compiler.assert, view = view, version = utils.version, metadata = compiler.metadata, ["ast-source"] = utils["ast-source"], list = utils.list, ["list?"] = utils["list?"], ["table?"] = utils["table?"], sequence = utils.sequence, ["sequence?"] = utils["sequence?"], sym = utils.sym, ["sym?"] = utils["sym?"], ["multi-sym?"] = utils["multi-sym?"], comment = utils.comment, ["comment?"] = utils["comment?"], ["varg?"] = utils["varg?"], gensym = _549_, ["get-scope"] = _550_, ["in-scope?"] = _551_, macroexpand = _552_}
    env._G = env
    return setmetatable(env, {__index = provided, __newindex = provided, __pairs = combined_mt_pairs})
  end
  local function _554_(...)
    local tbl_14_auto = {}
    local i_15_auto = #tbl_14_auto
    for c in string.gmatch((package.config or ""), "([^\n]+)") do
      local val_16_auto = c
      if (nil ~= val_16_auto) then
        i_15_auto = (i_15_auto + 1)
        do end (tbl_14_auto)[i_15_auto] = val_16_auto
      else
      end
    end
    return tbl_14_auto
  end
  local _local_553_ = _554_(...)
  local dirsep = _local_553_[1]
  local pathsep = _local_553_[2]
  local pathmark = _local_553_[3]
  local pkg_config = {dirsep = (dirsep or "/"), pathmark = (pathmark or ";"), pathsep = (pathsep or "?")}
  local function escapepat(str)
    return string.gsub(str, "[^%w]", "%%%1")
  end
  local function search_module(modulename, _3fpathstring)
    local pathsepesc = escapepat(pkg_config.pathsep)
    local pattern = ("([^%s]*)%s"):format(pathsepesc, pathsepesc)
    local no_dot_module = modulename:gsub("%.", pkg_config.dirsep)
    local fullpath = ((_3fpathstring or utils["fennel-module"].path) .. pkg_config.pathsep)
    local function try_path(path)
      local filename = path:gsub(escapepat(pkg_config.pathmark), no_dot_module)
      local filename2 = path:gsub(escapepat(pkg_config.pathmark), modulename)
      local _556_ = (io.open(filename) or io.open(filename2))
      if (nil ~= _556_) then
        local file = _556_
        file:close()
        return filename
      elseif true then
        local _ = _556_
        return nil, ("no file '" .. filename .. "'")
      else
        return nil
      end
    end
    local function find_in_path(start, _3ftried_paths)
      local _558_ = fullpath:match(pattern, start)
      if (nil ~= _558_) then
        local path = _558_
        local _559_, _560_ = try_path(path)
        if (nil ~= _559_) then
          local filename = _559_
          return filename
        elseif ((_559_ == nil) and (nil ~= _560_)) then
          local error = _560_
          local function _562_()
            local _561_ = (_3ftried_paths or {})
            table.insert(_561_, error)
            return _561_
          end
          return find_in_path((start + #path + 1), _562_())
        else
          return nil
        end
      elseif true then
        local _ = _558_
        local function _564_()
          local tried_paths = table.concat((_3ftried_paths or {}), "\n\9")
          if (_VERSION < "Lua 5.4") then
            return ("\n\9" .. tried_paths)
          else
            return tried_paths
          end
        end
        return nil, _564_()
      else
        return nil
      end
    end
    return find_in_path(1)
  end
  local function make_searcher(_3foptions)
    local function _567_(module_name)
      local opts = utils.copy(utils.root.options)
      for k, v in pairs((_3foptions or {})) do
        opts[k] = v
      end
      opts["module-name"] = module_name
      local _568_, _569_ = search_module(module_name)
      if (nil ~= _568_) then
        local filename = _568_
        local _572_
        do
          local _570_ = filename
          local _571_ = opts
          local function _573_(...)
            return utils["fennel-module"].dofile(_570_, _571_, ...)
          end
          _572_ = _573_
        end
        return _572_, filename
      elseif ((_568_ == nil) and (nil ~= _569_)) then
        local error = _569_
        return error
      else
        return nil
      end
    end
    return _567_
  end
  local function dofile_with_searcher(fennel_macro_searcher, filename, opts, ...)
    local searchers = (package.loaders or package.searchers or {})
    local _ = table.insert(searchers, 1, fennel_macro_searcher)
    local m = utils["fennel-module"].dofile(filename, opts, ...)
    table.remove(searchers, 1)
    return m
  end
  local function fennel_macro_searcher(module_name)
    local opts
    do
      local _575_ = utils.copy(utils.root.options)
      do end (_575_)["module-name"] = module_name
      _575_["env"] = "_COMPILER"
      _575_["requireAsInclude"] = false
      _575_["allowedGlobals"] = nil
      opts = _575_
    end
    local _576_ = search_module(module_name, utils["fennel-module"]["macro-path"])
    if (nil ~= _576_) then
      local filename = _576_
      local _577_
      if (opts["compiler-env"] == _G) then
        local _578_ = fennel_macro_searcher
        local _579_ = filename
        local _580_ = opts
        local function _582_(...)
          return dofile_with_searcher(_578_, _579_, _580_, ...)
        end
        _577_ = _582_
      else
        local _583_ = filename
        local _584_ = opts
        local function _586_(...)
          return utils["fennel-module"].dofile(_583_, _584_, ...)
        end
        _577_ = _586_
      end
      return _577_, filename
    else
      return nil
    end
  end
  local function lua_macro_searcher(module_name)
    local _589_ = search_module(module_name, package.path)
    if (nil ~= _589_) then
      local filename = _589_
      local code
      do
        local f = io.open(filename)
        local function close_handlers_8_auto(ok_9_auto, ...)
          f:close()
          if ok_9_auto then
            return ...
          else
            return error(..., 0)
          end
        end
        local function _591_()
          return assert(f:read("*a"))
        end
        code = close_handlers_8_auto(_G.xpcall(_591_, (package.loaded.fennel or debug).traceback))
      end
      local chunk = load_code(code, make_compiler_env(), filename)
      return chunk, filename
    else
      return nil
    end
  end
  local macro_searchers = {fennel_macro_searcher, lua_macro_searcher}
  local function search_macro_module(modname, n)
    local _593_ = macro_searchers[n]
    if (nil ~= _593_) then
      local f = _593_
      local _594_, _595_ = f(modname)
      if ((nil ~= _594_) and true) then
        local loader = _594_
        local _3ffilename = _595_
        return loader, _3ffilename
      elseif true then
        local _ = _594_
        return search_macro_module(modname, (n + 1))
      else
        return nil
      end
    else
      return nil
    end
  end
  local function sandbox_fennel_module(modname)
    if ((modname == "fennel.macros") or (package and package.loaded and ("table" == type(package.loaded[modname])) and (package.loaded[modname].metadata == compiler.metadata))) then
      return {metadata = compiler.metadata, view = view}
    else
      return nil
    end
  end
  local function _599_(modname)
    local function _600_()
      local loader, filename = search_macro_module(modname, 1)
      compiler.assert(loader, (modname .. " module not found."))
      do end (macro_loaded)[modname] = loader(modname, filename)
      return macro_loaded[modname]
    end
    return (macro_loaded[modname] or sandbox_fennel_module(modname) or _600_())
  end
  safe_require = _599_
  local function add_macros(macros_2a, ast, scope)
    compiler.assert(utils["table?"](macros_2a), "expected macros to be table", ast)
    for k, v in pairs(macros_2a) do
      compiler.assert((type(v) == "function"), "expected each macro to be function", ast)
      compiler["check-binding-valid"](utils.sym(k), scope, ast, {["macro?"] = true})
      do end (scope.macros)[k] = v
    end
    return nil
  end
  local function resolve_module_name(_601_, _scope, _parent, opts)
    local _arg_602_ = _601_
    local filename = _arg_602_["filename"]
    local second = _arg_602_[2]
    local filename0 = (filename or (utils["table?"](second) and second.filename))
    local module_name = utils.root.options["module-name"]
    local modexpr = compiler.compile(second, opts)
    local modname_chunk = load_code(modexpr)
    return modname_chunk(module_name, filename0)
  end
  SPECIALS["require-macros"] = function(ast, scope, parent, _3freal_ast)
    compiler.assert((#ast == 2), "Expected one module name argument", (_3freal_ast or ast))
    local modname = resolve_module_name(ast, scope, parent, {})
    compiler.assert(utils["string?"](modname), "module name must compile to string", (_3freal_ast or ast))
    if not macro_loaded[modname] then
      local loader, filename = search_macro_module(modname, 1)
      compiler.assert(loader, (modname .. " module not found."), ast)
      do end (macro_loaded)[modname] = compiler.assert(utils["table?"](loader(modname, filename)), "expected macros to be table", (_3freal_ast or ast))
    else
    end
    if ("import-macros" == tostring(ast[1])) then
      return macro_loaded[modname]
    else
      return add_macros(macro_loaded[modname], ast, scope, parent)
    end
  end
  doc_special("require-macros", {"macro-module-name"}, "Load given module and use its contents as macro definitions in current scope.\nMacro module should return a table of macro functions with string keys.\nConsider using import-macros instead as it is more flexible.")
  local function emit_included_fennel(src, path, opts, sub_chunk)
    local subscope = compiler["make-scope"](utils.root.scope.parent)
    local forms = {}
    if utils.root.options.requireAsInclude then
      subscope.specials.require = compiler["require-include"]
    else
    end
    for _, val in parser.parser(parser["string-stream"](src), path) do
      table.insert(forms, val)
    end
    for i = 1, #forms do
      local subopts
      if (i == #forms) then
        subopts = {tail = true}
      else
        subopts = {nval = 0}
      end
      utils["propagate-options"](opts, subopts)
      compiler.compile1(forms[i], subscope, sub_chunk, subopts)
    end
    return nil
  end
  local function include_path(ast, opts, path, mod, fennel_3f)
    utils.root.scope.includes[mod] = "fnl/loading"
    local src
    do
      local f = assert(io.open(path))
      local function close_handlers_8_auto(ok_9_auto, ...)
        f:close()
        if ok_9_auto then
          return ...
        else
          return error(..., 0)
        end
      end
      local function _608_()
        return assert(f:read("*all")):gsub("[\13\n]*$", "")
      end
      src = close_handlers_8_auto(_G.xpcall(_608_, (package.loaded.fennel or debug).traceback))
    end
    local ret = utils.expr(("require(\"" .. mod .. "\")"), "statement")
    local target = ("package.preload[%q]"):format(mod)
    local preload_str = (target .. " = " .. target .. " or function(...)")
    local temp_chunk, sub_chunk = {}, {}
    compiler.emit(temp_chunk, preload_str, ast)
    compiler.emit(temp_chunk, sub_chunk)
    compiler.emit(temp_chunk, "end", ast)
    for _, v in ipairs(temp_chunk) do
      table.insert(utils.root.chunk, v)
    end
    if fennel_3f then
      emit_included_fennel(src, path, opts, sub_chunk)
    else
      compiler.emit(sub_chunk, src, ast)
    end
    utils.root.scope.includes[mod] = ret
    return ret
  end
  local function include_circular_fallback(mod, modexpr, fallback, ast)
    if (utils.root.scope.includes[mod] == "fnl/loading") then
      compiler.assert(fallback, "circular include detected", ast)
      return fallback(modexpr)
    else
      return nil
    end
  end
  SPECIALS.include = function(ast, scope, parent, opts)
    compiler.assert((#ast == 2), "expected one argument", ast)
    local modexpr
    do
      local _611_, _612_ = pcall(resolve_module_name, ast, scope, parent, opts)
      if ((_611_ == true) and (nil ~= _612_)) then
        local modname = _612_
        modexpr = utils.expr(string.format("%q", modname), "literal")
      elseif true then
        local _ = _611_
        modexpr = (compiler.compile1(ast[2], scope, parent, {nval = 1}))[1]
      else
        modexpr = nil
      end
    end
    if ((modexpr.type ~= "literal") or ((modexpr[1]):byte() ~= 34)) then
      if opts.fallback then
        return opts.fallback(modexpr)
      else
        return compiler.assert(false, "module name must be string literal", ast)
      end
    else
      local mod = load_code(("return " .. modexpr[1]))()
      local oldmod = utils.root.options["module-name"]
      local _
      utils.root.options["module-name"] = mod
      _ = nil
      local res
      local function _616_()
        local _615_ = search_module(mod)
        if (nil ~= _615_) then
          local fennel_path = _615_
          return include_path(ast, opts, fennel_path, mod, true)
        elseif true then
          local _0 = _615_
          local lua_path = search_module(mod, package.path)
          if lua_path then
            return include_path(ast, opts, lua_path, mod, false)
          elseif opts.fallback then
            return opts.fallback(modexpr)
          else
            return compiler.assert(false, ("module not found " .. mod), ast)
          end
        else
          return nil
        end
      end
      res = ((utils["member?"](mod, (utils.root.options.skipInclude or {})) and opts.fallback(modexpr, true)) or include_circular_fallback(mod, modexpr, opts.fallback, ast) or utils.root.scope.includes[mod] or _616_())
      utils.root.options["module-name"] = oldmod
      return res
    end
  end
  doc_special("include", {"module-name-literal"}, "Like require but load the target module during compilation and embed it in the\nLua output. The module must be a string literal and resolvable at compile time.")
  local function eval_compiler_2a(ast, scope, parent)
    local env = make_compiler_env(ast, scope, parent)
    local opts = utils.copy(utils.root.options)
    opts.scope = compiler["make-scope"](compiler.scopes.compiler)
    opts.allowedGlobals = current_global_names(env)
    return load_code(compiler.compile(ast, opts), wrap_env(env))(opts["module-name"], ast.filename)
  end
  SPECIALS.macros = function(ast, scope, parent)
    compiler.assert((#ast == 2), "Expected one table argument", ast)
    return add_macros(eval_compiler_2a(ast[2], scope, parent), ast, scope, parent)
  end
  doc_special("macros", {"{:macro-name-1 (fn [...] ...) ... :macro-name-N macro-body-N}"}, "Define all functions in the given table as macros local to the current scope.")
  SPECIALS["eval-compiler"] = function(ast, scope, parent)
    local old_first = ast[1]
    ast[1] = utils.sym("do")
    local val = eval_compiler_2a(ast, scope, parent)
    do end (ast)[1] = old_first
    return val
  end
  doc_special("eval-compiler", {"..."}, "Evaluate the body at compile-time. Use the macro system instead if possible.", true)
  return {doc = doc_2a, ["current-global-names"] = current_global_names, ["load-code"] = load_code, ["macro-loaded"] = macro_loaded, ["macro-searchers"] = macro_searchers, ["make-compiler-env"] = make_compiler_env, ["search-module"] = search_module, ["make-searcher"] = make_searcher, ["wrap-env"] = wrap_env}
end
package.preload["fennel.compiler"] = package.preload["fennel.compiler"] or function(...)
  local utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local friend = require("fennel.friend")
  local unpack = (table.unpack or _G.unpack)
  local scopes = {}
  local function make_scope(_3fparent)
    local parent = (_3fparent or scopes.global)
    local _257_
    if parent then
      _257_ = ((parent.depth or 0) + 1)
    else
      _257_ = 0
    end
    return {includes = setmetatable({}, {__index = (parent and parent.includes)}), macros = setmetatable({}, {__index = (parent and parent.macros)}), manglings = setmetatable({}, {__index = (parent and parent.manglings)}), specials = setmetatable({}, {__index = (parent and parent.specials)}), symmeta = setmetatable({}, {__index = (parent and parent.symmeta)}), unmanglings = setmetatable({}, {__index = (parent and parent.unmanglings)}), gensyms = setmetatable({}, {__index = (parent and parent.gensyms)}), autogensyms = setmetatable({}, {__index = (parent and parent.autogensyms)}), vararg = (parent and parent.vararg), depth = _257_, hashfn = (parent and parent.hashfn), refedglobals = {}, parent = parent}
  end
  local function assert_msg(ast, msg)
    local ast_tbl
    if ("table" == type(ast)) then
      ast_tbl = ast
    else
      ast_tbl = {}
    end
    local m = getmetatable(ast)
    local filename = ((m and m.filename) or ast_tbl.filename or "unknown")
    local line = ((m and m.line) or ast_tbl.line or "?")
    local col = ((m and m.col) or ast_tbl.col or "?")
    local target = tostring((utils["sym?"](ast_tbl[1]) or ast_tbl[1] or "()"))
    return string.format("%s:%s:%s Compile error in '%s': %s", filename, line, col, target, msg)
  end
  local function assert_compile(condition, msg, ast)
    if not condition then
      local _let_260_ = (utils.root.options or {})
      local source = _let_260_["source"]
      local unfriendly = _let_260_["unfriendly"]
      if (nil == utils.hook("assert-compile", condition, msg, ast, utils.root.reset)) then
        utils.root.reset()
        if (unfriendly or not friend or not _G.io or not _G.io.read) then
          error(assert_msg(ast, msg), 0)
        else
          friend["assert-compile"](condition, msg, ast, source)
        end
      else
      end
    else
    end
    return condition
  end
  scopes.global = make_scope()
  scopes.global.vararg = true
  scopes.compiler = make_scope(scopes.global)
  scopes.macro = scopes.global
  local serialize_subst = {["\7"] = "\\a", ["\8"] = "\\b", ["\9"] = "\\t", ["\n"] = "n", ["\11"] = "\\v", ["\12"] = "\\f"}
  local function serialize_string(str)
    local function _264_(_241)
      return ("\\" .. _241:byte())
    end
    return string.gsub(string.gsub(string.format("%q", str), ".", serialize_subst), "[\128-\255]", _264_)
  end
  local function global_mangling(str)
    if utils["valid-lua-identifier?"](str) then
      return str
    else
      local function _265_(_241)
        return string.format("_%02x", _241:byte())
      end
      return ("__fnl_global__" .. str:gsub("[^%w]", _265_))
    end
  end
  local function global_unmangling(identifier)
    local _267_ = string.match(identifier, "^__fnl_global__(.*)$")
    if (nil ~= _267_) then
      local rest = _267_
      local _268_
      local function _269_(_241)
        return string.char(tonumber(_241:sub(2), 16))
      end
      _268_ = string.gsub(rest, "_[%da-f][%da-f]", _269_)
      return _268_
    elseif true then
      local _ = _267_
      return identifier
    else
      return nil
    end
  end
  local allowed_globals = nil
  local function global_allowed_3f(name)
    return (not allowed_globals or utils["member?"](name, allowed_globals))
  end
  local function unique_mangling(original, mangling, scope, append)
    if (scope.unmanglings[mangling] and not scope.gensyms[mangling]) then
      return unique_mangling(original, (original .. append), scope, (append + 1))
    else
      return mangling
    end
  end
  local function local_mangling(str, scope, ast, _3ftemp_manglings)
    assert_compile(not utils["multi-sym?"](str), ("unexpected multi symbol " .. str), ast)
    local raw
    if ((utils["lua-keywords"])[str] or str:match("^%d")) then
      raw = ("_" .. str)
    else
      raw = str
    end
    local mangling
    local function _273_(_241)
      return string.format("_%02x", _241:byte())
    end
    mangling = string.gsub(string.gsub(raw, "-", "_"), "[^%w_]", _273_)
    local unique = unique_mangling(mangling, mangling, scope, 0)
    do end (scope.unmanglings)[unique] = str
    do
      local manglings = (_3ftemp_manglings or scope.manglings)
      do end (manglings)[str] = unique
    end
    return unique
  end
  local function apply_manglings(scope, new_manglings, ast)
    for raw, mangled in pairs(new_manglings) do
      assert_compile(not scope.refedglobals[mangled], ("use of global " .. raw .. " is aliased by a local"), ast)
      do end (scope.manglings)[raw] = mangled
    end
    return nil
  end
  local function combine_parts(parts, scope)
    local ret = (scope.manglings[parts[1]] or global_mangling(parts[1]))
    for i = 2, #parts do
      if utils["valid-lua-identifier?"](parts[i]) then
        if (parts["multi-sym-method-call"] and (i == #parts)) then
          ret = (ret .. ":" .. parts[i])
        else
          ret = (ret .. "." .. parts[i])
        end
      else
        ret = (ret .. "[" .. serialize_string(parts[i]) .. "]")
      end
    end
    return ret
  end
  local function next_append()
    utils.root.scope["gensym-append"] = ((utils.root.scope["gensym-append"] or 0) + 1)
    return ("_" .. utils.root.scope["gensym-append"] .. "_")
  end
  local function gensym(scope, _3fbase, _3fsuffix)
    local mangling = ((_3fbase or "") .. next_append() .. (_3fsuffix or ""))
    while scope.unmanglings[mangling] do
      mangling = ((_3fbase or "") .. next_append() .. (_3fsuffix or ""))
    end
    scope.unmanglings[mangling] = (_3fbase or true)
    do end (scope.gensyms)[mangling] = true
    return mangling
  end
  local function combine_auto_gensym(parts, first)
    parts[1] = first
    local last = table.remove(parts)
    local last2 = table.remove(parts)
    local last_joiner = ((parts["multi-sym-method-call"] and ":") or ".")
    table.insert(parts, (last2 .. last_joiner .. last))
    return table.concat(parts, ".")
  end
  local function autogensym(base, scope)
    local _276_ = utils["multi-sym?"](base)
    if (nil ~= _276_) then
      local parts = _276_
      return combine_auto_gensym(parts, autogensym(parts[1], scope))
    elseif true then
      local _ = _276_
      local function _277_()
        local mangling = gensym(scope, base:sub(1, ( - 2)), "auto")
        do end (scope.autogensyms)[base] = mangling
        return mangling
      end
      return (scope.autogensyms[base] or _277_())
    else
      return nil
    end
  end
  local function check_binding_valid(symbol, scope, ast, _3fopts)
    local name = tostring(symbol)
    local macro_3f
    do
      local t_279_ = _3fopts
      if (nil ~= t_279_) then
        t_279_ = (t_279_)["macro?"]
      else
      end
      macro_3f = t_279_
    end
    assert_compile(not name:find("&"), "invalid character: &")
    assert_compile(not name:find("^%."), "invalid character: .")
    assert_compile(not (scope.specials[name] or (not macro_3f and scope.macros[name])), ("local %s was overshadowed by a special form or macro"):format(name), ast)
    return assert_compile(not utils["quoted?"](symbol), string.format("macro tried to bind %s without gensym", name), symbol)
  end
  local function declare_local(symbol, meta, scope, ast, _3ftemp_manglings)
    check_binding_valid(symbol, scope, ast)
    local name = tostring(symbol)
    assert_compile(not utils["multi-sym?"](name), ("unexpected multi symbol " .. name), ast)
    do end (scope.symmeta)[name] = meta
    return local_mangling(name, scope, ast, _3ftemp_manglings)
  end
  local function hashfn_arg_name(name, multi_sym_parts, scope)
    if not scope.hashfn then
      return nil
    elseif (name == "$") then
      return "$1"
    elseif multi_sym_parts then
      if (multi_sym_parts and (multi_sym_parts[1] == "$")) then
        multi_sym_parts[1] = "$1"
      else
      end
      return table.concat(multi_sym_parts, ".")
    else
      return nil
    end
  end
  local function symbol_to_expression(symbol, scope, _3freference_3f)
    utils.hook("symbol-to-expression", symbol, scope, _3freference_3f)
    local name = symbol[1]
    local multi_sym_parts = utils["multi-sym?"](name)
    local name0 = (hashfn_arg_name(name, multi_sym_parts, scope) or name)
    local parts = (multi_sym_parts or {name0})
    local etype = (((1 < #parts) and "expression") or "sym")
    local local_3f = scope.manglings[parts[1]]
    if (local_3f and scope.symmeta[parts[1]]) then
      scope.symmeta[parts[1]]["used"] = true
    else
    end
    assert_compile(not scope.macros[parts[1]], "tried to reference a macro at runtime", symbol)
    assert_compile((not scope.specials[parts[1]] or ("require" == parts[1])), "tried to reference a special form at runtime", symbol)
    assert_compile((not _3freference_3f or local_3f or ("_ENV" == parts[1]) or global_allowed_3f(parts[1])), ("unknown identifier in strict mode: " .. tostring(parts[1])), symbol)
    if (allowed_globals and not local_3f and scope.parent) then
      scope.parent.refedglobals[parts[1]] = true
    else
    end
    return utils.expr(combine_parts(parts, scope), etype)
  end
  local function emit(chunk, out, _3fast)
    if (type(out) == "table") then
      return table.insert(chunk, out)
    else
      return table.insert(chunk, {ast = _3fast, leaf = out})
    end
  end
  local function peephole(chunk)
    if chunk.leaf then
      return chunk
    elseif ((3 <= #chunk) and (chunk[(#chunk - 2)].leaf == "do") and not chunk[(#chunk - 1)].leaf and (chunk[#chunk].leaf == "end")) then
      local kid = peephole(chunk[(#chunk - 1)])
      local new_chunk = {ast = chunk.ast}
      for i = 1, (#chunk - 3) do
        table.insert(new_chunk, peephole(chunk[i]))
      end
      for i = 1, #kid do
        table.insert(new_chunk, kid[i])
      end
      return new_chunk
    else
      return utils.map(chunk, peephole)
    end
  end
  local function flatten_chunk_correlated(main_chunk, options)
    local function flatten(chunk, out, last_line, file)
      local last_line0 = last_line
      if chunk.leaf then
        out[last_line0] = ((out[last_line0] or "") .. " " .. chunk.leaf)
      else
        for _, subchunk in ipairs(chunk) do
          if (subchunk.leaf or (0 < #subchunk)) then
            local source = utils["ast-source"](subchunk.ast)
            if (file == source.filename) then
              last_line0 = math.max(last_line0, (source.line or 0))
            else
            end
            last_line0 = flatten(subchunk, out, last_line0, file)
          else
          end
        end
      end
      return last_line0
    end
    local out = {}
    local last = flatten(main_chunk, out, 1, options.filename)
    for i = 1, last do
      if (out[i] == nil) then
        out[i] = ""
      else
      end
    end
    return table.concat(out, "\n")
  end
  local function flatten_chunk(file_sourcemap, chunk, tab, depth)
    if chunk.leaf then
      local _let_291_ = utils["ast-source"](chunk.ast)
      local filename = _let_291_["filename"]
      local line = _let_291_["line"]
      table.insert(file_sourcemap, {filename, line})
      return chunk.leaf
    else
      local tab0
      do
        local _292_ = tab
        if (_292_ == true) then
          tab0 = "  "
        elseif (_292_ == false) then
          tab0 = ""
        elseif (_292_ == tab) then
          tab0 = tab
        elseif (_292_ == nil) then
          tab0 = ""
        else
          tab0 = nil
        end
      end
      local function parter(c)
        if (c.leaf or (0 < #c)) then
          local sub = flatten_chunk(file_sourcemap, c, tab0, (depth + 1))
          if (0 < depth) then
            return (tab0 .. sub:gsub("\n", ("\n" .. tab0)))
          else
            return sub
          end
        else
          return nil
        end
      end
      return table.concat(utils.map(chunk, parter), "\n")
    end
  end
  local sourcemap = {}
  local function make_short_src(source)
    local source0 = source:gsub("\n", " ")
    if (#source0 <= 49) then
      return ("[fennel \"" .. source0 .. "\"]")
    else
      return ("[fennel \"" .. source0:sub(1, 46) .. "...\"]")
    end
  end
  local function flatten(chunk, options)
    local chunk0 = peephole(chunk)
    if options.correlate then
      return flatten_chunk_correlated(chunk0, options), {}
    else
      local file_sourcemap = {}
      local src = flatten_chunk(file_sourcemap, chunk0, options.indent, 0)
      file_sourcemap.short_src = (options.filename or make_short_src((options.source or src)))
      if options.filename then
        file_sourcemap.key = ("@" .. options.filename)
      else
        file_sourcemap.key = src
      end
      sourcemap[file_sourcemap.key] = file_sourcemap
      return src, file_sourcemap
    end
  end
  local function make_metadata()
    local function _300_(self, tgt, key)
      if self[tgt] then
        return self[tgt][key]
      else
        return nil
      end
    end
    local function _302_(self, tgt, key, value)
      self[tgt] = (self[tgt] or {})
      do end (self[tgt])[key] = value
      return tgt
    end
    local function _303_(self, tgt, ...)
      local kv_len = select("#", ...)
      local kvs = {...}
      if ((kv_len % 2) ~= 0) then
        error("metadata:setall() expected even number of k/v pairs")
      else
      end
      self[tgt] = (self[tgt] or {})
      for i = 1, kv_len, 2 do
        self[tgt][kvs[i]] = kvs[(i + 1)]
      end
      return tgt
    end
    return setmetatable({}, {__index = {get = _300_, set = _302_, setall = _303_}, __mode = "k"})
  end
  local function exprs1(exprs)
    return table.concat(utils.map(exprs, tostring), ", ")
  end
  local function keep_side_effects(exprs, chunk, start, ast)
    local start0 = (start or 1)
    for j = start0, #exprs do
      local se = exprs[j]
      if ((se.type == "expression") and (se[1] ~= "nil")) then
        emit(chunk, string.format("do local _ = %s end", tostring(se)), ast)
      elseif (se.type == "statement") then
        local code = tostring(se)
        local disambiguated
        if (code:byte() == 40) then
          disambiguated = ("do end " .. code)
        else
          disambiguated = code
        end
        emit(chunk, disambiguated, ast)
      else
      end
    end
    return nil
  end
  local function handle_compile_opts(exprs, parent, opts, ast)
    if opts.nval then
      local n = opts.nval
      local len = #exprs
      if (n ~= len) then
        if (n < len) then
          keep_side_effects(exprs, parent, (n + 1), ast)
          for i = (n + 1), len do
            exprs[i] = nil
          end
        else
          for i = (#exprs + 1), n do
            exprs[i] = utils.expr("nil", "literal")
          end
        end
      else
      end
    else
    end
    if opts.tail then
      emit(parent, string.format("return %s", exprs1(exprs)), ast)
    else
    end
    if opts.target then
      local result = exprs1(exprs)
      local function _311_()
        if (result == "") then
          return "nil"
        else
          return result
        end
      end
      emit(parent, string.format("%s = %s", opts.target, _311_()), ast)
    else
    end
    if (opts.tail or opts.target) then
      return {returned = true}
    else
      local _313_ = exprs
      _313_["returned"] = true
      return _313_
    end
  end
  local function find_macro(ast, scope)
    local macro_2a
    do
      local _315_ = utils["sym?"](ast[1])
      if (_315_ ~= nil) then
        local _316_ = tostring(_315_)
        if (_316_ ~= nil) then
          macro_2a = scope.macros[_316_]
        else
          macro_2a = _316_
        end
      else
        macro_2a = _315_
      end
    end
    local multi_sym_parts = utils["multi-sym?"](ast[1])
    if (not macro_2a and multi_sym_parts) then
      local nested_macro = utils["get-in"](scope.macros, multi_sym_parts)
      assert_compile((not scope.macros[multi_sym_parts[1]] or (type(nested_macro) == "function")), "macro not found in imported macro module", ast)
      return nested_macro
    else
      return macro_2a
    end
  end
  local function propagate_trace_info(_320_, _index, node)
    local _arg_321_ = _320_
    local filename = _arg_321_["filename"]
    local line = _arg_321_["line"]
    local bytestart = _arg_321_["bytestart"]
    local byteend = _arg_321_["byteend"]
    do
      local src = utils["ast-source"](node)
      if (("table" == type(node)) and (filename ~= src.filename)) then
        src.filename, src.line, src["from-macro?"] = filename, line, true
        src.bytestart, src.byteend = bytestart, byteend
      else
      end
    end
    return ("table" == type(node))
  end
  local function max_n(t)
    local n = 0
    for k in pairs(t) do
      if ("number" == type(k)) then
        n = math.max(k, n)
      else
      end
    end
    return n
  end
  local function quote_literal_nils(index, node, parent)
    if (parent and utils["list?"](parent)) then
      for i = 1, max_n(parent) do
        local _324_ = parent[i]
        if (_324_ == nil) then
          parent[i] = utils.sym("nil")
        else
        end
      end
    else
    end
    return index, node, parent
  end
  local function comp(f, g)
    local function _327_(...)
      return f(g(...))
    end
    return _327_
  end
  local function built_in_3f(m)
    local found_3f = false
    for _, f in pairs(scopes.global.macros) do
      if found_3f then break end
      found_3f = (f == m)
    end
    return found_3f
  end
  local function macroexpand_2a(ast, scope, _3fonce)
    local _328_
    if utils["list?"](ast) then
      _328_ = find_macro(ast, scope)
    else
      _328_ = nil
    end
    if (_328_ == false) then
      return ast
    elseif (nil ~= _328_) then
      local macro_2a = _328_
      local old_scope = scopes.macro
      local _
      scopes.macro = scope
      _ = nil
      local ok, transformed = nil, nil
      local function _330_()
        return macro_2a(unpack(ast, 2))
      end
      local function _331_()
        if built_in_3f(macro_2a) then
          return tostring
        else
          return debug.traceback
        end
      end
      ok, transformed = xpcall(_330_, _331_())
      local _333_
      do
        local _332_ = ast
        local function _334_(...)
          return propagate_trace_info(_332_, ...)
        end
        _333_ = _334_
      end
      utils["walk-tree"](transformed, comp(_333_, quote_literal_nils))
      scopes.macro = old_scope
      assert_compile(ok, transformed, ast)
      if (_3fonce or not transformed) then
        return transformed
      else
        return macroexpand_2a(transformed, scope)
      end
    elseif true then
      local _ = _328_
      return ast
    else
      return nil
    end
  end
  local function compile_special(ast, scope, parent, opts, special)
    local exprs = (special(ast, scope, parent, opts) or utils.expr("nil", "literal"))
    local exprs0
    if ("table" ~= type(exprs)) then
      exprs0 = utils.expr(exprs, "expression")
    else
      exprs0 = exprs
    end
    local exprs2
    if utils["expr?"](exprs0) then
      exprs2 = {exprs0}
    else
      exprs2 = exprs0
    end
    if not exprs2.returned then
      return handle_compile_opts(exprs2, parent, opts, ast)
    elseif (opts.tail or opts.target) then
      return {returned = true}
    else
      return exprs2
    end
  end
  local function compile_function_call(ast, scope, parent, opts, compile1, len)
    local fargs = {}
    local fcallee = (compile1(ast[1], scope, parent, {nval = 1}))[1]
    assert_compile((utils["sym?"](ast[1]) or utils["list?"](ast[1]) or ("string" == type(ast[1]))), ("cannot call literal value " .. tostring(ast[1])), ast)
    for i = 2, len do
      local subexprs
      local _340_
      if (i ~= len) then
        _340_ = 1
      else
        _340_ = nil
      end
      subexprs = compile1(ast[i], scope, parent, {nval = _340_})
      table.insert(fargs, subexprs[1])
      if (i == len) then
        for j = 2, #subexprs do
          table.insert(fargs, subexprs[j])
        end
      else
        keep_side_effects(subexprs, parent, 2, ast[i])
      end
    end
    local pat
    if ("string" == type(ast[1])) then
      pat = "(%s)(%s)"
    else
      pat = "%s(%s)"
    end
    local call = string.format(pat, tostring(fcallee), exprs1(fargs))
    return handle_compile_opts({utils.expr(call, "statement")}, parent, opts, ast)
  end
  local function compile_call(ast, scope, parent, opts, compile1)
    utils.hook("call", ast, scope)
    local len = #ast
    local first = ast[1]
    local multi_sym_parts = utils["multi-sym?"](first)
    local special = (utils["sym?"](first) and scope.specials[tostring(first)])
    assert_compile((0 < len), "expected a function, macro, or special to call", ast)
    if special then
      return compile_special(ast, scope, parent, opts, special)
    elseif (multi_sym_parts and multi_sym_parts["multi-sym-method-call"]) then
      local table_with_method = table.concat({unpack(multi_sym_parts, 1, (#multi_sym_parts - 1))}, ".")
      local method_to_call = multi_sym_parts[#multi_sym_parts]
      local new_ast = utils.list(utils.sym(":", ast), utils.sym(table_with_method, ast), method_to_call, select(2, unpack(ast)))
      return compile1(new_ast, scope, parent, opts)
    else
      return compile_function_call(ast, scope, parent, opts, compile1, len)
    end
  end
  local function compile_varg(ast, scope, parent, opts)
    local _345_
    if scope.hashfn then
      _345_ = "use $... in hashfn"
    else
      _345_ = "unexpected vararg"
    end
    assert_compile(scope.vararg, _345_, ast)
    return handle_compile_opts({utils.expr("...", "varg")}, parent, opts, ast)
  end
  local function compile_sym(ast, scope, parent, opts)
    local multi_sym_parts = utils["multi-sym?"](ast)
    assert_compile(not (multi_sym_parts and multi_sym_parts["multi-sym-method-call"]), "multisym method calls may only be in call position", ast)
    local e
    if (ast[1] == "nil") then
      e = utils.expr("nil", "literal")
    else
      e = symbol_to_expression(ast, scope, true)
    end
    return handle_compile_opts({e}, parent, opts, ast)
  end
  local function serialize_number(n)
    local _348_ = string.gsub(tostring(n), ",", ".")
    return _348_
  end
  local function compile_scalar(ast, _scope, parent, opts)
    local serialize
    do
      local _349_ = type(ast)
      if (_349_ == "nil") then
        serialize = tostring
      elseif (_349_ == "boolean") then
        serialize = tostring
      elseif (_349_ == "string") then
        serialize = serialize_string
      elseif (_349_ == "number") then
        serialize = serialize_number
      else
        serialize = nil
      end
    end
    return handle_compile_opts({utils.expr(serialize(ast), "literal")}, parent, opts)
  end
  local function compile_table(ast, scope, parent, opts, compile1)
    local buffer = {}
    local function write_other_values(k)
      if ((type(k) ~= "number") or (math.floor(k) ~= k) or (k < 1) or (#ast < k)) then
        if ((type(k) == "string") and utils["valid-lua-identifier?"](k)) then
          return {k, k}
        else
          local _let_351_ = compile1(k, scope, parent, {nval = 1})
          local compiled = _let_351_[1]
          local kstr = ("[" .. tostring(compiled) .. "]")
          return {kstr, k}
        end
      else
        return nil
      end
    end
    do
      local keys
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for k, v in utils.stablepairs(ast) do
          local val_16_auto = write_other_values(k, v)
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        keys = tbl_14_auto
      end
      local function _357_(_355_)
        local _arg_356_ = _355_
        local k1 = _arg_356_[1]
        local k2 = _arg_356_[2]
        local _let_358_ = compile1(ast[k2], scope, parent, {nval = 1})
        local v = _let_358_[1]
        return string.format("%s = %s", k1, tostring(v))
      end
      utils.map(keys, _357_, buffer)
    end
    for i = 1, #ast do
      local nval = ((i ~= #ast) and 1)
      table.insert(buffer, exprs1(compile1(ast[i], scope, parent, {nval = nval})))
    end
    return handle_compile_opts({utils.expr(("{" .. table.concat(buffer, ", ") .. "}"), "expression")}, parent, opts, ast)
  end
  local function compile1(ast, scope, parent, _3fopts)
    local opts = (_3fopts or {})
    local ast0 = macroexpand_2a(ast, scope)
    if utils["list?"](ast0) then
      return compile_call(ast0, scope, parent, opts, compile1)
    elseif utils["varg?"](ast0) then
      return compile_varg(ast0, scope, parent, opts)
    elseif utils["sym?"](ast0) then
      return compile_sym(ast0, scope, parent, opts)
    elseif (type(ast0) == "table") then
      return compile_table(ast0, scope, parent, opts, compile1)
    elseif ((type(ast0) == "nil") or (type(ast0) == "boolean") or (type(ast0) == "number") or (type(ast0) == "string")) then
      return compile_scalar(ast0, scope, parent, opts)
    else
      return assert_compile(false, ("could not compile value of type " .. type(ast0)), ast0)
    end
  end
  local function destructure(to, from, ast, scope, parent, opts)
    local opts0 = (opts or {})
    local _let_360_ = opts0
    local isvar = _let_360_["isvar"]
    local declaration = _let_360_["declaration"]
    local forceglobal = _let_360_["forceglobal"]
    local forceset = _let_360_["forceset"]
    local symtype = _let_360_["symtype"]
    local symtype0 = ("_" .. (symtype or "dst"))
    local setter
    if declaration then
      setter = "local %s = %s"
    else
      setter = "%s = %s"
    end
    local new_manglings = {}
    local function getname(symbol, up1)
      local raw = symbol[1]
      assert_compile(not (opts0.nomulti and utils["multi-sym?"](raw)), ("unexpected multi symbol " .. raw), up1)
      if declaration then
        return declare_local(symbol, nil, scope, symbol, new_manglings)
      else
        local parts = (utils["multi-sym?"](raw) or {raw})
        local meta = scope.symmeta[parts[1]]
        assert_compile(not raw:find(":"), "cannot set method sym", symbol)
        if ((#parts == 1) and not forceset) then
          assert_compile(not (forceglobal and meta), string.format("global %s conflicts with local", tostring(symbol)), symbol)
          assert_compile(not (meta and not meta.var), ("expected var " .. raw), symbol)
          assert_compile((meta or not opts0.noundef), ("expected local " .. parts[1]), symbol)
        else
        end
        if forceglobal then
          assert_compile(not scope.symmeta[scope.unmanglings[raw]], ("global " .. raw .. " conflicts with local"), symbol)
          do end (scope.manglings)[raw] = global_mangling(raw)
          do end (scope.unmanglings)[global_mangling(raw)] = raw
          if allowed_globals then
            table.insert(allowed_globals, raw)
          else
          end
        else
        end
        return symbol_to_expression(symbol, scope)[1]
      end
    end
    local function compile_top_target(lvalues)
      local inits
      local function _366_(_241)
        if scope.manglings[_241] then
          return _241
        else
          return "nil"
        end
      end
      inits = utils.map(lvalues, _366_)
      local init = table.concat(inits, ", ")
      local lvalue = table.concat(lvalues, ", ")
      local plast = parent[#parent]
      local plen = #parent
      local ret = compile1(from, scope, parent, {target = lvalue})
      if declaration then
        for pi = plen, #parent do
          if (parent[pi] == plast) then
            plen = pi
          else
          end
        end
        if ((#parent == (plen + 1)) and parent[#parent].leaf) then
          parent[#parent]["leaf"] = ("local " .. parent[#parent].leaf)
        elseif (init == "nil") then
          table.insert(parent, (plen + 1), {ast = ast, leaf = ("local " .. lvalue)})
        else
          table.insert(parent, (plen + 1), {ast = ast, leaf = ("local " .. lvalue .. " = " .. init)})
        end
      else
      end
      return ret
    end
    local function destructure_sym(left, rightexprs, up1, top_3f)
      local lname = getname(left, up1)
      check_binding_valid(left, scope, left)
      if top_3f then
        compile_top_target({lname})
      else
        emit(parent, setter:format(lname, exprs1(rightexprs)), left)
      end
      if declaration then
        scope.symmeta[tostring(left)] = {var = isvar}
        return nil
      else
        return nil
      end
    end
    local unpack_fn = "function (t, k, e)\n                        local mt = getmetatable(t)\n                        if 'table' == type(mt) and mt.__fennelrest then\n                          return mt.__fennelrest(t, k)\n                        elseif e then\n                          local rest = {}\n                          for k, v in pairs(t) do\n                            if not e[k] then rest[k] = v end\n                          end\n                          return rest\n                        else\n                          return {(table.unpack or unpack)(t, k)}\n                        end\n                      end"
    local function destructure_kv_rest(s, v, left, excluded_keys, destructure1)
      local exclude_str
      local _373_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, k in ipairs(excluded_keys) do
          local val_16_auto = string.format("[%s] = true", serialize_string(k))
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _373_ = tbl_14_auto
      end
      exclude_str = table.concat(_373_, ", ")
      local subexpr = utils.expr(string.format(string.gsub(("(" .. unpack_fn .. ")(%s, %s, {%s})"), "\n%s*", " "), s, tostring(v), exclude_str), "expression")
      return destructure1(v, {subexpr}, left)
    end
    local function destructure_rest(s, k, left, destructure1)
      local unpack_str = ("(" .. unpack_fn .. ")(%s, %s)")
      local formatted = string.format(string.gsub(unpack_str, "\n%s*", " "), s, k)
      local subexpr = utils.expr(formatted, "expression")
      assert_compile((utils["sequence?"](left) and (nil == left[(k + 2)])), "expected rest argument before last parameter", left)
      return destructure1(left[(k + 1)], {subexpr}, left)
    end
    local function destructure_table(left, rightexprs, top_3f, destructure1)
      local s = gensym(scope, symtype0)
      local right
      do
        local _375_
        if top_3f then
          _375_ = exprs1(compile1(from, scope, parent))
        else
          _375_ = exprs1(rightexprs)
        end
        if (_375_ == "") then
          right = "nil"
        elseif (nil ~= _375_) then
          local right0 = _375_
          right = right0
        else
          right = nil
        end
      end
      local excluded_keys = {}
      emit(parent, string.format("local %s = %s", s, right), left)
      for k, v in utils.stablepairs(left) do
        if not (("number" == type(k)) and tostring(left[(k - 1)]):find("^&")) then
          if (utils["sym?"](k) and (tostring(k) == "&")) then
            destructure_kv_rest(s, v, left, excluded_keys, destructure1)
          elseif (utils["sym?"](v) and (tostring(v) == "&")) then
            destructure_rest(s, k, left, destructure1)
          elseif (utils["sym?"](k) and (tostring(k) == "&as")) then
            destructure_sym(v, {utils.expr(tostring(s))}, left)
          elseif (utils["sequence?"](left) and (tostring(v) == "&as")) then
            local _, next_sym, trailing = select(k, unpack(left))
            assert_compile((nil == trailing), "expected &as argument before last parameter", left)
            destructure_sym(next_sym, {utils.expr(tostring(s))}, left)
          else
            local key
            if (type(k) == "string") then
              key = serialize_string(k)
            else
              key = k
            end
            local subexpr = utils.expr(string.format("%s[%s]", s, key), "expression")
            if (type(k) == "string") then
              table.insert(excluded_keys, k)
            else
            end
            destructure1(v, {subexpr}, left)
          end
        else
        end
      end
      return nil
    end
    local function destructure_values(left, up1, top_3f, destructure1)
      local left_names, tables = {}, {}
      for i, name in ipairs(left) do
        if utils["sym?"](name) then
          table.insert(left_names, getname(name, up1))
        else
          local symname = gensym(scope, symtype0)
          table.insert(left_names, symname)
          do end (tables)[i] = {name, utils.expr(symname, "sym")}
        end
      end
      assert_compile(left[1], "must provide at least one value", left)
      assert_compile(top_3f, "can't nest multi-value destructuring", left)
      compile_top_target(left_names)
      if declaration then
        for _, sym in ipairs(left) do
          if utils["sym?"](sym) then
            scope.symmeta[tostring(sym)] = {var = isvar}
          else
          end
        end
      else
      end
      for _, pair in utils.stablepairs(tables) do
        destructure1(pair[1], {pair[2]}, left)
      end
      return nil
    end
    local function destructure1(left, rightexprs, up1, top_3f)
      if (utils["sym?"](left) and (left[1] ~= "nil")) then
        destructure_sym(left, rightexprs, up1, top_3f)
      elseif utils["table?"](left) then
        destructure_table(left, rightexprs, top_3f, destructure1)
      elseif utils["list?"](left) then
        destructure_values(left, up1, top_3f, destructure1)
      else
        assert_compile(false, string.format("unable to bind %s %s", type(left), tostring(left)), (((type((up1)[2]) == "table") and (up1)[2]) or up1))
      end
      if top_3f then
        return {returned = true}
      else
        return nil
      end
    end
    local ret = destructure1(to, nil, ast, true)
    utils.hook("destructure", from, to, scope, opts0)
    apply_manglings(scope, new_manglings, ast)
    return ret
  end
  local function require_include(ast, scope, parent, opts)
    opts.fallback = function(e, no_warn)
      if (not no_warn and ("literal" == e.type)) then
        utils.warn(("include module not found, falling back to require: %s"):format(tostring(e)))
      else
      end
      return utils.expr(string.format("require(%s)", tostring(e)), "statement")
    end
    return scopes.global.specials.include(ast, scope, parent, opts)
  end
  local function compile_stream(strm, options)
    local opts = utils.copy(options)
    local old_globals = allowed_globals
    local scope = (opts.scope or make_scope(scopes.global))
    local vals = {}
    local chunk = {}
    do end (function(tgt, m, ...) return tgt[m](tgt, ...) end)(utils.root, "set-reset")
    allowed_globals = opts.allowedGlobals
    if (opts.indent == nil) then
      opts.indent = "  "
    else
    end
    if opts.requireAsInclude then
      scope.specials.require = require_include
    else
    end
    utils.root.chunk, utils.root.scope, utils.root.options = chunk, scope, opts
    for _, val in parser.parser(strm, opts.filename, opts) do
      table.insert(vals, val)
    end
    for i = 1, #vals do
      local exprs = compile1(vals[i], scope, chunk, {nval = (((i < #vals) and 0) or nil), tail = (i == #vals)})
      keep_side_effects(exprs, chunk, nil, vals[i])
      if (i == #vals) then
        utils.hook("chunk", vals[i], scope)
      else
      end
    end
    allowed_globals = old_globals
    utils.root.reset()
    return flatten(chunk, opts)
  end
  local function compile_string(str, opts)
    return compile_stream(parser["string-stream"](str), (opts or {}))
  end
  local function compile(ast, opts)
    local opts0 = utils.copy(opts)
    local old_globals = allowed_globals
    local chunk = {}
    local scope = (opts0.scope or make_scope(scopes.global))
    do end (function(tgt, m, ...) return tgt[m](tgt, ...) end)(utils.root, "set-reset")
    allowed_globals = opts0.allowedGlobals
    if (opts0.indent == nil) then
      opts0.indent = "  "
    else
    end
    if opts0.requireAsInclude then
      scope.specials.require = require_include
    else
    end
    utils.root.chunk, utils.root.scope, utils.root.options = chunk, scope, opts0
    local exprs = compile1(ast, scope, chunk, {tail = true})
    keep_side_effects(exprs, chunk, nil, ast)
    utils.hook("chunk", ast, scope)
    allowed_globals = old_globals
    utils.root.reset()
    return flatten(chunk, opts0)
  end
  local function traceback_frame(info)
    if ((info.what == "C") and info.name) then
      return string.format("  [C]: in function '%s'", info.name)
    elseif (info.what == "C") then
      return "  [C]: in ?"
    else
      local remap = sourcemap[info.source]
      if (remap and remap[info.currentline]) then
        if ((remap[info.currentline][1] or "unknown") ~= "unknown") then
          info.short_src = sourcemap[("@" .. remap[info.currentline][1])].short_src
        else
          info.short_src = remap.short_src
        end
        info.currentline = (remap[info.currentline][2] or -1)
      else
      end
      if (info.what == "Lua") then
        local function _395_()
          if info.name then
            return ("'" .. info.name .. "'")
          else
            return "?"
          end
        end
        return string.format("  %s:%d: in function %s", info.short_src, info.currentline, _395_())
      elseif (info.short_src == "(tail call)") then
        return "  (tail call)"
      else
        return string.format("  %s:%d: in main chunk", info.short_src, info.currentline)
      end
    end
  end
  local function traceback(_3fmsg, _3fstart)
    local msg = tostring((_3fmsg or ""))
    if ((msg:find("^Compile error") or msg:find("^Parse error")) and not utils["debug-on?"]("trace")) then
      return msg
    else
      local lines = {}
      if (msg:find(":%d+: Compile error") or msg:find(":%d+: Parse error")) then
        table.insert(lines, msg)
      else
        local newmsg = msg:gsub("^[^:]*:%d+:%s+", "runtime error: ")
        table.insert(lines, newmsg)
      end
      table.insert(lines, "stack traceback:")
      local done_3f, level = false, (_3fstart or 2)
      while not done_3f do
        do
          local _399_ = debug.getinfo(level, "Sln")
          if (_399_ == nil) then
            done_3f = true
          elseif (nil ~= _399_) then
            local info = _399_
            table.insert(lines, traceback_frame(info))
          else
          end
        end
        level = (level + 1)
      end
      return table.concat(lines, "\n")
    end
  end
  local function entry_transform(fk, fv)
    local function _402_(k, v)
      if (type(k) == "number") then
        return k, fv(v)
      else
        return fk(k), fv(v)
      end
    end
    return _402_
  end
  local function mixed_concat(t, joiner)
    local seen = {}
    local ret, s = "", ""
    for k, v in ipairs(t) do
      table.insert(seen, k)
      ret = (ret .. s .. v)
      s = joiner
    end
    for k, v in utils.stablepairs(t) do
      if not seen[k] then
        ret = (ret .. s .. "[" .. k .. "]" .. "=" .. v)
        s = joiner
      else
      end
    end
    return ret
  end
  local function do_quote(form, scope, parent, runtime_3f)
    local function q(x)
      return do_quote(x, scope, parent, runtime_3f)
    end
    if utils["varg?"](form) then
      assert_compile(not runtime_3f, "quoted ... may only be used at compile time", form)
      return "_VARARG"
    elseif utils["sym?"](form) then
      local filename
      if form.filename then
        filename = string.format("%q", form.filename)
      else
        filename = "nil"
      end
      local symstr = tostring(form)
      assert_compile(not runtime_3f, "symbols may only be used at compile time", form)
      if (symstr:find("#$") or symstr:find("#[:.]")) then
        return string.format("sym('%s', {filename=%s, line=%s})", autogensym(symstr, scope), filename, (form.line or "nil"))
      else
        return string.format("sym('%s', {quoted=true, filename=%s, line=%s})", symstr, filename, (form.line or "nil"))
      end
    elseif (utils["list?"](form) and utils["sym?"](form[1]) and (tostring(form[1]) == "unquote")) then
      local payload = form[2]
      local res = unpack(compile1(payload, scope, parent))
      return res[1]
    elseif utils["list?"](form) then
      local mapped
      local function _407_()
        return nil
      end
      mapped = utils.kvmap(form, entry_transform(_407_, q))
      local filename
      if form.filename then
        filename = string.format("%q", form.filename)
      else
        filename = "nil"
      end
      assert_compile(not runtime_3f, "lists may only be used at compile time", form)
      return string.format(("setmetatable({filename=%s, line=%s, bytestart=%s, %s}" .. ", getmetatable(list()))"), filename, (form.line or "nil"), (form.bytestart or "nil"), mixed_concat(mapped, ", "))
    elseif utils["sequence?"](form) then
      local mapped = utils.kvmap(form, entry_transform(q, q))
      local source = getmetatable(form)
      local filename
      if source.filename then
        filename = string.format("%q", source.filename)
      else
        filename = "nil"
      end
      local _410_
      if source then
        _410_ = source.line
      else
        _410_ = "nil"
      end
      return string.format("setmetatable({%s}, {filename=%s, line=%s, sequence=%s})", mixed_concat(mapped, ", "), filename, _410_, "(getmetatable(sequence()))['sequence']")
    elseif (type(form) == "table") then
      local mapped = utils.kvmap(form, entry_transform(q, q))
      local source = getmetatable(form)
      local filename
      if source.filename then
        filename = string.format("%q", source.filename)
      else
        filename = "nil"
      end
      local function _413_()
        if source then
          return source.line
        else
          return "nil"
        end
      end
      return string.format("setmetatable({%s}, {filename=%s, line=%s})", mixed_concat(mapped, ", "), filename, _413_())
    elseif (type(form) == "string") then
      return serialize_string(form)
    else
      return tostring(form)
    end
  end
  return {compile = compile, compile1 = compile1, ["compile-stream"] = compile_stream, ["compile-string"] = compile_string, ["check-binding-valid"] = check_binding_valid, emit = emit, destructure = destructure, ["require-include"] = require_include, autogensym = autogensym, gensym = gensym, ["do-quote"] = do_quote, ["global-mangling"] = global_mangling, ["global-unmangling"] = global_unmangling, ["apply-manglings"] = apply_manglings, macroexpand = macroexpand_2a, ["declare-local"] = declare_local, ["make-scope"] = make_scope, ["keep-side-effects"] = keep_side_effects, ["symbol-to-expression"] = symbol_to_expression, assert = assert_compile, scopes = scopes, traceback = traceback, metadata = make_metadata(), sourcemap = sourcemap}
end
package.preload["fennel.friend"] = package.preload["fennel.friend"] or function(...)
  local utils = require("fennel.utils")
  local utf8_ok_3f, utf8 = pcall(require, "utf8")
  local suggestions = {["unexpected multi symbol (.*)"] = {"removing periods or colons from %s"}, ["use of global (.*) is aliased by a local"] = {"renaming local %s", "refer to the global using _G.%s instead of directly"}, ["local (.*) was overshadowed by a special form or macro"] = {"renaming local %s"}, ["global (.*) conflicts with local"] = {"renaming local %s"}, ["expected var (.*)"] = {"declaring %s using var instead of let/local", "introducing a new local instead of changing the value of %s"}, ["expected macros to be table"] = {"ensuring your macro definitions return a table"}, ["expected each macro to be function"] = {"ensuring that the value for each key in your macros table contains a function", "avoid defining nested macro tables"}, ["macro not found in macro module"] = {"checking the keys of the imported macro module's returned table"}, ["macro tried to bind (.*) without gensym"] = {"changing to %s# when introducing identifiers inside macros"}, ["unknown identifier in strict mode: (.*)"] = {"looking to see if there's a typo", "using the _G table instead, eg. _G.%s if you really want a global", "moving this code to somewhere that %s is in scope", "binding %s as a local in the scope of this code"}, ["expected a function.* to call"] = {"removing the empty parentheses", "using square brackets if you want an empty table"}, ["cannot call literal value"] = {"checking for typos", "checking for a missing function name"}, ["unexpected vararg"] = {"putting \"...\" at the end of the fn parameters if the vararg was intended"}, ["multisym method calls may only be in call position"] = {"using a period instead of a colon to reference a table's fields", "putting parens around this"}, ["unused local (.*)"] = {"renaming the local to _%s if it is meant to be unused", "fixing a typo so %s is used", "disabling the linter which checks for unused locals"}, ["expected parameters"] = {"adding function parameters as a list of identifiers in brackets"}, ["unable to bind (.*)"] = {"replacing the %s with an identifier"}, ["expected rest argument before last parameter"] = {"moving & to right before the final identifier when destructuring"}, ["expected vararg as last parameter"] = {"moving the \"...\" to the end of the parameter list"}, ["expected symbol for function parameter: (.*)"] = {"changing %s to an identifier instead of a literal value"}, ["could not compile value of type "] = {"debugging the macro you're calling to return a list or table"}, ["expected local"] = {"looking for a typo", "looking for a local which is used out of its scope"}, ["expected body expression"] = {"putting some code in the body of this form after the bindings"}, ["expected binding and iterator"] = {"making sure you haven't omitted a local name or iterator"}, ["expected binding sequence"] = {"placing a table here in square brackets containing identifiers to bind"}, ["expected even number of name/value bindings"] = {"finding where the identifier or value is missing"}, ["may only be used at compile time"] = {"moving this to inside a macro if you need to manipulate symbols/lists", "using square brackets instead of parens to construct a table"}, ["unexpected closing delimiter (.)"] = {"deleting %s", "adding matching opening delimiter earlier"}, ["mismatched closing delimiter (.), expected (.)"] = {"replacing %s with %s", "deleting %s", "adding matching opening delimiter earlier"}, ["expected even number of values in table literal"] = {"removing a key", "adding a value"}, ["expected whitespace before opening delimiter"] = {"adding whitespace"}, ["invalid character: (.)"] = {"deleting or replacing %s", "avoiding reserved characters like \", \\, ', ~, ;, @, `, and comma"}, ["could not read number (.*)"] = {"removing the non-digit character", "beginning the identifier with a non-digit if it is not meant to be a number"}, ["can't start multisym segment with a digit"] = {"removing the digit", "adding a non-digit before the digit"}, ["malformed multisym"] = {"ensuring each period or colon is not followed by another period or colon"}, ["method must be last component"] = {"using a period instead of a colon for field access", "removing segments after the colon", "making the method call, then looking up the field on the result"}, ["$ and $... in hashfn are mutually exclusive"] = {"modifying the hashfn so it only contains $... or $, $1, $2, $3, etc"}, ["tried to reference a macro at runtime"] = {"renaming the macro so as not to conflict with locals"}, ["tried to reference a special form at runtime"] = {"wrapping the special in a function if you need it to be first class"}, ["missing subject"] = {"adding an item to operate on"}, ["expected even number of pattern/body pairs"] = {"checking that every pattern has a body to go with it", "adding _ before the final body"}, ["expected at least one pattern/body pair"] = {"adding a pattern and a body to execute when the pattern matches"}, ["unexpected arguments"] = {"removing an argument", "checking for typos"}, ["unexpected iterator clause"] = {"removing an argument", "checking for typos"}}
  local unpack = (table.unpack or _G.unpack)
  local function suggest(msg)
    local suggestion = nil
    for pat, sug in pairs(suggestions) do
      local matches = {msg:match(pat)}
      if (0 < #matches) then
        if ("table" == type(sug)) then
          local out = {}
          for _, s in ipairs(sug) do
            table.insert(out, s:format(unpack(matches)))
          end
          suggestion = out
        else
          suggestion = sug(matches)
        end
      else
      end
    end
    return suggestion
  end
  local function read_line(filename, line, _3fsource)
    if _3fsource then
      local matcher = string.gmatch((_3fsource .. "\n"), "(.-)(\13?\n)")
      for _ = 2, line do
        matcher()
      end
      return matcher()
    else
      local f = assert(io.open(filename))
      local function close_handlers_8_auto(ok_9_auto, ...)
        f:close()
        if ok_9_auto then
          return ...
        else
          return error(..., 0)
        end
      end
      local function _178_()
        for _ = 2, line do
          f:read()
        end
        return f:read()
      end
      return close_handlers_8_auto(_G.xpcall(_178_, (package.loaded.fennel or debug).traceback))
    end
  end
  local function sub(str, start, _end)
    if (_end < start) then
      return ""
    elseif utf8_ok_3f then
      return string.sub(str, utf8.offset(str, start), ((utf8.offset(str, (_end + 1)) or (utf8.len(str) + 1)) - 1))
    else
      return string.sub(str, start, math.min(_end, str:len()))
    end
  end
  local function highlight_line(codeline, col, _3fendcol)
    local endcol = (_3fendcol or col)
    local eol
    if utf8_ok_3f then
      eol = utf8.len(codeline)
    else
      eol = string.len(codeline)
    end
    return (sub(codeline, 1, col) .. "\27[7m" .. sub(codeline, (col + 1), (endcol + 1)) .. "\27[0m" .. sub(codeline, (endcol + 2), eol))
  end
  local function friendly_msg(msg, _182_, source)
    local _arg_183_ = _182_
    local filename = _arg_183_["filename"]
    local line = _arg_183_["line"]
    local col = _arg_183_["col"]
    local endcol = _arg_183_["endcol"]
    local ok, codeline = pcall(read_line, filename, line, source)
    local out = {msg, ""}
    if (ok and codeline) then
      if col then
        table.insert(out, highlight_line(codeline, col, endcol))
      else
        table.insert(out, codeline)
      end
    else
    end
    for _, suggestion in ipairs((suggest(msg) or {})) do
      table.insert(out, ("* Try %s."):format(suggestion))
    end
    return table.concat(out, "\n")
  end
  local function assert_compile(condition, msg, ast, source)
    if not condition then
      local _let_186_ = utils["ast-source"](ast)
      local filename = _let_186_["filename"]
      local line = _let_186_["line"]
      local col = _let_186_["col"]
      error(friendly_msg(("Compile error in %s:%s:%s\n  %s"):format((filename or "unknown"), (line or "?"), (col or "?"), msg), utils["ast-source"](ast), source), 0)
    else
    end
    return condition
  end
  local function parse_error(msg, filename, line, col, source)
    return error(friendly_msg(("Parse error in %s:%s:%s\n  %s"):format(filename, line, col, msg), {filename = filename, line = line, col = col}, source), 0)
  end
  return {["assert-compile"] = assert_compile, ["parse-error"] = parse_error}
end
package.preload["fennel.parser"] = package.preload["fennel.parser"] or function(...)
  local utils = require("fennel.utils")
  local friend = require("fennel.friend")
  local unpack = (table.unpack or _G.unpack)
  local function granulate(getchunk)
    local c, index, done_3f = "", 1, false
    local function _188_(parser_state)
      if not done_3f then
        if (index <= #c) then
          local b = c:byte(index)
          index = (index + 1)
          return b
        else
          local _189_ = getchunk(parser_state)
          local function _190_()
            local char = _189_
            return (char ~= "")
          end
          if ((nil ~= _189_) and _190_()) then
            local char = _189_
            c = char
            index = 2
            return c:byte()
          elseif true then
            local _ = _189_
            done_3f = true
            return nil
          else
            return nil
          end
        end
      else
        return nil
      end
    end
    local function _194_()
      c = ""
      return nil
    end
    return _188_, _194_
  end
  local function string_stream(str)
    local str0 = str:gsub("^#!", ";;")
    local index = 1
    local function _195_()
      local r = str0:byte(index)
      index = (index + 1)
      return r
    end
    return _195_
  end
  local delims = {[40] = 41, [41] = true, [91] = 93, [93] = true, [123] = 125, [125] = true}
  local function sym_char_3f(b)
    local b0
    if ("number" == type(b)) then
      b0 = b
    else
      b0 = string.byte(b)
    end
    return ((32 < b0) and not delims[b0] and (b0 ~= 127) and (b0 ~= 34) and (b0 ~= 39) and (b0 ~= 126) and (b0 ~= 59) and (b0 ~= 44) and (b0 ~= 64) and (b0 ~= 96))
  end
  local prefixes = {[35] = "hashfn", [39] = "quote", [44] = "unquote", [96] = "quote"}
  local function char_starter_3f(b)
    return ((function(_197_,_198_,_199_) return (_197_ < _198_) and (_198_ < _199_) end)(1,b,127) or (function(_200_,_201_,_202_) return (_200_ < _201_) and (_201_ < _202_) end)(192,b,247))
  end
  local function parser_fn(getbyte, filename, _203_)
    local _arg_204_ = _203_
    local source = _arg_204_["source"]
    local unfriendly = _arg_204_["unfriendly"]
    local comments = _arg_204_["comments"]
    local options = _arg_204_
    local stack = {}
    local line, byteindex, col, prev_col, lastb = 1, 0, 0, 0, nil
    local function ungetb(ub)
      if char_starter_3f(ub) then
        col = (col - 1)
      else
      end
      if (ub == 10) then
        line, col = (line - 1), prev_col
      else
      end
      byteindex = (byteindex - 1)
      lastb = ub
      return nil
    end
    local function getb()
      local r = nil
      if lastb then
        r, lastb = lastb, nil
      else
        r = getbyte({["stack-size"] = #stack})
      end
      byteindex = (byteindex + 1)
      if (r and char_starter_3f(r)) then
        col = (col + 1)
      else
      end
      if (r == 10) then
        line, col, prev_col = (line + 1), 0, col
      else
      end
      return r
    end
    local function whitespace_3f(b)
      local function _214_()
        local t_213_ = options.whitespace
        if (nil ~= t_213_) then
          t_213_ = (t_213_)[b]
        else
        end
        return t_213_
      end
      return ((b == 32) or (function(_210_,_211_,_212_) return (_210_ <= _211_) and (_211_ <= _212_) end)(9,b,13) or _214_())
    end
    local function parse_error(msg, _3fcol_adjust)
      local col0 = (col + (_3fcol_adjust or -1))
      if (nil == utils["hook-opts"]("parse-error", options, msg, filename, (line or "?"), col0, source, utils.root.reset)) then
        utils.root.reset()
        if (unfriendly or not _G.io or not _G.io.read) then
          return error(string.format("%s:%s:%s Parse error: %s", filename, (line or "?"), col0, msg), 0)
        else
          return friend["parse-error"](msg, filename, (line or "?"), col0, source)
        end
      else
        return nil
      end
    end
    local function parse_stream()
      local whitespace_since_dispatch, done_3f, retval = true
      local function set_source_fields(source0)
        source0.byteend, source0.endcol = byteindex, (col - 1)
        return nil
      end
      local function dispatch(v)
        local _218_ = stack[#stack]
        if (_218_ == nil) then
          retval, done_3f, whitespace_since_dispatch = v, true, false
          return nil
        elseif ((_G.type(_218_) == "table") and (nil ~= (_218_).prefix)) then
          local prefix = (_218_).prefix
          local source0
          do
            local _219_ = table.remove(stack)
            set_source_fields(_219_)
            source0 = _219_
          end
          local list = utils.list(utils.sym(prefix, source0), v)
          for k, v0 in pairs(source0) do
            list[k] = v0
          end
          return dispatch(list)
        elseif (nil ~= _218_) then
          local top = _218_
          whitespace_since_dispatch = false
          return table.insert(top, v)
        else
          return nil
        end
      end
      local function badend()
        local accum = utils.map(stack, "closer")
        local _221_
        if (#stack == 1) then
          _221_ = ""
        else
          _221_ = "s"
        end
        return parse_error(string.format("expected closing delimiter%s %s", _221_, string.char(unpack(accum))))
      end
      local function skip_whitespace(b)
        if (b and whitespace_3f(b)) then
          whitespace_since_dispatch = true
          return skip_whitespace(getb())
        elseif (not b and (0 < #stack)) then
          return badend()
        else
          return b
        end
      end
      local function parse_comment(b, contents)
        if (b and (10 ~= b)) then
          local function _225_()
            local _224_ = contents
            table.insert(_224_, string.char(b))
            return _224_
          end
          return parse_comment(getb(), _225_())
        elseif comments then
          ungetb(10)
          return dispatch(utils.comment(table.concat(contents), {line = (line - 1), filename = filename}))
        else
          return nil
        end
      end
      local function open_table(b)
        if not whitespace_since_dispatch then
          parse_error(("expected whitespace before opening delimiter " .. string.char(b)))
        else
        end
        return table.insert(stack, {bytestart = byteindex, closer = delims[b], filename = filename, line = line, col = (col - 1)})
      end
      local function close_list(list)
        return dispatch(setmetatable(list, getmetatable(utils.list())))
      end
      local function close_sequence(tbl)
        local val = utils.sequence(unpack(tbl))
        for k, v in pairs(tbl) do
          getmetatable(val)[k] = v
        end
        return dispatch(val)
      end
      local function add_comment_at(comments0, index, node)
        local _228_ = (comments0)[index]
        if (nil ~= _228_) then
          local existing = _228_
          return table.insert(existing, node)
        elseif true then
          local _ = _228_
          comments0[index] = {node}
          return nil
        else
          return nil
        end
      end
      local function next_noncomment(tbl, i)
        if utils["comment?"](tbl[i]) then
          return next_noncomment(tbl, (i + 1))
        else
          return tbl[i]
        end
      end
      local function extract_comments(tbl)
        local comments0 = {keys = {}, values = {}, last = {}}
        while utils["comment?"](tbl[#tbl]) do
          table.insert(comments0.last, 1, table.remove(tbl))
        end
        local last_key_3f = false
        for i, node in ipairs(tbl) do
          if not utils["comment?"](node) then
            last_key_3f = not last_key_3f
          elseif last_key_3f then
            add_comment_at(comments0.values, next_noncomment(tbl, i), node)
          else
            add_comment_at(comments0.keys, next_noncomment(tbl, i), node)
          end
        end
        for i = #tbl, 1, -1 do
          if utils["comment?"](tbl[i]) then
            table.remove(tbl, i)
          else
          end
        end
        return comments0
      end
      local function close_curly_table(tbl)
        local comments0 = extract_comments(tbl)
        local keys = {}
        local val = {}
        if ((#tbl % 2) ~= 0) then
          byteindex = (byteindex - 1)
          parse_error("expected even number of values in table literal")
        else
        end
        setmetatable(val, tbl)
        for i = 1, #tbl, 2 do
          if ((tostring(tbl[i]) == ":") and utils["sym?"](tbl[(i + 1)]) and utils["sym?"](tbl[i])) then
            tbl[i] = tostring(tbl[(i + 1)])
          else
          end
          val[tbl[i]] = tbl[(i + 1)]
          table.insert(keys, tbl[i])
        end
        tbl.comments = comments0
        tbl.keys = keys
        return dispatch(val)
      end
      local function close_table(b)
        local top = table.remove(stack)
        if (top == nil) then
          parse_error(("unexpected closing delimiter " .. string.char(b)))
        else
        end
        if (top.closer and (top.closer ~= b)) then
          parse_error(("mismatched closing delimiter " .. string.char(b) .. ", expected " .. string.char(top.closer)))
        else
        end
        set_source_fields(top)
        if (b == 41) then
          return close_list(top)
        elseif (b == 93) then
          return close_sequence(top)
        else
          return close_curly_table(top)
        end
      end
      local function parse_string_loop(chars, b, state)
        table.insert(chars, b)
        local state0
        do
          local _238_ = {state, b}
          if ((_G.type(_238_) == "table") and ((_238_)[1] == "base") and ((_238_)[2] == 92)) then
            state0 = "backslash"
          elseif ((_G.type(_238_) == "table") and ((_238_)[1] == "base") and ((_238_)[2] == 34)) then
            state0 = "done"
          elseif ((_G.type(_238_) == "table") and ((_238_)[1] == "backslash") and ((_238_)[2] == 10)) then
            table.remove(chars, (#chars - 1))
            state0 = "base"
          elseif true then
            local _ = _238_
            state0 = "base"
          else
            state0 = nil
          end
        end
        if (b and (state0 ~= "done")) then
          return parse_string_loop(chars, getb(), state0)
        else
          return b
        end
      end
      local function escape_char(c)
        return ({[7] = "\\a", [8] = "\\b", [9] = "\\t", [10] = "\\n", [11] = "\\v", [12] = "\\f", [13] = "\\r"})[c:byte()]
      end
      local function parse_string()
        table.insert(stack, {closer = 34})
        local chars = {34}
        if not parse_string_loop(chars, getb(), "base") then
          badend()
        else
        end
        table.remove(stack)
        local raw = string.char(unpack(chars))
        local formatted = raw:gsub("[\7-\13]", escape_char)
        local _242_ = (rawget(_G, "loadstring") or load)(("return " .. formatted))
        if (nil ~= _242_) then
          local load_fn = _242_
          return dispatch(load_fn())
        elseif (_242_ == nil) then
          return parse_error(("Invalid string: " .. raw))
        else
          return nil
        end
      end
      local function parse_prefix(b)
        table.insert(stack, {prefix = prefixes[b], filename = filename, line = line, bytestart = byteindex, col = (col - 1)})
        local nextb = getb()
        if (whitespace_3f(nextb) or (true == delims[nextb])) then
          if (b ~= 35) then
            parse_error("invalid whitespace after quoting prefix")
          else
          end
          table.remove(stack)
          dispatch(utils.sym("#"))
        else
        end
        return ungetb(nextb)
      end
      local function parse_sym_loop(chars, b)
        if (b and sym_char_3f(b)) then
          table.insert(chars, b)
          return parse_sym_loop(chars, getb())
        else
          if b then
            ungetb(b)
          else
          end
          return chars
        end
      end
      local function parse_number(rawstr)
        local number_with_stripped_underscores = (not rawstr:find("^_") and rawstr:gsub("_", ""))
        if rawstr:match("^%d") then
          dispatch((tonumber(number_with_stripped_underscores) or parse_error(("could not read number \"" .. rawstr .. "\""))))
          return true
        else
          local _248_ = tonumber(number_with_stripped_underscores)
          if (nil ~= _248_) then
            local x = _248_
            dispatch(x)
            return true
          elseif true then
            local _ = _248_
            return false
          else
            return nil
          end
        end
      end
      local function check_malformed_sym(rawstr)
        local function col_adjust(pat)
          return (rawstr:find(pat) - utils.len(rawstr) - 1)
        end
        if (rawstr:match("^~") and (rawstr ~= "~=")) then
          return parse_error("invalid character: ~")
        elseif rawstr:match("%.[0-9]") then
          return parse_error(("can't start multisym segment with a digit: " .. rawstr), col_adjust("%.[0-9]"))
        elseif (rawstr:match("[%.:][%.:]") and (rawstr ~= "..") and (rawstr ~= "$...")) then
          return parse_error(("malformed multisym: " .. rawstr), col_adjust("[%.:][%.:]"))
        elseif ((rawstr ~= ":") and rawstr:match(":$")) then
          return parse_error(("malformed multisym: " .. rawstr), col_adjust(":$"))
        elseif rawstr:match(":.+[%.:]") then
          return parse_error(("method must be last component of multisym: " .. rawstr), col_adjust(":.+[%.:]"))
        else
          return rawstr
        end
      end
      local function parse_sym(b)
        local source0 = {bytestart = byteindex, filename = filename, line = line, col = (col - 1)}
        local rawstr = string.char(unpack(parse_sym_loop({b}, getb())))
        set_source_fields(source0)
        if (rawstr == "true") then
          return dispatch(true)
        elseif (rawstr == "false") then
          return dispatch(false)
        elseif (rawstr == "...") then
          return dispatch(utils.varg(source0))
        elseif rawstr:match("^:.+$") then
          return dispatch(rawstr:sub(2))
        elseif not parse_number(rawstr) then
          return dispatch(utils.sym(check_malformed_sym(rawstr), source0))
        else
          return nil
        end
      end
      local function parse_loop(b)
        if not b then
        elseif (b == 59) then
          parse_comment(getb(), {";"})
        elseif (type(delims[b]) == "number") then
          open_table(b)
        elseif delims[b] then
          close_table(b)
        elseif (b == 34) then
          parse_string(b)
        elseif prefixes[b] then
          parse_prefix(b)
        elseif (sym_char_3f(b) or (b == string.byte("~"))) then
          parse_sym(b)
        elseif not utils["hook-opts"]("illegal-char", options, b, getb, ungetb, dispatch) then
          parse_error(("invalid character: " .. string.char(b)))
        else
        end
        if not b then
          return nil
        elseif done_3f then
          return true, retval
        else
          return parse_loop(skip_whitespace(getb()))
        end
      end
      return parse_loop(skip_whitespace(getb()))
    end
    local function _255_()
      stack, line, byteindex, col, lastb = {}, 1, 0, 0, nil
      return nil
    end
    return parse_stream, _255_
  end
  local function parser(stream_or_string, _3ffilename, _3foptions)
    local filename = (_3ffilename or "unknown")
    local options = (_3foptions or utils.root.options or {})
    assert(("string" == type(filename)), "expected filename as second argument to parser")
    if ("string" == type(stream_or_string)) then
      return parser_fn(string_stream(stream_or_string), filename, options)
    else
      return parser_fn(stream_or_string, filename, options)
    end
  end
  return {granulate = granulate, parser = parser, ["string-stream"] = string_stream, ["sym-char?"] = sym_char_3f}
end
local utils
package.preload["fennel.view"] = package.preload["fennel.view"] or function(...)
  local type_order = {number = 1, boolean = 2, string = 3, table = 4, ["function"] = 5, userdata = 6, thread = 7}
  local lua_pairs = pairs
  local lua_ipairs = ipairs
  local function pairs(t)
    local _1_ = getmetatable(t)
    if ((_G.type(_1_) == "table") and (nil ~= (_1_).__pairs)) then
      local p = (_1_).__pairs
      return p(t)
    elseif true then
      local _ = _1_
      return lua_pairs(t)
    else
      return nil
    end
  end
  local function ipairs(t)
    local _3_ = getmetatable(t)
    if ((_G.type(_3_) == "table") and (nil ~= (_3_).__ipairs)) then
      local i = (_3_).__ipairs
      return i(t)
    elseif true then
      local _ = _3_
      return lua_ipairs(t)
    else
      return nil
    end
  end
  local function length_2a(t)
    local _5_ = getmetatable(t)
    if ((_G.type(_5_) == "table") and (nil ~= (_5_).__len)) then
      local l = (_5_).__len
      return l(t)
    elseif true then
      local _ = _5_
      return #t
    else
      return nil
    end
  end
  local function sort_keys(_7_, _9_)
    local _arg_8_ = _7_
    local a = _arg_8_[1]
    local _arg_10_ = _9_
    local b = _arg_10_[1]
    local ta = type(a)
    local tb = type(b)
    if ((ta == tb) and ((ta == "string") or (ta == "number"))) then
      return (a < b)
    else
      local dta = type_order[ta]
      local dtb = type_order[tb]
      if (dta and dtb) then
        return (dta < dtb)
      elseif dta then
        return true
      elseif dtb then
        return false
      else
        return (ta < tb)
      end
    end
  end
  local function max_index_gap(kv)
    local gap = 0
    if (0 < length_2a(kv)) then
      local i = 0
      for _, _13_ in ipairs(kv) do
        local _each_14_ = _13_
        local k = _each_14_[1]
        if (gap < (k - i)) then
          gap = (k - i)
        else
        end
        i = k
      end
    else
    end
    return gap
  end
  local function fill_gaps(kv)
    local missing_indexes = {}
    local i = 0
    for _, _17_ in ipairs(kv) do
      local _each_18_ = _17_
      local j = _each_18_[1]
      i = (i + 1)
      while (i < j) do
        table.insert(missing_indexes, i)
        i = (i + 1)
      end
    end
    for _, k in ipairs(missing_indexes) do
      table.insert(kv, k, {k})
    end
    return nil
  end
  local function table_kv_pairs(t, options)
    local assoc_3f = false
    local kv = {}
    local insert = table.insert
    for k, v in pairs(t) do
      if ((type(k) ~= "number") or (k < 1)) then
        assoc_3f = true
      else
      end
      insert(kv, {k, v})
    end
    table.sort(kv, sort_keys)
    if not assoc_3f then
      if (options["max-sparse-gap"] < max_index_gap(kv)) then
        assoc_3f = true
      else
        fill_gaps(kv)
      end
    else
    end
    if (length_2a(kv) == 0) then
      return kv, "empty"
    else
      local function _22_()
        if assoc_3f then
          return "table"
        else
          return "seq"
        end
      end
      return kv, _22_()
    end
  end
  local function count_table_appearances(t, appearances)
    if (type(t) == "table") then
      if not appearances[t] then
        appearances[t] = 1
        for k, v in pairs(t) do
          count_table_appearances(k, appearances)
          count_table_appearances(v, appearances)
        end
      else
        appearances[t] = ((appearances[t] or 0) + 1)
      end
    else
    end
    return appearances
  end
  local function save_table(t, seen)
    local seen0 = (seen or {len = 0})
    local id = (seen0.len + 1)
    if not (seen0)[t] then
      seen0[t] = id
      seen0.len = id
    else
    end
    return seen0
  end
  local function detect_cycle(t, seen, _3fk)
    if ("table" == type(t)) then
      seen[t] = true
      local _27_, _28_ = next(t, _3fk)
      if ((nil ~= _27_) and (nil ~= _28_)) then
        local k = _27_
        local v = _28_
        return (seen[k] or detect_cycle(k, seen) or seen[v] or detect_cycle(v, seen) or detect_cycle(t, seen, k))
      else
        return nil
      end
    else
      return nil
    end
  end
  local function visible_cycle_3f(t, options)
    return (options["detect-cycles?"] and detect_cycle(t, {}) and save_table(t, options.seen) and (1 < (options.appearances[t] or 0)))
  end
  local function table_indent(indent, id)
    local opener_length
    if id then
      opener_length = (length_2a(tostring(id)) + 2)
    else
      opener_length = 1
    end
    return (indent + opener_length)
  end
  local pp = nil
  local function concat_table_lines(elements, options, multiline_3f, indent, table_type, prefix, last_comment_3f)
    local indent_str = ("\n" .. string.rep(" ", indent))
    local open
    local function _32_()
      if ("seq" == table_type) then
        return "["
      else
        return "{"
      end
    end
    open = ((prefix or "") .. _32_())
    local close
    if ("seq" == table_type) then
      close = "]"
    else
      close = "}"
    end
    local oneline = (open .. table.concat(elements, " ") .. close)
    if (not options["one-line?"] and (multiline_3f or (options["line-length"] < (indent + length_2a(oneline))) or last_comment_3f)) then
      local function _34_()
        if last_comment_3f then
          return indent_str
        else
          return ""
        end
      end
      return (open .. table.concat(elements, indent_str) .. _34_() .. close)
    else
      return oneline
    end
  end
  local function utf8_len(x)
    local n = 0
    for _ in string.gmatch(x, "[%z\1-\127\192-\247]") do
      n = (n + 1)
    end
    return n
  end
  local function comment_3f(x)
    if ("table" == type(x)) then
      local fst = x[1]
      return (("string" == type(fst)) and (nil ~= fst:find("^;")))
    else
      return false
    end
  end
  local function pp_associative(t, kv, options, indent)
    local multiline_3f = false
    local id = options.seen[t]
    if (options.depth <= options.level) then
      return "{...}"
    elseif (id and options["detect-cycles?"]) then
      return ("@" .. id .. "{...}")
    else
      local visible_cycle_3f0 = visible_cycle_3f(t, options)
      local id0 = (visible_cycle_3f0 and options.seen[t])
      local indent0 = table_indent(indent, id0)
      local slength
      if options["utf8?"] then
        slength = utf8_len
      else
        local function _37_(_241)
          return #_241
        end
        slength = _37_
      end
      local prefix
      if visible_cycle_3f0 then
        prefix = ("@" .. id0)
      else
        prefix = ""
      end
      local items
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, _40_ in ipairs(kv) do
          local _each_41_ = _40_
          local k = _each_41_[1]
          local v = _each_41_[2]
          local val_16_auto
          do
            local k0 = pp(k, options, (indent0 + 1), true)
            local v0 = pp(v, options, (indent0 + slength(k0) + 1))
            multiline_3f = (multiline_3f or k0:find("\n") or v0:find("\n"))
            val_16_auto = (k0 .. " " .. v0)
          end
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        items = tbl_14_auto
      end
      return concat_table_lines(items, options, multiline_3f, indent0, "table", prefix, false)
    end
  end
  local function pp_sequence(t, kv, options, indent)
    local multiline_3f = false
    local id = options.seen[t]
    if (options.depth <= options.level) then
      return "[...]"
    elseif (id and options["detect-cycles?"]) then
      return ("@" .. id .. "[...]")
    else
      local visible_cycle_3f0 = visible_cycle_3f(t, options)
      local id0 = (visible_cycle_3f0 and options.seen[t])
      local indent0 = table_indent(indent, id0)
      local prefix
      if visible_cycle_3f0 then
        prefix = ("@" .. id0)
      else
        prefix = ""
      end
      local last_comment_3f = comment_3f(t[#t])
      local items
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, _45_ in ipairs(kv) do
          local _each_46_ = _45_
          local _0 = _each_46_[1]
          local v = _each_46_[2]
          local val_16_auto
          do
            local v0 = pp(v, options, indent0)
            multiline_3f = (multiline_3f or v0:find("\n") or v0:find("^;"))
            val_16_auto = v0
          end
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        items = tbl_14_auto
      end
      return concat_table_lines(items, options, multiline_3f, indent0, "seq", prefix, last_comment_3f)
    end
  end
  local function concat_lines(lines, options, indent, force_multi_line_3f)
    if (length_2a(lines) == 0) then
      if options["empty-as-sequence?"] then
        return "[]"
      else
        return "{}"
      end
    else
      local oneline
      local _50_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for _, line in ipairs(lines) do
          local val_16_auto = line:gsub("^%s+", "")
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _50_ = tbl_14_auto
      end
      oneline = table.concat(_50_, " ")
      if (not options["one-line?"] and (force_multi_line_3f or oneline:find("\n") or (options["line-length"] < (indent + length_2a(oneline))))) then
        return table.concat(lines, ("\n" .. string.rep(" ", indent)))
      else
        return oneline
      end
    end
  end
  local function pp_metamethod(t, metamethod, options, indent)
    if (options.depth <= options.level) then
      if options["empty-as-sequence?"] then
        return "[...]"
      else
        return "{...}"
      end
    else
      local _
      local function _55_(_241)
        return visible_cycle_3f(_241, options)
      end
      options["visible-cycle?"] = _55_
      _ = nil
      local lines, force_multi_line_3f = metamethod(t, pp, options, indent)
      options["visible-cycle?"] = nil
      local _56_ = type(lines)
      if (_56_ == "string") then
        return lines
      elseif (_56_ == "table") then
        return concat_lines(lines, options, indent, force_multi_line_3f)
      elseif true then
        local _0 = _56_
        return error("__fennelview metamethod must return a table of lines")
      else
        return nil
      end
    end
  end
  local function pp_table(x, options, indent)
    options.level = (options.level + 1)
    local x0
    do
      local _59_
      if options["metamethod?"] then
        local _60_ = x
        if (nil ~= _60_) then
          local _61_ = getmetatable(_60_)
          if (nil ~= _61_) then
            _59_ = (_61_).__fennelview
          else
            _59_ = _61_
          end
        else
          _59_ = _60_
        end
      else
        _59_ = nil
      end
      if (nil ~= _59_) then
        local metamethod = _59_
        x0 = pp_metamethod(x, metamethod, options, indent)
      elseif true then
        local _ = _59_
        local _65_, _66_ = table_kv_pairs(x, options)
        if (true and (_66_ == "empty")) then
          local _0 = _65_
          if options["empty-as-sequence?"] then
            x0 = "[]"
          else
            x0 = "{}"
          end
        elseif ((nil ~= _65_) and (_66_ == "table")) then
          local kv = _65_
          x0 = pp_associative(x, kv, options, indent)
        elseif ((nil ~= _65_) and (_66_ == "seq")) then
          local kv = _65_
          x0 = pp_sequence(x, kv, options, indent)
        else
          x0 = nil
        end
      else
        x0 = nil
      end
    end
    options.level = (options.level - 1)
    return x0
  end
  local function number__3estring(n)
    local _70_ = string.gsub(tostring(n), ",", ".")
    return _70_
  end
  local function colon_string_3f(s)
    return s:find("^[-%w?^_!$%&*+./@|<=>]+$")
  end
  local utf8_inits = {{["min-byte"] = 0, ["max-byte"] = 127, ["min-code"] = 0, ["max-code"] = 127, len = 1}, {["min-byte"] = 192, ["max-byte"] = 223, ["min-code"] = 128, ["max-code"] = 2047, len = 2}, {["min-byte"] = 224, ["max-byte"] = 239, ["min-code"] = 2048, ["max-code"] = 65535, len = 3}, {["min-byte"] = 240, ["max-byte"] = 247, ["min-code"] = 65536, ["max-code"] = 1114111, len = 4}}
  local function utf8_escape(str)
    local function validate_utf8(str0, index)
      local inits = utf8_inits
      local byte = string.byte(str0, index)
      local init
      do
        local ret = nil
        for _, init0 in ipairs(inits) do
          if ret then break end
          ret = (byte and (function(_71_,_72_,_73_) return (_71_ <= _72_) and (_72_ <= _73_) end)(init0["min-byte"],byte,init0["max-byte"]) and init0)
        end
        init = ret
      end
      local code
      local function _74_()
        local code0
        if init then
          code0 = (byte - init["min-byte"])
        else
          code0 = nil
        end
        for i = (index + 1), (index + init.len + -1) do
          local byte0 = string.byte(str0, i)
          code0 = (byte0 and code0 and (function(_76_,_77_,_78_) return (_76_ <= _77_) and (_77_ <= _78_) end)(128,byte0,191) and ((code0 * 64) + (byte0 - 128)))
        end
        return code0
      end
      code = (init and _74_())
      if (code and (function(_79_,_80_,_81_) return (_79_ <= _80_) and (_80_ <= _81_) end)(init["min-code"],code,init["max-code"]) and not (function(_82_,_83_,_84_) return (_82_ <= _83_) and (_83_ <= _84_) end)(55296,code,57343)) then
        return init.len
      else
        return nil
      end
    end
    local index = 1
    local output = {}
    while (index <= #str) do
      local nexti = (string.find(str, "[\128-\255]", index) or (#str + 1))
      local len = validate_utf8(str, nexti)
      table.insert(output, string.sub(str, index, (nexti + (len or 0) + -1)))
      if (not len and (nexti <= #str)) then
        table.insert(output, string.format("\\%03d", string.byte(str, nexti)))
      else
      end
      if len then
        index = (nexti + len)
      else
        index = (nexti + 1)
      end
    end
    return table.concat(output)
  end
  local function pp_string(str, options, indent)
    local escs
    local _88_
    if (options["escape-newlines?"] and (length_2a(str) < (options["line-length"] - indent))) then
      _88_ = "\\n"
    else
      _88_ = "\n"
    end
    local function _90_(_241, _242)
      return ("\\%03d"):format(_242:byte())
    end
    escs = setmetatable({["\7"] = "\\a", ["\8"] = "\\b", ["\12"] = "\\f", ["\11"] = "\\v", ["\13"] = "\\r", ["\9"] = "\\t", ["\\"] = "\\\\", ["\""] = "\\\"", ["\n"] = _88_}, {__index = _90_})
    local str0 = ("\"" .. str:gsub("[%c\\\"]", escs) .. "\"")
    if options["utf8?"] then
      return utf8_escape(str0)
    else
      return str0
    end
  end
  local function make_options(t, options)
    local defaults = {["line-length"] = 80, ["one-line?"] = false, depth = 128, ["detect-cycles?"] = true, ["empty-as-sequence?"] = false, ["metamethod?"] = true, ["prefer-colon?"] = false, ["escape-newlines?"] = false, ["utf8?"] = true, ["max-sparse-gap"] = 10}
    local overrides = {level = 0, appearances = count_table_appearances(t, {}), seen = {len = 0}}
    for k, v in pairs((options or {})) do
      defaults[k] = v
    end
    for k, v in pairs(overrides) do
      defaults[k] = v
    end
    return defaults
  end
  local function _92_(x, options, indent, colon_3f)
    local indent0 = (indent or 0)
    local options0 = (options or make_options(x))
    local x0
    if options0.preprocess then
      x0 = options0.preprocess(x, options0)
    else
      x0 = x
    end
    local tv = type(x0)
    local function _95_()
      local _94_ = getmetatable(x0)
      if (nil ~= _94_) then
        return (_94_).__fennelview
      else
        return _94_
      end
    end
    if ((tv == "table") or ((tv == "userdata") and _95_())) then
      return pp_table(x0, options0, indent0)
    elseif (tv == "number") then
      return number__3estring(x0)
    else
      local function _97_()
        if (colon_3f ~= nil) then
          return colon_3f
        elseif ("function" == type(options0["prefer-colon?"])) then
          return options0["prefer-colon?"](x0)
        else
          return options0["prefer-colon?"]
        end
      end
      if ((tv == "string") and colon_string_3f(x0) and _97_()) then
        return (":" .. x0)
      elseif (tv == "string") then
        return pp_string(x0, options0, indent0)
      elseif ((tv == "boolean") or (tv == "nil")) then
        return tostring(x0)
      else
        return ("#<" .. tostring(x0) .. ">")
      end
    end
  end
  pp = _92_
  local function view(x, _3foptions)
    return pp(x, make_options(x, _3foptions), 0)
  end
  return view
end
package.preload["fennel.utils"] = package.preload["fennel.utils"] or function(...)
  local view = require("fennel.view")
  local version = "1.2.1"
  local function luajit_vm_3f()
    return ((nil ~= _G.jit) and (type(_G.jit) == "table") and (nil ~= _G.jit.on) and (nil ~= _G.jit.off) and (type(_G.jit.version_num) == "number"))
  end
  local function luajit_vm_version()
    local jit_os
    if (_G.jit.os == "OSX") then
      jit_os = "macOS"
    else
      jit_os = _G.jit.os
    end
    return (_G.jit.version .. " " .. jit_os .. "/" .. _G.jit.arch)
  end
  local function fengari_vm_3f()
    return ((nil ~= _G.fengari) and (type(_G.fengari) == "table") and (nil ~= _G.fengari.VERSION) and (type(_G.fengari.VERSION_NUM) == "number"))
  end
  local function fengari_vm_version()
    return (_G.fengari.RELEASE .. " (" .. _VERSION .. ")")
  end
  local function lua_vm_version()
    if luajit_vm_3f() then
      return luajit_vm_version()
    elseif fengari_vm_3f() then
      return fengari_vm_version()
    else
      return ("PUC " .. _VERSION)
    end
  end
  local function runtime_version()
    return ("Fennel " .. version .. " on " .. lua_vm_version())
  end
  local function warn(message)
    if (_G.io and _G.io.stderr) then
      return (_G.io.stderr):write(("--WARNING: %s\n"):format(tostring(message)))
    else
      return nil
    end
  end
  local len
  do
    local _102_, _103_ = pcall(require, "utf8")
    if ((_102_ == true) and (nil ~= _103_)) then
      local utf8 = _103_
      len = utf8.len
    elseif true then
      local _ = _102_
      len = string.len
    else
      len = nil
    end
  end
  local function mt_keys_in_order(t, out, used_keys)
    for _, k in ipairs(getmetatable(t).keys) do
      if (t[k] and not used_keys[k]) then
        used_keys[k] = true
        table.insert(out, k)
      else
      end
    end
    for k in pairs(t) do
      if not used_keys[k] then
        table.insert(out, k)
      else
      end
    end
    return out
  end
  local function stablepairs(t)
    local keys
    local _108_
    do
      local t_107_ = getmetatable(t)
      if (nil ~= t_107_) then
        t_107_ = (t_107_).keys
      else
      end
      _108_ = t_107_
    end
    if _108_ then
      keys = mt_keys_in_order(t, {}, {})
    else
      local _110_
      do
        local tbl_14_auto = {}
        local i_15_auto = #tbl_14_auto
        for k in pairs(t) do
          local val_16_auto = k
          if (nil ~= val_16_auto) then
            i_15_auto = (i_15_auto + 1)
            do end (tbl_14_auto)[i_15_auto] = val_16_auto
          else
          end
        end
        _110_ = tbl_14_auto
      end
      local function _112_(_241, _242)
        return (tostring(_241) < tostring(_242))
      end
      table.sort(_110_, _112_)
      keys = _110_
    end
    local succ
    do
      local tbl_11_auto = {}
      for i, k in ipairs(keys) do
        local _114_, _115_ = k, keys[(i + 1)]
        if ((nil ~= _114_) and (nil ~= _115_)) then
          local k_12_auto = _114_
          local v_13_auto = _115_
          tbl_11_auto[k_12_auto] = v_13_auto
        else
        end
      end
      succ = tbl_11_auto
    end
    local function stablenext(tbl, key)
      local next_key
      if (key == nil) then
        next_key = keys[1]
      else
        next_key = succ[key]
      end
      return next_key, tbl[next_key]
    end
    return stablenext, t, nil
  end
  local function get_in(tbl, path, _3ffallback)
    assert(("table" == type(tbl)), "get-in expects path to be a table")
    if (0 == #path) then
      return _3ffallback
    else
      local _118_
      do
        local t = tbl
        for _, k in ipairs(path) do
          if (nil == t) then break end
          local _119_ = type(t)
          if (_119_ == "table") then
            t = t[k]
          else
            t = nil
          end
        end
        _118_ = t
      end
      if (nil ~= _118_) then
        local res = _118_
        return res
      elseif true then
        local _ = _118_
        return _3ffallback
      else
        return nil
      end
    end
  end
  local function map(t, f, _3fout)
    local out = (_3fout or {})
    local f0
    if (type(f) == "function") then
      f0 = f
    else
      local function _123_(_241)
        return (_241)[f]
      end
      f0 = _123_
    end
    for _, x in ipairs(t) do
      local _125_ = f0(x)
      if (nil ~= _125_) then
        local v = _125_
        table.insert(out, v)
      else
      end
    end
    return out
  end
  local function kvmap(t, f, _3fout)
    local out = (_3fout or {})
    local f0
    if (type(f) == "function") then
      f0 = f
    else
      local function _127_(_241)
        return (_241)[f]
      end
      f0 = _127_
    end
    for k, x in stablepairs(t) do
      local _129_, _130_ = f0(k, x)
      if ((nil ~= _129_) and (nil ~= _130_)) then
        local key = _129_
        local value = _130_
        out[key] = value
      elseif (nil ~= _129_) then
        local value = _129_
        table.insert(out, value)
      else
      end
    end
    return out
  end
  local function copy(from, _3fto)
    local tbl_11_auto = (_3fto or {})
    for k, v in pairs((from or {})) do
      local _132_, _133_ = k, v
      if ((nil ~= _132_) and (nil ~= _133_)) then
        local k_12_auto = _132_
        local v_13_auto = _133_
        tbl_11_auto[k_12_auto] = v_13_auto
      else
      end
    end
    return tbl_11_auto
  end
  local function member_3f(x, tbl, _3fn)
    local _135_ = tbl[(_3fn or 1)]
    if (_135_ == x) then
      return true
    elseif (_135_ == nil) then
      return nil
    elseif true then
      local _ = _135_
      return member_3f(x, tbl, ((_3fn or 1) + 1))
    else
      return nil
    end
  end
  local function allpairs(tbl)
    assert((type(tbl) == "table"), "allpairs expects a table")
    local t = tbl
    local seen = {}
    local function allpairs_next(_, state)
      local next_state, value = next(t, state)
      if seen[next_state] then
        return allpairs_next(nil, next_state)
      elseif next_state then
        seen[next_state] = true
        return next_state, value
      else
        local _137_ = getmetatable(t)
        if ((_G.type(_137_) == "table") and true) then
          local __index = (_137_).__index
          if ("table" == type(__index)) then
            t = __index
            return allpairs_next(t)
          else
            return nil
          end
        else
          return nil
        end
      end
    end
    return allpairs_next
  end
  local function deref(self)
    return self[1]
  end
  local nil_sym = nil
  local function list__3estring(self, _3ftostring2)
    local safe = {}
    local max = 0
    for k in pairs(self) do
      if ((type(k) == "number") and (max < k)) then
        max = k
      else
      end
    end
    for i = 1, max do
      safe[i] = (((self[i] == nil) and nil_sym) or self[i])
    end
    return ("(" .. table.concat(map(safe, (_3ftostring2 or view)), " ", 1, max) .. ")")
  end
  local function comment_view(c)
    return c, true
  end
  local function sym_3d(a, b)
    return ((deref(a) == deref(b)) and (getmetatable(a) == getmetatable(b)))
  end
  local function sym_3c(a, b)
    return (a[1] < tostring(b))
  end
  local symbol_mt = {__fennelview = deref, __tostring = deref, __eq = sym_3d, __lt = sym_3c, "SYMBOL"}
  local expr_mt
  local function _142_(x)
    return tostring(deref(x))
  end
  expr_mt = {__tostring = _142_, "EXPR"}
  local list_mt = {__fennelview = list__3estring, __tostring = list__3estring, "LIST"}
  local comment_mt = {__fennelview = comment_view, __tostring = deref, __eq = sym_3d, __lt = sym_3c, "COMMENT"}
  local sequence_marker = {"SEQUENCE"}
  local varg_mt = {__fennelview = deref, __tostring = deref, "VARARG"}
  local getenv
  local function _143_()
    return nil
  end
  getenv = ((os and os.getenv) or _143_)
  local function debug_on_3f(flag)
    local level = (getenv("FENNEL_DEBUG") or "")
    return ((level == "all") or level:find(flag))
  end
  local function list(...)
    return setmetatable({...}, list_mt)
  end
  local function sym(str, _3fsource)
    local _144_
    do
      local tbl_11_auto = {str}
      for k, v in pairs((_3fsource or {})) do
        local _145_, _146_ = nil, nil
        if (type(k) == "string") then
          _145_, _146_ = k, v
        else
          _145_, _146_ = nil
        end
        if ((nil ~= _145_) and (nil ~= _146_)) then
          local k_12_auto = _145_
          local v_13_auto = _146_
          tbl_11_auto[k_12_auto] = v_13_auto
        else
        end
      end
      _144_ = tbl_11_auto
    end
    return setmetatable(_144_, symbol_mt)
  end
  nil_sym = sym("nil")
  local function sequence(...)
    return setmetatable({...}, {sequence = sequence_marker})
  end
  local function expr(strcode, etype)
    return setmetatable({type = etype, strcode}, expr_mt)
  end
  local function comment_2a(contents, _3fsource)
    local _let_149_ = (_3fsource or {})
    local filename = _let_149_["filename"]
    local line = _let_149_["line"]
    return setmetatable({filename = filename, line = line, contents}, comment_mt)
  end
  local function varg(_3fsource)
    local _150_
    do
      local tbl_11_auto = {"..."}
      for k, v in pairs((_3fsource or {})) do
        local _151_, _152_ = nil, nil
        if (type(k) == "string") then
          _151_, _152_ = k, v
        else
          _151_, _152_ = nil
        end
        if ((nil ~= _151_) and (nil ~= _152_)) then
          local k_12_auto = _151_
          local v_13_auto = _152_
          tbl_11_auto[k_12_auto] = v_13_auto
        else
        end
      end
      _150_ = tbl_11_auto
    end
    return setmetatable(_150_, varg_mt)
  end
  local function expr_3f(x)
    return ((type(x) == "table") and (getmetatable(x) == expr_mt) and x)
  end
  local function varg_3f(x)
    return ((type(x) == "table") and (getmetatable(x) == varg_mt) and x)
  end
  local function list_3f(x)
    return ((type(x) == "table") and (getmetatable(x) == list_mt) and x)
  end
  local function sym_3f(x)
    return ((type(x) == "table") and (getmetatable(x) == symbol_mt) and x)
  end
  local function sequence_3f(x)
    local mt = ((type(x) == "table") and getmetatable(x))
    return (mt and (mt.sequence == sequence_marker) and x)
  end
  local function comment_3f(x)
    return ((type(x) == "table") and (getmetatable(x) == comment_mt) and x)
  end
  local function table_3f(x)
    return ((type(x) == "table") and not varg_3f(x) and (getmetatable(x) ~= list_mt) and (getmetatable(x) ~= symbol_mt) and not comment_3f(x) and x)
  end
  local function string_3f(x)
    return (type(x) == "string")
  end
  local function multi_sym_3f(str)
    if sym_3f(str) then
      return multi_sym_3f(tostring(str))
    elseif (type(str) ~= "string") then
      return false
    else
      local parts = {}
      for part in str:gmatch("[^%.%:]+[%.%:]?") do
        local last_char = part:sub(( - 1))
        if (last_char == ":") then
          parts["multi-sym-method-call"] = true
        else
        end
        if ((last_char == ":") or (last_char == ".")) then
          parts[(#parts + 1)] = part:sub(1, ( - 2))
        else
          parts[(#parts + 1)] = part
        end
      end
      return ((0 < #parts) and (str:match("%.") or str:match(":")) and not str:match("%.%.") and (str:byte() ~= string.byte(".")) and (str:byte(( - 1)) ~= string.byte(".")) and parts)
    end
  end
  local function quoted_3f(symbol)
    return symbol.quoted
  end
  local function ast_source(ast)
    if (table_3f(ast) or sequence_3f(ast)) then
      return (getmetatable(ast) or {})
    elseif ("table" == type(ast)) then
      return ast
    else
      return {}
    end
  end
  local function walk_tree(root, f, _3fcustom_iterator)
    local function walk(iterfn, parent, idx, node)
      if f(idx, node, parent) then
        for k, v in iterfn(node) do
          walk(iterfn, node, k, v)
        end
        return nil
      else
        return nil
      end
    end
    walk((_3fcustom_iterator or pairs), nil, nil, root)
    return root
  end
  local lua_keywords = {"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while", "goto"}
  for i, v in ipairs(lua_keywords) do
    lua_keywords[v] = i
  end
  local function valid_lua_identifier_3f(str)
    return (str:match("^[%a_][%w_]*$") and not lua_keywords[str])
  end
  local propagated_options = {"allowedGlobals", "indent", "correlate", "useMetadata", "env", "compiler-env", "compilerEnv"}
  local function propagate_options(options, subopts)
    for _, name in ipairs(propagated_options) do
      subopts[name] = options[name]
    end
    return subopts
  end
  local root
  local function _160_()
  end
  root = {chunk = nil, scope = nil, options = nil, reset = _160_}
  root["set-reset"] = function(_161_)
    local _arg_162_ = _161_
    local chunk = _arg_162_["chunk"]
    local scope = _arg_162_["scope"]
    local options = _arg_162_["options"]
    local reset = _arg_162_["reset"]
    root.reset = function()
      root.chunk, root.scope, root.options, root.reset = chunk, scope, options, reset
      return nil
    end
    return root.reset
  end
  local warned = {}
  local function check_plugin_version(_163_)
    local _arg_164_ = _163_
    local name = _arg_164_["name"]
    local versions = _arg_164_["versions"]
    local plugin = _arg_164_
    if (not member_3f(version:gsub("-dev", ""), (versions or {})) and not warned[plugin]) then
      warned[plugin] = true
      return warn(string.format("plugin %s does not support Fennel version %s", (name or "unknown"), version))
    else
      return nil
    end
  end
  local function hook_opts(event, _3foptions, ...)
    local plugins
    local function _167_(...)
      local t_166_ = _3foptions
      if (nil ~= t_166_) then
        t_166_ = (t_166_).plugins
      else
      end
      return t_166_
    end
    local function _170_(...)
      local t_169_ = root.options
      if (nil ~= t_169_) then
        t_169_ = (t_169_).plugins
      else
      end
      return t_169_
    end
    plugins = (_167_(...) or _170_(...))
    if plugins then
      local result = nil
      for _, plugin in ipairs(plugins) do
        if result then break end
        check_plugin_version(plugin)
        local _172_ = plugin[event]
        if (nil ~= _172_) then
          local f = _172_
          result = f(...)
        else
          result = nil
        end
      end
      return result
    else
      return nil
    end
  end
  local function hook(event, ...)
    return hook_opts(event, root.options, ...)
  end
  return {warn = warn, allpairs = allpairs, stablepairs = stablepairs, copy = copy, ["get-in"] = get_in, kvmap = kvmap, map = map, ["walk-tree"] = walk_tree, ["member?"] = member_3f, list = list, sequence = sequence, sym = sym, varg = varg, expr = expr, comment = comment_2a, ["comment?"] = comment_3f, ["expr?"] = expr_3f, ["list?"] = list_3f, ["multi-sym?"] = multi_sym_3f, ["sequence?"] = sequence_3f, ["sym?"] = sym_3f, ["table?"] = table_3f, ["varg?"] = varg_3f, ["quoted?"] = quoted_3f, ["string?"] = string_3f, ["valid-lua-identifier?"] = valid_lua_identifier_3f, ["lua-keywords"] = lua_keywords, hook = hook, ["hook-opts"] = hook_opts, ["propagate-options"] = propagate_options, root = root, ["debug-on?"] = debug_on_3f, ["ast-source"] = ast_source, version = version, ["runtime-version"] = runtime_version, len = len, path = table.concat({"./?.fnl", "./?/init.fnl", getenv("FENNEL_PATH")}, ";"), ["macro-path"] = table.concat({"./?.fnl", "./?/init-macros.fnl", "./?/init.fnl", getenv("FENNEL_MACRO_PATH")}, ";")}
end
utils = require("fennel.utils")
local parser = require("fennel.parser")
local compiler = require("fennel.compiler")
local specials = require("fennel.specials")
local repl = require("fennel.repl")
local view = require("fennel.view")
local function eval_env(env, opts)
  if (env == "_COMPILER") then
    local env0 = specials["make-compiler-env"](nil, compiler.scopes.compiler, {}, opts)
    if (opts.allowedGlobals == nil) then
      opts.allowedGlobals = specials["current-global-names"](env0)
    else
    end
    return specials["wrap-env"](env0)
  else
    return (env and specials["wrap-env"](env))
  end
end
local function eval_opts(options, str)
  local opts = utils.copy(options)
  if (opts.allowedGlobals == nil) then
    opts.allowedGlobals = specials["current-global-names"](opts.env)
  else
  end
  if (not opts.filename and not opts.source) then
    opts.source = str
  else
  end
  if (opts.env == "_COMPILER") then
    opts.scope = compiler["make-scope"](compiler.scopes.compiler)
  else
  end
  return opts
end
local function eval(str, options, ...)
  local opts = eval_opts(options, str)
  local env = eval_env(opts.env, opts)
  local lua_source = compiler["compile-string"](str, opts)
  local loader
  local function _735_(...)
    if opts.filename then
      return ("@" .. opts.filename)
    else
      return str
    end
  end
  loader = specials["load-code"](lua_source, env, _735_(...))
  opts.filename = nil
  return loader(...)
end
local function dofile_2a(filename, options, ...)
  local opts = utils.copy(options)
  local f = assert(io.open(filename, "rb"))
  local source = assert(f:read("*all"), ("Could not read " .. filename))
  f:close()
  opts.filename = filename
  return eval(source, opts, ...)
end
local function syntax()
  local body_3f = {"when", "with-open", "collect", "icollect", "fcollect", "lambda", "\206\187", "macro", "match", "match-try", "accumulate", "doto"}
  local binding_3f = {"collect", "icollect", "fcollect", "each", "for", "let", "with-open", "accumulate"}
  local define_3f = {"fn", "lambda", "\206\187", "var", "local", "macro", "macros", "global"}
  local out = {}
  for k, v in pairs(compiler.scopes.global.specials) do
    local metadata = (compiler.metadata[v] or {})
    do end (out)[k] = {["special?"] = true, ["body-form?"] = metadata["fnl/body-form?"], ["binding-form?"] = utils["member?"](k, binding_3f), ["define?"] = utils["member?"](k, define_3f)}
  end
  for k, v in pairs(compiler.scopes.global.macros) do
    out[k] = {["macro?"] = true, ["body-form?"] = utils["member?"](k, body_3f), ["binding-form?"] = utils["member?"](k, binding_3f), ["define?"] = utils["member?"](k, define_3f)}
  end
  for k, v in pairs(_G) do
    local _736_ = type(v)
    if (_736_ == "function") then
      out[k] = {["global?"] = true, ["function?"] = true}
    elseif (_736_ == "table") then
      for k2, v2 in pairs(v) do
        if (("function" == type(v2)) and (k ~= "_G")) then
          out[(k .. "." .. k2)] = {["function?"] = true, ["global?"] = true}
        else
        end
      end
      out[k] = {["global?"] = true}
    else
    end
  end
  return out
end
local mod = {list = utils.list, ["list?"] = utils["list?"], sym = utils.sym, ["sym?"] = utils["sym?"], ["multi-sym?"] = utils["multi-sym?"], sequence = utils.sequence, ["sequence?"] = utils["sequence?"], ["table?"] = utils["table?"], comment = utils.comment, ["comment?"] = utils["comment?"], varg = utils.varg, ["varg?"] = utils["varg?"], ["sym-char?"] = parser["sym-char?"], parser = parser.parser, compile = compiler.compile, ["compile-string"] = compiler["compile-string"], ["compile-stream"] = compiler["compile-stream"], eval = eval, repl = repl, view = view, dofile = dofile_2a, ["load-code"] = specials["load-code"], doc = specials.doc, metadata = compiler.metadata, traceback = compiler.traceback, version = utils.version, ["runtime-version"] = utils["runtime-version"], ["ast-source"] = utils["ast-source"], path = utils.path, ["macro-path"] = utils["macro-path"], ["macro-loaded"] = specials["macro-loaded"], ["macro-searchers"] = specials["macro-searchers"], ["search-module"] = specials["search-module"], ["make-searcher"] = specials["make-searcher"], searcher = specials["make-searcher"](), syntax = syntax, gensym = compiler.gensym, scope = compiler["make-scope"], mangle = compiler["global-mangling"], unmangle = compiler["global-unmangling"], compile1 = compiler.compile1, ["string-stream"] = parser["string-stream"], granulate = parser.granulate, loadCode = specials["load-code"], make_searcher = specials["make-searcher"], makeSearcher = specials["make-searcher"], searchModule = specials["search-module"], macroPath = utils["macro-path"], macroSearchers = specials["macro-searchers"], macroLoaded = specials["macro-loaded"], compileStream = compiler["compile-stream"], compileString = compiler["compile-string"], stringStream = parser["string-stream"], runtimeVersion = utils["runtime-version"]}
mod.install = function(_3fopts)
  table.insert((package.searchers or package.loaders), specials["make-searcher"](_3fopts))
  return mod
end
utils["fennel-module"] = mod
do
  local builtin_macros = [===[;; These macros are awkward because their definition cannot rely on the any
  ;; built-in macros, only special forms. (no when, no icollect, etc)
  
  (fn copy [t]
    (let [out []]
      (each [_ v (ipairs t)] (table.insert out v))
      (setmetatable out (getmetatable t))))
  
  (fn ->* [val ...]
    "Thread-first macro.
  Take the first value and splice it into the second form as its first argument.
  The value of the second form is spliced into the first arg of the third, etc."
    (var x val)
    (each [_ e (ipairs [...])]
      (let [elt (if (list? e) (copy e) (list e))]
        (table.insert elt 2 x)
        (set x elt)))
    x)
  
  (fn ->>* [val ...]
    "Thread-last macro.
  Same as ->, except splices the value into the last position of each form
  rather than the first."
    (var x val)
    (each [_ e (ipairs [...])]
      (let [elt (if (list? e) (copy e) (list e))]
        (table.insert elt x)
        (set x elt)))
    x)
  
  (fn -?>* [val ?e ...]
    "Nil-safe thread-first macro.
  Same as -> except will short-circuit with nil when it encounters a nil value."
    (if (= nil ?e)
        val
        (let [el (if (list? ?e) (copy ?e) (list ?e))
              tmp (gensym)]
          (table.insert el 2 tmp)
          `(let [,tmp ,val]
             (if (not= nil ,tmp)
                 (-?> ,el ,...)
                 ,tmp)))))
  
  (fn -?>>* [val ?e ...]
    "Nil-safe thread-last macro.
  Same as ->> except will short-circuit with nil when it encounters a nil value."
    (if (= nil ?e)
        val
        (let [el (if (list? ?e) (copy ?e) (list ?e))
              tmp (gensym)]
          (table.insert el tmp)
          `(let [,tmp ,val]
             (if (not= ,tmp nil)
                 (-?>> ,el ,...)
                 ,tmp)))))
  
  (fn ?dot [tbl ...]
    "Nil-safe table look up.
  Same as . (dot), except will short-circuit with nil when it encounters
  a nil value in any of subsequent keys."
    (let [head (gensym :t)
          lookups `(do
                     (var ,head ,tbl)
                     ,head)]
      (each [_ k (ipairs [...])]
        ;; Kinda gnarly to reassign in place like this, but it emits the best lua.
        ;; With this impl, it emits a flat, concise, and readable set of ifs
        (table.insert lookups (# lookups) `(if (not= nil ,head)
                                             (set ,head (. ,head ,k)))))
      lookups))
  
  (fn doto* [val ...]
    "Evaluate val and splice it into the first argument of subsequent forms."
    (assert (not= val nil) "missing subject")
    (let [rebind? (or (not (sym? val))
                      (multi-sym? val))
          name (if rebind? (gensym)            val)
          form (if rebind? `(let [,name ,val]) `(do))]
      (each [_ elt (ipairs [...])]
        (let [elt (if (list? elt) (copy elt) (list elt))]
          (table.insert elt 2 name)
          (table.insert form elt)))
      (table.insert form name)
      form))
  
  (fn when* [condition body1 ...]
    "Evaluate body for side-effects only when condition is truthy."
    (assert body1 "expected body")
    `(if ,condition
         (do
           ,body1
           ,...)))
  
  (fn with-open* [closable-bindings ...]
    "Like `let`, but invokes (v:close) on each binding after evaluating the body.
  The body is evaluated inside `xpcall` so that bound values will be closed upon
  encountering an error before propagating it."
    (let [bodyfn `(fn []
                    ,...)
          closer `(fn close-handlers# [ok# ...]
                    (if ok# ... (error ... 0)))
          traceback `(. (or package.loaded.fennel debug) :traceback)]
      (for [i 1 (length closable-bindings) 2]
        (assert (sym? (. closable-bindings i))
                "with-open only allows symbols in bindings")
        (table.insert closer 4 `(: ,(. closable-bindings i) :close)))
      `(let ,closable-bindings
         ,closer
         (close-handlers# (_G.xpcall ,bodyfn ,traceback)))))
  
  (fn extract-into [iter-tbl]
    (var (into iter-out found?) (values [] (copy iter-tbl)))
    (for [i (length iter-tbl) 2 -1]
      (let [item (. iter-tbl i)]
        (if (or (= `&into item)
                (= :into  item))
            (do
              (assert (not found?) "expected only one &into clause")
              (set found? true)
              (set into (. iter-tbl (+ i 1)))
              (table.remove iter-out i)
              (table.remove iter-out i)))))
    (assert (or (not found?) (sym? into) (table? into) (list? into))
            "expected table, function call, or symbol in &into clause")
    (values into iter-out))
  
  (fn collect* [iter-tbl key-expr value-expr ...]
    "Return a table made by running an iterator and evaluating an expression that
  returns key-value pairs to be inserted sequentially into the table.  This can
  be thought of as a table comprehension. The body should provide two expressions
  (used as key and value) or nil, which causes it to be omitted.
  
  For example,
    (collect [k v (pairs {:apple \"red\" :orange \"orange\"})]
      (values v k))
  returns
    {:red \"apple\" :orange \"orange\"}
  
  Supports an &into clause after the iterator to put results in an existing table.
  Supports early termination with an &until clause."
    (assert (and (sequence? iter-tbl) (<= 2 (length iter-tbl)))
            "expected iterator binding table")
    (assert (not= nil key-expr) "expected key and value expression")
    (assert (= nil ...)
            "expected 1 or 2 body expressions; wrap multiple expressions with do")
    (let [kv-expr (if (= nil value-expr) key-expr `(values ,key-expr ,value-expr))
          (into iter) (extract-into iter-tbl)]
      `(let [tbl# ,into]
         (each ,iter
           (match ,kv-expr
             (k# v#) (tset tbl# k# v#)))
         tbl#)))
  
  (fn seq-collect [how iter-tbl value-expr ...]
    "Common part between icollect and fcollect for producing sequential tables.
  
  Iteration code only deffers in using the for or each keyword, the rest
  of the generated code is identical."
    (assert (not= nil value-expr) "expected table value expression")
    (assert (= nil ...)
            "expected exactly one body expression. Wrap multiple expressions in do")
    (let [(into iter) (extract-into iter-tbl)]
      `(let [tbl# ,into]
         ;; believe it or not, using a var here has a pretty good performance
         ;; boost: https://p.hagelb.org/icollect-performance.html
         (var i# (length tbl#))
         (,how ,iter
               (let [val# ,value-expr]
                 (when (not= nil val#)
                   (set i# (+ i# 1))
                   (tset tbl# i# val#))))
         tbl#)))
  
  (fn icollect* [iter-tbl value-expr ...]
    "Return a sequential table made by running an iterator and evaluating an
  expression that returns values to be inserted sequentially into the table.
  This can be thought of as a table comprehension. If the body evaluates to nil
  that element is omitted.
  
  For example,
    (icollect [_ v (ipairs [1 2 3 4 5])]
      (when (not= v 3)
        (* v v)))
  returns
    [1 4 16 25]
  
  Supports an &into clause after the iterator to put results in an existing table.
  Supports early termination with an &until clause."
    (assert (and (sequence? iter-tbl) (<= 2 (length iter-tbl)))
            "expected iterator binding table")
    (seq-collect 'each iter-tbl value-expr ...))
  
  (fn fcollect* [iter-tbl value-expr ...]
    "Return a sequential table made by advancing a range as specified by
  for, and evaluating an expression that returns values to be inserted
  sequentially into the table.  This can be thought of as a range
  comprehension. If the body evaluates to nil that element is omitted.
  
  For example,
    (fcollect [i 1 10 2]
      (when (not= i 3)
        (* i i)))
  returns
    [1 25 49 81]
  
  Supports an &into clause after the range to put results in an existing table.
  Supports early termination with an &until clause."
    (assert (and (sequence? iter-tbl) (< 2 (length iter-tbl)))
            "expected range binding table")
    (seq-collect 'for iter-tbl value-expr ...))
  
  (fn accumulate* [iter-tbl body ...]
    "Accumulation macro.
  
  It takes a binding table and an expression as its arguments.  In the binding
  table, the first form starts out bound to the second value, which is an initial
  accumulator. The rest are an iterator binding table in the format `each` takes.
  
  It runs through the iterator in each step of which the given expression is
  evaluated, and the accumulator is set to the value of the expression. It
  eventually returns the final value of the accumulator.
  
  For example,
    (accumulate [total 0
                 _ n (pairs {:apple 2 :orange 3})]
      (+ total n))
  returns 5"
    (assert (and (sequence? iter-tbl) (<= 4 (length iter-tbl)))
            "expected initial value and iterator binding table")
    (assert (not= nil body) "expected body expression")
    (assert (= nil ...)
            "expected exactly one body expression. Wrap multiple expressions with do")
    (let [accum-var (. iter-tbl 1)
          accum-init (. iter-tbl 2)]
      `(do
         (var ,accum-var ,accum-init)
         (each ,[(unpack iter-tbl 3)]
           (set ,accum-var ,body))
         ,(if (list? accum-var)
              (list (sym :values) (unpack accum-var))
              accum-var))))
  
  (fn double-eval-safe? [x type]
    (or (= :number type) (= :string type) (= :boolean type)
        (and (sym? x) (not (multi-sym? x)))))
  
  (fn partial* [f ...]
    "Return a function with all arguments partially applied to f."
    (assert f "expected a function to partially apply")
    (let [bindings []
          args []]
      (each [_ arg (ipairs [...])]
        (if (double-eval-safe? arg (type arg))
          (table.insert args arg)
          (let [name (gensym)]
            (table.insert bindings name)
            (table.insert bindings arg)
            (table.insert args name))))
      (let [body (list f (unpack args))]
        (table.insert body _VARARG)
        ;; only use the extra let if we need double-eval protection
        (if (= 0 (length bindings))
            `(fn [,_VARARG] ,body)
            `(let ,bindings
               (fn [,_VARARG] ,body))))))
  
  (fn pick-args* [n f]
    "Create a function of arity n that applies its arguments to f.
  
  For example,
    (pick-args 2 func)
  expands to
    (fn [_0_ _1_] (func _0_ _1_))"
    (if (and _G.io _G.io.stderr)
        (_G.io.stderr:write
         "-- WARNING: pick-args is deprecated and will be removed in the future.\n"))
    (assert (and (= (type n) :number) (= n (math.floor n)) (<= 0 n))
            (.. "Expected n to be an integer literal >= 0, got " (tostring n)))
    (let [bindings []]
      (for [i 1 n]
        (tset bindings i (gensym)))
      `(fn ,bindings
         (,f ,(unpack bindings)))))
  
  (fn pick-values* [n ...]
    "Evaluate to exactly n values.
  
  For example,
    (pick-values 2 ...)
  expands to
    (let [(_0_ _1_) ...]
      (values _0_ _1_))"
    (assert (and (= :number (type n)) (<= 0 n) (= n (math.floor n)))
            (.. "Expected n to be an integer >= 0, got " (tostring n)))
    (let [let-syms (list)
          let-values (if (= 1 (select "#" ...)) ... `(values ,...))]
      (for [i 1 n]
        (table.insert let-syms (gensym)))
      (if (= n 0) `(values)
          `(let [,let-syms ,let-values]
             (values ,(unpack let-syms))))))
  
  (fn lambda* [...]
    "Function literal with nil-checked arguments.
  Like `fn`, but will throw an exception if a declared argument is passed in as
  nil, unless that argument's name begins with a question mark."
    (let [args [...]
          has-internal-name? (sym? (. args 1))
          arglist (if has-internal-name? (. args 2) (. args 1))
          docstring-position (if has-internal-name? 3 2)
          has-docstring? (and (< docstring-position (length args))
                              (= :string (type (. args docstring-position))))
          arity-check-position (- 4 (if has-internal-name? 0 1)
                                  (if has-docstring? 0 1))
          empty-body? (< (length args) arity-check-position)]
      (fn check! [a]
        (if (table? a)
            (each [_ a (pairs a)]
              (check! a))
            (let [as (tostring a)]
              (and (not (as:match "^?")) (not= as "&") (not= as "_")
                   (not= as "...") (not= as "&as")))
            (table.insert args arity-check-position
                          `(_G.assert (not= nil ,a)
                                      ,(: "Missing argument %s on %s:%s" :format
                                          (tostring a)
                                          (or a.filename :unknown)
                                          (or a.line "?"))))))
  
      (assert (= :table (type arglist)) "expected arg list")
      (each [_ a (ipairs arglist)]
        (check! a))
      (if empty-body?
          (table.insert args (sym :nil)))
      `(fn ,(unpack args))))
  
  (fn macro* [name ...]
    "Define a single macro."
    (assert (sym? name) "expected symbol for macro name")
    (local args [...])
    `(macros {,(tostring name) (fn ,(unpack args))}))
  
  (fn macrodebug* [form return?]
    "Print the resulting form after performing macroexpansion.
  With a second argument, returns expanded form as a string instead of printing."
    (let [handle (if return? `do `print)]
      `(,handle ,(view (macroexpand form _SCOPE)))))
  
  (fn import-macros* [binding1 module-name1 ...]
    "Bind a table of macros from each macro module according to a binding form.
  Each binding form can be either a symbol or a k/v destructuring table.
  Example:
    (import-macros mymacros                 :my-macros    ; bind to symbol
                   {:macro1 alias : macro2} :proj.macros) ; import by name"
    (assert (and binding1 module-name1 (= 0 (% (select "#" ...) 2)))
            "expected even number of binding/modulename pairs")
    (for [i 1 (select "#" binding1 module-name1 ...) 2]
      ;; delegate the actual loading of the macros to the require-macros
      ;; special which already knows how to set up the compiler env and stuff.
      ;; this is weird because require-macros is deprecated but it works.
      (let [(binding modname) (select i binding1 module-name1 ...)
            scope (get-scope)
            ;; if the module-name is an expression (and not just a string) we
            ;; patch our expression to have the correct source filename so
            ;; require-macros can pass it down when resolving the module-name.
            expr `(import-macros ,modname)
            filename (if (list? modname) (. modname 1 :filename) :unknown)
            _ (tset expr :filename filename)
            macros* (_SPECIALS.require-macros expr scope {} binding)]
        (if (sym? binding)
            ;; bind whole table of macros to table bound to symbol
            (tset scope.macros (. binding 1) macros*)
            ;; 1-level table destructuring for importing individual macros
            (table? binding)
            (each [macro-name [import-key] (pairs binding)]
              (assert (= :function (type (. macros* macro-name)))
                      (.. "macro " macro-name " not found in module "
                          (tostring modname)))
              (tset scope.macros import-key (. macros* macro-name))))))
    nil)
  
  ;;; Pattern matching
  
  (fn match-values [vals pattern unifications match-pattern]
    (let [condition `(and)
          bindings []]
      (each [i pat (ipairs pattern)]
        (let [(subcondition subbindings) (match-pattern [(. vals i)] pat
                                                        unifications)]
          (table.insert condition subcondition)
          (each [_ b (ipairs subbindings)]
            (table.insert bindings b))))
      (values condition bindings)))
  
  (fn match-table [val pattern unifications match-pattern]
    (let [condition `(and (= (_G.type ,val) :table))
          bindings []]
      (each [k pat (pairs pattern)]
        (if (= pat `&)
            (let [rest-pat (. pattern (+ k 1))
                  rest-val `(select ,k ((or table.unpack _G.unpack) ,val))
                  subcondition (match-table `(pick-values 1 ,rest-val)
                                            rest-pat unifications match-pattern)]
              (if (not (sym? rest-pat))
                  (table.insert condition subcondition))
              (assert (= nil (. pattern (+ k 2)))
                      "expected & rest argument before last parameter")
              (table.insert bindings rest-pat)
              (table.insert bindings [rest-val]))
            (= k `&as)
            (do
              (table.insert bindings pat)
              (table.insert bindings val))
            (and (= :number (type k)) (= `&as pat))
            (do
              (assert (= nil (. pattern (+ k 2)))
                      "expected &as argument before last parameter")
              (table.insert bindings (. pattern (+ k 1)))
              (table.insert bindings val))
            ;; don't process the pattern right after &/&as; already got it
            (or (not= :number (type k)) (and (not= `&as (. pattern (- k 1)))
                                             (not= `& (. pattern (- k 1)))))
            (let [subval `(. ,val ,k)
                  (subcondition subbindings) (match-pattern [subval] pat
                                                            unifications)]
              (table.insert condition subcondition)
              (each [_ b (ipairs subbindings)]
                (table.insert bindings b)))))
      (values condition bindings)))
  
  (fn match-pattern [vals pattern unifications]
    "Take the AST of values and a single pattern and returns a condition
  to determine if it matches as well as a list of bindings to
  introduce for the duration of the body if it does match."
    ;; we have to assume we're matching against multiple values here until we
    ;; know we're either in a multi-valued clause (in which case we know the #
    ;; of vals) or we're not, in which case we only care about the first one.
    (let [[val] vals]
      (if (or (and (sym? pattern) ; unification with outer locals (or nil)
                   (not= "_" (tostring pattern)) ; never unify _
                   (or (in-scope? pattern) (= :nil (tostring pattern))))
              (and (multi-sym? pattern) (in-scope? (. (multi-sym? pattern) 1))))
          (values `(= ,val ,pattern) [])
          ;; unify a local we've seen already
          (and (sym? pattern) (. unifications (tostring pattern)))
          (values `(= ,(. unifications (tostring pattern)) ,val) [])
          ;; bind a fresh local
          (sym? pattern)
          (let [wildcard? (: (tostring pattern) :find "^_")]
            (if (not wildcard?) (tset unifications (tostring pattern) val))
            (values (if (or wildcard? (string.find (tostring pattern) "^?")) true
                        `(not= ,(sym :nil) ,val)) [pattern val]))
          ;; guard clause
          (and (list? pattern) (= (. pattern 2) `?))
          (let [(pcondition bindings) (match-pattern vals (. pattern 1)
                                                     unifications)
                condition `(and ,(unpack pattern 3))]
            (values `(and ,pcondition
                          (let ,bindings
                            ,condition)) bindings))
          ;; multi-valued patterns (represented as lists)
          (list? pattern)
          (match-values vals pattern unifications match-pattern)
          ;; table patterns
          (= (type pattern) :table)
          (match-table val pattern unifications match-pattern)
          ;; literal value
          (values `(= ,val ,pattern) []))))
  
  (fn match-condition [vals clauses]
    "Construct the actual `if` AST for the given match values and clauses."
    (if (not= 0 (% (length clauses) 2)) ; treat odd final clause as default
        (table.insert clauses (length clauses) (sym "_")))
    (let [out `(if)]
      (for [i 1 (length clauses) 2]
        (let [pattern (. clauses i)
              body (. clauses (+ i 1))
              (condition bindings) (match-pattern vals pattern {})]
          (table.insert out condition)
          (table.insert out `(let ,bindings
                               ,body))))
      out))
  
  (fn match-val-syms [clauses]
    "How many multi-valued clauses are there? return a list of that many gensyms."
    (let [syms (list (gensym))]
      (for [i 1 (length clauses) 2]
        (let [clause (if (and (list? (. clauses i)) (= `? (. clauses i 2)))
                         (. clauses i 1)
                         (. clauses i))]
          (if (list? clause)
              (each [valnum (ipairs clause)]
                (if (not (. syms valnum))
                    (tset syms valnum (gensym)))))))
      syms))
  
  (fn match* [val ...]
    ;; Old implementation of match macro, which doesn't directly support
    ;; `where' and `or'. New syntax is implemented in `match-where',
    ;; which simply generates old syntax and feeds it to `match*'.
    (let [clauses [...]
          vals (match-val-syms clauses)]
      ;; protect against multiple evaluation of the value, bind against as
      ;; many values as we ever match against in the clauses.
      (list `let [vals val] (match-condition vals clauses))))
  
  ;; Construction of old match syntax from new syntax
  
  (fn partition-2 [seq]
    ;; Partition `seq` by 2.
    ;; If `seq` has odd amount of elements, the last one is dropped.
    ;;
    ;; Input: [1 2 3 4 5]
    ;; Output: [[1 2] [3 4]]
    (let [firsts []
          seconds []
          res []]
      (for [i 1 (length seq) 2]
        (let [first (. seq i)
              second (. seq (+ i 1))]
          (table.insert firsts (if (not= nil first) first `nil))
          (table.insert seconds (if (not= nil second) second `nil))))
      (each [i v1 (ipairs firsts)]
        (let [v2 (. seconds i)]
          (if (not= nil v2)
              (table.insert res [v1 v2]))))
      res))
  
  (fn transform-or [[_ & pats] guards]
    ;; Transforms `(or pat pats*)` lists into match `guard` patterns.
    ;;
    ;; (or pat1 pat2), guard => [(pat1 ? guard) (pat2 ? guard)]
    (let [res []]
      (each [_ pat (ipairs pats)]
        (table.insert res (list pat `? (unpack guards))))
      res))
  
  (fn transform-cond [cond]
    ;; Transforms `where` cond into sequence of `match` guards.
    ;;
    ;; pat => [pat]
    ;; (where pat guard) => [(pat ? guard)]
    ;; (where (or pat1 pat2) guard) => [(pat1 ? guard) (pat2 ? guard)]
    (if (and (list? cond) (= (. cond 1) `where))
        (let [second (. cond 2)]
          (if (and (list? second) (= (. second 1) `or))
              (transform-or second [(unpack cond 3)])
              :else
              [(list second `? (unpack cond 3))]))
        :else
        [cond]))
  
  (fn match-where [val ...]
    "Perform pattern matching on val. See reference for details.
  
  Syntax:
  
  (match data-expression
    pattern body
    (where pattern guard guards*) body
    (where (or pattern patterns*) guard guards*) body)"
    (assert (not= val nil) "missing subject")
    (assert (= 0 (math.fmod (select :# ...) 2))
            "expected even number of pattern/body pairs")
    (assert (not= 0 (select :# ...))
            "expected at least one pattern/body pair")
    (let [conds-bodies (partition-2 [...])
          match-body []]
      (each [_ [cond body] (ipairs conds-bodies)]
        (each [_ cond (ipairs (transform-cond cond))]
          (table.insert match-body cond)
          (table.insert match-body body)))
      (match* val (unpack match-body))))
  
  (fn match-try-step [expr else pattern body ...]
    (if (= nil pattern body)
        expr
        ;; unlike regular match, we can't know how many values the value
        ;; might evaluate to, so we have to capture them all in ... via IIFE
        ;; to avoid double-evaluation.
        `((fn [...]
            (match ...
              ,pattern ,(match-try-step body else ...)
              ,(unpack else)))
          ,expr)))
  
  (fn match-try* [expr pattern body ...]
    "Perform chained pattern matching for a sequence of steps which might fail.
  
  The values from the initial expression are matched against the first pattern.
  If they match, the first body is evaluated and its values are matched against
  the second pattern, etc.
  
  If there is a (catch pat1 body1 pat2 body2 ...) form at the end, any mismatch
  from the steps will be tried against these patterns in sequence as a fallback
  just like a normal match. If there is no catch, the mismatched values will be
  returned as the value of the entire expression."
    (let [clauses [pattern body ...]
          last (. clauses (length clauses))
          catch (if (= `catch (and (= :table (type last)) (. last 1)))
                   (let [[_ & e] (table.remove clauses)] e) ; remove `catch sym
                   [`_# `...])]
      (assert (= 0 (math.fmod (length clauses) 2))
              "expected every pattern to have a body")
      (assert (= 0 (math.fmod (length catch) 2))
              "expected every catch pattern to have a body")
      (match-try-step expr catch (unpack clauses))))
  
  {:-> ->*
   :->> ->>*
   :-?> -?>*
   :-?>> -?>>*
   :?. ?dot
   :doto doto*
   :when when*
   :with-open with-open*
   :collect collect*
   :icollect icollect*
   :fcollect fcollect*
   :accumulate accumulate*
   :partial partial*
   :lambda lambda*
   :pick-args pick-args*
   :pick-values pick-values*
   :macro macro*
   :macrodebug macrodebug*
   :import-macros import-macros*
   :match match-where
   :match-try match-try*}
  ]===]
  local module_name = "fennel.macros"
  local _
  local function _739_()
    return mod
  end
  package.preload[module_name] = _739_
  _ = nil
  local env
  do
    local _740_ = specials["make-compiler-env"](nil, compiler.scopes.compiler, {})
    do end (_740_)["utils"] = utils
    _740_["fennel"] = mod
    env = _740_
  end
  local built_ins = eval(builtin_macros, {env = env, scope = compiler.scopes.compiler, allowedGlobals = false, useMetadata = true, filename = "src/fennel/macros.fnl", moduleName = module_name})
  for k, v in pairs(built_ins) do
    compiler.scopes.global.macros[k] = v
  end
  compiler.scopes.global.macros["\206\187"] = compiler.scopes.global.macros.lambda
  package.preload[module_name] = nil
end
return mod
