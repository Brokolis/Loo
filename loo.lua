local _type, _tonumber = type, tonumber

local reservedClassKeywords = {
  ["__prop__"] = true;
  ["base"] = true;
  ["this"] = true;
}

local primitiveTypes = {
  ["nil"] = true;
  ["number"] = true;
  ["string"] = true;
  ["table"] = true;
  ["function"] = true;
  ["thread"] = true;
}

local null = setmetatable({}, {__type = "null", __tostring = function () return "null" end})

local function type (val, ...)
  if _type(val) == "table" and _type(getmetatable(val)) == "table" and getmetatable(val).__type ~= nil then
    return tostring(getmetatable(val).__type)
  else
    return _type(val, ...)
  end
end

local function tonumber (num, base, ...)
  if _type(num) == "table" and _type(getmetatable(num)) == "table" and _type(getmetatable(num).__tonumber) == "function" then
    return getmetatable(num).__tonumber(num, base, ...)
  else
    return _tonumber(num, base, ...)
  end
end

local function expect (value, expectType, default, name, level)
  level = level == nil and 2 or level == 0 and 0 or level + 1

  local foundType = type(value)

  if default ~= nil and value == nil then
    return default
  elseif foundType ~= expectType then
    if name then
      error(expectType .. " expected at field '" .. name .. "', got " .. foundType, level)
    else
      error(expectType .. " expected, got " .. foundType, level)
    end
  end

  return value
end

local function isidentifier (str)
  expect(str, "string")

  return str:match("^[_a-zA-Z][_a-zA-Z0-9]*$")
end

local function isprimitive (val)
  return primitiveTypes[type(val)] == true
end

local function isin (val, tab)
  for k, v in pairs(tab) do
    if v == val then
      return true
    end
  end

  return false
end

local function split (str, char)
  expect(str, "string", nil, "string")
  expect(char, "string", nil, "delimiter")

  local result = {}

  for match in str:gmatch("[^" .. char .. "]+") do
    result[#result + 1] = match
  end

  return result
end

local function cast (obj, typ, level)
  level = level == nil and 2 or level == 0 and 0 or level + 1

  if isprimitive(obj) then
    error("Cannot cast primitive type " .. type(obj), level)
  elseif obj.__prop__ and type(obj.__prop__.selfChain) == "table" then
    for i, self in ipairs(obj.__prop__.selfChain) do
      if self.__prop__.classFullName == typ then
        return self
      end
    end
  end

  error("Cannot cast type " .. type(obj) .. " to type " .. typ, level)
end

local function is (obj, typ)
  if type(obj) == typ then
    return true
  elseif not isprimitive(obj) and type(obj.__prop__) == "table" and type(obj.__prop__.selfChain) == "table" then
    for i, self in ipairs(obj.__prop__.selfChain) do
      if self.__prop__.classFullName == typ then
        return true
      end
    end
  end

  return false
end

local packages = {}

local function isPackageNameValid (name)
  return type(name) == "string" and name:gsub("[^_a-zA-Z0-9%.]+", ""):gsub("%.+", "."):gsub("^%.", ""):gsub("%.$", ""):gsub("%.(%d)", "") == name
end

local function resolvePackage (name)
  local identifiers = split(name, ".")
  local pkg = packages

  for i, identifier in ipairs(identifiers) do
    if not pkg[identifier] then
      pkg[identifier] = {}
    elseif type(pkg[identifier]) ~= "table" then
      return nil, pkg[identifier]
    end

    pkg = pkg[identifier]
  end

  return pkg
end

local _interfaces = {}
local interface = {}

function interface.resolvePackage (env)
  return isPackageNameValid(env.__package) and env.__package or ""
end

function interface.getFullName (env, interfaceName)
  if isPackageNameValid(env.__package) and _interfaces[env.__package .. "." .. interfaceName] then
    return env.__package .. "." .. interfaceName
  end

  if type(env.__imports) == "table" then
    for package in pairs(env.__imports) do
      if isPackageNameValid(package) and _interfaces[package .. "." .. interfaceName] then
        return package .. "." .. interfaceName
      end
    end
  end

  if _interfaces[interfaceName] then
    return interfaceName
  end
end

setmetatable(interface, {
  __call = function (_, env, name, prop, level)
    level = level == nil and 2 or level == 0 and 0 or level

    expect(name, "string", nil, "interface name", level)
    expect(prop, "table", true, "interface fields", level)

    local package = interface.resolvePackage(env)

    local this = {
      name = name;
      package = package;
      fullName = (#package > 0 and package .. "." or "") .. name;

      globals = {};
    }

    local proxy
    proxy = setmetatable({}, {
      __call = function (_, prop, level)
        level = level or 2

        expect(prop, "table", nil, "interface fields", level)

        for field, fieldType in pairs(prop) do
          expect(field, "string", nil, tostring(field), level)
          expect(fieldType, "string", nil, tostring(fieldType), level)

          if not isidentifier(field) then
            error("Field '" .. field .. "' is not a valid identifier.", level)
          end

          this.globals[field] = fieldType
        end

        _interfaces[this.fullName] = this
      end;
    })

    if prop then
      return proxy(prop, 1 + level)
    end

    return proxy
  end;
})

local _classes = {}
local classes = {}
local class = {}

function class.resolvePackage (env)
  return isPackageNameValid(env.__package) and env.__package or ""
end

function class.getFullName (env, className)
  if isPackageNameValid(env.__package) and classes[env.__package .. "." .. className] then
    return env.__package .. "." .. className
  end

  if type(env.__imports) == "table" then
    for package in pairs(env.__imports) do
      if isPackageNameValid(package) and classes[package .. "." .. className] then
        return package .. "." .. className
      end
    end
  end

  if classes[className] then
    return className
  end
end

setmetatable(class, {
  __call = function (_, env, name, prop, level)
    level = level == nil and 2 or level == 0 and 0 or level

    expect(name, "string", nil, "class name", level)
    expect(prop, "table", true, "class members", level)

    local package = class.resolvePackage(env)

    local this = {
      name = name;
      package = package;
      fullName = (#package > 0 and package .. "." or "") .. name;

      constructor = function () end;

      locals = {};
      globals = {};
      static = {};

      meta = {};

      interfaces = nil;
      inherits = nil;
    }

    local function checkIdentifier (identifier, level)
      local id = isidentifier(identifier)

      if not id then
        error("Identifier '" .. identifier .. "' is not a valid identifier.", 1 + level)
      end

      return id
    end

    local function getInherit (class)
      return {
        name = class.name;
        package = class.package;
        fullName = class.fullName;

        constructor = class.constructor;

        locals = class.locals;
        globals = class.globals;
        static = class.static;

        meta = class.meta;

        inherits = class.inherits and getInherit(class.inherits) or nil;
        interfaces = (function ()
          if not class.interfaces then
            return nil
          end

          local interfaces = {}

          for i, interface in ipairs(class.interfaces) do
            interfaces[#interfaces + 1] = {
              name = interface.name;
              package = interface.package;
              fullName = interface.fullName;

              requiredGlobals = interface.requiredGlobals;
              globals = {};
            }

            for name, global in pairs(interface.globals) do
              interfaces[#interfaces].globals[name] = {
                value = global.value;
                of = global.of;
                locked = global.locked;
              }
            end
          end

          return interfaces
        end)();
      }
    end

    this.inherits = _classes["loo.Object"] and getInherit(_classes["loo.Object"]) or nil

    local proxy, thisProxy
    proxy = setmetatable({
      inherits = function (_, classInherits)
        expect(classInherits, "string", nil, "class members")

        local classInherit = _classes[class.getFullName(env, classInherits)]

        if classInherit then
          this.inherits = getInherit(classInherit)
        else
          error("Cannot inherit a non-existant class " .. classInherits, 2)
        end

        return proxy
      end;
      implements = function (_, ...)
        interfacesImplements = {...}

        for i, implement in ipairs(interfacesImplements) do
          local interfaceImplement = _interfaces[interface.getFullName(env, implement)]

          if interfaceImplement then
            this.interfaces = this.interfaces or {}

            this.interfaces[#this.interfaces + 1] = {
              name = interfaceImplement.name;
              package = interfaceImplement.package;
              fullName = interfaceImplement.fullName;
      
              requiredGlobals = interfaceImplement.globals;
              globals = {};
            }
          else
            error("Cannot implement a non-existant interface " .. tostring(implement), 2)
          end
        end

        return proxy
      end;
    }, {
      __call = function (_, properties, level)
        level = level or 2

        expect(properties, "table", nil, level)

        for variable, value in pairs(properties) do
          expect(variable, "string", nil, tostring(variable) .. " (class: " .. this.fullName .. ")", level)

          local identifier

          local isLocal = variable:match("^local_")
          local isStatic = variable:match("^static_")
          local isMeta = variable:match("^meta_")
          local isOverride = variable:match("^override_")
          local isOf, interfaceName = variable:match("^(of)_([_a-zA-Z][a-zA-Z0-9]*)_")
          local isVirtual = variable:match("^virtual_")

          if isLocal then
            identifier = checkIdentifier(variable:sub(7), level)

            this.locals[identifier] = {
              value = value;
              locked = type(value) == "function" or nil;
            }
          elseif isStatic then
            identifier = checkIdentifier(variable:sub(8), level)

            if type(value) == "function" then
              local val = value

              value = function (...)
                return val(thisProxy, ...)
              end
            end

            this.static[identifier] = {
              value = value;
              isStatic = true;
              locked = type(value) == "function" or nil;
            }
          elseif isMeta then
            identifier = checkIdentifier(variable:sub(6), level)

            this.meta["__" .. identifier] = value
          elseif isOverride then
            identifier = checkIdentifier(variable:sub(10), level)

            local function findOverride (class, var)
              if not class.inherits then
                return false
              elseif class.inherits.globals[var] and class.inherits.globals[var].isVirtual then
                return true, class.inherits
              else
                return findOverride(class.inherits, var)
              end
            end

            local canOverride, inherited = findOverride(this, identifier)

            if canOverride then
              this.globals[identifier] = {
                value = value;
                override = inherited.fullName;
                locked = type(value) == "function" or nil;
              }
            else
              error("Cannot override a non-existant virtual member " .. identifier .. " in class " .. this.fullName, level)
            end
          elseif isOf then
            identifier = checkIdentifier(variable:sub(5 + #interfaceName), level)
            interfaceName = interface.getFullName(env, interfaceName) or interfaceName

            local function findInterface (class, interface)
              if not class.interfaces then
                return false
              end

              for i, inter in ipairs(class.interfaces) do
                if inter.fullName == interface then
                  return true, inter
                end
              end

              if class.inherits then
                return findInterface(class.inherits, interface)
              end

              return false
            end

            local foundInterface, interface = findInterface(this, interfaceName)

            if foundInterface then
              if not interface.requiredGlobals[identifier] then
                error("Cannot implement field " .. identifier .. " for interface " .. interfaceName .. " because the field does not exist.", level)
              end

              interface.globals[identifier] = {
                value = value;
                of = this.fullName;
                locked = type(value) == "function" or nil;
              }
            else
              error("Cannot implement field " .. identifier .. " for non-existant interface " .. interfaceName, level)
            end
          else
            identifier = isVirtual and checkIdentifier(variable:sub(9), level) or checkIdentifier(variable, level)

            if identifier == this.name then
              expect(value, "function", "class constructor", 2 + level)

              this.constructor = value
            else
              this.globals[identifier] = {
                value = value;
                isVirtual = not not isVirtual;
                locked = type(value) == "function" or nil;
              }
            end
          end
        end

        if this.interfaces then
          for i, interface in ipairs(this.interfaces) do
            for name, valueType in pairs(interface.requiredGlobals) do
              if (not this.globals[name] or type(this.globals[name].value) ~= valueType) and (not interface.globals[name] or type(interface.globals[name].value) ~= valueType) then
                error("Field " .. name .. " (" .. valueType .. ") of interface " .. interface.fullName .. " was not implemented in class " .. this.fullName, level)
              end
            end
          end
        end

        _classes[this.fullName] = this

        thisProxy = setmetatable({}, {
          __type = "class";
          __index = function (_, k)
            if k == "__prop__" then
              return {
                className = this.name;
                classPackage = this.package;
                classFullName = this.fullName;
              }
            end

            return this.static[k].value
          end;
          __newindex = function (_, k, v)
            if reservedClassKeywords[k] then
              error("Attempt to override reserved field " .. tostring(k) .. " of class " .. this.fullName .. ".", 2)
            elseif not this.static[k] then
              error("Attempt to set a non-existant static field " .. tostring(k) .. " of class " .. this.fullName .. ".", 2)
            elseif this.static[k].locked then
              error("Attempt to override static method " .. tostring(k) .. " of class " .. this.fullName .. ".", 2)
            end

            this.static[k].value = v
          end;
          __call = function (_, ...)
            local selfChain, typeChain = {}

            local function buildSelf (classInstance, index, showLocals, canOverride)
              canOverride = expect(canOverride, "boolean", true)

              local self, metatable, mt = {}, {}, {}

              if classInstance.isInterface then
                self[#self + 1] = classInstance.globals

                for i = 1, #typeChain do
                  local instanceType = typeChain[i]

                  if instanceType.isClass then
                    local interfaceFields = {}

                    for name, global in pairs(instanceType.globals) do
                      if classInstance.class.requiredGlobals[name] then
                        interfaceFields[name] = global
                      end
                    end

                    self[#self + 1] = interfaceFields
                  end
                end
              elseif classInstance.isClass then
                local isOverridable = {}

                if showLocals then
                  self[#self + 1] = classInstance.locals
                end

                if canOverride then
                  for i = index, #typeChain do
                    local instanceType = typeChain[i]

                    if instanceType.isClass then
                      isOverridable[instanceType.fullName] = true
                    end
                  end

                  for i = index - 1, 1, -1 do
                    local instanceType = typeChain[i]

                    if instanceType.isClass then
                      local overriddenFields = {}

                      for name, global in pairs(instanceType.globals) do
                        if global.override and isOverridable[global.override] then
                          overriddenFields[name] = global
                        end
                      end

                      self[#self + 1] = overriddenFields
                    end
                  end
                end

                for i = index, #typeChain do
                  local instanceType = typeChain[i]

                  if instanceType.isClass then
                    self[#self + 1] = instanceType.globals

                    if showLocals then
                      self[#self + 1] = instanceType.static
                    end
                  end
                end

                for i = #typeChain, index, -1 do
                  local instanceType = typeChain[i]
                  
                  if instanceType.isClass then
                    for name, metamethod in pairs(instanceType.meta) do
                      metatable[name] = {
                        value = metamethod;
                        selfInstance = instanceType;
                      }
                    end
                  end
                end
              end

              if classInstance.isClass then
                for k, v in pairs(metatable) do
                  if isin(k:sub(3), {"add", "sub", "mul", "div", "mod", "pow", "concat"}) then
                    mt[k] = function (a, b, ...)
                      if is(a, classInstance.fullName) then
                        return v.value(v.selfInstance.localSelf or v.selfInstance.self, b, ...)
                      end

                      return v.value(a, v.selfInstance.localSelf or v.selfInstance.self, ...)
                    end
                  elseif type(v.value) == "function" then
                    mt[k] = function (_, ...)
                      return v.value(v.selfInstance.localSelf or v.selfInstance.self, ...)
                    end
                  else
                    mt[k] = v.value
                  end
                end
              end

              if not mt.__tostring then
                mt.__tostring = function ()
                  return classInstance.fullName .. ": " .. tostring(classInstance)
                end
              end

              local msgClass = (classInstance.isClass and "class" or "interface") .. " " .. classInstance.fullName

              mt.__type = classInstance.fullName
              mt.__index = function (_, k)
                if k == "__prop__" then
                  return {
                    className = classInstance.name;
                    classPackage = classInstance.package;
                    classFullName = classInstance.fullName;
                    selfChain = classInstance.selfChain;
                  }
                elseif k == "base" then
                  return showLocals and classInstance.class.inherits and (function ()
                      local indx

                      for i = index + 1, #typeChain do
                        if typeChain[i].fullName == classInstance.class.inherits.fullName then
                          indx = i
                          break;
                        end
                      end

                      if indx then
                        return setmetatable({}, {
                          __index = buildSelf(typeChain[indx], indx, false, false);
                          __call =  indx and not typeChain.constructed and function (_, ...) classInstance.class.inherits.constructor(buildSelf(typeChain[indx], indx, true), ...) end
                                    or
                                    function () error("Attempt to call a constructor of an already constructed class.", 2) end
                        })
                      end
                    end)() or nil
                elseif k == "this" then
                  return showLocals and classInstance.self or nil
                end

                for i, container in ipairs(self) do
                  if container[k] then
                    return container[k].value
                  end
                end

                if metatable.__index then
                  local var = metatable.__index

                  if type(var.value) == "function" then
                    return var.value(var.selfInstance.localSelf or var.selfInstance.self, k)
                  elseif type(var) == "table" then
                    return var.value[k]
                  end
                end

                error("Field " .. tostring(k) .. " in " .. msgClass .. " does not exist.", 2)
              end
              mt.__newindex = function (_, k, v)
                if reservedClassKeywords[k] then
                  error("Attempt to override reserved field " .. tostring(k) .. " of " .. msgClass .. ".", 2)
                end

                for i, container in ipairs(self) do
                  if container[k] then
                    if container[k].isStatic then
                      error("Attempt to set static field " .. k .. " of " .. msgClass .. " through an instance.", 2)
                    elseif container[k].locked then
                      error("Attempt to override locked field " .. k .. " of " .. msgClass .. ".", 2)
                    end

                    container[k].value = v
                    return
                  end
                end

                if metatable.__newindex and type(metatable.__newindex.value) == "function" then
                  return metatable.__newindex.value(metatable.__newindex.selfInstance.localSelf or metatable.__newindex.selfInstance.self, k, v)
                end

                error("Attempt to set a non-existant field " .. tostring(k) .. " in " .. msgClass .. ".", 2)
              end

              return setmetatable({}, mt)
            end

            local function getTypeChain (typeChain, class, isInterface)
              typeChain = typeChain or {}

              if not isInterface then
                local classInstance = {
                  isClass = true;
                  class = class;
                  selfChain = selfChain;

                  name = class.name;
                  package = class.package;
                  fullName = class.fullName;

                  locals = {};
                  globals = {};
                  static = class.static;

                  meta = class.meta;

                  self = nil;
                  localSelf = nil;
                }

                for name, var in pairs(class.locals) do
                  classInstance.locals[name] = {
                    value = var.value;
                    locked = var.locked;
                  }
                end

                for name, var in pairs(class.globals) do
                  local value = var.value

                  if type(value) == "function" then
                    value = function (self, ...)
                      return var.value(classInstance.localSelf, ...)
                    end
                  end

                  classInstance.globals[name] = {
                    value = value;
                    isVirtual = var.isVirtual;
                    override = var.override;
                    locked = var.locked;
                  }
                end

                typeChain[#typeChain + 1] = classInstance

                if class.interfaces then
                  for i = 1, #class.interfaces do
                    getTypeChain(typeChain, class.interfaces[i], true)
                  end
                end

                if class.inherits then
                  getTypeChain(typeChain, class.inherits)
                end
              else
                local interfaceInstance = {
                  isInterface = true;
                  class = class;
                  selfChain = selfChain;

                  name = class.name;
                  package = class.package;
                  fullName = class.fullName;

                  globals = {};

                  self = nil;
                };

                for name, var in pairs(class.globals) do
                  interfaceInstance.globals[name] = {
                    value = var.value;
                    of = var.of;
                    locked = var.locked;
                  }
                end

                typeChain[#typeChain + 1] = interfaceInstance
              end

              return typeChain
            end

            typeChain = getTypeChain({}, this)

            for i, typeInstance in ipairs(typeChain) do
              typeInstance.self = buildSelf(typeInstance, i)

              if typeInstance.isClass then
                typeInstance.localSelf = buildSelf(typeInstance, i, true)
              end

              selfChain[#selfChain + 1] = typeInstance.self
            end

            for i, typeInstance in ipairs(typeChain) do
              if typeInstance.isInterface then
                for name, var in pairs(typeInstance.globals) do
                  if type(var.value) == "function" then
                    local value, self = var.value

                    for i, typeInstance in ipairs(typeChain) do
                      if typeInstance.fullName == var.of then
                        self = typeInstance.localSelf
                        break;
                      end
                    end

                    var.value = function (_, ...)
                      return value(self, ...)
                    end
                  end
                end
              end
            end

            this.constructor(typeChain[1].localSelf, ...)
            typeChain.constructed = true

            return typeChain[1].self
          end;
        })

        classes[this.fullName] = thisProxy
        resolvePackage(this.package)[this.name] = thisProxy

        return thisProxy
      end;
    })

    if prop then
      return proxy(prop, 1 + level)
    end

    return proxy
  end
})

local function resolveType (env, typ)
  local _typ = class.getFullName(env, typ)
  return classes[_typ] and _typ or interface.getFullName(env, typ) or typ
end

local function switch (val, statements)
  local function c (s)
    if s[val] then
      return true, s[val](val)
    elseif s.default then
      return false, s.default(val)
    end

    return false
  end

  if statements then
    return c(statements)
  end

  return c
end

local function try (tryFunc, catchFunc)
  local ok, err = pcall(tryFunc)

  local function c (_, catchFunc)
    if not ok then
      catchFunc(err)
    end
  end

  if catchFunc then
    return c(_, catchFunc)
  end

  return {catch = c}
end

local function hook (env, root)
  local _env = expect(env, "table", {}, "environment")
  root = expect(root, "string", "/", "root")

  env = setmetatable({}, { __index = function (_, k)
    if isPackageNameValid(env.__package) then
      local pkg = resolvePackage(env.__package)

      if pkg and pkg[k] then
        return pkg[k]
      end
    end

    for import in pairs(env.__imports) do
      if isPackageNameValid(import) then
        local pkg, class = resolvePackage(import)
        
        if pkg and pkg[k] then
          return pkg[k]
        elseif class and class.__prop__.className == k then
          return class
        end
      end
    end

    if packages[k] then
      return packages[k]
    end

    return _env[k]
  end; })

  env.null = null

  env.type = type
  env.tonumber = tonumber
  env.isidentifier = isidentifier
  env.isprimitive = isprimitive
  env.isin = isin
  env.split = split
  env.isPackageNameValid = isPackageNameValid
  env.classes = classes
  env.hook = hook
  env.switch = switch
  env.try = try

  env.resolveType = function (env, typ)
    return resolveType(env, typ)
  end

  env.expect = function (value, expectType, default, name, level)
    expectType = resolveType(env, expectType)
    level = level == nil and 2 or level == 0 and 0 or level

    return expect(value, expectType, default, name, level)
  end

  env.cast = function (obj, typ, level)
    typ = resolveType(env, typ)
    level = level == nil and 1 or level == 0 and 0 or level + 1

    return cast(obj, typ, level)
  end

  env.is = function (obj, typ)
    typ = resolveType(env, typ)

    return is(obj, typ)
  end

  env.interface = setmetatable({}, {
    __index = {
      resolvePackage = function ()
        return interface.resolvePackage(env)
      end;
      getFullName = function (name)
        return interface.getFullName(env, name)
      end;
    };
    __call = function (_, name, prop, level)
      level = level == nil and 3 or level == 0 and 0 or level + 1

      return interface(env, name, prop, level)
    end;
  })

  env.class = setmetatable({}, {
    __index = {
      resolvePackage = function ()
        return class.resolvePackage(env)
      end;
      getFullName = function (name)
        return class.getFullName(env, name)
      end;
    };
    __call = function (_, name, prop, level)
      level = level == nil and 3 or level == 0 and 0 or level + 1

      return class(env, name, prop, level)
    end;
  })

  env.__package = ""
  function env.package (package, importPackage)
    expect(package, "string", nil, "package")
    importPackage = expect(importPackage, "boolean", true, "import package")

    if not isPackageNameValid(package) then
      error("Invalid package name '" .. package .. "'.", 2)
    end

    packages[package] = resolvePackage(package)
    env.__package = package

    if importPackage and not packages[package][1] then
      packages[package][1] = true

      env.import(package)

      packages[package][1] = false
    end
  end

  env.__imports = {}
  function env.import (name)
    expect(name, "string", nil, "import name")

    if env.__imports[name] then
      return
    end

    if packages[name] and packages[name][2] then -- already imported
      env.__imports[name] = true
      return
    end

    local pkg = split(name, ".")
    if #pkg >= 2 then
      local className = pkg[#pkg]
      local package = name:sub(1, -#className - 2)
      if packages[package] and packages[package][className] then
        env.__imports[name] = true
        return
      end
    end

    local function loadFile (path)
      local env = hook(_env, root)

      local func, err = loadfile(path, nil, env)

      if func then
        if setfenv then
          setfenv(func, env)
        end

        local ok, err = pcall(func)

        if not ok then
          error("An error occured while loading '" .. path .. "':\n" .. err, 3)
        end

        if packages[name] then
          packages[name][2] = true -- imported
        end
      else
        error("An error occured while loading '" .. path .. "':\n" .. err, 3)
      end
    end
    
    name = fs.combine("", (name:gsub("%.", "/")))

    local path = fs.combine(root, name)
    
    if fs.isDir(path) then
      for i, file in ipairs(fs.list(path)) do
        if file:sub(-4) == ".lua" and not fs.isDir(fs.combine(path, file)) then
          loadFile(fs.combine(path, file))
          env.__imports[name] = true
        end
      end
    elseif fs.exists(path .. ".lua") then
      loadFile(path .. ".lua")
      env.__imports[name] = true
    end
  end

  return env
end

local loo = hook(getfenv and getfenv() or _ENV)

loo.package ("loo", false)

loo.class "Object" {
  virtual_toString = function (self)
    return "Object: " .. self.__prop__.selfChain[1].__prop__.classFullName
  end;

  meta_tostring = function (self)
    return self:toString()
  end;
}

loo.class "Enumerable" {
  virtual_next = function (self, control)
    error("Method 'next' was not overridden.", 2)
  end;

  meta_call = function (self, state, control)
    return self:next(state, control)
  end;
}

loo.class "Array" : inherits "Enumerable" {
  local_itemType = null;
  local_items = null;

  Array = function (self, itemType, items)
    self.itemType = expect(itemType, "string", nil, "item type")
    self.items = expect(items, "table", {}, "items")

    for k, v in pairs(self.items) do
      if type(k) ~= "number" or k <= 0 or k > #self.items or type(v) ~= self.itemType then
        error("array of " .. self.itemType .. "s expected.", 3)
      end
    end
  end;

  insert = function (self, i, v)
    if v == nil then
      i, v = #self.items + 1, i
    end

    expect(i, "number", nil, "index")
    expect(v, self.itemType, nil, "value")

    if i <= 0 or i > #self.items + 1 then
      error("Array index out of range.", 2)
    end

    table.insert(self.items, i, v)
  end;

  remove = function (self, i)
    expect(i, "number", nil, "index")

    if i <= 0 or i > #self.items then
      error("Array index out of range.", 2)
    end

    return table.remove(self.items, i)
  end;

  each = function (self, func)
    expect(func, "function", nil, "each function")

    for i = 1, #self.items do
      func(i, self.items[i])
    end
  end;

  toTable = function (self)
    local tab = {}

    self:each(function (i, item)
      tab[i] = item
    end)

    return tab
  end;

  override_next = function (self, state, i)
    i = i and i + 1 or state or 1

    if i <= 0 or i > #self.items then
      return nil
    end

    return i, self.items[i]
  end;

  override_toString = function (self)
    local str = "{"

    self:each(function (i, item)
      str = str .. tostring(item) .. ", "
    end)

    return str:sub(1, -3) .. "}"
  end;

  meta_index = function (self, i)
    expect(i, "number", nil, "index", 2)
    
    if i <= 0 or i > #self.items then
      error("Array index out of range.", 2)
    end

    return self.items[i]
  end;

  meta_newindex = function (self, i, v)
    expect(i, "number", nil, "index", 2)
    expect(v, self.itemType, nil, "value", 2)

    if i <= 0 or i > #self.items + 1 then
      error("Array index out of range.", 2)
    end

    self.items[i] = v
  end;
}

return loo