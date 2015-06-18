# Loo
An OOP framework for Lua in ComputerCraft

Loo adds classes, interfaces, try..catch block, switch statement, new metamethods and some useful functions for you to easier write programs. Loo also has support for working with multiple files, to help maintain bigger projects easier.

Although it's not required, I recommend you to put this code at the top of your every main file of your project to load Loo into the environment. This code works with both, Lua 5.1 and Lua 5.2 environments:

```lua
-- load the loo API
local looPath = "loo.lua" -- the relative to the program path of the Loo API
local programRoot = fs.getDir(shell.getRunningProgram()) .. "/" local loo = dofile(programRoot .. looPath) local _ENV = loo.hook(getfenv and getfenv() or _ENV, programRoot) if setfenv then setfenv(1, _ENV) end
-- after this point Loo has injected itself into this script, so we can access variables directly
```

## Classes
The most important part of Loo are classes. Loo's class system is very rich and powerful compared to the generic class systems seen in other Lua projects. Loo classes feature:
* public members
* local (private) members
* static members
* inheritance
* multiple interface support
* metamethods
* type casting

## Interfaces
Loo's classes are capable of implementing multiple interfaces, which are simply objects which tell what public members a class should have.

## Override inherited members
Loo's classes which inherit from another class can override their members only if the inherited class allows it. Every overriden inherited class's member can still be accessed through the local 'base' member of the class.

## Type casting
Any Loo class can be casted to any type that is inside the class's inheritence chain, which not only includes classes but interfaces too.

## Example code
Here's a simple example showing how you can create a class and how to access it's members:

```lua
-- load the loo API
local looPath = "loo.lua" -- the relative to the program path of the Loo API
local programRoot = fs.getDir(shell.getRunningProgram()) .. "/" local loo = dofile(programRoot .. looPath) local _ENV = loo.hook(getfenv and getfenv() or _ENV, programRoot) if setfenv then setfenv(1, _ENV) end
-- after this point Loo has injected itself into this script, so we can access variables directly

-- create a class named 'A' with a local (private) field 'x' and two public members: 'getX' and setX'
class "A" {
  local_x = 5;
  
  getX = function (self)
    return self.x
  end;
  
  setX = function (self, x)
    self.x = x
  end;
}

-- create an instance of the class 'A'
local a = A()

-- call the public method 'getX'
print( a:getX() ) --> 5

-- set 'x' to a different value
a:setX(10)

print( a:getX() ) --> 10

-- note that we cannot access the member 'x' because it's local
print( a.x ) -- error: Field x in class A does not exist.
```

## Loo's own classes and interfaces
Currently, Loo only has three classes and none interfaces. The classes are: Object, Enumerable and Array; The Object class is the base class of every other class, which means it's the only class that doesn't inherit from something. It adds an overridable method 'toString' and a metamethod '__tostring' which calls the 'toString' method. Enumerable is a class which adds an easier interface when you want to make a class which can be enumerated. Array is a class which inherits Enumerable and allows you to create arrays which can only hold one type. It demonstrates how easy it is to create a custom class which can be passed straight into a for..in loop:

```lua
-- load the loo API
local looPath = "loo.lua" -- the relative to the program path of the Loo API
local programRoot = fs.getDir(shell.getRunningProgram()) .. "/" local loo = dofile(programRoot .. looPath) local _ENV = loo.hook(getfenv and getfenv() or _ENV, programRoot) if setfenv then setfenv(1, _ENV) end
-- after this point Loo has injected itself into this script, so we can access variables directly

-- import the Array class
import "loo.Array"

-- create a number array
local array = Array("number", {
  10, 20, 30
})

-- no need for 'pairs', 'ipairs' or anything similar!
for i, item in array do
  print(i, ". ", item)
end

--prints:
-- 1. 10
-- 2. 20
-- 3. 30
```
