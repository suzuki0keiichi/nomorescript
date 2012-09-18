/**
 * @constructor
 */
Global1 = function() {
  window.alert("hello Global1");
};

/**
 * @function
 */
function hello() {
  window.alert("hoge");
}

hello();

/**
 * @constructor
 */
UsingTest = function() {
};

/**
 * @function
 * @param {A} resource
 */
UsingTest.prototype.using = function(resource) {
  /**
   * @function
   * @param {A => B} func
   */
  return function(func) {
  try {
    return func(resource);
  } catch (__match_target__) {
    if (__match_target__ instanceof Exception) {
      var e = __match_target__;
      throw e;
    } else {
      throw new scala.Exception();
    }
  } finally {
    if (resource != null) {
      resource.close();
    }
  }
  }
};

if (typeof com === "undefined") { com = {}; }
if (typeof com.github === "undefined") { com.github = {}; }
if (typeof com.github.suzuki0keiichi === "undefined") { com.github.suzuki0keiichi = {}; }
if (typeof com.github.suzuki0keiichi.compilertest === "undefined") { com.github.suzuki0keiichi.compilertest = {}; }

/**
 * @interface
 */
com.github.suzuki0keiichi.compilertest.Trait1 = function() {
  window.alert("Trait1 constructor called");
};

com.github.suzuki0keiichi.compilertest.Trait1.__impl__ = {};

/**
 * @function
 */
com.github.suzuki0keiichi.compilertest.Trait1.prototype.trait1Def = function(){};

com.github.suzuki0keiichi.compilertest.Trait1.__impl__.trait1Def = function() {
  return "trait";
};

/**
 * @interface
 */
com.github.suzuki0keiichi.compilertest.Trait2 = function() {
};

com.github.suzuki0keiichi.compilertest.Trait2.__impl__ = {};

/**
 * @function
 */
com.github.suzuki0keiichi.compilertest.Trait2.prototype.trait2Def = function(){};

com.github.suzuki0keiichi.compilertest.Trait2.__impl__.trait2Def = function() {
  return "trait";
};

/**
 * @constructor
 * @implements {com.github.suzuki0keiichi.compilertest.Trait1}
 * @implements {com.github.suzuki0keiichi.compilertest.Trait2}
 */
com.github.suzuki0keiichi.compilertest.Class1 = function() {
  window.alert("Class1 say hello");
};

com.github.suzuki0keiichi.compilertest.Class1.prototype.__super_traits__ = Array(com.github.suzuki0keiichi.compilertest.Trait1, com.github.suzuki0keiichi.compilertest.Trait2);

com.github.suzuki0keiichi.compilertest.Class1.prototype.trait1Def = com.github.suzuki0keiichi.compilertest.Trait1.__impl__.trait1Def;
com.github.suzuki0keiichi.compilertest.Class1.prototype.trait2Def = com.github.suzuki0keiichi.compilertest.Trait2.__impl__.trait2Def;

/**
 * @function
 * @param {number} param1
 * @param {string} param2
 */
com.github.suzuki0keiichi.compilertest.Class1.prototype.class1Def = function(param1, param2) {
  param1 * 10 / this.class1DefInt();
  window.alert("class1Def say " + param1 + param2 + this.trait1Def());
  if (param1 == 0) {
    return "abc";
  } else {
    return "def";
  }
};

/**
 * @function
 */
com.github.suzuki0keiichi.compilertest.Class1.prototype.class1DefInt = function() {
  return 10;
};

/**
 * @constructor
 * @property {number} val1
 * @property {string} var1
 * @param {number} val1
 * @param {string} var1
 */
com.github.suzuki0keiichi.compilertest.Class2 = function(val1, var1) {
  this.val1 = null;
  this.var1 = null;
  this.val1 = val1;
  this.var1 = var1;
  this.class2Def(10)(
  function() {
    window.alert("hello anonymouse class");
  }
  );
};

/**
 * @function
 * @param {number} param1
 */
com.github.suzuki0keiichi.compilertest.Class2.prototype.class2Def = function(param1) {
  /**
   * @function
   * @param {() => Unit} f1
   */
  return function(f1) {
  f1();
  window.alert(param1);
  }
};

/**
 * @constructor
 * @property {string} val1
 * @property {Object} val2
 * @property {number} val3
 * @property {string} var1
 * @param {string} val1
 * @param {Object} val2
 * @param {number} notMemberVal1
 */
com.github.suzuki0keiichi.compilertest.Class3 = function(val1, val2, notMemberVal1) {
  this.val1 = null;
  this.val2 = null;
  this.val3 = null;
  this.var1 = null;
  this.val1 = val1;
  this.val2 = val2;
  this.val3 = 10;
  new com.github.suzuki0keiichi.compilertest.Class2(10, "innerClass1 hello");
};

window.alert("hello Object1");
/**
 * @function
 * @param {number} param1
 */
function object1Def(param1) {
  window.alert(param1);
}


if (typeof com === "undefined") { com = {}; }
if (typeof com.github === "undefined") { com.github = {}; }
if (typeof com.github.suzuki0keiichi === "undefined") { com.github.suzuki0keiichi = {}; }
if (typeof com.github.suzuki0keiichi.compilertest === "undefined") { com.github.suzuki0keiichi.compilertest = {}; }
if (typeof com.github.suzuki0keiichi.compilertest.childpackage === "undefined") { com.github.suzuki0keiichi.compilertest.childpackage = {}; }

/**
 * @constructor
 */
com.github.suzuki0keiichi.compilertest.childpackage.ChildPackageClass1 = function() {
};

/**
 * @function
 */
com.github.suzuki0keiichi.compilertest.childpackage.ChildPackageClass1.prototype.childPackageClass1Def = function() {
  window.alert("childPackageClass1Def");
};

$("hoge");

