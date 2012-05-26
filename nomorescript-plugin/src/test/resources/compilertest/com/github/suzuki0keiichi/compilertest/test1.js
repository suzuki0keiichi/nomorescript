Global1 = (function() {
  function Global1() {
    window.alert("hello Global1");
  }
  
  return Global1;
})();

function hello() {
  window.alert("hoge");
}

hello();

UsingTest = (function() {
  function UsingTest() {
  }
  
  UsingTest.prototype.using = function(resource, func) {
    try {
      return func(resource);
    } catch (_) {
      if (_ instanceof Exception) {
        var e = _;
        throw e;
      }
      {
        throw new scala.Exception();
      
      }
    } finally {
      if (resource != null) {
        resource.close();
      }
    }
  }
  
  return UsingTest;
})();

namespace(["com", "github", "suzuki0keiichi", "compilertest"]);

com.github.suzuki0keiichi.compilertest.Trait1 = (function() {
  function Trait1() {
  }
  
  Trait1.prototype.trait1Def = function() {
    return "trait";
  }
  
  return Trait1;
})();

com.github.suzuki0keiichi.compilertest.Class1 = (function() {
  function Class1() {
    window.alert("Class1 say hello");
  }
  
  Class1.prototype.class1Def = function(param1, param2) {
    param1 * 10 / this.class1DefInt();
    window.alert("class1Def say " + param1 + param2 + this.trait1Def());
    if (param1 == 0) {
      return "abc";
    } else {
      return "def";
    }
  }
  
  Class1.prototype.class1DefInt = function() {
    return 10;
  }
  
  return Class1;
})();

com.github.suzuki0keiichi.compilertest.Class2 = (function() {
  function Class2(val1, var1) {
    this.val1 = val1;
    this.var1 = var1;
    this.class2Def(10)();
  }
  
  Class2.prototype.class2Def = function(param1, f1) {
    f1();
    window.alert(param1);
  }
  
  return Class2;
})();

com.github.suzuki0keiichi.compilertest.Class3 = (function() {
  function Class3(val1, val2, notMemberVal1) {
    this.val1 = val1;
    this.val2 = val2;
    this.val3 = 10;
    new Class2(10, "innerClass1 hello");
  }
  
  return Class3;
})();

window.alert("hello Object1");
function object1Def(param1) {
  window.alert(param1);
}


namespace(["com", "github", "suzuki0keiichi", "compilertest", "childpackage"]);

com.github.suzuki0keiichi.compilertest.childpackage.ChildPackageClass1 = (function() {
  function ChildPackageClass1() {
  }
  
  ChildPackageClass1.prototype.childPackageClass1Def = function() {
    window.alert("childPackageClass1Def");
  }
  
  return ChildPackageClass1;
})();

$("hoge");

