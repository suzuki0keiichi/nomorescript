MethodTest = function () {
    this.method3("hello");
};

MethodTest.prototype.method1 = function (param1) {
    println(param1)
    10
}

MethodTest.prototype.method2 = function (param2) {
    method1(param2)
}

MethodTest.prototype.method3 = function (param3) {
    method2(param3)
}
