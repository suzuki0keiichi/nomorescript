if (typeof com === "undefined") { com = {}; }
if (typeof com.github === "undefined") { com.github = {}; }
if (typeof com.github.suzuki0keiichi === "undefined") { com.github.suzuki0keiichi = {}; }
if (typeof com.github.suzuki0keiichi.nomorescript === "undefined") { com.github.suzuki0keiichi.nomorescript = {}; }

/**
 * @constructor
 */
com.github.suzuki0keiichi.nomorescript.Hoge = function() {
};

/**
 * @function
 */
com.github.suzuki0keiichi.nomorescript.Hoge.prototype.abc = function() {
  var i = 10;
  console.log("" + i);
  for (var i__scoped__1 = 0; i__scoped__1 < 10; i__scoped__1 ++) {
    this.defg(i__scoped__1);
    this.defg(i__scoped__1 + 10);
  }
};

/**
 * @function
 * @param {number} num
 */
com.github.suzuki0keiichi.nomorescript.Hoge.prototype.defg = function(num) {
  console.log("" + num);
};

/**
 * @constructor
 */
com.github.suzuki0keiichi.nomorescript.Hoge__dummy_constructor__ = function() {};
com.github.suzuki0keiichi.nomorescript.Hoge__dummy_constructor__.prototype = com.github.suzuki0keiichi.nomorescript.Hoge.prototype;

