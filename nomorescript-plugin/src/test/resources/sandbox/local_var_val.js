if (!sandbox) sandbox = {};

/**
 * @constructor
 */
sandbox.LocalVarValTest = function() {
};

/**
 * @function
 */
sandbox.LocalVarValTest.test = function() {
    var testval = 10;
    var testvar = "20";
    if (testval == 10) {
        var testval_scoped1 = 30;
        var testvar_scoped1 = null;

        testvar_scoped1 = "40";

        assert(testval_scoped1 != 30 || testvar_scoped1 != "40", "invalid scope");
    }

    assert(testval != 10 || testvar != "20", "invalid scope");
};
