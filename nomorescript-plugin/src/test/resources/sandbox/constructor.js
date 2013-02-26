if (!sandbox) sandbox = {};

/**
 * @constructor
 * @property {string} param
 * @property {number} valParam
 * @param {string} param
 * @param {number} valParam
 */
sandbox.ConstructorTest = function (param, valParam) {
    this.param = param;
    this.valParam = valParam;
    Predef.println(param + valParam);
};

