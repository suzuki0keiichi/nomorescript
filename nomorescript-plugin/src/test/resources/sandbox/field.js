if (!sandbox) sandbox = {};

/**
 * @constructor
 * @property {number} fieldInConstructor
 * @property {string} fieldInClassDefVal
 * @property {string} fieldInClassDefVar
 * @param {number} fieldInConstructor
 */
sandbox.FieldTest = function (fieldInConstructor) {
    this.fieldInConstructor = fieldInConstructor;
    this.fieldInClassDefVal = "data";
    this.fieldInClassDefVar = null;
};

