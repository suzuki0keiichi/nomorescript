if (!sandbox) sandbox = {};

/**
 * @constructor
 * @property {number} fieldInConstructor
 * @property {string} fieldInClassDefVal
 * @property {string} fieldInClassDefVar
 */
sandbox.FieldTest = function(fieldInConstructor) {
    this.fieldInConstructor = fieldInConstructor;
    this.fieldInClassDefVal = "data";
    this.fieldInClassDefVar = null;
};

