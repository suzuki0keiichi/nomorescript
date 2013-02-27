if (!sandbox) sandbox = new ns.Namespace("sandbox");

/**
 * @constructor
 */
sandbox.PackageTestInSandbox = function () {
};

if (!sandbox) sandbox = new ns.Namespace("sandbox");
sandbox.add(["nest"]);

/**
 * @constructor
 */
sandbox.nest.PackageTestInSandboxNest = function () {
};

/**
 * @constructor
 */
PackageTestGlobal = function () {
};

