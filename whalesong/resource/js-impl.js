var resourceType = MACHINE.modules['whalesong/resource/structs.rkt'].getExternalExports().get('struct:resource');

var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;

var checkResource = plt.baselib.check.makeCheckArgumentType(
    resourceType.predicate,
    "resource");

var getResourcePath = function(r) { return resourceType.accessor(r, 0); };
var getResourceKey = function(r) { return resourceType.accessor(r, 1); };



EXPORTS['resource->url'] = makePrimitiveProcedure(
    'resource->url',
    1,
    function(MACHINE) {
        var resource = checkResource(MACHINE, 'resource->url', 0);
        return String(getResourceKey(resource));
    });
