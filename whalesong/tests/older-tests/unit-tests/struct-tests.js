
var parent = types.makeStructureType('parent', false, 1, 0, false, function(args, name, k) { return k(args); });
var child1 = types.makeStructureType('child1', parent, 1, 1, 'auto1',
		function(args, name, k) { return k([args[0]+'-g1a', args[1]+'-g1b']); });
var child2 = types.makeStructureType('child2', parent, 2, 1, 'auto2', false);

var subchild1 = types.makeStructureType('subchild1', child1, 0, 0, false,
		function(args, name, k) { return k([args[0]+'-g2a', args[1]+'-g2b']); });
//		function(arg1, arg2, name) { return new types.ValuesWrapper([arg1+'-g2a', arg2+'-g2b']); });
var subchild2 = types.makeStructureType('subchild2', child2, 2, 2, 'auto2b', false);



// Constructors
var parentInstance = parent.constructor('a');
var child1Instance = child1.constructor('b', '1');
var child2Instance = child2.constructor('c', '2', '3');
var subchild1Instance = subchild1.constructor('d', '4');
var subchild2Instance = subchild2.constructor('e', '5', '6', 7, 8);

sys.print(sys.inspect(parentInstance) + '\n');
sys.print(sys.inspect(child1Instance) + '\n');
sys.print(sys.inspect(child2Instance) + '\n');
sys.print(sys.inspect(subchild1Instance) + '\n');
sys.print(sys.inspect(subchild2Instance) + '\n');


// Predicates
assert.ok( parent.predicate(parentInstance) );
assert.ok( parent.predicate(child1Instance) );
assert.ok( parent.predicate(child2Instance) );
assert.ok( parent.predicate(subchild1Instance) );
assert.ok( parent.predicate(subchild2Instance) );

assert.ok( !child1.predicate(parentInstance) );
assert.ok( child1.predicate(child1Instance) );
assert.ok( !child1.predicate(child2Instance) );
assert.ok( child1.predicate(subchild1Instance) );
assert.ok( !child1.predicate(subchild2Instance) );

assert.ok( !child2.predicate(parentInstance) );
assert.ok( !child2.predicate(child1Instance) );
assert.ok( child2.predicate(child2Instance) );
assert.ok( !child2.predicate(subchild1Instance) );
assert.ok( child2.predicate(subchild2Instance) );

assert.ok( !subchild1.predicate(parentInstance) );
assert.ok( !subchild1.predicate(child1Instance) );
assert.ok( !subchild1.predicate(child2Instance) );
assert.ok( subchild1.predicate(subchild1Instance) );
assert.ok( !subchild1.predicate(subchild2Instance) );

assert.ok( !subchild2.predicate(parentInstance) );
assert.ok( !subchild2.predicate(child1Instance) );
assert.ok( !subchild2.predicate(child2Instance) );
assert.ok( !subchild2.predicate(subchild1Instance) );
assert.ok( subchild2.predicate(subchild2Instance) );


// Accessors
assert.deepEqual(parent.accessor(parentInstance, 0), 'a');
assert.deepEqual(parent.accessor(child1Instance, 0), 'b-g1a');
assert.deepEqual(parent.accessor(child2Instance, 0), 'c');
assert.deepEqual(parent.accessor(subchild1Instance, 0), 'd-g2a-g1a');
assert.deepEqual(parent.accessor(subchild2Instance, 0), 'e');

assert.deepEqual(child1.accessor(child1Instance, 0), '1-g1b');
assert.deepEqual(child1.accessor(child1Instance, 1), 'auto1');
assert.deepEqual(child1.accessor(subchild1Instance, 0), '4-g2b-g1b');
assert.deepEqual(child1.accessor(subchild1Instance, 1), 'auto1');

assert.deepEqual(child2.accessor(child2Instance, 0), '2');
assert.deepEqual(child2.accessor(child2Instance, 1), '3');
assert.deepEqual(child2.accessor(child2Instance, 2), 'auto2');
assert.deepEqual(child2.accessor(subchild2Instance, 0), '5');
assert.deepEqual(child2.accessor(subchild2Instance, 1), '6');
assert.deepEqual(child2.accessor(subchild2Instance, 2), 'auto2');

assert.deepEqual(subchild2.accessor(subchild2Instance, 0), 7);
assert.deepEqual(subchild2.accessor(subchild2Instance, 1), 8);
assert.deepEqual(subchild2.accessor(subchild2Instance, 2), 'auto2b');
assert.deepEqual(subchild2.accessor(subchild2Instance, 3), 'auto2b');

sys.print('All tests passed!!\n');
