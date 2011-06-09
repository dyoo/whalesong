

EXPORTS['is-color?'] =
    plt.runtime.makePrimitiveProcedure(
        'is-color?',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            return (//(plt.runtime.isString(elt) || plt.runtime.isSymbol(elt)) &&
	        typeof(colorDb.get(elt)) != 'undefined');
        });
