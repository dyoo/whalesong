EXPORTS['image-color?'] =
    plt.runtime.makePrimitiveProcedure(
        'image-color?',
        1,
        function(MACHINE) {
            var elt = MACHINE.env[MACHINE.env.length - 1];
            return (isColorOrColorString(elt));
        });
