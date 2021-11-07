from nes.pycore.system import NES as pyNES

# import pyjion
#run_tests()

# pyjion.enable()
# pyjion.config(graph=True, level=2)
nes = None
nes = pyNES("./roms/Snake.nes")


if nes is not None:
    nes.run()

