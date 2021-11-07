from nes.pycore.system import NES as pyNES

# import pyjion
#run_tests()

# pyjion.enable()
# pyjion.config(graph=True, level=2)
nes = None


#python version:
#nes = NES("./roms/Super Mario Bros (E).nes", sync_mode=SYNC_AUDIO, opengl=True)
nes = pyNES("./roms/Super Mario Bros (E).nes")


if nes is not None:
    nes.run()

