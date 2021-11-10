from nes.pycore.system import NES as pyNES

import pyjion
#run_tests()

#pyjion.enable()
#pyjion.config(graph=True, level=2)
nes = None
nes = pyNES("./roms/Mario.nes")


if nes is not None:
    nes.run()

from nes.pycore.ppu import NESPPU
from nes.pycore.mos6502 import MOS6502

print(pyjion.graph(NESPPU.run_cycles.__code__))
print(pyjion.graph(MOS6502.run_next_instr.__code__))
