#!/usr/bin/python
import os

if not os.path.isfile("basic6-0.rom") or not os.path.isfile("basic6-1.rom") or\
   not os.path.isfile("basic6-2.rom") or not os.path.isfile("basic6-3.rom") or\
   not os.path.isfile("mo6-0.rom") or not os.path.isfile("mo6-1.rom"):
    print("\n\n  Missing 'mo6.rom' or 'mo5.rom' or 'cd-351-0.rom'\n\n")
    exit()

   
fou = open("rom_pack.vhd","w")
fou.write("LIBRARY IEEE;\nUSE IEEE.std_logic_1164.ALL;\nUSE IEEE.numeric_std.ALL;\n");
fou.write("PACKAGE rom_pack IS\n");
fou.write("\n");
fou.write("  TYPE arr8  IS ARRAY(natural RANGE<>) OF unsigned(7 DOWNTO 0);\n");
fou.write("\n");

def gen(nom,fil):
    i=0
    fou.write("  CONSTANT " + nom + " : arr8 := (\n    ");
    with open(fil,"rb") as fin:
        b = fin.read(1)
        while b:
            if i!=0:
                fou.write(",")
                if i& 15==0:
                    fou.write("\n    ")
            fou.write("x\"{:02X}\"".format(ord(b)))
            b = fin.read(1)
            i = i +1
        fou.write(");\n");
    fou.write("\n");

gen("ROM_BASIC6_0","basic6-0.rom")
gen("ROM_BASIC6_1","basic6-1.rom")
gen("ROM_BASIC6_2","basic6-2.rom")
gen("ROM_BASIC6_3","basic6-3.rom")
gen("ROM_MO6_0","mo6-0.rom")
gen("ROM_MO6_1","mo6-1.rom")
    
fou.write("END PACKAGE;");
fou.close

print("Created 'rom_pack.vhd'")
