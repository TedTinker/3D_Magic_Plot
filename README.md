# 3D_Magic_Plot
A 3D plot of Magic: The Gathering cards by power, toughness, mana-cost, and color, and the R-Script which produced it.
The dataset in use is the AllPrintings JSON-file from mtgjson.com.

RGL.ply is a file which can be imported into Blender with File > Import > Stanford (.ply)

In Blender, the plot will appear gray. (At least, it did for me!) 

To see color, add a material-property whose Surface is "Diffuse BSDF."

Open the Shader-Editor and click Add > Input > Attribute.

Add name of the plot's Vector-Color to the attribute (it should be "Col" by default).

Connect the attribute's "Vector" to the Diffuse BSDF "Color."

Then, with cursor over the plot, type "Z" and select "Material Preview." The plot should appear in color!

Press Shift + \` to enter fly-mode, navigating with WASD, Q, and E. 

Running make_3D_plot.R will reproduce the 3D plot with RGL and export it as a PLY-file.
