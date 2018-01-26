
## AWIPS Colormaps

The hold-right-click menu allows you to change **Image Properties** and **Colormaps** for each loaded resource.

![image](../images/lP4W1kmTIh.gif)

Colormaps are presented in a heirarchical dropdown menu, including **GFE**, **Grid**, **Radar**, **Satellite** and colormaps ported from [**Matplotlib**](https://matplotlib.org/tutorials/colors/colormaps.html).

### Change Colormap



### Image Properties (Ctrl + I)

The Image Properties dialog box can be opened here or by using the Image Properties iconified button on the D2D Toolbar, or using using the Ctrl + I keyboard shortcut. This dialog box provides options that allow you to change the color table; adjust the brightness, contrast, and alpha of either a single image or combined images; fade between combined images; and/or interpolate the displayed data.

## Colormap Updates in AWIPS 17.1.1-5

In EDEX and CAVE 17.1.1-5, some existing colormaps have been removed, combined, and renamed (for example, the default colormap was changed from "Grid/gridded data" to "Grid/Gridded Data", and "topo" was updated to "Topography").

To prevent errors when loading bundle files which reference these old colormap names, CAVE 17.1.1-5 will not load the default color map (Grid/Gridded Data) if a defined colormap can not be found on the EDEX server.  An AlertView warning message will pop up when this happens, but CAVE will no longer throw an exception and prevent imagery from loading without a colormap.