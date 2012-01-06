# This module defines a 3D wireframe visualization widget
# for Tk user interfaces.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2008-4-29
#

"""
3D wireframe canvas widget for Tk

This module provides a special widget for Tk user interfaces
which permits the display of 3D wireframe structures with interactive
manipulation.

Note that this widget can become sluggish if two many lines are to
be displayed. An OpenGL widget should be used for serious visualization
needs. The advantage of this module is that it requires no special
graphics libraries in addition to Tk.

@undocumented: PolyPoints3D
"""

import Tkinter
from Canvas import Line
import string
from Scientific import N
from Scientific.Geometry import Vector
from Scientific.Geometry.Transformation import Rotation

class PolyPoints3D:

    def __init__(self, points, attr):
        self.points = N.array(points)
        self.scaled = self.points
        self.attributes = {}
        for name, value in self._attributes.items():
            try:
                value = attr[name]
            except KeyError: pass
            self.attributes[name] = value

    def boundingBox(self):
        return N.minimum.reduce(self.points), \
               N.maximum.reduce(self.points)

    def project(self, axis, plane):
        self.depth = N.dot(self.points, axis)
        self.projection = N.dot(self.points, plane)

    def boundingBoxPlane(self):
        return N.minimum.reduce(self.projection), \
               N.maximum.reduce(self.projection)

    def scaleAndShift(self, scale=1, shift=0):
        self.scaled = scale*self.projection+shift


class PolyLine3D(PolyPoints3D):

    """
    Multiple connected lines
    """

    def __init__(self, points, **attr):
        """
        @param points: any sequence of (x, y, z) coordinate triples
        @param attr: line attributes

        @keyword width: line width (default: 1)
        @type width: C{int}
        @keyword color: a Tk color name (default: C{"black"})
        @type color: C{str}
        """
        PolyPoints3D.__init__(self, points, attr)

    _attributes = {'color': 'black',
                   'width': 1}

    def lines(self):
        color = self.attributes['color']
        width = self.attributes['width']
        lines = []
        depths = []
        for i in range(len(self.scaled)-1):
            x1, y1 = self.scaled[i]
            x2, y2 = self.scaled[i+1]
            lines.append((x1, y1, x2, y2,
                          color, width))
            depths.append(min(self.depth[i], self.depth[i+1]))
        return lines, depths


class VisualizationGraphics:

    """
    Compound graphics object

    @undocumented: boundingBox
    @undocumented: boundingBoxPlane
    @undocumented: project
    @undocumented: scaleAndShift
    @undocumented: lines
    """
    
    def __init__(self, objects):
        """
        @param objects: a list of graphics objects (L{PolyLine3D},
                        L{VisualizationGraphics})
        """
        self.objects = objects

    def boundingBox(self):
        p1, p2 = self.objects[0].boundingBox()
        for o in self.objects[1:]:
            p1o, p2o = o.boundingBox()
            p1 = N.minimum(p1, p1o)
            p2 = N.maximum(p2, p2o)
        return p1, p2

    def project(self, axis, plane):
        for o in self.objects:
            o.project(axis, plane)

    def boundingBoxPlane(self):
        p1, p2 = self.objects[0].boundingBoxPlane()
        for o in self.objects[1:]:
            p1o, p2o = o.boundingBoxPlane()
            p1 = N.minimum(p1, p1o)
            p2 = N.maximum(p2, p2o)
        return p1, p2

    def scaleAndShift(self, scale=1, shift=0):
        for o in self.objects:
            o.scaleAndShift(scale, shift)

    def lines(self):
        items = []
        depths = []
        for o in self.objects:
            i, d = o.lines()
            items = items + i
            depths = depths + d
        return items, depths

    def __len__(self):
        return len(self.objects)

    def __getitem__(self, item):
        return self.objects[item]


class VisualizationCanvas(Tkinter.Frame):

    """
    Tk visualization widget

    VisualizationCanvas objects support all operations of Tk widgets.

    Interactive manipulation of the display is possible with
    click-and-drag operations. The left mouse button rotates the
    objects, the middle button translates it, and the right button
    scales it up or down.
    """
    
    def __init__(self, master, width, height, background='white', **attr):
        """
        @param master: the parent widget
        @param width: the initial width of the canvas
        @type width: C{int}
        @param height: the initial height of the canvas
        @type height: C{int}
        @param background: the background color
        @type background: C{str}
        @param attr: widget attributes
        """
        apply(Tkinter.Frame.__init__, (self, master), attr)
        self.canvas = Tkinter.Canvas(self, width=width, height=height,
                                     background=background)
        self.canvas.pack(fill=Tkinter.BOTH, expand=Tkinter.YES)
        border_w = self.canvas.winfo_reqwidth() - \
                   string.atoi(self.canvas.cget('width'))
        border_h = self.canvas.winfo_reqheight() - \
                   string.atoi(self.canvas.cget('height'))
        self.border = (border_w, border_h)
        self.canvas.bind('<Configure>', self.reconfigure)
        self.canvas.bind('<1>', self.clickhandler1)
        self.canvas.bind('<ButtonRelease-1>', self.releasehandler1)
        self.canvas.bind('<2>', self.clickhandler2)
        self.canvas.bind('<ButtonRelease-2>', self.releasehandler2)
        self.canvas.bind('<3>', self.clickhandler3)
        self.canvas.bind('<ButtonRelease-3>', self.releasehandler3)
        self._setsize()
        self.scale = None
        self.translate = N.array([0., 0.])
        self.last_draw = None
        self.axis = N.array([0.,0.,1.])
        self.plane = N.array([[1.,0.], [0.,1.], [0.,0.]])

    def reconfigure(self, event):
        new_width = event.width-self.border[0]
        new_height = event.height-self.border[1]
        width = string.atoi(self.canvas.cget('width'))
        height = string.atoi(self.canvas.cget('height'))
        if new_width == width and new_height == height:
            return
        self.canvas.configure(width=new_width, height=new_height)
        self._setsize()
        self.clear(1)
        self.redraw()

    def _setsize(self):
        self.width = string.atoi(self.canvas.cget('width'))
        self.height = string.atoi(self.canvas.cget('height'))
        self.plotbox_size = 0.97*N.array([self.width, -self.height])
        xo = 0.5*(self.width-self.plotbox_size[0])
        yo = self.height-0.5*(self.height+self.plotbox_size[1])
        self.plotbox_origin = N.array([xo, yo])

    def copyViewpointFrom(self, other):
        self.axis = other.axis
        self.plane = other.plane
        self.scale = other.scale
        self.translate = other.translate

    def setViewpoint(self, axis, plane, scale=None, translate=None):
        self.axis = axis
        self.plane = plane
        if scale is not None:
            self.scale = scale
        if translate is not None:
            self.translate = translate

    def draw(self, graphics):
        """
        Draw something on the canvas

        @param graphics: the graphics object (L{PolyLine3D},
                         or L{VisualizationGraphics}) to be drawn
        """
        self.last_draw = (graphics, )
        self.configure(cursor='watch')
        self.update_idletasks()
        graphics.project(self.axis, self.plane)
        p1, p2 = graphics.boundingBoxPlane()
        center = 0.5*(p1+p2)
        scale = self.plotbox_size / (p2-p1)
        sign = scale/N.fabs(scale)
        if self.scale is None:
            minscale = N.minimum.reduce(N.fabs(scale))
            self.scale = 0.9*minscale
        scale = sign*self.scale
        box_center = self.plotbox_origin + 0.5*self.plotbox_size
        shift = -center*scale + box_center + self.translate
        graphics.scaleAndShift(scale, shift)
        items, depths = graphics.lines()
        sort = N.argsort(depths)
        for index in sort:
            x1, y1, x2, y2, color, width = items[index]
            Line(self.canvas, x1, y1, x2, y2, fill=color, width=width)
        self.configure(cursor='top_left_arrow')
        self.update_idletasks()

    def redraw(self):
        if self.last_draw is not None:
            apply(self.draw, self.last_draw)

    def clear(self, keepscale = 0):
        """
        Clear the canvas
        """
        self.canvas.delete('all')
        if not keepscale:
            self.scale = None

    def clickhandler1(self, event):
        self.click1x = event.x
        self.click1y = event.y
        self.configure(cursor='exchange')
        self.update_idletasks()

    def clickhandler2(self, event):
        self.click2x = event.x
        self.click2y = event.y
        self.configure(cursor='fleur')
        self.update_idletasks()

    def clickhandler3(self, event):
        self.click3x = event.x
        self.click3y = event.y
        self.configure(cursor='sizing')
        self.update_idletasks()

    def releasehandler1(self, event):
        self.configure(cursor='top_left_arrow')
        self.update_idletasks()
        try:
            dx = event.x - self.click1x
            dy = event.y - self.click1y
        except AttributeError:
            return
        if dx != 0 or dy != 0:
            normal = Vector(self.axis)
            move = Vector(-dx*self.plane[:,0]+dy*self.plane[:,1])
            axis = normal.cross(move) / \
                   N.minimum.reduce(N.fabs(self.plotbox_size))
            rot = Rotation(axis.normal(), axis.length())
            self.axis = rot(normal).array
            self.plane[:,0] = rot(Vector(self.plane[:,0])).array
            self.plane[:,1] = rot(Vector(self.plane[:,1])).array
            self.clear(1)
            self.redraw()

    def releasehandler2(self, event):
        self.configure(cursor='top_left_arrow')
        self.update_idletasks()
        try:
            dx = event.x - self.click2x
            dy = event.y - self.click2y
        except AttributeError:
            return
        if dx != 0 or dy != 0:
            self.translate = self.translate + N.array([dx, dy])
            self.clear(1)
            self.redraw()

    def releasehandler3(self, event):
        self.configure(cursor='top_left_arrow')
        self.update_idletasks()
        try:
            dy = event.y - self.click3y
        except AttributeError:
            return
        if dy != 0:
            ratio = -dy/self.plotbox_size[1]
            self.scale = self.scale * (1.+ratio)
            self.clear(1)
            self.redraw()


if __name__ == '__main__':

    from Scientific.IO.TextFile import TextFile
    from Scientific.IO.FortranFormat import FortranFormat, FortranLine
    import string

    generic_format = FortranFormat('A6')
    atom_format = FortranFormat('A6,I5,1X,A4,A1,A3,1X,A1,I4,A1,' +
                                '3X,3F8.3,2F6.2,7X,A4,2A2')

    # Read the PDB file and make a list of all C-alpha positions
    def readCAlphaPositions(filename):
        positions = []
        chains = [positions]
        for line in TextFile(filename):
            record_type = FortranLine(line, generic_format)[0]
            if record_type == 'ATOM  ' or record_type == 'HETATM':
                data = FortranLine(line, atom_format)
                atom_name = string.strip(data[2])
                position = N.array(data[8:11])
                if atom_name == 'CA':
                    positions.append(position)
                elif atom_name == 'OXT':
                    positions = []
                    chains.append(positions)
        if len(chains[-1]) == 0:
            del chains[-1]
        return chains

    conf = readCAlphaPositions('/Users/hinsen/Desktop/5CYT.pdb')
    colors = ['black', 'red', 'green', 'blue', 'yellow']
    colors = (len(conf)*colors)[:len(conf)]
    objects = []
    for chain, color in map(None, conf, colors):
        objects.append(PolyLine3D(chain, color=color))
    graphics = VisualizationGraphics(objects)

    window = Tkinter.Frame()
    window.pack(fill=Tkinter.BOTH, expand=Tkinter.YES)

    c = VisualizationCanvas(window, "100m", "100m", relief=Tkinter.SUNKEN,
                            border=2)
    c.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=Tkinter.YES)
    c.draw(graphics)

    Tkinter.Button(window, text='Draw',
                   command=lambda o=graphics: c.draw(o)).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Clear',
                   command=c.clear).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Redraw',
                   command=c.redraw).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Quit',
                   command=window.quit).pack(side=Tkinter.RIGHT)

    window.mainloop()
