# This module defines a plot widget for Tk user interfaces.
# It supports only elementary line plots at the moment.
# See the example at the end for documentation...
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# With contributions from RajGopal Srinivasan <raj@cherubino.med.jhmi.edu>
# Last revision: 2006-6-12
#

"""
Plot widget for Tk user interfaces

A plot widget acts like a canvas for special graphics objects
that represent curves shown by lines or markers.

Note that this module is not meant to replace a full-featured
plot program. It was designed to permit the simple integration of
plots into Tk-based user interfaces.
"""

import Tkinter
from Scientific import N
from Canvas import Line, CanvasText
import string

class PolyPoints:

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

    def scaleAndShift(self, scale=1, shift=0):
        self.scaled = scale*self.points+shift

    def writeToFile(self, file, separator):
        if self.points:
            for p in self.points:
                file.write(`p[0]` + ' ' + `p[1]` + '\n')
            return 1
        else:
            return 0

class PolyLine(PolyPoints):

    """
    Multiple connected lines

    @undocumented: draw
    """

    def __init__(self, points, **attr):
        """
        @param points: any sequence of (x, y) number pairs
        @param attr: line attributes

        @keyword width: the line width (default: 1)
        @keyword color: a string whose value is one of the
                        color names defined in Tk (default: C{"black"})
        @keyword stipple: a string whose value is the name of a bitmap
                          defined in Tk, or C{None} for no bitmap
                          (default: C{None})
        """
        PolyPoints.__init__(self, points, attr)

    _attributes = {'color': 'black',
                   'width': 1,
                   'stipple': None}

    def draw(self, canvas, bbox):
        if len(self.points) > 1:
            color = self.attributes['color']
            width = self.attributes['width']
            stipple = self.attributes['stipple']
            arguments = (canvas,)
            options = {'fill': color, 'width': width}
            if stipple:
                options['stipple'] = stipple
            for i in range(len(self.points)-1):
                x1, y1 = self.scaled[i]
                x2, y2 = self.scaled[i+1]
                arguments = arguments + (x1, y1, x2, y2)
            apply(Line, arguments, options)

class VerticalLine(PolyLine):

    """
    A vertical line
    """

    def __init__(self, xpos, **attr):
        """
        @param xpos: the x coordinate of the line
        @type xpos: C{float}
        @param attr: line attributes

        @keyword width: the line width (default: 1)
        @keyword color: a string whose value is one of the
                        color names defined in Tk (default: C{"black"})
        @keyword stipple: a string whose value is the name of a bitmap
                          defined in Tk, or C{None} for no bitmap
                          (default: C{None})
        """
        apply(PolyLine.__init__, (self, 2*[(xpos, 0.)]), attr)

    def draw(self, canvas, bbox):
        self.scaled[0, 1] = bbox[0][1]
        self.scaled[1, 1] = bbox[1][1]
        PolyLine.draw(self, canvas, bbox)

    def writeToFile(self, file, separator):
        return 0

class HorizontalLine(PolyLine):

    """
    A horizontal line
    """

    def __init__(self, ypos, **attr):
        """
        @param ypos: the y coordinate of the line
        @type ypos: C{float}
        @param attr: line attributes

        @keyword width: the line width (default: 1)
        @keyword color: a string whose value is one of the
                        color names defined in Tk (default: C{"black"})
        @keyword stipple: a string whose value is the name of a bitmap
                          defined in Tk, or C{None} for no bitmap
                          (default: C{None})
        """
        apply(PolyLine.__init__, (self, 2*[(0., ypos)]), attr)

    def draw(self, canvas, bbox):
        self.scaled[0, 0] = bbox[0][0]
        self.scaled[1, 0] = bbox[1][0]
        PolyLine.draw(self, canvas, bbox)

    def writeToFile(self, file, separator):
        return 0

class PolyMarker(PolyPoints):

    """
    Series of markers
    """

    def __init__(self, points, **attr):
        """
        @param points: any sequence of (x, y) number pairs
        @param attr: marker attributes

        @keyword width: the line width for drawing the marker (default: 1)
        @keyword color: a string whose value is one of the color names
                        defined in Tk, defines the color of the line forming
                        the marker (default: C{"black"})
        @keyword fillcolor: a string whose value is one of the color names
                            defined in Tk, defines the color of the interior
                            of the marker (default: C{"black"})
        @keyword marker: one of C{'circle'} (default), C{'dot'}, C{'square'},
                         C{'triangle'}, C{'triangle_down'}, C{'cross'},
                         C{'plus'}
        """
        PolyPoints.__init__(self, points, attr)

    _attributes = {'color': 'black',
                   'width': 1,
                   'fillcolor': 'black',
                   'size': 2,
                   'fillstyle': '',  # need to make this a meaningful option.
                   'marker': 'circle'}

    def draw(self, canvas, bbox):
        color = self.attributes['color']
        size = self.attributes['size']
        fillcolor = self.attributes['fillcolor']
        marker = self.attributes['marker']
        fillstyle = self.attributes['fillstyle']

        self._drawmarkers(canvas, self.scaled, marker, color, fillstyle,
                          fillcolor, size)

    def _drawmarkers(self, c, coords, marker='circle', color='black',
                     fillstyle='', fillcolor='', size=2):
        l = []
        f = getattr(self, '_' + marker)
        for xc, yc in coords:
            id = f(c, xc, yc, outline=color, size=size,
                   fill=fillcolor, fillstyle=fillstyle)
            if type(id) is type(()):
                for item in id: l.append(item)
            else:
                l.append(id)
        return l
    
    def _circle(self, c, xc, yc, size=1, fill='', outline='black',
                fillstyle=''):
        id = c.create_oval(xc-0.5, yc-0.5, xc+0.5, yc+0.5, 
                           fill=fill, outline=outline,
                           stipple=fillstyle)
        c.scale(id, xc, yc, size*5, size*5)
        return id

    def _dot(self, c, xc, yc, size=1, fill='', outline='black', fillstyle=''):
        id = c.create_oval(xc-0.5, yc-0.5, xc+0.5, yc+0.5, 
                           fill=fill, outline=outline,
                           stipple=fillstyle)
        c.scale(id, xc, yc, size*2.5, size*2.5)
        return id

    def _square(self, c, xc, yc, size=1, fill='', outline='black',
                fillstyle=''):
        id = c.create_rectangle(xc-0.5, yc-0.5, xc+0.5, yc+0.5,
                                fill=fill, outline=outline,
                                stipple=fillstyle)
        c.scale(id, xc, yc, size*5, size*5)
        return id
    
    def _triangle(self, c, xc, yc, size=1, fill='', outline='black',
                  fillstyle=''):
        id = c.create_polygon(-0.5, 0.288675134595, 0.5, 0.288675134595,
                              0.0, -0.577350269189, fill=fill,
                              outline=outline, stipple=fillstyle)
        c.move(id, xc, yc)
        c.scale(id, xc, yc, size*5, size*5)
        return id

    def _triangle_down(self, c, xc, yc, size=1, fill='', outline='black',
                       fillstyle=''):
        id = c.create_polygon(-0.5, -0.288675134595, 0.5, -0.288675134595,
                           0.0, 0.577350269189, fill=fill, outline=outline,
                           stipple=fillstyle)
        c.move(id, xc, yc)
        c.scale(id, xc, yc, size*5, size*5)
        return id

    def _cross(self, c, xc, yc, size=1, fill='black', outline=None,
               fillstyle=''):
        if outline: fill=outline
        id1 = c.create_line(xc-0.5, yc-0.5, xc+0.5, yc+0.5, fill=fill)
        id2 = c.create_line(xc-0.5, yc+0.5, xc+0.5, yc-0.5, fill=fill)
        c.scale(id1, xc, yc, size*5, size*5)
        c.scale(id2, xc, yc, size*5, size*5)
        return id1, id2

    def _plus(self, c, xc, yc, size=1, fill='black', outline=None,
              fillstyle=''):
        if outline: fill=outline
        id1 = c.create_line(xc-0.5, yc, xc+0.5, yc, fill=fill)
        id2 = c.create_line(xc, yc+0.5, xc, yc-0.5, fill=fill)
        c.scale(id1, xc, yc, size*5, size*5)
        c.scale(id2, xc, yc, size*5, size*5)
        return id1, id2


class PlotGraphics:

    """
    Compound graphics object

    @undocumented: boundingBox
    @undocumented: scaleAndShift
    @undocumented: draw
    @undocumented: writeToFile
    """
    
    def __init__(self, objects):
        """
        @param objects: a list of graphics objects (L{PolyLine},
                        L{PolyMarker}, L{PlotGraphics})
        @type objects: C{list}
        """
        self.objects = objects

    def boundingBox(self):
        p1, p2 = self.objects[0].boundingBox()
        for o in self.objects[1:]:
            p1o, p2o = o.boundingBox()
            p1 = N.minimum(p1, p1o)
            p2 = N.maximum(p2, p2o)
        return p1, p2

    def scaleAndShift(self, scale=1, shift=0):
        for o in self.objects:
            o.scaleAndShift(scale, shift)

    def draw(self, canvas, bbox):
        for o in self.objects:
            o.draw(canvas, bbox)

    def __len__(self):
        return len(self.objects)

    def __getitem__(self, item):
        return self.objects[item]

    def writeToFile(self, file, separator):
        data = 0
        for o in self.objects:
            if data:
                file.write(separator)
            data = o.writeToFile(file, separator)

class PlotCanvas(Tkinter.Frame):

    """
    Tk plot widget

    PlotCanvas objects support all operations of Tk widgets.
    """
    
    def __init__(self, master, width, height, background='white',
                 font="-*-helvetica-medium-r-normal--10-*-*-*-*-*-*-*",
                 **attr):
        """
        @param master: the parent widget
        @param width: the initial width of the canvas
        @type width: C{int}
        @param height: the initial height of the canvas
        @type height: C{int}
        @param background: the background color
        @type background: C{str}
        @param font: the font used for the axis labels
        @type font: C{str}
        @param attr: widget attributes
        
        @keyword zoom: a flag that indicates whether interactive
                       zooming (using the left mouse button) is enabled; the
                       default is C{False} (no zoom)
        @type zoom: C{bool}
        
        @keyword select: enables the user to select a range along the x axis
                         by dragging the mouse (with the left button pressed)
                         in the area B{under} the x axis. If select is 0,
                         no selection is possible. Otherwise the value of
                         select must be a callable object that is called
                         whenever the selection changes, with a single
                         argument that can be C{None} (no selection) or
                         a tuple containing two x values.
        """
        self.zoom = 0
        if attr.has_key('zoom'):
            self.zoom = attr['zoom']
            del attr['zoom']
        self.selectfn = None
        if attr.has_key('select'):
            self.selectfn = attr['select']
            del attr['select']
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
        if self.zoom or self.selectfn is not None:
            self.mouse_state = 0
            self.canvas.bind('<Button-1>', self._mousePressed)
            self.canvas.bind('<B1-Motion>', self._mouseMotion)
            self.canvas.bind('<ButtonRelease-1>', self._mouseRelease)
        self.popup_menu = Tkinter.Menu(self.canvas, tearoff=0)
        self.label = None
        self.canvas.bind('<Button-2>', self._showValue)
        self.canvas.bind('<ButtonRelease-2>', self._hideValue)
        self.popup_menu.add_command(label='Auto Scale',
                                    command=self._autoScale)
        self.popup_menu.add_command(label='Run Xmgrace',
                                    command=self._xmgr)
        self.canvas.bind('<Button-3>', self._popupMenu)
        self._setsize()
        self.last_draw = None
        self.font = self._testFont(font)
        self.rubberband = None
        self.rectangle = None
        self.selected_range = None

    def reconfigure(self, event):
        new_width = event.width-self.border[0]
        new_height = event.height-self.border[1]
        width = string.atoi(self.canvas.cget('width'))
        height = string.atoi(self.canvas.cget('height'))
        if new_width == width and new_height == height:
            return
        self.canvas.configure(width=new_width, height=new_height)
        self._setsize()
        self.clear()
        self.redraw()

    def bind(self, *args):
        apply(self.canvas.bind, args)

    def _testFont(self, font):
        if font is not None:
            bg = self.canvas.cget('background')
            try:
                item = CanvasText(self.canvas, 0, 0, anchor=Tkinter.NW,
                                  text='0', fill=bg, font=font)
                self.canvas.delete(item)
            except Tkinter.TclError:
                font = None
        return font

    def _setsize(self):
        self.width = string.atoi(self.canvas.cget('width'))
        self.height = string.atoi(self.canvas.cget('height'))
        self.plotbox_size = 0.97*N.array([self.width, -self.height])
        xo = 0.5*(self.width-self.plotbox_size[0])
        yo = self.height-0.5*(self.height+self.plotbox_size[1])
        self.plotbox_origin = N.array([xo, yo])

    def draw(self, graphics, xaxis = None, yaxis = None):
        """
        Draw something on the canvas

        @param graphics: the graphics object (L{PolyLine}, L{PolyMarker},
                         or L{PlotGraphics}) to be drawn
        @param xaxis: C{None} (no x-axis), C{"automatic"} (automatic scaling),
                      or a pair (x1, x2) defining the range of the x-axis
        @param yaxis: C{None} (no y-axis), C{"automatic"} (automatic scaling),
                      or a pair (y1, y2) defining the range of the y-axis
        """
        self.last_draw = (graphics, xaxis, yaxis)
        p1, p2 = graphics.boundingBox()
        xaxis = self._axisInterval(xaxis, p1[0], p2[0])
        yaxis = self._axisInterval(yaxis, p1[1], p2[1])
        text_width = [0., 0.]
        text_height = [0., 0.]
        if xaxis is not None:
            p1[0] = xaxis[0]
            p2[0] = xaxis[1]
            xticks = self._ticks(xaxis[0], xaxis[1])
            bb = self._textBoundingBox(xticks[0][1])
            text_height[1] = bb[3]-bb[1]
            text_width[0] = 0.5*(bb[2]-bb[0])
            bb = self._textBoundingBox(xticks[-1][1])
            text_width[1] = 0.5*(bb[2]-bb[0])
        else:
            xticks = None
        if yaxis is not None:
            p1[1] = yaxis[0]
            p2[1] = yaxis[1]
            yticks = self._ticks(yaxis[0], yaxis[1])
            for y in yticks:
                bb = self._textBoundingBox(y[1])
                w = bb[2]-bb[0]
                text_width[0] = max(text_width[0], w)
            h = 0.5*(bb[3]-bb[1])
            text_height[0] = h
            text_height[1] = max(text_height[1], h)
        else:
            yticks = None
        text1 = N.array([text_width[0], -text_height[1]])
        text2 = N.array([text_width[1], -text_height[0]])
        scale = (self.plotbox_size-text1-text2) / (p2-p1)
        shift = -p1*scale + self.plotbox_origin + text1
        self.transformation = (scale, shift)
        self.bbox = (p1, p2)
        self._drawAxes(self.canvas, xaxis, yaxis, p1, p2,
                       scale, shift, xticks, yticks)
        graphics.scaleAndShift(scale, shift)
        graphics.draw(self.canvas, (scale*p1+shift, scale*p2+shift))

    def _axisInterval(self, spec, lower, upper):
        if spec is None:
            return None
        if spec == 'minimal':
            if lower == upper:
                return lower-0.5, upper+0.5
            else:
                return lower, upper
        if spec == 'automatic':
            range = upper-lower
            if range == 0.:
                return lower-0.5, upper+0.5
            log = N.log10(range)
            power = N.floor(log)
            fraction = log-power
            if fraction <= 0.05:
                power = power-1
            grid = 10.**power
            lower = lower - lower % grid
            mod = upper % grid
            if mod != 0:
                upper = upper - mod + grid
            return lower, upper
        if type(spec) == type(()):
            lower, upper = spec
            if lower <= upper:
                return lower, upper
            else:
                return upper, lower
        raise ValueError(str(spec) + ': illegal axis specification')

    def _drawAxes(self, canvas, xaxis, yaxis,
                  bb1, bb2, scale, shift, xticks, yticks):
        dict = {'anchor': Tkinter.N, 'fill': 'black'}
        if self.font is not None:
            dict['font'] = self.font
        if xaxis is not None:
            lower, upper = xaxis
            text = 1
            for y, d in [(bb1[1], -3), (bb2[1], 3)]:
                p1 = scale*N.array([lower, y])+shift
                p2 = scale*N.array([upper, y])+shift
                Line(self.canvas, p1[0], p1[1], p2[0], p2[1],
                     fill = 'black', width = 1)
                for x, label in xticks:
                    p = scale*N.array([x, y])+shift
                    Line(self.canvas, p[0], p[1], p[0], p[1]+d,
                         fill = 'black', width = 1)
                    if text:
                        dict['text'] = label
                        apply(CanvasText, (self.canvas, p[0], p[1]), dict)
                text = 0

        dict['anchor'] = Tkinter.E
        if yaxis is not None:
            lower, upper = yaxis
            text = 1
            for x, d in [(bb1[0], -3), (bb2[0], 3)]:
                p1 = scale*N.array([x, lower])+shift
                p2 = scale*N.array([x, upper])+shift
                Line(self.canvas, p1[0], p1[1], p2[0], p2[1],
                     fill = 'black', width = 1)
                for y, label in yticks:
                    p = scale*N.array([x, y])+shift
                    Line(self.canvas, p[0], p[1], p[0]-d, p[1],
                         fill = 'black', width = 1)
                    if text:
                        dict['text'] = label
                        apply(CanvasText, (self.canvas, p[0], p[1]), dict)
                text = 0

    def _ticks(self, lower, upper):
        ideal = (upper-lower)/7.
        if ideal == 0.:
            ideal = 1./7.
        log = N.log10(ideal)
        power = N.floor(log)
        fraction = log-power
        factor = 1.
        error = fraction
        for f, lf in self._multiples:
            e = N.fabs(fraction-lf)
            if e < error:
                error = e
                factor = f
        grid = factor * 10.**power
        if power > 3 or power < -3:
            format = '%+7.0e'
        elif power >= 0:
            digits = max(1, int(power))
            format = '%' + `digits`+'.0f'
        else:
            digits = -int(power)
            format = '%'+`digits+2`+'.'+`digits`+'f'
        ticks = []
        t = -grid*N.floor(-lower/grid)
        while t <= upper and len(ticks) < 200:
            ticks.append((t, format % (t,)))
            t = t + grid
        return ticks

    _multiples = [(2., N.log10(2.)), (5., N.log10(5.))]

    def _textBoundingBox(self, text):
        bg = self.canvas.cget('background')
        dict = {'anchor': Tkinter.NW, 'text': text, 'fill': bg}
        if self.font is not None:
            dict['font'] = self.font
        item = apply(CanvasText, (self.canvas, 0., 0.), dict)
        bb = self.canvas.bbox(item)
        self.canvas.delete(item)
        return bb

    def clear(self):
        """
        Clear the canvas
        """
        self.canvas.delete('all')
        self.rectangle = None
        self.rubberband = None
        self.selected_range = None

    def redraw(self):
        """
        Redraw the most recent canvas contents
        """
        if self.last_draw is not None:
            apply(self.draw, self.last_draw)

    def _mousePressed(self, event):
        self.startx = self.canvas.canvasx(event.x)
        self.starty = self.canvas.canvasy(event.y)

    def _mouseMotion(self, event):
        if self.mouse_state == 0:
            scale, shift = self.transformation
            p = (N.array([self.startx, self.starty])-shift)/scale
            bb1, bb2 = self.bbox
            if self.selectfn is not None and p[1] < bb1[1]:
                self.mouse_state = 2
                self.canvas.delete(self.rectangle)
                self.rectangle = \
                     self.canvas.create_rectangle(self.startx, self.starty,
                                                  self.startx, self.starty,
                                                  fill='yellow', outline='',
                                                  stipple='gray50', width=0)
                self.canvas.lower(self.rectangle)
            elif self.zoom:
                self.mouse_state = 1
        if self.mouse_state == 1:
            x = self.canvas.canvasx(event.x)
            y = self.canvas.canvasy(event.y)
            if (self.startx != event.x)  and (self.starty != event.y) : 
                self.canvas.delete(self.rubberband)
                self.rubberband = self.canvas.create_rectangle(self.startx,
                                                               self.starty,
                                                               x, y)
                self.update_idletasks()
        elif self.mouse_state == 2:
            self.canvas.coords(self.rectangle, self.startx, 1.,
                               self.canvas.canvasx(event.x), self.height)
            self.update_idletasks()

    def _mouseRelease(self, event):
        if self.mouse_state == 1:
            self.canvas.delete(self.rubberband)
            self.rubberband = None
            p1 = N.array([self.startx, self.starty])
            p2 = N.array([self.canvas.canvasx(event.x),
                                self.canvas.canvasy(event.y)])
            if N.minimum.reduce(N.fabs(p1-p2)) > 5:
                scale, shift = self.transformation
                p1 = (p1-shift)/scale
                p2 = (p2-shift)/scale
                graphics, xaxis, yaxis = self.last_draw
                if xaxis is not None:
                    xaxis = (p1[0], p2[0])
                if yaxis is not None:
                    yaxis = (p2[1], p1[1])
                self.clear()
                self.draw(graphics, xaxis, yaxis)
        elif self.mouse_state == 2:
            scale, shift = self.transformation
            x1 = (self.startx-shift[0])/scale[0]
            x2 = (self.canvas.canvasx(event.x)-shift[0])/scale[0]
            if x1 < x2:
                self.selected_range = (x1, x2)
            else:
                self.selected_range = (x2, x1)
            if self.selectfn is not None:
                self.selectfn(self.selected_range)
        else:
            self.canvas.delete(self.rectangle)
            self.rectangle = None
            self.selected_range = None
            if self.selectfn is not None:
                self.selectfn(self.selected_range)
        self.mouse_state = 0

    def select(self, range):
        """
        Highlight a range on the x-axis

        @param range: the range on the x-axis to be highlighted. It can be
                      C{None} (no selection) or a sequence of two values on the
                      x-axis.
        """
        if range is None:
            if self.selected_range is not None:
                self.canvas.delete(self.rectangle)
                self.rectangle = None
                self.selected_range = None
        else:
            scale, shift = self.transformation
            x1 = scale[0]*range[0]+shift[0]
            x2 = scale[0]*range[1]+shift[0]
            if self.rectangle is None:
                self.rectangle = \
                     self.canvas.create_rectangle(x1, 1., x2, self.height,
                                                  fill='yellow', outline='',
                                                  stipple='gray50', width=0)
                self.canvas.lower(self.rectangle)
            else:
                self.canvas.coords(self.rectangle, x1, 1., x2, self.height)
            self.selected_range = range
        self.update_idletasks()

    def _popupMenu(self, event):
        self.popup_menu.post(event.x_root, event.y_root)

    def _autoScale(self):
        if self.last_draw is not None:
            graphics, xaxis, yaxis = self.last_draw
            if xaxis is not None:
                xaxis = 'automatic'
            if yaxis is not None:
                yaxis = 'automatic'
            self.clear()
            self.draw(graphics, xaxis, yaxis)

    def _xmgr(self):
        if self.last_draw is not None:
            import os, tempfile
            filename = tempfile.mktemp()
            file = open(filename, 'w')
            graphics, xaxis, yaxis = self.last_draw
            graphics.writeToFile(file, '!\n')
            file.close()
            os.system('(xmgrace %s ; rm %s ) &' % (filename, filename))

    def _showValue(self, event):
        scale, shift = self.transformation
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        point = N.array([x, y])
        point = (point-shift)/scale
        text = "x = %f\ny = %f" % tuple(point)
        self.label = self.canvas.create_window(x, y,
                                               window=Label(self.canvas,
                                                            text=text))

    def _hideValue(self, event):
        if self.label is not None:
            self.canvas.delete(self.label)
            self.label = None


if __name__ == '__main__':

    window = Tkinter.Frame()
    window.pack(fill=Tkinter.BOTH, expand=Tkinter.YES)

    data1 = 2.*N.pi*N.arange(200)/200.
    data1.shape = (100, 2)
    data1[:,1] = N.sin(data1[:,0])
    lines1 = PolyLine(data1, color='green')

    pi = N.pi
    lines2 = PolyLine([(0., 0.), (pi/2., 1.), (pi, 0.), (3.*pi/2., -1),
                       (2.*pi, 0.)], color='red')

    markers = PolyMarker([(0., 0.), (pi/2., 1.), (pi, 0.), (3.*pi/2., -1),
                          (2.*pi, 0.)], color='blue', fillcolor='blue', 
                         marker='triangle')

    object = PlotGraphics([lines1, lines2, markers])

    def display(value):
        select(value)
        print value

    c = PlotCanvas(window, "300", "200", relief=Tkinter.SUNKEN, border=2,
                   zoom = 1, select = display)
    c.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=Tkinter.YES)

    def select(value):
        c.select(value)

    Tkinter.Button(window, text='Draw', command=lambda o=object:
                   c.draw(o, 'automatic', 'automatic')).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Clear',
                   command=c.clear).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Redraw',
                   command=c.redraw).pack(side=Tkinter.LEFT)
    Tkinter.Button(window, text='Quit',
                   command=window.quit).pack(side=Tkinter.RIGHT)

    window.mainloop()
