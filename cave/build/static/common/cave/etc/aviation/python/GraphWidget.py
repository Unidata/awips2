##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
#    Name:
#       GraphWidget.py
#       GFS1-NHD:A7895.0000-SCRIPT;1.7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.7 (DELIVERED)
#         Created:  24-AUG-2005 19:21:46      TROJAN
#           spr 7002
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-JUL-2005 12:45:24      TROJAN
#           spr 6913
#       
#       Revision 1.5 (DELIVERED)
#         Created:  12-MAY-2005 14:36:46      TROJAN
#           spr 6839
#       
#       Revision 1.4 (REVIEW)
#         Created:  07-MAY-2005 11:33:44      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  25-APR-2005 20:40:03      TROJAN
#           stdr917
#       
#       Revision 1.2 (DELIVERED)
#         Created:  02-APR-2005 17:02:16      TROJAN
#           spr 6763
#       
#       Revision 1.1 (DELIVERED)
#         Created:  09-JUL-2004 19:53:23      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7002
#       	Action Date:       02-SEP-2005 12:22:12
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Change ceiling and visibility axes annotation in the weater plot dialog
#       
#
# GraphWidget.py
# Based on: John E. Grayson, 'Python and Tkinter Programming', 
#   Manning, 2000, Chapter 11
# last update: 08/24/05

from Tkinter import *
import math

BIG = 10000000

def minBound(clist):
    x = min([BIG]+[c[0] for c in clist])
    y = min([BIG]+[min(c[1:]) for c in clist])
    return x, y

def maxBound(clist):
    x = max([-BIG]+[c[0] for c in clist])
    y = max([-BIG]+[max(c[1:]) for c in clist])
    return x, y

###############################################################################
class Points:
    _attributes = {
        'tags': ('point',)
        }

    def __init__(self, points, **attr):
        self.points = points
        self.scaled = self.points
        self.attributes = {}
        for name, value in self._attributes.items():
            try:
                value = attr[name]
            except KeyError:
                pass
            self.attributes[name] = value

    def boundingBox(self):
        return minBound(self.points),  maxBound(self.points)

    def fitToScale(self, scale=(1,1), shift=(0,0)):
        self.attributes.get('anchor', 0.0)
        self.anchor = scale[1]*self.attributes.get('anchor', 0.0)+shift[1]
        if not self.points:
            self.scaled = []
            return
        dim = len(self.points[0])
        sc = [scale[0]]+[scale[1]]*(dim-1)
        sh = [shift[0]]+[shift[1]]*(dim-1)
        def _map(x):
            y = [None]*dim
            for i in range(dim):
                if x[i] is not None:
                    y[i] = sc[i]*x[i]+sh[i]
            return y
        self.scaled = [_map(x) for x in self.points]

    def draw(self, canvas):
        pass

###############################################################################
class Line(Points):
    _attributes = {
        'fill': 'black',
        'width': 1,
        'smooth': 0,
        'splinesteps': 12,
        'tags': ('line',)
        }

    def __init__(self, points, **attr):
        Points.__init__(self, points, **attr)

    def draw(self, canvas):
	arguments = ()
        if self.attributes['smooth']:
            for i in range(len(self.points)):
                x1, y1 = self.scaled[i]
                if y1 is not None:
                    arguments = arguments + (x1, y1)
        else:
            for i in range(len(self.points)-1):
                x1, y1 = self.scaled[i]
                x2, y2 = self.scaled[i+1]
                if None not in (y1, y2):
                    arguments = arguments + (x1, y1, x2, y2)
        canvas.create_line(*arguments, **self.attributes)

###############################################################################
class Bars(Points):
    _attributes = {
        'fill': 'black',
        'width': 1,
        'size': 3,
        'stipple': '', 
        'outline': 'black',
        'tags': ('bars',)
        }

    def __init__(self, points, **attr):
        Points.__init__(self, points, **attr)
        self.spread = self.attributes['size']
        del self.attributes['size']

    def draw(self, canvas):
        if self.spread == 'auto':
            npoints = len(self.points)
            self.spread = (self.scaled[npoints-1][0]-self.scaled[0][0]) \
                / (npoints-1) / 2.5
        for i in range(npoints):
            x1, y1 = self.scaled[i]
            if y1 is not None:
                canvas.create_rectangle(x1-self.spread, y1, x1+self.spread, 
                    self.anchor, **self.attributes)

###############################################################################
class Steps(Points):
    _attributes = {
        'fill': 'black',
        'width': 1,
        'tags': ('steps',)
        }

    def __init__(self, points, **attr):
        Points.__init__(self, points, **attr)

    def draw(self, canvas):
        npoints = len(self.points)
	arguments = ()
        for i in range(npoints-1):
            x1, y1 = self.scaled[i]
            if y1 is None:
                continue
            x2, y2 = self.scaled[i+1]
            if y2 is None:
                arguments = arguments + (x1, y1, x2, y1)
            else:
                arguments = arguments + (x1, y1, x2, y1, x2, y2)
        if arguments:
	    canvas.create_line(*arguments, **self.attributes)

###############################################################################
class StepsRange(Points):
    _attributes = {
        'fill': 'black',
        'width': 1,
        'stipple': '', 
        'tags': ('stepsrange',)
        }

    def __init__(self, points, **attr):
        Points.__init__(self, points, **attr)
        self.attributes['outline'] = self.attributes['fill']

    def draw(self, canvas):
        npoints = len(self.points)
        d1 = dict(self.attributes)
        d2 = dict(self.attributes)
        del d2['stipple']
        del d2['outline']
        for i in range(npoints-1):
            x1, y11, y12 = self.scaled[i]
            x2, y21, y22 = self.scaled[i+1]
            if None not in (y11, y12):
                canvas.create_rectangle(x1, y11, x2, y12, **d1)
                if None not in (y21, y22):
	            canvas.create_line(x2, y11, x2, y21, **d2)

###############################################################################
class Winds(Points):
    _attributes = {
        'fill': 'black',
        'width': 1,
        'font': 'fixed',
        'tags': ('wind',)
        }
    _relrad = 0.1
    _rel10kt = 0.4
    _relspace = 0.18
    _relwidth = 0.2
    _arrowshape = (0.2, 0.25, 0.1)

    def __init__(self, points, winds, **attr):
        assert(len(points) == len(winds))
        points = [(x, 0) for x in points]
        Points.__init__(self, points, **attr)
        self.winds = winds
        self.textattr = dict(self.attributes)
        self.ovalattr = dict(self.attributes)
        del self.attributes['font']
        del self.textattr['width']
        self.ovalattr['outline'] = self.ovalattr['fill']
        del self.ovalattr['fill']
        del self.ovalattr['font']

    def _drawWind(self, canvas, xc, yc, data):
        dd, ff = data.get('dd', None), data.get('ff', None)
        if None in (dd, ff):
            return
        gg = data.get('gg', None) 
        if dd == 0:
            r = 2.0*self._relrad*self.len
        elif dd == 'VRB':
            r = 3.0*self._relrad*self.len
        else:
            r = self._relrad*self.len
        x0, y0 = xc - r, yc - r
        x1, y1 = xc + r, yc + r
        canvas.create_oval(x0, y0, x1, y1, **self.ovalattr)
        if dd == 0 or dd == 'VRB':
            if gg is not None:
                self.textattr['text'] = str(gg)     
                self.textattr['anchor'] = 'n'
                canvas.create_text(xc, yc+r+2, **self.textattr)
            return
        dd = math.radians(dd-90.0)
        ddbarb = dd + 1.0
        dx, dy = self.len*math.cos(dd), self.len*math.sin(dd)
        # annotated arrow
        if gg is not None:
            x0, y0 = xc - dx * self._relrad, yc - dy * self._relrad
            x1, y1 = xc - dx, yc - dy
            canvas.create_line(x0, y0, x1, y1, arrow='last', 
                arrowshape=self.shape, **self.attributes)
            if x0 < x1:
                self.textattr['anchor'] = 'w'
                off = 2
            else:
                self.textattr['anchor'] = 'e'
                off = -2
            self.textattr['text'] = str(gg)
            canvas.create_text(x1+off, y1, **self.textattr)
        x0, y0 = xc + dx*self._relrad, yc + dy*self._relrad  
        x1, y1 = xc + dx, yc + dy
        canvas.create_line(x0, y0, x1, y1, **self.attributes)
        dx1 = self.len*self._rel10kt*math.cos(ddbarb)
        dy1 = self.len*self._rel10kt*math.sin(ddbarb)
        x0, y0 = x1, y1
        while ff > 47.5:
            x1 = x0 - self._relwidth * dx + dx1
            y1 = y0 - self._relwidth * dy + dy1
            x2 = x0 - self._relwidth * dx
            y2 = y0 - self._relwidth * dy
            canvas.create_line(x0, y0, x1, y1, x2, y2, **self.attributes)
            x0 = x2 - self._relspace * dx
            y0 = y2 - self._relspace * dy
            ff -= 50.0
        while ff > 7.5:
            x1, y1 = x0 + dx1, y0 + dy1
            canvas.create_line(x0, y0, x1, y1, **self.attributes)
            x0 -= self._relspace * dx
            y0 -= self._relspace * dy
            ff -= 10
        if ff > 2.5:
            x1, y1 = x0 + 0.5 * dx1, y0 + 0.5 * dy1 
            canvas.create_line(x0, y0, x1, y1, **self.attributes)

    def boundingBox(self):
        p1, p2 = Points.boundingBox(self)
        return (p1[0], p1[1]-1.0), (p2[0], p2[1]+1.0)

    def fitToScale(self, scale=(1,1), shift=(0,0)):
        Points.fitToScale(self, scale, shift)
        f = abs(scale[1])
        self.len = f*0.8
        self.shape = tuple([f*x for x in self._arrowshape])

    def draw(self, canvas):
        npoints = len(self.points)
        for i in range(npoints):
            if self.winds[i] is not None:
                x1, y1 = self.scaled[i]
                self._drawWind(canvas, x1, y1, self.winds[i])

###############################################################################
class Objects:
    def __init__(self, objects):
        self.objects = objects

    def boundingBox(self):
        c1, c2 = self.objects[0].boundingBox()
        for object in self.objects[1:]:
            c1o, c2o = object.boundingBox()
            c1 = minBound([c1, c1o])
            c2 = maxBound([c2, c2o])
        return c1, c2

    def fitToScale(self, scale=(1,1), shift=(0,0)):
        for object in self.objects:
            object.fitToScale(scale, shift)

    def draw(self, canvas):
        for object in self.objects:
            object.draw(canvas)

###############################################################################
class Graph(Frame):
    _multiples = [(2., math.log10(2.0)), (5., math.log10(5.0))]

    def __init__(self, master, width, height, **kw):
        try:
            bg = kw['background']
            del kw['background']
        except KeyError:
            bg = 'white'
        try:
            font = kw['font']
            del kw['font']
        except KeyError:
            font = 'fixed'
        Frame.__init__(self, master, **kw)
        self.font = font
        self.canvas = Canvas(self, width=width, height=height, background=bg)
        self.canvas.pack(fill='both', expand='yes')
        border_w = self.canvas.winfo_reqwidth() - int(self.canvas.cget('width'))
        border_h = self.canvas.winfo_reqheight() - \
            int(self.canvas.cget('height'))
        self.border = (border_w, border_h)
        self.canvas.bind('<Configure>', self._reconfigure)
        self.plotarea_size = [None, None]
        self._setsize()
        self.last_drawn = None

    def _setsize(self):
        self.width = int(self.canvas.cget('width'))
        self.height = int(self.canvas.cget('height'))
        self.plotarea_size[0] = 0.97 * self.width
        self.plotarea_size[1] = 0.97 * -self.height
        xo = 0.5*(self.width-self.plotarea_size[0])
        yo = self.height-0.5*(self.height+self.plotarea_size[1])
        self.plotarea_origin = (xo, yo)

    def _axisInterval(self, spec, lower, upper):
        if spec is None:
            return None
        if spec == 'minimal':
            if lower == upper:
                return lower, upper+1.0
            else:
                return lower, upper
        if spec == 'auto':
            range = upper-lower
            if range == 0.0:
                return lower, upper+1.0
            log = math.log10(range)
            power = math.floor(log)
            fraction = log-power
            if fraction <= 0.05:
                power = power-1
            grid = 10.0**power
            lower = lower - lower % grid
            mod = upper % grid
            if mod != 0:
                upper = upper - mod + grid
            return lower, upper
        if type(spec) == type(()) or type(spec) == type([]):
            return spec[0][0], spec[-1][0]
        raise ValueError, str(spec) + ': illegal axis specification'

    def _drawGrid(self, xaxis, yaxis, bb1, bb2, scale, shift, xticks, yticks,
        numYLabels):
        dd_line = {'fill': 'black', 'width': 1, 'stipple': 'gray50'}
        dd_text = {'fill': 'black'}
        if self.font is not None:
            dd_text['font'] = self.font
        c = self.canvas
        p1 = (scale[0]*xaxis[0])+shift[0], (scale[1]*bb1[1])+shift[1]
        p2 = (scale[0]*xaxis[1])+shift[0], (scale[1]*bb2[1])+shift[1]
        c.create_rectangle(p1[0], p1[1], p2[0], p2[1], width=1)
        if xticks:
            for x, label in xticks:
                px = (scale[0]*x)+shift[0]
                c.create_line(px, p1[1], px, p2[1], **dd_line)
                dd_text['text'] = label
                dd_text['anchor'] = 'n'
                c.create_text(px, p1[1]+2, **dd_text)
                dd_text['anchor'] = 's'
                c.create_text(px, p2[1]+2, **dd_text)

        if yticks:
            for y, label in yticks:
                py = (scale[1]*y)+shift[1]
                c.create_line(p1[0], py, p2[0], py, **dd_line)
                dd_text['text'] = label.rstrip()
                dd_text['anchor'] = 'e'
                c.create_text(p1[0]-2, py, **dd_text)
                dd_text['text'] = label.lstrip()
                dd_text['anchor'] = 'w'
                c.create_text(p2[0]+3, py, **dd_text)
            # test
            for y, label in yticks[1:-1]:
                py = (scale[1]*y)+shift[1]
                dd_text['text'] = label.rstrip()
                dd_text['anchor'] = 'e'
                for k in range(1, numYLabels):
                    x = p1[0] + k*(p2[0]-p1[0])/(numYLabels)
                    c.create_text(x, py, **dd_text)

    def _drawAxes(self, xaxis, yaxis, bb1, bb2, scale, shift, xticks, yticks):
        dd_line = {'fill': 'black', 'width': 1}
        dd_text = {'fill': 'black'}
        if self.font is not None:
            dd_text['font'] = self.font
        c = self.canvas
        if xaxis is not None:
            for y, d, anchor in [(bb1[1], -3, 'n'), (bb2[1], 3, 's')]:
                p1 = (scale[0]*xaxis[0])+shift[0], (scale[1]*y)+shift[1]
                p2 = (scale[0]*xaxis[1])+shift[0], (scale[1]*y)+shift[1]
                c.create_line(p1[0], p1[1], p2[0], p2[1], **dd_line)
                if xticks:
                    dd_text['anchor'] = anchor
                    for x, label in xticks:
                        p = (scale[0]*x)+shift[0], (scale[1]*y)+shift[1]
                        c.create_line(p[0], p[1], p[0], p[1]+d, **dd_line)
                        dd_text['text'] = label
                        c.create_text(p[0], p[1]+1, **dd_text)

        if yaxis is not None:
            for x, d, anchor in [(bb1[0], -3, 'e'), (bb2[0], 3, 'w')]:
                p1 = (scale[0]*x)+shift[0], (scale[1]*yaxis[0])+shift[1]
                p2 = (scale[0]*x)+shift[0], (scale[1]*yaxis[1])+shift[1]
                c.create_line(p1[0], p1[1], p2[0], p2[1], **dd_line) 
                if yticks:
                    dd_text['anchor'] = anchor
                    for y, label in yticks:
                        p = (scale[0]*x)+shift[0], (scale[1]*y)+shift[1]
                        c.create_line(p[0], p[1], p[0]-d, p[1], **dd_line)
                        dd_text['text'] = label.rstrip()
                        c.create_text(p[0]+d+1, p[1], **dd_text)

    def _ticks(self, lower, upper, spec):
        if type(spec) == type(()):
            return spec
        ideal = (upper-lower)/7.
        log = math.log10(ideal)
        power = math.floor(log)
        fraction = log-power
        factor = 1.
        error = fraction
        for f, lf in self._multiples:
            e = math.fabs(fraction-lf)
            if e < error:
                error = e
                factor = f
        grid = factor * 10.0**power
        if power > 3 or power < -3:
            format = '%+7.0e'
        elif power >= 0:
            digits = max(1, int(power))
            format = '%' + str(digits) +'.0f'
        else:
            digits = -int(power)
            format = '%'+str(digits+2)+'.'+str(digits)+'f'
        ticks = []
        t = -grid*math.floor(-lower/grid)
        while t <= upper and len(ticks) < 200:
            ticks.append((t, format % (t,)))
            t = t + grid
        return ticks

    def _textBoundingBox(self, text):
        bg = self.canvas.cget('background')
        dd = {'anchor': 'nw', 'text': text, 'fill': bg}
        if self.font is not None:
            dd['font'] = self.font
        item = self.canvas.create_text(0., 0., **dd)
        bb = self.canvas.bbox(item)
        self.canvas.delete(item)
        return bb

    def bind(self, *args):
        self.canvas.bind(*args)

    def clear(self, tags='all'):
        self.canvas.delete(tags)

    def _reconfigure(self, event):
        new_width = event.width-self.border[0]
        new_height = event.height-self.border[1]
        width = int(self.canvas.cget('width'))
        height = int(self.canvas.cget('height'))
        if new_width == width and new_height == height:
            return
        self.canvas.configure(width=new_width, height=new_height)
        self._setsize()
        self.clear()
        if self.last_drawn is not None:
            self.draw(*self.last_drawn)
        
    def draw(self, graphics, xaxisspec=None, yaxisspec=None, grid=False,
        numYLabels=0):
        seq = []
        for o in graphics.objects:
            seq.extend(o.attributes['tags'])
        self.tags = tuple(seq)

        self.last_drawn = (graphics, xaxisspec, yaxisspec, grid)
        text_width = [0., 0.]
        text_height = [0., 0.]
        p1, p2 = graphics.boundingBox()
        xaxis = self._axisInterval(xaxisspec, p1[0], p2[0])
        yaxis = self._axisInterval(yaxisspec, p1[1], p2[1])
        if xaxis is not None:
            p1 = xaxis[0], p1[1]
            p2 = xaxis[1], p2[1]
            xticks = self._ticks(xaxis[0], xaxis[1], xaxisspec)
            bb = self._textBoundingBox(xticks[0][1])
            text_height[1] = bb[3]-bb[1]
            text_width[0] = 0.5*(bb[2]-bb[0])
            bb = self._textBoundingBox(xticks[-1][1])
            text_width[1] = 0.5*(bb[2]-bb[0])
        else:
            xticks = None
        if yaxis is not None:
            p1 = p1[0], yaxis[0]
            p2 = p2[0], yaxis[1]
            yticks = self._ticks(yaxis[0], yaxis[1], yaxisspec)
            for y in yticks:
                bb = self._textBoundingBox(y[1])
                w = bb[2]-bb[0]
                text_width[0] = max(text_width[0], w)
                h = 0.5*(bb[3]-bb[1])
                text_height[0] = h
                text_height[1] = max(text_height[1], h)
        else:
            yticks = None
        text1 = [text_width[0], -text_height[1]]
        text2 = [text_width[1], -text_height[0]]
        scale = ((self.plotarea_size[0]-2*text1[0]-2*text2[0]) / (p2[0]-p1[0]),
            (self.plotarea_size[1]-2*text1[1]-2*text2[1]) / (p2[1]-p1[1]))
        shift = ((-p1[0]*scale[0]) + self.plotarea_origin[0] + text1[0],
            (-p1[1]*scale[1]) + self.plotarea_origin[1] + text1[1])
        if grid:
            self._drawGrid(xaxis, yaxis, p1, p2, scale, shift, xticks, yticks,
                numYLabels)
        else:
            self._drawAxes(xaxis, yaxis, p1, p2, scale, shift, xticks, yticks)
        graphics.fitToScale(scale, shift)
        graphics.draw(self.canvas)

###############################################################################
if __name__ == '__main__':
    root = Tk()
    root.title('Graph Widget - Bar Graph')
    
    data = [(0,30),(1,145),(2,151),(3,147),(4,22),(5,31),
        (6,77),(7,125),(8,220),(9,550),(10,560),(11,340)]
    data2 = [(0,30,30),(1,145,145),(2,151,151),(3,147,160),(4,22,50),(5,31,31),
        (6,77,77),(7,125,160),(8,220,220),(9,550,700),(10,560,680),(11,340,340)]
    data3 = [0, 1, 4, 8]
    winds = [{'dd': 0, 'ff': 0}, {'dd': 30, 'ff': 24, 'gg': 35}, \
            {'dd': 210, 'ff': 12}, {'dd': 330, 'ff': 65}]
    xaxis = ((-0.5, ''),)+tuple([(d[0], str(d[0])+'.') for d in data])+\
        ((11.5, ''),)
#   xaxis = ((-0.5, ''),)+tuple([(d, str(d)+'.') for d in data3])+((8.5, ''),)
#   line1 = Bars(data, fill='green', size='auto')
    line1 = Line(data, color='red', width=1, smooth=0)
#   line2 = Steps(data, color='blue', width=1)
    line2 = StepsRange(data2, fill='blue')
    line3 = Winds(data3, winds, fill='red')
    
    graphObject  = Objects([line1])

    f = Frame(root)
    graph = Graph(f, 500, 350, relief=SUNKEN, background='gray90',
        font='helvb17gr', border=2)
    graph.pack(side=LEFT, fill='both', expand='yes')
    graph.draw(graphObject, xaxis, 'auto', grid=True)
    f.pack(side=TOP, expand='yes', fill='both')

    Button(root, text='Clear', command=graph.clear).pack(side=LEFT)
    Button(root, text='Quit', command=root.quit).pack(side=RIGHT)

    root.mainloop()
