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
#       Selection.py
#       GFS1-NHD:A9058.0000-SCRIPT;3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 3 (DELIVERED)
#         Created:  20-MAR-2007 13:36:53      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 2 (DELIVERED)
#         Created:  16-FEB-2006 14:32:56      TROJAN
#           removed unnecessary imports, renamed shadowed variables
#       
#       Revision 1 (APPROVED)
#         Created:  13-FEB-2006 10:22:43      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7284
#       	Action Date:       02-MAY-2007 10:53:18
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Climate tools input fields should be consistent.
#       
#
import exceptions, math
import Tkinter as tk
import Pmw
import Avn

##############################################################################
def hour_value_validator(text):
    if len(text) > 5:
        return Pmw.ERROR
    text = text.strip()
    if len(text) < 5:
        return Pmw.PARTIAL
    try:
        h, m = text.split(':')
        if len(h) != 2 or len(m) != 2:
            raise ValueError
        if not 0 <= int(h) < 24:
            raise ValueError
        if not 0 <= int(m) < 59:
            raise ValueError
    except (IndexError, ValueError):
        return Pmw.ERROR
    return Pmw.OK

def hour_range_validator(text):
    if len(text) > 11:
        return Pmw.ERROR
    text = text.strip()
    if len(text) < 11 or '-' not in text:
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if len(from_) != 5 or len(to) != 5:
            return Pmw.ERROR
    except (IndexError, ValueError):
        return Pmw.ERROR
    for x in from_, to:
        if x[2] != ':':
            return Pmw.ERROR
        h = int(x[:2])
        if not 0 <= h < 24:
            return Pmw.ERROR
        m = int(x[3:])
        if not 0 <= m < 60:
            return Pmw.ERROR
    return Pmw.OK

def date_value_validator(text):
    if len(text) > 4:
        return Pmw.ERROR
    text = text.strip()
    if len(text) < 4:
        return Pmw.PARTIAL
    try:
        m, d = text[:2], text[2:]
        if len(m) != 2 or len(d) != 2:
            raise ValueError
        if not 0 < int(m) <= 12:
            raise ValueError
        if not 0 < int(d) <= 31:
            raise ValueError
    except (IndexError, ValueError):
        return Pmw.ERROR
    return Pmw.OK

def date_range_validator(text):
    if len(text) > 9:
        return Pmw.ERROR
    text = text.strip()
    if len(text) < 9 or '-' not in text:
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if len(from_) != 4 or len(to) != 4:
            raise ValueError
        int(from_)
        int(to)
    except (IndexError, ValueError):
        return Pmw.ERROR
    for x in from_, to:
        month = int(x[:2])
        if not 1 <= month <= 12:
            return Pmw.ERROR
        day = int(x[2:])
        if not 1 <= day <= 31:
            return Pmw.ERROR
    return Pmw.OK

def wind_dir_value_validator(text):
    if len(text) > 4:
        return Pmw.ERROR
    text = text.strip().upper()
    if len(text) < 3:
        return Pmw.PARTIAL
    if text == 'CALM':
        return Pmw.OK
    if text.startswith('c'):
        return Pmw.PARTIAL
    try:
        if not 0 <= int(text) <= 360:
            raise ValueError
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

def wind_dir_range_validator(text):
    if len(text) > 7:
        return Pmw.ERROR
    text = text.strip()
    if len(text) < 7 or '-' not in text:
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if len(from_) != 3 or len(to) != 3:
            raise ValueError
        if not 0 < int(from_) <= 360 or not 0 < int(to) <= 360:
            raise ValueError
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

def wind_speed_value_validator(text):
    if len(text) > 5:
        return Pmw.ERROR
    text = text.strip()
    if text.endswith('+'):
        text = text[:-1]
    try:
        if not 0.0 <= float(text) <= 99.0:
            raise ValueError
    except ValueError:
        return Pmw.PARTIAL
    return Pmw.OK

def wind_speed_range_validator(text):
    if len(text) > 11:
        return Pmw.ERROR
    text = text.strip()
    if text.endswith('+'):
        text = text[:-1]
    if '-' not in text or text.endswith('-'):
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if not 0.0 <= float(from_) <= 99.0 or not 0.0 < float(to) <= 99.0:
            raise ValueError
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

def ceiling_value_validator(text):
    if len(text) > 5:
        return Pmw.ERROR
    text = text.strip()
    try:
        if not 0 <= int(text) <= 30000:
            raise ValueError
    except ValueError:
        return Pmw.PARTIAL
    return Pmw.OK

def ceiling_range_validator(text):
    if len(text) > 11:
        return Pmw.ERROR
    text = text.strip().upper()
    if '-' not in text or text.endswith('-'):
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if from_.endswith('+'):
            from_ = from_[:-1]
        if to.endswith('+'):
            to = from_[:-1]
        if not 0 <= int(from_) <= 30000:
            raise ValueError
        if to == 'UNL':
            return Pmw.OK
        elif not 0 <= int(to) <= 30000:
            raise ValueError
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

def visibility_value_validator(text):
    if len(text) > 5:
        return Pmw.ERROR
    text = text.strip()
    try:
        if not 0 <= float(text) <= 20.0:
            raise ValueError
    except ValueError:
        return Pmw.PARTIAL
    return Pmw.OK

def visibility_range_validator(text):
    if len(text) > 11:
        return Pmw.ERROR
    text = text.strip()
    if text.endswith('+'):
        text = text[:-1]
    if '-' not in text or text.endswith('-'):
        return Pmw.PARTIAL
    try:
        from_, to = text.split('-')
        if not 0 <= float(from_) <= 20.0:
            raise ValueError
        if not 0 <= float(to) <= 20.0:
            raise ValueError
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

##############################################################################
class _BaseLine(Pmw.MegaWidget):
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('background', 'grey', None),
            ('width', 250, Pmw.INITOPT),
            ('height', 60, Pmw.INITOPT),
            ('margin', 10, Pmw.INITOPT),
            ('valuecolor', 'blue', Pmw.INITOPT),
            ('rangecolor', 'red', Pmw.INITOPT),
            ('valuevalidator', None, Pmw.INITOPT),
            ('rangevalidator', None, Pmw.INITOPT),
        )

        self.defineoptions(kw, optiondefs)

        Pmw.MegaWidget.__init__(self, parent)

        # Create the components.
        interior = self.interior()

        label = self.createcomponent('title',
            (), None,
            tk.Label, (interior,),
            relief='raised', 
            borderwidth=1, 
            text=self['title'],
        )
        label.grid(row=0, column=0, sticky='ew')
        frame = self.createcomponent('frame',
            (), None,
            tk.Frame, (interior,),
            relief='raised',
            borderwidth=1,
        )
        frame.grid(row=2, column=0, sticky='news')
        self._valueentry = self.createcomponent('value',
            (), None,
            Pmw.EntryField, (frame,), 
            labelpos='w',
            label_text='value:',
            label_justify='right',
            validate=self['valuevalidator'],
            entry_width=4,
            command=self.draw_value,
        )
        self._valueentry.pack(side='left', fill='x', expand=1, padx=3)
        self._rangeentry = self.createcomponent('range',
            (), None,
            Pmw.EntryField, (frame,), 
            labelpos='w',
            label_text='range:',
            label_justify='right',
            validate=self['rangevalidator'],
            entry_width=9,
            command=self.draw_range,
        )
        self._rangeentry.pack(side='left', fill='x', expand=1, padx=3)
        Pmw.alignlabels([self._valueentry, self._rangeentry])

        c = self._canvas = self.createcomponent('canvas', 
            (), None, 
            tk.Canvas, (interior,), 
            width=self['width'], 
            height=self['height'],
        )
        c.grid(row=1, column=0)

        # Draw labeled scale
        self.yc = self['height']/2
        self.x0, self.x1 = self['margin'], self['width']-self['margin']
        c.create_line(self.x0, self.yc, self.x1, self.yc, fill='black', 
            width=1)
        for v, text in self.ticks():
            x = self.value2x(v)
            c.create_line(x, self.yc, x, self.yc-5, fill='black', width=1)
            if text:
                c.create_text(x, self.yc-5, text=text, anchor='s')

        c.bind('<Any-ButtonRelease>', self.btnUp)
        c.bind('<B1-Motion>', self.btnMove)
        c.bind('<B2-Motion>', self.btnMove)

        self._cursor = c.cget('cursor')
        self._movePending = False

        # Check keywords and initialise options.
        self.initialiseoptions()

    def __move_value(self):
        # redraws value as an arrow on the X axis
        x = self.value2x(self._value)
        self._canvas.coords(self._canvas.find_withtag('arrow'), 
            x, self.yc, x+5, self.yc-8, x-5, self.yc-8)

    def __move_range(self):
        # redraws range as a shaded rectangle on the X axis
        c = self._canvas
        x0, x1 = self.value2x(self._range[0]), self.value2x(self._range[1])
        y0, y1 = self.yc+2, self.yc+20
        c.coords(c.find_withtag('range'), x0, y0, x1, y1)
        c.coords(c.find_withtag('from'), x0, y0, x0, y1)
        c.coords(c.find_withtag('to'), x1, y0, x1, y1)

    def __moveCompressed(self):
        c = self._canvas
        x = max(self.x0, c.canvasx(self._event.x))
        current_value = self.x2value(x)
        items = c.find_withtag('active')
        if not items:
            return
        if items[0] == c.find_withtag('arrow')[0]:
            if self._value != current_value:
                oldx = self.value2x(self._value)
                self.set_value(current_value)
                x = self.value2x(self._value)
                self.__move_value()
                if self._button == 2:
                    dx = x - oldx
                    x0, y0, x1, y1 = c.coords(c.find_withtag('range'))
                    x0 = max(self.x0, min(self.x1, x0+dx))
                    x1 = max(self.x0, min(self.x1, x1+dx))
                    from_, to = self.x2value(x0), self.x2value(x1)
                    self.set_range(from_, to)
                    self.__move_range()
        elif items[0] == c.find_withtag('from')[0]:
            if self._range[0] != current_value:
                self.set_range(current_value, self._range[1])
                self.__move_range()
        elif items[0] == c.find_withtag('to')[0]:
            if self._range[1] != current_value:
                self.set_range(self._range[0], current_value)
                self.__move_range()
        c.update_idletasks()
        self._movePending = False

    #########################################################################
    # Public methods
    def btnDown(self, event):
        # sets active tag
        c = self._canvas
        c.addtag_withtag('active', 'current')
        c.lift('active')
        self._movePending = False
        self._button = event.num
        c.configure(cursor='arrow')

    def btnUp(self, event):
        # deletes active tag
        c = self._canvas
        c.configure(cursor=self._cursor)
        tags = c.find_withtag('active')
        if not tags:
            return
        c.dtag('active')

    def btnMove(self, event):
        # saves mouse coordinates, schedules __moveCompressed()
        tags = self._canvas.find_withtag('active')
        if not tags:
            return
        self._event = event
        if not self._movePending:
            timerId = self._canvas.after_idle(self.__moveCompressed)
            self._movePending = True

    def set_value(self, value):
        # sets value in valueentry widget 
        self._value, s = self.scale(self.normalize(value))
        self._valueentry.setvalue(s)

    def get_value(self):
        # returns value from valueentry widget 
        if not self._valueentry.valid():
            raise ValueError('Invalid entry: '+\
                str(self._valueentry.getvalue()))
        return self.parse(self._valueentry.getvalue().strip().upper())

    def set_range(self, from_, to):
        # sets value in rangeentry widget 
        if from_ > to:
            from_, to = to, from_
        from_, s_from = self.scale(self.normalize(from_))
        to, s_to = self.scale(self.normalize(to))
        self._range = from_, to
        self._rangeentry.setvalue(s_from+'-'+s_to)

    def get_range(self):
        if not self._rangeentry.valid():
            raise ValueError('Invalid entry: '+\
                str(self._rangeentry.getvalue()))
        return [self.parse(s.strip()) for s in \
            self._rangeentry.getvalue().split('-')]

    def draw_value(self, value=None):
        # draws value as an arrow on the X axis
        if value is None:
            self.set_value(self.get_value())
        else:
            self.set_value(value)
        if not self._valueentry.valid():
            return
        c = self._canvas
        c.delete('arrow')
        x = self.value2x(self._value)
        c.create_polygon(x, self.yc, x+5, self.yc-8, x-5, self.yc-8, 
            fill=self['valuecolor'], width=1, tag='arrow')
        c.tag_bind('arrow', '<ButtonPress-1>', self.btnDown)
        c.tag_bind('arrow', '<ButtonPress-2>', self.btnDown)

    def draw_range(self, from_=None, to=None):
        # draws range as a shaded rectangle on the X axis
        if from_ is None or to is None:
            self.set_range(*self.get_range())
        else:
            self.set_range(from_, to)
        if not self._rangeentry.valid():
            return
        c = self._canvas
        c.delete('range', 'from', 'to')
        x0, x1 = self.value2x(self._range[0]), self.value2x(self._range[1])
        color = self['rangecolor']
        c.create_rectangle(x0, self.yc+2, x1, self.yc+20, outline=color, 
            fill=color, stipple='gray25', tag='range')
        c.create_line(x0, self.yc+2, x0, self.yc+20, fill=color, tag='from')
        c.create_line(x1, self.yc+2, x1, self.yc+20, fill=color, tag='to')
        c.tag_bind('from', '<ButtonPress-1>', self.btnDown)
        c.tag_bind('to', '<ButtonPress-1>', self.btnDown)

    def ticks(self):
        raise Avn.AvnError('Not implemented')

    def normalize(self, value):
        raise Avn.AvnError('Not implemented')

    def parse(self, text):
        raise Avn.AvnError('Not implemented')

    def scale(self, value):
        raise Avn.AvnError('Not implemented')

    def value2x(self, value):
        raise Avn.AvnError('Not implemented')

    def x2value(self, x):
        raise Avn.AvnError('Not implemented')

##############################################################################
class Ceiling(_BaseLine):
    '''Widget to select ceiling value and match range'''
    UNLIMITED = 99999.0
    LIMITED = 25000.0
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Ceiling', None),
            ('top', 5000.0, Pmw.INITOPT),
            ('shift', 100.0, Pmw.INITOPT),
            ('valuevalidator', ceiling_value_validator, Pmw.INITOPT),
            ('rangevalidator', ceiling_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        _BaseLine.__init__(self, parent)

    def ticks(self):
        ticks = [(0.0, '0'), (200.0, '2'), (600.0, '6'), (1000.0, '10'), \
            (3100.0, '31')]
        return ticks + [(self['top'], '%d' % (self['top']//100)), \
            (self.LIMITED, '%d+' % (self['top']//100+1)), \
            (self.UNLIMITED, 'UNL')]

    def get_range(self):
        range = _BaseLine.get_range(self)
        if range[0] == self.LIMITED:
            range[0] = self['top']+100.0
        return range

    def normalize(self, value):
        if value < 0.0:
            value = 0.0
        if value <= self['top']:
            return value
        elif value <= self.LIMITED:
            return self.LIMITED
        else:
            return self.UNLIMITED

    def scale(self, value):
        if value <= self['shift']: 
            value = 0.0
        if value <= self['top']: 
            return value, '%.0f' % value
        elif value == self.LIMITED:
            return self.LIMITED, '%.0f+' % (self['top']+100)
        elif value == self.UNLIMITED:
            return self.UNLIMITED, 'UNL'
        else:
            print '==== BAD thing happened #1 ===='

    def parse(self, text):
        if text == 'UNL':
            return self.UNLIMITED
        elif text.endswith('+'):
            return self.LIMITED
        else:
            return self.normalize(float(text))

    def value2x(self, value):
        value = self.normalize(value)
        if value <= self['shift']:
            value = self['shift']
        if value <= self['top']:
            v = math.sqrt((value-self['shift'])/ \
                (self['top']-self['shift']))*0.8
        elif value <= self.LIMITED:
            v = 0.9
        else:
            v = 1.0
        return self.x0 + v*(self.x1-self.x0)

    def x2value(self, x):
        v = (x-self.x0)/(self.x1-self.x0)
        if v <= 0.8:
            return (v/0.8)**2*(self['top']-self['shift']) + self['shift']
        elif v <= 0.9:
            return self.LIMITED
        else:
            return self.UNLIMITED

##############################################################################
class Visibility(_BaseLine):
    '''Widget to select visibility value and match range'''
    UNLIMITED = 20.0
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Visibility', None),
            ('top', 10.0, Pmw.INITOPT),
            ('shift', 0.1, Pmw.INITOPT),
            ('valuevalidator', visibility_value_validator, Pmw.INITOPT),
            ('rangevalidator', visibility_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        _BaseLine.__init__(self, parent)

    def ticks(self):
        ticks = [(0.0, '0'), (0.25, '1/4'), (1.0, '1'), (3.0, '3'), \
            (5.0, '5'), (6.0, '6')]
        return ticks + [(self['top'], '%d' % (self['top']//1.0)), \
            (self.UNLIMITED, '%d+' % ((self['top']+1)//1.0))]

    def normalize(self, value):
        if value < 0.0:
            value = 0.0
        if value <= self['top']:
            return value
        else:
            return self.UNLIMITED

    def scale(self, value):
        if value <= self['shift']: 
            value = 0.0
        if value < 1.0:
            return value, '%.2f' % value
        elif value <= self['top']: 
            return value, '%.1f' % value
        elif value == self.UNLIMITED:
            return self.UNLIMITED, '%d+' % ((self['top']+1)//1.0)
        else:
            print '=====', value
            print '==== BAD thing happened #1 ===='

    def parse(self, text):
        if text.endswith('+'):
            return self.UNLIMITED
        else:
            return self.normalize(float(text))

    def value2x(self, value):
        value = self.normalize(value)
        if value <= self['shift']:
            value = self['shift']
        if value <= self['top']:
            v = math.sqrt((value-self['shift'])/ \
                (self['top']-self['shift']))*0.9
        else:
            v = 1.0
        return self.x0 + v*(self.x1-self.x0)

    def x2value(self, x):
        v = (x-self.x0)/(self.x1-self.x0)
        if v <= 0.9:
            return (v/0.9)**2*(self['top']-self['shift']) + self['shift']
        else:
            return self.UNLIMITED

##############################################################################
class WindSpeed(_BaseLine):
    '''Widget to select wind speed value and match range'''
    UNLIMITED = 50.0
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Wind Speed', None),
            ('top', 30.0, Pmw.INITOPT),
            ('shift', 2.5, Pmw.INITOPT),
            ('valuevalidator', wind_speed_value_validator, Pmw.INITOPT),
            ('rangevalidator', wind_speed_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        _BaseLine.__init__(self, parent)

    def ticks(self):
        ticks = [(0.0, '0'), (4.0, '4'), (10.0, '10')]
        return ticks + [(self['top'], '%d' % (self['top']//1.0)), \
            (self.UNLIMITED, '%d+' % ((self['top']+1)//1.0))]

    def normalize(self, value):
        if value < 0.0:
            value = 0.0
        if value <= self['top']:
            return value
        else:
            return self.UNLIMITED

    def scale(self, value):
        if value <= self['shift']: 
            value = 0.0
        if value <= self['top']: 
            return value, '%.1f' % value
        elif value == self.UNLIMITED:
            return self.UNLIMITED, '%d+' % ((self['top']+1)//1.0)
        else:
            print '=====', value
            print '==== BAD thing happened #1 ===='

    def parse(self, text):
        if text.endswith('+'):
            return self.UNLIMITED
        else:
            return self.normalize(float(text))

    def value2x(self, value):
        value = self.normalize(value)
        if value <= self['shift']:
            value = self['shift']
        if value <= self['top']:
            v = math.sqrt((value-self['shift'])/(self['top']-self['shift']))*0.9
        else:
            v = 1.0
        return self.x0 + v*(self.x1-self.x0)

    def x2value(self, x):
        v = (x-self.x0)/(self.x1-self.x0)
        if v <= 0.9:
            return (v/0.9)**2*(self['top']-self['shift']) + self['shift']
        else:
            return self.UNLIMITED

##############################################################################
class _BaseCircle(Pmw.MegaWidget):
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('background', 'grey', None),
            ('size', 110, Pmw.INITOPT),
            ('margin', 20, Pmw.INITOPT),
            ('valuecolor', 'blue', Pmw.INITOPT),
            ('rangecolor', 'red', Pmw.INITOPT),
            ('valuevalidator', None, Pmw.INITOPT),
            ('rangevalidator', None, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        Pmw.MegaWidget.__init__(self, parent)

        # Create the components.
        interior = self.interior()
        interior.grid_columnconfigure(2, weight=1)
        interior.grid_rowconfigure(1, weight=1)
        frame = self.createcomponent('frame',
            (), None,
            tk.Frame, (interior,),
            relief='raised',
            borderwidth=1,
        )
        frame.grid(row=0, column=1, sticky='news')

        label = self.createcomponent('title',
            (), None,
            tk.Label, (frame,),
            borderwidth=1, 
            text=self['title'],
        )
        label.pack(side='top')
        self._valueentry = self.createcomponent('value',
            (), None,
            Pmw.EntryField, (frame,), 
            labelpos='w',
            label_text='value:',
            label_justify='right',
            validate=self['valuevalidator'],
            entry_width=12,
            command=self.draw_value,
        )
        self._valueentry.pack(side='top', fill='x', expand=1)

        self._rangeentry = self.createcomponent('range',
            (), None,
            Pmw.EntryField, (frame,), 
            labelpos='w',
            label_text='range:',
            label_justify='right',
            validate=self['rangevalidator'],
            entry_width=12,
            command=self.draw_range,
        )
        self._rangeentry.pack(side='top', fill='x', expand=1)
        Pmw.alignlabels([self._valueentry, self._rangeentry])

        c = self._canvas = self.createcomponent('canvas', 
            (), None, 
            tk.Canvas, (interior,), 
            width=self['size'], 
            height=self['size'],
        )
        c.grid(row=0, column=0)

        # Draw labelled circle
        x0 = y0 = self['margin']
        x1 = y1 = self['size'] - self['margin']
        self._bbox = x0, y0, x1, y1
        c.create_oval(self._bbox, outline='black') 
        self.xc = self.yc = self['size']/2
        self.rad = self.xc - self['margin']
        rad1 = self.rad + 5
        for v, (text, anchor) in self.ticks():
            fi = math.radians(90-self.value2fi(v))
            x, y = math.cos(fi), -math.sin(fi)
            x0 = self.xc + x*self.rad
            x1 = self.xc + x*rad1
            y0 = self.yc + y*self.rad
            y1 = self.yc + y*rad1
            c.create_line(x0, y0, x1, y1, fill='black')
            if text:
                c.create_text(x1, y1, text=text, anchor=anchor)

        c.bind('<Any-ButtonRelease>', self.btnUp)
        c.bind('<B1-Motion>', self.btnMove)
        c.bind('<B2-Motion>', self.btnMove)

        self._cursor = c.cget('cursor')
        self._movePending = False

        # Check keywords and initialise options.
        self.initialiseoptions()

    def __rotate_value(self):
        c = self._canvas
        c.coords(c.find_withtag('arrow'), *self.value_coords())

    def __move_range(self):
        c = self._canvas
        fi_from = self.value2fi(self._range[0])
        fi_to = self.value2fi(self._range[1])
        extent = fi_to-fi_from
        if extent < 0:
            extent += 360
        c.itemconfigure(c.find_withtag('range'), start=90-fi_to, extent=extent)
        xe, ye = self._fi2xy(fi_from)
        c.coords(c.find_withtag('from'), self.xc, self.yc, xe, ye)
        xe, ye = self._fi2xy(fi_to)
        c.coords(c.find_withtag('to'), self.xc, self.yc, xe, ye)

    def _fi2xy(self, fi):
        fi = math.radians(fi)
        x = self.xc + self.rad*math.sin(fi)
        y = self.yc - self.rad*math.cos(fi)
        return x, y

    def _xy2fi(self, x, y):
        return math.degrees(math.atan2(x-self.xc, -y+self.yc))

    def __moveCompressed(self):
        c = self._canvas
        x0, y0 = c.canvasx(self._event.x), c.canvasy(self._event.y)
        current_fi = self.fi2value(self._xy2fi(x0, y0))
        items = c.find_withtag('active')
        if not items:
            return
        if items[0] == c.find_withtag('arrow')[0]:
            if self._value != current_fi:
                old_value = self._value
                self.set_value(current_fi)
                delta = self._value - old_value
                self.__rotate_value()
                if self._button == 2:
                    from_, to = self._range[0]+delta, self._range[1]+delta
                    self.set_range(from_, to)
                    self.__move_range()
        elif items[0] == c.find_withtag('from')[0]:
            if self._range[0] != current_fi:
                self.set_range(current_fi, self._range[1])
                self.__move_range()
        elif items[0] == c.find_withtag('to')[0]:
            if self._range[1] != current_fi:
                self.set_range(self._range[0], current_fi)
                self.__move_range()
        c.update_idletasks()
        self._movePending = False

    def ticks(self):
        raise Avn.AvnError('Not implemented')

    def normalize(self, value):
        raise Avn.AvnError('Not implemented')

    def parse(self, text):
        raise Avn.AvnError('Not implemented')

    def scale(self, value):
        raise Avn.AvnError('Not implemented')

    def value2fi(self, value):
        raise Avn.AvnError('Not implemented')

    def fi2value(self, fi):
        raise Avn.AvnError('Not implemented')

    ##########################################################################
    # Public methods
    def btnDown(self, event):
        c = self._canvas
        c.addtag_withtag('active', 'current')
        c.lift('active')
        self._movePending = False
        self._button = event.num
        c.configure(cursor='arrow')

    def btnUp(self, event):
        c = self._canvas
        c.configure(cursor=self._cursor)
        tags = c.find_withtag('active')
        if not tags:
            return
        c.dtag('active')

    def btnMove(self, event):
        tags = self._canvas.find_withtag('active')
        if not tags:
            return
        self._event = event
        if not self._movePending:
            timerId = self._canvas.after_idle(self.__moveCompressed)
            self._movePending = True

    def value_coords(self):
        fi = math.radians(self.value2fi(self._value))
        x = math.sin(fi)
        y = -math.cos(fi)
        x0 = self.xc - x*self.rad/5
        y0 = self.yc - y*self.rad/5
        x1 = self.xc + x*(self.rad-5)
        y1 = self.yc + y*(self.rad-5)
        fi1 = fi+math.pi/12
        x2 = x1 - math.sin(fi1)*10
        y2 = y1 - -math.cos(fi1)*10
        fi2 = fi-math.pi/12
        x3 = x1 - math.sin(fi2)*10
        y3 = y1 - -math.cos(fi2)*10
        return x0, y0, x1, y1, x2, y2, x3, y3, x1, y1

    def set_value(self, value):
        self._value, s = self.scale(self.normalize(value))
        self._valueentry.setvalue(s)

    def get_value(self):
        if not self._valueentry.valid():
            raise ValueError('Invalid entry: '+ \
                str(self._valueentry.getvalue()))
        return self.parse(self._valueentry.getvalue().strip().upper())

    def set_range(self, from_, to):
        from_, s_from = self.scale(self.normalize(from_))
        to_, s_to = self.scale(self.normalize(to))
        self._range = from_, to
        self._rangeentry.setvalue(s_from+'-'+s_to)

    def get_range(self):
        if not self._rangeentry.valid():
            raise ValueError('Invalid entry: '+ \
                str(self._rangeentry.getvalue()))
        return [self.parse(s.strip()) for s in \
            self._rangeentry.getvalue().split('-')]

    def draw_value(self, value=None):
        if value is None:
            self.set_value(self.get_value())
        else:
            self.set_value(value)
        if not self._valueentry.valid():
            return
        c = self._canvas
        c.delete('arrow')
        kwds = {'width': 2, 'fill': self['valuecolor'], 'tag': 'arrow'}
        c.create_line(*self.value_coords(), **kwds)
        if c.gettags('range'):
            c.tag_raise('arrow', 'range')
        c.tag_bind('arrow', '<ButtonPress-1>', self.btnDown)
        c.tag_bind('arrow', '<ButtonPress-2>', self.btnDown)

    def draw_range(self, from_=None, to=None):
        if from_ is None or to is None:
            self.set_range(*self.get_range())
        else:
            self.set_range(from_, to)
        if not self._rangeentry.valid():
            return
        c = self._canvas
        c.delete('range', 'from', 'to')
        fi_from = self.value2fi(self._range[0])
        fi_to = self.value2fi(self._range[1])
        extent = fi_to-fi_from
        if extent < 0:
            extent += 360
        c.create_arc(self._bbox, start=90-fi_to, extent=extent, 
            outline=self['rangecolor'], fill=self['rangecolor'],
            stipple='gray25', tag='range')
        if c.gettags('arrow'):
            c.tag_lower('range', 'arrow')
        xe, ye = self._fi2xy(fi_from)
        c.create_line(self.xc, self.yc, xe, ye, fill=self['rangecolor'], 
            tag='from')
        c.tag_bind('from', '<ButtonPress-1>', self.btnDown)
        xe, ye = self._fi2xy(fi_to)
        c.create_line(self.xc, self.yc, xe, ye, fill=self['rangecolor'],
            tag='to')
        c.tag_bind('to', '<ButtonPress-1>', self.btnDown)

##############################################################################
class HourOfDay(_BaseCircle):
    '''Widget to select hour of the day and match range'''
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Hour', None),
            ('valuevalidator', hour_value_validator, Pmw.INITOPT),
            ('rangevalidator', hour_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        _BaseCircle.__init__(self, parent)

        self._canvas.create_oval(self.xc-3, self.yc-3, self.xc+3, self.yc+3, 
            fill=self['valuecolor'], outline=self['valuecolor']) 

    def ticks(self):
        # wind specific
        def _tick(hour):
            if hour%4 == 0:
                a = {0: 's', 4: 'w', 8: 'w', 12: 'n', 16: 'e', 20: 'e'}[hour]
                return hour, ('%02d' % hour, a)
            else:
                return hour, (None, None)
        return map(_tick, range(24)) 

    def normalize(self, value):
        return value%24

    def parse(self, text):
        h, m = [int(x) for x in text.split(':')]
        return h + m/60.0

    def scale(self, value):
        h, m = int(value), int(value%1.0*60)
        return value, '%02d:%02d' % (h, m)

    def value2fi(self, value):
        # wind specific
        return 15*self.normalize(value)

    def fi2value(self, fi):
        # wind specific
        return fi/15.0

##############################################################################
class Date(_BaseCircle):
    '''Widget to select julian day of year and match range'''
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Date', None),
            ('valuevalidator', date_value_validator, Pmw.INITOPT),
            ('rangevalidator', date_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        days = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        self.days = [sum(days[:n]) for n in range(1, len(days)+1)]

        _BaseCircle.__init__(self, parent)

        self._canvas.create_oval(self.xc-3, self.yc-3, self.xc+3, self.yc+3, 
            fill=self['valuecolor'], outline=self['valuecolor']) 

    def ticks(self):
        return zip([(d0+d1)/2 for d0, d1 in Avn.window(self.days)], 
            [('J', 's'), ('F', 'w'), ('M', 'w'), ('A', 'w'), ('M', 'w'), \
            ('J', 'n'), ('J', 'n'), ('A', 'n'), ('S', 'e'), ('O', 'e'), \
            ('N', 's'), ('D', 's')])

    def normalize(self, value):
        return value%365

    def parse(self, text):
        month, day = int(text[:2]), int(text[2:4])
        if month == 2 and day == 29:
            day = 28
        return self.days[month-1] + day - 1

    def scale(self, value):
        for m, (d0, d1) in enumerate(Avn.window(self.days)):
            if d0 <= value < d1:
                month, day = m+1, value-d0+1
                break
        return value, '%02d%02d' % (month, day)

    def value2fi(self, value):
        # wind specific
        return 360.0/365.0*self.normalize(value)

    def fi2value(self, fi):
        # wind specific
        return fi*365.0/360.0

##############################################################################
class WindDirection(_BaseCircle):
    '''Widget to select wind direction and match range'''
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('title', 'Wind Direction', None),
            ('valuevalidator', wind_dir_value_validator, Pmw.INITOPT),
            ('rangevalidator', wind_dir_range_validator, Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)

        _BaseCircle.__init__(self, parent)
        self._last_value = 180

    def btnDown3(self, event):
        # toggle calm/variable wind
        if self._value == 0:
            self.draw_value(self._last_value)
            self.draw_range(*self._last_range)
        else:
            self._last_value = self._value
            self._last_range = self._range
            self.set_value('CALM')
            self.draw_value()
            self.set_range(1, 360)
            self.draw_range()

    def set_value(self, value):
        # wind specific, overrides method in the base class
        if type(value) in [type(1), type(1.0)]:
            self._value, s = self.scale(self.normalize(value))
        else:
            self._value, s = 0, 'CALM'
        self._valueentry.setvalue(s)

    def draw_value(self, value=None):
        # wind specific, overrides method in the base class
        if value is None:
            self.set_value(self.get_value())
        else:
            self.set_value(value)
        if not self._valueentry.valid():
            return
        c = self._canvas
        c.delete('arrow')
        if self._value == 0:
            # Calm or variable wind
            kwds = {'width': 2, 'outline': self['valuecolor'], 'tag': 'arrow'}
            c.create_oval(self.xc-8, self.yc-8, self.xc+8, self.yc+8, **kwds) 
            c.tag_bind('wind', '<ButtonPress-3>', self.btnDown3)
        else:
            kwds = {'width': 2, 'fill': self['valuecolor'], 'tag': 'arrow'}
            c.create_line(*self.value_coords(), **kwds)
            if c.gettags('range'):
                c.tag_raise('arrow', 'range')
            c.tag_bind('arrow', '<ButtonPress-1>', self.btnDown)
            c.tag_bind('arrow', '<ButtonPress-2>', self.btnDown)
            c.tag_bind('arrow', '<ButtonPress-3>', self.btnDown3)

    def value_coords(self):
        # wind specific, overrides method in the base class
        fi = math.radians(self.value2fi(self._value))
        x = math.sin(fi)
        y = -math.cos(fi)
        r = self.rad - 5
        x0, y0 = self.xc, self.yc
        x1 = self.xc + x*r
        y1 = self.yc + y*r
        fi1 = fi-0.6*math.pi
        x2 = x1 - math.sin(fi1)*12
        y2 = y1 - -math.cos(fi1)*12
        return x0, y0, x1, y1, x2, y2

    def ticks(self):
        return [(0, ('N', 's')), (45, ('NE', 's')), (90, ('E', 'w')), \
            (135, ('SE', 'n')), (180, ('S', 'n')), (225, ('SW', 'n')), \
            (270, ('W', 'e')), (315, ('NW', 's'))]

    def normalize(self, value):
        return value%360 or 360

    def parse(self, text):
        if text == 'CALM':
            return 'CALM'
        else:
            return self.normalize(int(text))

    def scale(self, value):
        return value, '%03d' % value

    def value2fi(self, value):
        return self.normalize(value)

    def fi2value(self, fi):
        return fi%360.0
