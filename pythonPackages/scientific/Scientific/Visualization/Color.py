# This module provides color definitions for use in Visualization.
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2008-8-18
#

"""
Color definitions for use in the modules VRML, VRML2, VMD, and PyMOL.
"""

from Scientific import N
import string

#
# Colors
#
class Color:

    """
    RGB Color specification

    Color objects can be added and multiplied with scalars.
    """
    
    def __init__(self, rgb):
        """
        @param rgb: a sequence of three numbers between zero and one,
                    specifying the intensities of red, green, and blue.
        @type rgb: C{int} or C{float}
        """
        self.rgb = (min(1.,max(0.,rgb[0])),
                    min(1.,max(0.,rgb[1])),
                    min(1.,max(0.,rgb[2])))

    def __mul__(self, scale):
        return Color(map(lambda i, s=scale: s*i, self.rgb))
    __rmul__ = __mul__

    def __add__(self, other):
        return Color(map(lambda a, b: a+b, self.rgb, other.rgb))

    def __cmp__(self, other):
        if not isinstance(other, Color):
            return NotImplemented
        return cmp(self.rgb, other.rgb)

    def __hash__(self):
        return hash(self.rgb)

    def __str__(self):
        return str(self.rgb[0])+' '+str(self.rgb[1])+' '+str(self.rgb[2])

    def __repr__(self):
        return 'Color(' + repr(self.rgb) + ')'

#
# Color scales
#
class ColorScale:

    """
    Mapping from a number interval to a color range

    The color scale is blue - green - yellow - orange - red.
    """

    def __init__(self, range):
        """
        @param range: a tuple of two numbers (the center of the interval
                      and its width), or a single number specifying the
                      width for a default center of zero
        @type range: C{float} or C{tuple}
        """
        if type(range) == type(()):
            self.zero, self.range = range
            self.range = self.range-self.zero
        else:
            self.range = range
            self.zero = 0.

    def __call__(self, value):
        """
        @param value: the value within the range for which the color
                      is requested. If the value is outside of the
                      specified range, the edge of the range is used instead.
        @type value: C{float}
        @returns: the color corresponding to value
        @rtype: L{Color}
        """
        value = (value-self.zero)/self.range
        value = max(min(value, 1.), 0.)
        if value <= 0.25:
            red = 0.
            green = 4.*value
            blue = 1.
        elif value <= 0.5:
            red = 0.
            green = 1.
            blue = 1.-4.*(value-0.25)
        elif value <= 0.75:
            red = 4.*(value-0.5)
            green = 1.
            blue = 0.
        else:
            red = 1.
            green = 1.-4.*(value-0.75)
            blue = 0.
        return Color((red, green, blue))

class SymmetricColorScale:

    """
    Mapping of a symmetric number interval to a color range

    The colors are red for negative numbers and green for positive
    numbers, with a color intensity proportional to the absolute
    value of the argument. Zero is mapped to white.
    """

    def __init__(self, max, n = 20):
        """
        @param max: a positive number defining the range, which is from
                    -max to +max.
        @type max: C{float}
        """
        self.range = max
        self.n = n
        self.colors = {}

    def __call__(self, value):
        """
        @param value: the value within the range for which the color
                      is requested. If the value is outside of the
                      specified range, the edge of the range is used instead.
        @type value: C{float}
        @returns: the color corresponding to value
        @rtype: L{Color}
        """
        negative = value < 0.
        index = N.floor(abs(value)*self.n/self.range)
        if index > self.n:
            raise ValueError('Value outside range')
        try:
            return self.colors[(negative, index)]
        except KeyError:
            white = 1.*(self.n-index)/self.n
            if negative:
                color = Color((1., white, white))
            else:
                color = Color((white, 1., white))
            self.colors[(negative, index)] = color
            return color

#
# Predefined colors
#
_full_colors = {
    'black': Color((0.,0.,0.)),
    'white': Color((1.,1.,1.)),
    'grey': Color((0.5,0.5,0.5)),
    'red': Color((1.,0.,0.)),
    'green': Color((0.,1.,0.)),
    'blue': Color((0.,0.,1.)),
    'yellow': Color((1.,1.,0.)),
    'magenta': Color((1.,0.,1.)),
    'cyan': Color((0.,1.,1.)),
    'orange': Color((1.,0.5,0.)),
    'violet': Color((1.,0.,0.5)),
    'olive': Color((0.1,0.6,0.2)),
    'brown': Color((0.6,0.4,0.)),
    }

_dark_colors = {}
for name, value in _full_colors.items():
    _dark_colors[name] = 0.3*value

_light_colors = {}
for name, value in _full_colors.items():
    _light_colors[name] = 0.7*value + 0.3*_full_colors['white']

del name
del value

def ColorByName(name):
    """
    @param name: one of the predefined color names: black, white, grey,
    red, green, blue, yellow, magenta, cyan, orange, violet, olive,
    and brown. Any color can be prefixed by "light " or "dark " to yield
    a variant. The prefix must be separated from the color name by white
    space, e.g. "light green".
    @type name: C{str}
    @returns: the color associated with name
    @rtype: L{Color}
    @raises KeyError: if the color name is not defined
    """
    name = string.split(string.lower(name))
    dict = _full_colors
    if len(name) == 2:
        if name[0] == 'light':
            dict = _light_colors
        elif name[0] == 'dark':
            dict = _dark_colors
    return dict[name[-1]]



