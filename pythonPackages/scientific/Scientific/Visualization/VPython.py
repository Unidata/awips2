# VPython interface
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2006-6-12
#

"""
Definitions of simple 3D graphics objects and scenes containing them,
to be rendered using VPython
"""

from Scientific.Geometry import Transformation, Vector
import os, string, sys, tempfile
if not sys.modules.has_key('epydoc'):
    import visual

from Color import *

#
# Scene
#
class Scene:

    """
    VPython scene

    A VPython scene is a collection of graphics objects that can be
    shown in a VPython window. When the "view" method is called,
    a new window is created and the graphics objects are displayed
    in it.
    """

    def __init__(self, objects = None, **options):
        """
        @param objects: a list of graphics objects, or C{None} for
                        an empty scene
        @type objects: C{list} or C{NoneType}
        @param options: options as keyword arguments
        @keyword title: the window title (default: "VPython scene")
        @type title: C{str}
        @keyword width: the window width in pixels (default: 300)
        @type width: C{int}
        @keyword height: the window height in pixels (default: 300)
        @type height: C{int}
        @keyword background: the background color (default: "black")
        @type background: C{str}
        """
        if objects is None:
            self.objects = []
        elif type(objects) == type([]):
            self.objects = objects
        else:
            self.objects = [objects]
        self.options = {"title": "VPython Scene",
                        "width": 300,
                        "height": 300,
                        "background": "black"}
        for key, value in options.items():
            if self.options.has_key(key):
                self.options[key] = value
            else:
                raise ValueError("undefined option: " + repr(key))

    def __len__(self):
        """
        @returns: the number of graphics objects in the scene
        @rtype: C{int}
        """
        return len(self.objects)

    def __getitem__(self, item):
        """
        @param item: an index
        @type item: C{int}
        @returns: the graphics object at the index position
        @rtype: L{GraphicsObject}
        """
        return self.object[item]

    def addObject(self, object):
        """
        @param object: a graphics object to be added to the scene
        @type object: L{GraphicsObject}
        """
        self.objects.append(object)

    def view(self):
        """
        Open a VPython window for the scene
        """
        color = self.options["background"]
        if type(color) == type(''):
            color = ColorByName(color)
        self.window = visual.display(title = self.options["title"],
                                     width = self.options["width"],
                                     height = self.options["height"],
                                     background = color.rgb,
                                     exit = 0)
        for o in self.objects:
            o.display(self.window)

#
# Base classes for graphics objects
#
class GraphicsObject:

    """
    Graphics object for VPython

    This is an abstract base class. Use one of the subclasses to generate
    graphics.
    """

    def __init__(self, attr):
        """
        @param attr: graphics attributes specified by keywords
        @keyword material: color and surface properties
        @type material: L{Material}
        """
        self.attr = {}
        for key, value in attr.items():
            if key in self.attribute_names:
                self.attr[key] = value
            else:
                raise AttributeError('illegal attribute: ' + str(key))

    attribute_names = ['comment']

    def __getitem__(self, attr):
        """
        @param attr: the name of a graphics attribute
        @type attr: C{str}
        @returns: the value of the attribute, or C{None} if the attribute
                  is undefined
        """
        try:
            return self.attr[attr]
        except KeyError:
            return None

    def __setitem__(self, attr, value):
        """
        @param attr: the name of a graphics attribute
        @type attr: C{str}
        @param value: a new value for the attribute
        """
        self.attr[attr] = value

    def __copy__(self):
        return copy.deepcopy(self)


class ShapeObject(GraphicsObject):

    """
    Graphics objects representing geometrical shapes

    This is an abstract base class. Use one of the subclasses to generate
    graphics.
    """

    attribute_names = ['comment', 'material']

    def __add__(self, other):
        return Group([self]) + Group([other])

    def display(self, window):
        material = self.attr.get('material', None)
        if material is None:
            color = ColorByName('white')
        else:
            color = material.attr.get('emissive_color', None)
            if color is None:
                color = material.attr.get('diffuse_color', None)
            if color is None:
                color = ColorByName('white')
        window.foreground = color.rgb
        self.show(window)

#
# Specific shape objects
#
class Sphere(ShapeObject):

    """
    Sphere
    """
    
    def __init__(self, center, radius, **attr):
        """
        @param center: the center of the sphere
        @type center: L{Scientific.Geometry.Vector}
        @param radius: the sphere radius
        @type radius: positive number
        @param attr: graphics attributes as keyword parameters
        """
        self.center = center
        self.radius = radius
        ShapeObject.__init__(self, attr)

    def show(self, window):
        self.object = visual.sphere(pos=tuple(self.center), radius=self.radius)


class Cube(ShapeObject):

    """
    Cube

    The edges of a cube are always parallel to the coordinate axes.
    """
    
    def __init__(self, center, edge, **attr):
        """
        @param center: the center of the sphere
        @type center: L{Scientific.Geometry.Vector}
        @param edge: the length of an edge
        @type edge: positive number
        @param attr: graphics attributes as keyword parameters
        """
        self.center = center
        self.edge = edge
        ShapeObject.__init__(self, attr)

    def show(self, window):
        self.object = visual.box(pos = tuple(self.center),
                                 length = self.edge,
                                 height = self.edge,
                                 width = self.edge)


class Cylinder(ShapeObject):

    """
    Cylinder
    """

    def __init__(self, point1, point2, radius, **attr):
        """
        @param point1: first end point of the cylinder axis
        @type point1: L{Scientific.Geometry.Vector}
        @param point2: second end point of the cylinder axis
        @type point2: L{Scientific.Geometry.Vector}
        @param radius: the cylinder radius
        @type radius: positive number
        @param attr: graphics attributes as keyword parameters
        """
        self.point1 = point1
        self.point2 = point2
        self.radius = radius
        ShapeObject.__init__(self, attr)

    # accept "faces" for compatibility with VRML module
    attribute_names = ShapeObject.attribute_names + ['faces']

    def show(self, window):
        self.object = visual.cylinder(pos = tuple(self.point1),
                                      axis = tuple(self.point2-self.point1),
                                      radius = self.radius)

class Arrow(ShapeObject):

    """
    Arrow
    """

    def __init__(self, point1, point2, radius, **attr):
        """
        @param point1: starting point of the arrow
        @type point1: L{Scientific.Geometry.Vector}
        @param point2: the tip of the arrow
        @type point2: L{Scientific.Geometry.Vector}
        @param radius: the radius of the shaft
        @type radius: positive number
        @param attr: graphics attributes as keyword parameters
        """
        self.point1 = point1
        self.point2 = point2
        self.radius = radius
        ShapeObject.__init__(self, attr)

    def show(self, window):
        self.object = visual.arrow(pos = tuple(self.point1),
                                   axis = tuple(self.point2-self.point1),
                                   shaftwidth = self.radius)


class Cone(ShapeObject):

    """
    Cone
    """

    def __init__(self, point1, point2, radius, face = 1, **attr):
        """
        @param point1: the tip of the cone
        @type point1: L{Scientific.Geometry.Vector}
        @param point2: end point of the cone axis
        @type point2: L{Scientific.Geometry.Vector}
        @param radius: the radius at the base
        @type radius: positive number
        @param face: a boolean flag, specifying if the circular
                      bottom is visible
        @type face: C{bool}
        @param attr: graphics attributes as keyword parameters
        """
        self.point1 = point1
        self.point2 = point2
        self.radius = radius
        ShapeObject.__init__(self, attr)

    # accept "face" for compatibility with VRML module
    attribute_names = ShapeObject.attribute_names + ['face']

    def show(self, window):
        self.object = visual.cone(pos = tuple(self.point2),
                                  axis = tuple(self.point1-self.point2),
                                  radius = self.radius)

class PolyLines(ShapeObject):

    """
    Multiple connected lines
    """

    def __init__(self, points, **attr):
        """
        @param points: a sequence of points to be connected by lines
        @type points: sequence of L{Scientific.Geometry.Vector}
        @param attr: graphics attributes as keyword parameters
        """
        self.points = points
        ShapeObject.__init__(self, attr)

    def show(self, window):
        self.object = visual.curve(pos = map(tuple, self.points),
                                   color = window.foreground)


class Line(PolyLines):

    """
    Line
    """

    def __init__(self, point1, point2, **attr):
        """
        @param point1: first end point
        @type point1: L{Scientific.Geometry.Vector}
        @param point2: second end point
        @type point2: L{Scientific.Geometry.Vector}
        @param attr: graphics attributes as keyword parameters
        """
        apply(PolyLines.__init__, (self, [point1, point2]), attr)


class Polygons(ShapeObject):

    """
    Polygons
    """

    def __init__(self, points, index_lists, **attr):
        """
        @param points: a sequence of points
        @type points: sequence of L{Scientific.Geometry.Vector}
        @param index_lists: a sequence of index lists, one for each polygon.
                            The index list for a polygon defines which points
                            are vertices of the polygon.
        @type index_lists: sequence of C{list}
        @param attr: graphics attributes as keyword parameters
        """
        self.points = points
        self.index_lists = index_lists
        ShapeObject.__init__(self, attr)

    def show(self, window):
        for indices in self.index_lists:
            points = []
            for index in indices:
                points.append(tuple(self.points[index]))
            visual.convex(pos = points, color = window.foreground)

#
# Groups
#
class Group:

    """
    Base class for composite objects
    """

    def __init__(self, objects, **attr):
        self.objects = []
        for o in objects:
            if isGroup(o):
                self.objects = self.objects + o.objects
            else:
                self.objects.append(o)
        for key, value in attr.items():
            for o in self.objects:
                o[key] = value

    is_group = 1

    def __len__(self):
        return len(self.objects)

    def __getitem__(self, item):
        return self.object[item]

    def __coerce__(self, other):
        if not isGroup(other):
            other = Group([other])
        return (self, other)

    def __add__(self, other):
        return Group(self.objects + other.objects)

    def show(self, window):
        for o in self.objects:
            o.show(window)


def isGroup(x):
    return hasattr(x, 'is_group')


#
# Materials
#
class Material(GraphicsObject):

    """
    Material specification for graphics objects

    A material defines the color and surface properties of an object.
    """

    def __init__(self, **attr):
        """
        @param attr: material attributes as keyword arguments
        @keyword diffuse_color: the color of a diffusely reflecting surface
        @type diffuse_color: L{Color}
        @keyword emissive_color: the color of emitted light
        @type emissive_color: L{Color}
        """
        GraphicsObject.__init__(self, attr)

    attribute_names = GraphicsObject.attribute_names + \
                      ['ambient_color', 'diffuse_color', 'specular_color',
                       'emissive_color', 'shininess', 'transparency']

#
# Predefined materials
#
def DiffuseMaterial(color):
    """
    @param color: a color object or a predefined color name
    @type color: L{Color} or C{str}
    @returns: a material with the 'diffuse color' attribute set to color
    @rtype: L{Material}
    """
    if type(color) is type(''):
        color = ColorByName(color)
    try:
        return _diffuse_material_dict[color]
    except KeyError:
        m = Material(diffuse_color = color)
        _diffuse_material_dict[color] = m
        return m

_diffuse_material_dict = {}

def EmissiveMaterial(color):
    """
    @param color: a color object or a predefined color name
    @type color: L{Color} or C{str}
    @returns: a material with the 'emissive color' attribute set to color
    @rtype: L{Material}
    """
    if type(color) is type(''):
        color = ColorByName(color)
    try:
        return _emissive_material_dict[color]
    except KeyError:
        m = Material(emissive_color = color)
        _emissive_material_dict[color] = m
        return m

_emissive_material_dict = {}

#
# Test code
#
if __name__ == '__main__':

    if 0:
        from Scientific.Geometry import null, ex, ey, ez
        spheres = EmissiveMaterial('blue')
        links = EmissiveMaterial('orange')
        s1 = Sphere(null, 0.05, material = spheres)
        s2 = Sphere(ex, 0.05, material = spheres)
        s3 = Sphere(ey, 0.05, material = spheres)
        s4 = Sphere(ez, 0.05, material = spheres)
        a1 = Arrow(null, ex, 0.01, material = links)
        a2 = Arrow(null, ey, 0.01, material = links)
        a3 = Arrow(null, ez, 0.01, material = links)
        scene = Scene([s1, s2, s3, s4, a1, a2, a3])
        scene.view()

    if 0:
        scene = Scene([])
        scale = ColorScale(10.)
        for x in range(11):
            color = scale(x)
            m = Material(diffuse_color = color)
            scene.addObject(Cube(Vector(x,0.,0.), 0.2, material=m))
        scene.view()

    if 1:
        points = [Vector(0., 0., 0.),
                  Vector(0., 1., 0.),
                  Vector(1., 1., 0.),
                  Vector(1., 0., 0.),
                  Vector(1., 0., 1.),
                  Vector(1., 1., 1.)]
        indices = [[0, 1, 2, 3, 0], [3, 4, 5, 2, 3]]
        scene = Scene(Polygons(points, indices,
                               material=EmissiveMaterial('blue')))
        scene.view()

    if 0:
        points = [Vector(0., 0., 0.),
                  Vector(0., 1., 0.),
                  Vector(1., 1., 0.),
                  Vector(1., 0., 0.),
                  Vector(1., 0., 1.),
                  Vector(1., 1., 1.)]
        scene = Scene(PolyLines(points, material = EmissiveMaterial('green')))
        scene.view()
