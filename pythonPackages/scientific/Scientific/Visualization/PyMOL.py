# This module provides classes that represent graphics objects to be
# output to PyMOL. This module is as compatible as possible with module
# VRML. Important differences:
# - No general polygon objects (yet)
# - Only the 'diffuse color' attribute of materials is used for rendering.
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2006-6-12
#

"""
Definitions of simple 3D graphics objects and scenes containing them,
in a form that can be fed to the molecular visualization program PyMOL

Scripts that use this module, directly or indirectly, must be run from
inside PyMOL, otherwise they will terminate with an error message.

This module is almost compatible with the modules VRML and VRML2, which
provide visualization by VRML browsers. There are no Polygon objects,
and the only material attribute supported is diffuse_color.

Example::

  >>> from Scientific.Visualization.PyMOL import *
  >>> scene = Scene([])
  >>> scale = ColorScale(10.)
  >>> for x in range(11):
  >>>     color = scale(x)
  >>>     scene.addObject(Cube(Vector(x, 0., 0.), 0.2,
  >>>                          material=Material(diffuse_color = color)))
  >>> scene.view()
"""

import sys
if not sys.modules.has_key('pymol') and not sys.modules.has_key('epydoc'):
    raise SystemExit("You have to run this script from inside PyMOL!")
del sys

from Scientific.IO.TextFile import TextFile
from Scientific.Geometry import Transformation, Vector
import os, string, sys, tempfile

from Color import *

if not sys.modules.has_key('epydoc'):
    from pymol import cmd, cgo

#
# Scene
#
class Scene:

    """
    PyMOL scene

    A PyMOL scene is a collection of graphics objects that can be
    loaded into PyMOL.
    """

    def __init__(self, objects=None, **options):
        """
        @param objects: a list of graphics objects, or C{None} for
                        an empty scene
        @type objects: C{list} or C{NoneType}
        @param options: options as keyword arguments. This is provided for
                        compatibility only, no options have any effect for
                        PyMOL graphics.
        """
        if objects is None:
            self.objects = []
        elif type(objects) == type([]):
            self.objects = objects
        else:
            self.objects = [objects]

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
        @rtype: L{PyMOLObject}
        """
        return self.object[item]

    def addObject(self, object):
        """
        @param object: a graphics object to be added to the scene
        @type object: L{PyMOLObject}
        """
        self.objects.append(object)

    def writeToFile(self, filename, delete = 0):
        """
        File I/O is not supported for PyMOL
        
        @raises ValueError: always
        """
        raise ValueError("no file support for PyMOL graphics")

    def view(self, name="external graphics"):
        """
        Load the scene into PyMOL

        @param name: the name of the PyMOL object corresponding to the scene
        """
        pymol_objects = []
        for o in self.objects:
            pymol_objects.extend(o.getPymolObjects())
        cmd.load_cgo(pymol_objects, name)

#
# Base class for everything that produces graphic objects
#
class PyMOLObject:

    """
    Graphics object for PyMOL

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

    def writeToFile(self, file):
        raise AttributeError('Class ' + self.__class__.__name__ +
                             ' does not implement file output.')

    def getPymolObjects(self):
        """
        @returns: a list of C{pymol.cgo} objects
        """
        raise AttributeError("to be implemented in subclasses")

#
# Molecules (via PDB)
#
class Molecules(PyMOLObject):

    """
    Molecules from a PDB file
    """
    
    def __init__(self, filename, **attr):
        """
        @param filename: the name of a PDB file
        @type filename: C{str}
        @param attr: keyword attributes
        """
        PyMOLObject.__init__(self, attr)
        self.object = filename

    def getPymolObjects(self):
        cmd.load_pdb(self.object)
        return []

#
# Shapes
#
class ShapeObject(PyMOLObject):

    """
    Graphics objects representing geometrical shapes

    This is an abstract base class. Use one of the subclasses to generate
    graphics.
    """

    attribute_names = PyMOLObject.attribute_names + ['material']

    def __add__(self, other):
        return Group([self]) + Group([other])

    def use(self, file):
        pass

    def getPymolObjects(self):
        material = self['material']
        if material is None:
            material = DiffuseMaterial('white')
        return self.cgoObjects(material)

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
        self.radius = radius
        self.center = center
        ShapeObject.__init__(self, attr)

    def cgoObjects(self, material):
        rgb = material.getRGB()
        return [cgo.COLOR] + rgb \
                + [cgo.SPHERE] + list(10.*self.center) + [10.*self.radius]


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
        self.edge = edge
        self.center = center
        ShapeObject.__init__(self, attr)

    def cgoObjects(self, material):
        raise ValueError("cubes not implemented yet")


class Cylinder(ShapeObject):

    """
    Cylinder
    """
    
    def __init__(self, point1, point2, radius, faces = (True, True, True),
                 **attr):
        """
        @param point1: first end point of the cylinder axis
        @type point1: L{Scientific.Geometry.Vector}
        @param point2: second end point of the cylinder axis
        @type point2: L{Scientific.Geometry.Vector}
        @param radius: the cylinder radius
        @type radius: positive number
        @param faces: a sequence of three boolean flags, corresponding to
                      the cylinder hull and the two circular end pieces,
                      specifying for each of these parts whether it is visible
                      or not
        @param attr: graphics attributes as keyword parameters
        """
        self.faces = faces
        self.radius = radius
        self.point1 = point1
        self.point2 = point2
        ShapeObject.__init__(self, attr)

    def cgoObjects(self, material):
        rgb = material.getRGB()
        return [cgo.CYLINDER] \
               + list(10.*self.point1) \
               + list(10.*self.point2) \
               + [10.*self.radius] \
               + 2*rgb


class Cone(ShapeObject):

    """
    Cone
    """

    def __init__(self, point1, point2, radius, face = True, **attr):
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
        self.face = face
        self.radius = radius
        self.point1 = point1
        self.point2 = point2
        ShapeObject.__init__(self, attr)

    def cgoObjects(self, material):
        raise ValueError("cones not implemented yet")


class Line(ShapeObject):

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
        self.point1 = point1
        self.point2 = point2
        ShapeObject.__init__(self, attr)

    def cgoObjects(self, material):
        rgb = material.getRGB()
        return [cgo.COLOR] + rgb \
                + [cgo.BEGIN, cgo.LINES,
                   cgo.VERTEX, self.point1[0], self.point1[1], self.point1[2],
                   cgo.VERTEX, self.point2[0], self.point2[1], self.point2[2],
                   cgo.END]

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

    def getPymolObjects(self):
        objects = []
        for o in self.objects:
            objects.extend(o.getPymolObjects())
        return objects

def isGroup(x):
    return hasattr(x, 'is_group')

#
# Composite Objects
#
class Arrow(Group):

    """
    Arrow

    An arrow consists of a cylinder and a cone.
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
        axis = point2-point1
        height = axis.length()
        axis = axis/height
        cone_height = min(height, 4.*radius)
        cylinder_height = height - cone_height
        junction = point2-axis*cone_height
        cone = apply(Cone, (point2, junction, 0.75*cone_height), attr)
        objects = [cone]
        if cylinder_height > 0.005*radius:
            cylinder = apply(Cylinder, (point1, junction, radius), attr)
            objects.append(cylinder)
        Group.__init__(self, objects)

#
# Materials
#
class Material(PyMOLObject):

    """
    Material specification for graphics objects

    A material defines the color and surface properties of an object.

    For compatibility with the module L{Scientific.Visualization.VRML},
    many material attributes are accepted but not used in any way.
    """

    def __init__(self, **attr):
        """
        @param attr: material attributes as keyword arguments
        @keyword diffuse_color: the color of a diffusely reflecting surface
        @type diffuse_color: L{Color}
        @keyword emissive_color: not used
        @keyword ambient_color: not used
        @keyword specular_color: not used
        @keyword shininess: not used
        @keyword transparency: not used
        """
        PyMOLObject.__init__(self, attr)

    attribute_names = PyMOLObject.attribute_names + \
                      ['ambient_color', 'diffuse_color', 'specular_color',
                       'emissive_color', 'shininess', 'transparency']

    def getRGB(self): 
        try:
            color = self.attr['diffuse_color']
        except KeyError:
            color = Color((1., 1., 1.))
        return [color.rgb[0], color.rgb[1], color.rgb[2]]

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

EmissiveMaterial = DiffuseMaterial

#
# Test code
#
if __name__ == '__main__':

    if 0:
        from Scientific.Geometry import null, ex, ey, ez
        spheres = DiffuseMaterial('green')
        links = DiffuseMaterial('red')
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
        scale = SymmetricColorScale(10., 10)
        for x in range(-10, 11):
            color = scale(x)
            m = Material(diffuse_color = color)
            scene.addObject(Cube(Vector(x,0.,0.), 0.2, material=m))
        scene.view()

    if 1:
        scene = Scene([])
        scale = ColorScale(10.)
        for x in range(11):
            color = scale(x)
            m = Material(diffuse_color = color)
            scene.addObject(Cube(Vector(x,0.,0.), 0.2, material=m))
        scene.view()

