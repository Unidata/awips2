# This module provides classes that represent graphics objects to be
# output to VMD. This module is as compatible as possible with module
# VRML. Important differences:
# - No general polygon objects.
# - Only the 'diffuse color' attribute of materials is used for rendering.
# Warning: loading cubes into VMD is very slow, as each cube is represented
# by 12 individual triangles.
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2008-8-18
#

"""
Definitions of simple 3D graphics objects and scenes containing them,
in a form that can be fed to the molecular visualization program VMD

Scenes can either be written as VMD script files, or visualized directly
by running VMD.

This module is almost compatible with the modules VRML and VRML2, which
provide visualization by VRML browsers. There is no Polygon class,
and the only material attribute supported is diffuse_color. Note
also that loading a scene with many cubes into VMD is very slow, because
each cube is represented by 12 individual triangles.

Example::

  >>> from VMD import *    
  >>> scene = Scene([])
  >>> scale = ColorScale(10.)
  >>> for x in range(11):
  >>>     color = scale(x)
  >>>     scene.addObject(Cube(Vector(x, 0., 0.), 0.2,
  >>>                          material=Material(diffuse_color = color)))
  >>> scene.view()
"""


from Scientific.IO.TextFile import TextFile
from Scientific.Geometry import Transformation, Vector, ex, ey, ez
import os, string, sys, tempfile

from Color import *

#
# VMD file
#
class SceneFile:

    def __init__(self, filename, mode = 'r', scale = 1., delete = False):
        if mode == 'r':
            raise TypeError('Not yet implemented.')
        self.file = TextFile(filename, 'w')
        self.memo = {}
        self.delete = delete
        self.scale = scale
        self.filename = filename
        self.writeString('proc python_graphics {} {\n')
        self.writeString('mol new\n')
        self.writeString('graphics 0 color 35\n')

    def __del__(self):
        self.close()

    def writeString(self, data):
        self.file.write(data)

    def writeVector(self, v):
        self.writeString(" {%g %g %g}" % tuple(v))

    def close(self):
        if self.file is not None:
            self.writeString('}\npython_graphics\n')
            self.writeString('display resetview\n')
            if self.delete:
                self.writeString('file delete ' + self.filename)
            self.file.close()
            self.file = None

    def write(self, object):
        object.writeToFile(self)

#
# Scene
#
class Scene:

    """
    VMD scene

    A VMD scene is a collection of graphics objects that can be
    loaded into VMD.
    """

    def __init__(self, objects=None, **options):
        """
        @param objects: a list of graphics objects, or C{None} for
                        an empty scene
        @type objects: C{list} or C{NoneType}
        @param options: options as keyword arguments
        @keyword scale: a scale factor applied to all coordinates of
                        geometrical objects B{except} for molecule objects,
                        which cannot be scaled
        @type scale: positive number
        """
        if objects is None:
            self.objects = []
        elif type(objects) == type([]):
            self.objects = objects
        else:
            self.objects = [objects]
        try:
            self.scale = options['scale']
        except KeyError:
            self.scale = 1.

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
        @rtype: L{VMDObject}
        """
        return self.object[item]

    def addObject(self, object):
        """
        @param object: a graphics object to be added to the scene
        @type object: L{VMDObject}
        """
        self.objects.append(object)

    def writeToFile(self, filename, delete = False):
        """
        Write the scene to a VMD script file

        @param filename: the name of the script
        @type filename: C{str}
        @param delete: flag that indicates whether the script should
                       delete itself as its last action; used for
                       temporary files
        @type delete: C{bool}
        """
        file = SceneFile(filename, 'w', self.scale, delete)
        for o in self.objects:
            o.writeToFile(file)
        file.close()

    def view(self, *args):
        """
        Start VMD and load the scene

        @param args: not used, for compatibility with VRML modules only
        """
        filename = tempfile.mktemp()
        self.writeToFile(filename, 1)
        if sys.platform == 'win32':
            #Unless VMD (or a batch file for it) is on the path
            #which is not done by their default install) we must
            #specify the path in full, which by default is
            #C:\Program Files\University of Illinois\VMD\vmd.exe
            #
            #Note that on non-English versions of Windows,
            #the name "Program Files" does change.  I believe
            #there is an API call to ask for it, but
            #there is also an Environment Variable:
            program_files = 'C:\\Program Files'
            if os.environ.has_key('PROGRAMFILES') :
                program_files = os.environ['PROGRAMFILES']
            vmd_exe = os.path.join(program_files, 'University of Illinois',
                                   'VMD','vmd.exe')

            #Check that vmd.exe does exist at this point, otherwise
            #will get a path not found error
            if os.path.exists(vmd_exe) :
                #Because the program path has spaces, it must be quoted.
                #The filename MAY have spaces, so quote that too.
                #
                #Is the pipe stuff ( 1> /dev/null 2>&1 ) doing anything
                #important? Leaving it off makes it work...
                #
                #os.system('"' + vmd_exe + '" -nt -e "' + filename + '"')
                #os.system can work, but there are two problems:
                # * it gives me grief with spaces in filenames
                #   (even if they are quoted)
                # * its a blocking function, unlike the VRML, VRML2
                #   and VPython visualisations which don't pause Python
                import win32api
                win32api.WinExec('"' + vmd_exe + '" -nt -e "' + filename + '"')
            else :
                print "Error - could not find VMD, tried:"
                print vmd_exe
        else:
            os.system('vmd -e ' + filename + ' 1> /dev/null 2>&1')

#
# Base class for everything that produces graphic objects
#
class VMDObject:

    """
    Graphics object for VMD

    This is an abstract base class. Use one of the subclasses to generate
    graphics.
    """

    def __init__(self, attr):
        """
        @param attr: graphics attributes specified by keywords
        @keyword material: color and surface properties
        @type material: L{Material}
        @keyword comment: a comment that is written to the script file
        @type comment: C{str}
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

#
# Molecules (via PDB)
#
class Molecules(VMDObject):

    """
    Molecules from a PDB file
    """
    
    def __init__(self, object, **attr):
        """
        @param object: the name of a PDB file or an MMTK object
        @type object: C{str} or MMTK.ChemicalObject
        @param attr: keyword attributes
        """
        VMDObject.__init__(self, attr)
        self.object = object

    def writeToFile(self, file):
        comment = self['comment']
        if comment is not None:
            file.writeString('# ' + comment + '\n')
        if type(self.object) == type(''):
            file.writeString('mol load pdb ' + self.object + '\n')
        else:
            tempdir = tempfile.tempdir
            tempfile.tempdir = os.path.split(file.filename)[0]
            filename = tempfile.mktemp()+'.pdb'
            tempfile.tempdir = tempdir
            self.object.writeToFile(filename)
            file.writeString('mol load pdb ' + filename + '\n')
            if file.delete:
                file.writeString('file delete ' + filename + '\n')
            
#
# Shapes
#
class ShapeObject(VMDObject):

    """
    Graphics objects representing geometrical shapes

    This is an abstract base class. Use one of the subclasses to generate
    graphics.
    """

    def __init__(self, attr):
        VMDObject.__init__(self, attr)

    attribute_names = VMDObject.attribute_names + ['material']

    def __add__(self, other):
        return Group([self]) + Group([other])

    def writeToFile(self, file):
        comment = self['comment']
        if comment is not None:
            file.writeString('# ' + comment + '\n')
        material = self['material']
        if material is not None:
            material.writeToFile(file)
        self.writeSpecification(file)

    def use(self, file):
        pass

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

    def writeSpecification(self, file):
        file.writeString('graphics 0 sphere')
        file.writeVector(self.center*file.scale)
        file.writeString(' radius ' + `self.radius*file.scale` + '\n')


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

    def writeSpecification(self, file):
        d = 0.5*self.edge
        for ext1, ext2 in [(ex, ey),
                           (ey, ez),
                           (ez, ex)]:
            norm = ext1.cross(ext2)
            for offset in [-1, 1]:
                p1 = d*(offset*norm-ext1-ext2)+self.center
                p2 = d*(offset*norm-ext1+ext2)+self.center
                p3 = d*(offset*norm+ext1-ext2)+self.center
                p4 = d*(offset*norm+ext1+ext2)+self.center
                file.writeString('graphics 0 triangle')
                file.writeVector(p1*file.scale)
                file.writeVector(p2*file.scale)
                file.writeVector(p3*file.scale)
                file.writeString('\n')
                file.writeString('graphics 0 triangle')
                file.writeVector(p2*file.scale)
                file.writeVector(p3*file.scale)
                file.writeVector(p4*file.scale)
                file.writeString('\n')

class Cylinder(ShapeObject):

    """
    Cylinder
    """

    def __init__(self, point1, point2, radius, faces = (1, 1, 1), **attr):
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

    def writeSpecification(self, file):
        file.writeString('graphics 0 cylinder')
        file.writeVector(self.point1*file.scale)
        file.writeVector(self.point2*file.scale)
        file.writeString(' radius ' + `self.radius*file.scale`)
        if self.faces[:2] == (1, 1):
            file.writeString(' filled yes')
        file.writeString('\n')


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
        self.face = face
        self.radius = radius
        self.point1 = point1
        self.point2 = point2
        ShapeObject.__init__(self, attr)

    def writeSpecification(self, file):
        file.writeString('graphics 0 cone')
        file.writeVector(self.point2*file.scale)
        file.writeVector(self.point1*file.scale)
        file.writeString(' radius ' + `self.radius*file.scale` +
                         ' resolution 12\n')


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

    def writeSpecification(self, file):
        file.writeString('graphics 0 line')
        file.writeVector(self.point1*file.scale)
        file.writeVector(self.point2*file.scale)
        file.writeString('\n')


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

    def writeToFile(self, file):
        for o in self.objects:
            o.writeToFile(file)

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
class Material(VMDObject):

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
        VMDObject.__init__(self, attr)

    attribute_names = VMDObject.attribute_names + \
                      ['ambient_color', 'diffuse_color', 'specular_color',
                       'emissive_color', 'shininess', 'transparency']

    def writeToFile(self, file):
        try:
            last = file.memo['material']
            if last == self: return
        except KeyError: pass
        try:
            color = self.attr['diffuse_color']
        except KeyError:
            color = Color((1., 1., 1.))
        file.writeString('color change rgb 35 ' + str(color) + '\n')
        file.memo['material'] = self

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
        scene.writeToFile('~/triangle.vmd')

