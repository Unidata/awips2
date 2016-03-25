# Parallel IO
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2007-5-25
#

"""
Parallel acces to netCDF files

One netCDF dimension is defined for splitting the data among
processors such that each processor is responsible for one slice of
the file along that dimension.

Since netCDF files can be very big, the distribution algorithm gives
priority to memory efficiency over CPU time efficiency. The processor
that handles the file treats only one slice per superstep, which means
that at no time more than one slice must be stored in any processor.
"""

from Scientific.IO.NetCDF import NetCDFFile
from Scientific.BSP.core import ParClass, ParBase, ParInvalid, is_invalid
from Scientific import N

class _ParNetCDFFile(ParBase):

    """
    Distributed netCDF file

    A ParNetCDFFile object acts as much as possible like a NetCDFFile object.
    Variables become ParNetCDFVariable objects, which behave like
    distributed sequences. Variables that use the dimension named by
    |split_dimension| are automatically distributed among the processors
    such that each treats only one slice of the whole file.
    """

    def __parinit__(self, pid, nprocs, filename, split_dimension,
                    mode = 'r', local_access = False):
        """
        @param filename: the name of the netCDF file
        @type filename: C{str}
        @param split_dimension: the name of the dimension along which the data
                                is distributed over the processors
        @type split_dimension: C{str}
        @param mode: read ('r'), write ('w'), or append ('a')
        @type mode: C{str}
        @param local_access: if C{False}, processor 0 is the only one to
                             access the file, all others communicate with
                             processor 0. If C{True} (only for reading), each
                             processor accesses the file directly. In the
                             latter case, the file must be accessible on all
                             processors under the same name. A third mode is
                             'auto', which uses some heuristics to decide
                             if the file is accessible everywhere: it checks
                             for existence of the file, then compares
                             the size on all processors, and finally verifies
                             that the same variables exist everywhere, with
                             identical names, types, and sizes.
        @type local_access: C{bool} or C{str}
        """
        if mode != 'r':
            local_access = 0
        self.pid = pid
        self.nprocs = nprocs
        self.filename = filename
        self.split = split_dimension
        self.local_access = local_access
        self.read_only = mode == 'r'
        if local_access or pid == 0:
            self.file = NetCDFFile(filename, mode)
            try:
                length = self.file.dimensions[split_dimension]
                if length is None:
                    length = -1
            except KeyError:
                length = None
            variables = {}
            for name, var in self.file.variables.items():
                variables[name] = (name, var.dimensions)
                if length < 0 and split_dimension in var.dimensions:
                    index = list(var.dimensions).index(split_dimension)
                    length = var.shape[index]
        else:
            self.file = None
            self.split = split_dimension
            length = None
            variables = None
        if not local_access:
            length = self.broadcast(length)
            variables = self.broadcast(variables)
        if length is not None:
            self._divideData(length)
        self.variables = {}
        for name, var in variables.items():
            self.variables[name] = _ParNetCDFVariable(self, var[0], var[1],
                                                      split_dimension)

    def __repr__(self):
        return repr(self.filename)

    def close(self):
        if self.local_access or self.pid == 0:
            self.file.close()

    def createDimension(self, name, length):
        if name == self.split:
            if length is None:
                raise ValueError("Split dimension cannot be unlimited")
            self._divideData(length)
        if self.pid == 0:
            self.file.createDimension(name, length)

    def createVariable(self, name, typecode, dimensions):
        if self.pid == 0:
            var = self.file.createVariable(name, typecode, dimensions)
            dim = var.dimensions
        else:
            dim = 0
        name, dim = self.broadcast((name, dim))
        self.variables[name] = _ParNetCDFVariable(self, name, dim, self.split)
        return self.variables[name]

    def _divideData(self, length):
        chunk = (length+self.nprocs-1)/self.nprocs
        self.first = min(self.pid*chunk, length)
        self.last = min(self.first+chunk, length)
        if (not self.local_access) and self.pid == 0:
            self.parts = []
            for pid in range(self.nprocs):
                first = pid*chunk
                last = min(first+chunk, length)
                self.parts.append((first, last))

    def sync(self):
        if self.pid == 0:
            self.file.sync()
    flush = sync

class _ParNetCDFVariable(ParBase):

    def __init__(self, file, name, dimensions, split_dimension):
        self.file = file
        self.pid = file.pid
        self.nprocs = file.nprocs
        self.name = name
        self.dimensions = dimensions
        self.value = self
        self.attributes = {}
        try:
            self.index = list(dimensions).index(split_dimension)
        except ValueError:
            self.index = None

    def __repr__(self):
        return repr(self.name)

    def __getitem__(self, item):
        item = self._prepareIndices(item)
        if self.file.local_access :
            data = self._readData(item, self.file.first, self.file.last)
        elif self.pid == 0:
            for pid in range(1, self.nprocs):
                first, last = self.file.parts[pid]
                data = self._readData(item, first, last)
                self.put(data, [pid])
            data = self._readData(item, self.file.first, self.file.last)
        else:
            for pid in range(1, self.nprocs):
                messages = self.put(None, [])
                if messages:
                    data = messages[0]
        if data is None:
            return ParInvalid
        else:
            return data

    def __getslice__(self, first, last):
        return self.__getitem__(slice(first, last))

    def __setitem__(self, item, value):
        item = self._prepareIndices(item)
        if is_invalid(value):
            value = None
        if self.pid == 0:
            if value is not None:
                self._writeData(item, value, self.file.first, self.file.last)
            if self.index is not None:
                for pid in range(1, self.nprocs):
                    first, last = self.file.parts[pid]
                    data = self.put(None, [])
                    if data and data[0] is not None:
                        self._writeData(item, data[0], first, last)
        else:
            if self.index is not None:
                for pid in range(1, self.nprocs):
                    if pid == self.pid:
                        self.put(value, [0])
                    else:
                        self.put(None, [])

    def __setslice__(self, first, last, value):
        self.__setitem__(slice(first, last), value)

    def _prepareIndices(self, item):
        if not hasattr(item, 'is_parindex'):
            if type(item) != type(()):
                item = (item,)
            item = item + (len(self.dimensions)-len(item))*(slice(None),)
        return item

    def _readData(self, item, part_first, part_last):
        item = self._indices(item, part_first, part_last)
        if item is None:
            return None
        else:
            return self.file.file.variables[self.name][item]

    def _writeData(self, item, data, part_first, part_last):
        try:
            if len(data) == 0:
                return
        except TypeError:
            pass
        item = self._indices(item, part_first, part_last)
        if item is not None:
            try:
                self.file.file.variables[self.name][item] = N.array(data)
            except:
                print self.file.file.variables[self.name].shape
                print item
                print N.array(data).shape
                raise

    def _indices(self, item, part_first, part_last):
        if hasattr(item, 'is_parindex'):
            if not item.valid:
                return None
            if item.skip == 0:
                return item.start+part_first
            else:
                return slice(item.start+part_first, item.stop+part_first,
                             item.skip)
        if self.index is not None:
            split = item[self.index]
            if isinstance(split, int):
                raise ValueError("Must use slice along split dimension")
            first, last, skip = split.start, split.stop, split.step
            if first is None: first = 0
            if skip is None: skip = 1
            n1 = max(0, (part_first-first+skip-1)/skip)
            first = first + n1*skip
            if last is None:
                last = part_last
            last = min(last, part_last)
            item = item[:self.index] + (slice(first, last, skip),) + \
                   item[self.index+1:]
        return item

    def __getattr__(self, attr):
        if self.file.local_access:
            return getattr(self.file.file.variables[self.name], attr)
        try:
            return self.attributes[attr]
        except KeyError:
            pass
        if self.pid == 0:
            value = getattr(self.file.file.variables[self.name], attr)
        else:
            value = None
        value = self.broadcast(value)
        if self.file.read_only:
            self.attributes[attr] = value
        return value

    def __len__(self):
        return self.file.last - self.file.first

ParNetCDFVariable = ParClass(_ParNetCDFVariable)
ParNetCDFFile = ParClass(_ParNetCDFFile)
