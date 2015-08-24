"""
  Matrix Market I/O in Python.
"""
#
# Author: Pearu Peterson <pearu@cens.ioc.ee>
# Created: October, 2004
#
# References:
#  http://math.nist.gov/MatrixMarket/
#

<<<<<<< HEAD
import os
from numpy import asarray, real, imag, conj, zeros, ndarray, concatenate, \
                  ones, ascontiguousarray, vstack, savetxt, fromfile, fromstring
=======
from __future__ import division, print_function, absolute_import

import os
import sys
from numpy import asarray, real, imag, conj, zeros, ndarray, concatenate, \
                  ones, ascontiguousarray, vstack, savetxt, fromfile, fromstring
from numpy.compat import asbytes, asstr
from scipy._lib.six import string_types
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

__all__ = ['mminfo','mmread','mmwrite', 'MMFile']


#-------------------------------------------------------------------------------
def mminfo(source):
    """
    Queries the contents of the Matrix Market file 'filename' to
    extract size and storage information.

    Parameters
    ----------

    source : file
        Matrix Market filename (extension .mtx) or open file object

    Returns
    -------

    rows,cols : int
       Number of matrix rows and columns
    entries : int
        Number of non-zero entries of a sparse matrix
        or rows*cols for a dense matrix
<<<<<<< HEAD

    format : {'coordinate', 'array'}

    field : {'real', 'complex', 'pattern', 'integer'}

    symm : {'general', 'symmetric', 'skew-symmetric', 'hermitian'}
=======
    format : str
        Either 'coordinate' or 'array'.
    field : str
        Either 'real', 'complex', 'pattern', or 'integer'.
    symm : str
        Either 'general', 'symmetric', 'skew-symmetric', or 'hermitian'.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """
    return MMFile.info(source)

#-------------------------------------------------------------------------------
<<<<<<< HEAD
=======


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def mmread(source):
    """
    Reads the contents of a Matrix Market file 'filename' into a matrix.

    Parameters
    ----------

    source : file
        Matrix Market filename (extensions .mtx, .mtz.gz)
        or open file object.

    Returns
    -------
    a:
        Sparse or full matrix

    """
    return MMFile().read(source)

#-------------------------------------------------------------------------------
<<<<<<< HEAD
def mmwrite(target, a, comment='', field=None, precision=None):
    """
    Writes the sparse or dense matrix A to a Matrix Market formatted file.

    Parameters
    ----------

    target : file
        Matrix Market filename (extension .mtx) or open file object
    a : array like
        Sparse or full matrix
    comment : str
        comments to be prepended to the Matrix Market file

    field : {'real', 'complex', 'pattern', 'integer'}, optional

    precision :
        Number of digits to display for real or complex values.

=======


def mmwrite(target, a, comment='', field=None, precision=None):
    """
    Writes the sparse or dense array `a` to a Matrix Market formatted file.

    Parameters
    ----------
    target : file
        Matrix Market filename (extension .mtx) or open file object
    a : array like
        Sparse or dense 2D array
    comment : str, optional
        comments to be prepended to the Matrix Market file
    field : None or str, optional
        Either 'real', 'complex', 'pattern', or 'integer'.
    precision : None or int, optional
        Number of digits to display for real or complex values.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    MMFile().write(target, a, comment, field, precision)


################################################################################
class MMFile (object):
    __slots__ = (
      '_rows',
      '_cols',
      '_entries',
      '_format',
      '_field',
      '_symmetry')

    @property
<<<<<<< HEAD
    def rows(self): return self._rows
    @property
    def cols(self): return self._cols
    @property
    def entries(self): return self._entries
    @property
    def format(self): return self._format
    @property
    def field(self): return self._field
    @property
    def symmetry(self): return self._symmetry
=======
    def rows(self):
        return self._rows

    @property
    def cols(self):
        return self._cols

    @property
    def entries(self):
        return self._entries

    @property
    def format(self):
        return self._format

    @property
    def field(self):
        return self._field

    @property
    def symmetry(self):
        return self._symmetry
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @property
    def has_symmetry(self):
        return self._symmetry in (self.SYMMETRY_SYMMETRIC,
          self.SYMMETRY_SKEW_SYMMETRIC, self.SYMMETRY_HERMITIAN)

    # format values
    FORMAT_COORDINATE = 'coordinate'
    FORMAT_ARRAY = 'array'
    FORMAT_VALUES = (FORMAT_COORDINATE, FORMAT_ARRAY)

    @classmethod
    def _validate_format(self, format):
        if format not in self.FORMAT_VALUES:
<<<<<<< HEAD
            raise ValueError,'unknown format type %s, must be one of %s'% \
              (`format`, `self.FORMAT_VALUES`)

    # field values
    FIELD_INTEGER = 'integer'
    FIELD_REAL    = 'real'
=======
            raise ValueError('unknown format type %s, must be one of %s' %
                                (format, self.FORMAT_VALUES))

    # field values
    FIELD_INTEGER = 'integer'
    FIELD_REAL = 'real'
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    FIELD_COMPLEX = 'complex'
    FIELD_PATTERN = 'pattern'
    FIELD_VALUES = (FIELD_INTEGER, FIELD_REAL, FIELD_COMPLEX, FIELD_PATTERN)

    @classmethod
    def _validate_field(self, field):
        if field not in self.FIELD_VALUES:
<<<<<<< HEAD
            raise ValueError,'unknown field type %s, must be one of %s'% \
              (`field`, `self.FIELD_VALUES`)

    # symmetry values
    SYMMETRY_GENERAL        = 'general'
    SYMMETRY_SYMMETRIC      = 'symmetric'
    SYMMETRY_SKEW_SYMMETRIC = 'skew-symmetric'
    SYMMETRY_HERMITIAN      = 'hermitian'
    SYMMETRY_VALUES = ( SYMMETRY_GENERAL,        SYMMETRY_SYMMETRIC,
=======
            raise ValueError('unknown field type %s, must be one of %s' %
                                (field, self.FIELD_VALUES))

    # symmetry values
    SYMMETRY_GENERAL = 'general'
    SYMMETRY_SYMMETRIC = 'symmetric'
    SYMMETRY_SKEW_SYMMETRIC = 'skew-symmetric'
    SYMMETRY_HERMITIAN = 'hermitian'
    SYMMETRY_VALUES = (SYMMETRY_GENERAL, SYMMETRY_SYMMETRIC,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                        SYMMETRY_SKEW_SYMMETRIC, SYMMETRY_HERMITIAN)

    @classmethod
    def _validate_symmetry(self, symmetry):
        if symmetry not in self.SYMMETRY_VALUES:
<<<<<<< HEAD
            raise ValueError,'unknown symmetry type %s, must be one of %s'% \
              (`symmetry`, `self.SYMMETRY_VALUES`)

    DTYPES_BY_FIELD = {
      FIELD_INTEGER: 'i',
      FIELD_REAL:    'd',
=======
            raise ValueError('unknown symmetry type %s, must be one of %s' %
                                (symmetry, self.SYMMETRY_VALUES))

    DTYPES_BY_FIELD = {
      FIELD_INTEGER: 'i',
      FIELD_REAL: 'd',
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      FIELD_COMPLEX: 'D',
      FIELD_PATTERN: 'd'}

    #---------------------------------------------------------------------------
    @staticmethod
<<<<<<< HEAD
    def reader(): pass

    #---------------------------------------------------------------------------
    @staticmethod
    def writer(): pass
=======
    def reader():
        pass

    #---------------------------------------------------------------------------
    @staticmethod
    def writer():
        pass
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    #---------------------------------------------------------------------------
    @classmethod
    def info(self, source):
        source, close_it = self._open(source)

        try:

            # read and validate header line
            line = source.readline()
<<<<<<< HEAD
            mmid, matrix, format, field, symmetry  = \
              [part.strip().lower() for part in line.split()]
            if not mmid.startswith('%%matrixmarket'):
                raise ValueError,'source is not in Matrix Market format'

            assert matrix == 'matrix',`line`

            # ??? Is this necessary?  I don't see 'dense' or 'sparse' in the spec
            # http://math.nist.gov/MatrixMarket/formats.html
            if format == 'dense': format = self.FORMAT_ARRAY
            elif format == 'sparse': format = self.FORMAT_COORDINATE

            # skip comments
            while line.startswith('%'): line = source.readline()

            line = line.split()
            if format == self.FORMAT_ARRAY:
                assert len(line)==2,`line`
                rows,cols = map(float, line)
                entries = rows*cols
            else:
                assert len(line)==3,`line`
                rows, cols, entries = map(float, line)

            return (rows, cols, entries, format, field, symmetry)

        finally:
            if close_it: source.close()

    #---------------------------------------------------------------------------
    @staticmethod
    def _open(filespec, mode='r'):
=======
            mmid, matrix, format, field, symmetry = \
              [asstr(part.strip()) for part in line.split()]
            if not mmid.startswith('%%MatrixMarket'):
                raise ValueError('source is not in Matrix Market format')
            if not matrix.lower() == 'matrix':
                raise ValueError("Problem reading file header: " + line)

            # http://math.nist.gov/MatrixMarket/formats.html
            if format.lower() == 'array':
                format = self.FORMAT_ARRAY
            elif format.lower() == 'coordinate':
                format = self.FORMAT_COORDINATE

            # skip comments
            while line.startswith(b'%'):
                line = source.readline()

            line = line.split()
            if format == self.FORMAT_ARRAY:
                if not len(line) == 2:
                    raise ValueError("Header line not of length 2: " + line)
                rows, cols = map(int, line)
                entries = rows * cols
            else:
                if not len(line) == 3:
                    raise ValueError("Header line not of length 3: " + line)
                rows, cols, entries = map(int, line)

            return (rows, cols, entries, format, field.lower(), symmetry.lower())

        finally:
            if close_it:
                source.close()

    #---------------------------------------------------------------------------
    @staticmethod
    def _open(filespec, mode='rb'):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        """
        Return an open file stream for reading based on source.  If source is
        a file name, open it (after trying to find it with mtx and gzipped mtx
        extensions).  Otherwise, just return source.
        """
        close_it = False
<<<<<<< HEAD
        if type(filespec) is type(''):
=======
        if isinstance(filespec, string_types):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            close_it = True

            # open for reading
            if mode[0] == 'r':

                # determine filename plus extension
                if not os.path.isfile(filespec):
                    if os.path.isfile(filespec+'.mtx'):
                        filespec = filespec + '.mtx'
                    elif os.path.isfile(filespec+'.mtx.gz'):
                        filespec = filespec + '.mtx.gz'
                    elif os.path.isfile(filespec+'.mtx.bz2'):
                        filespec = filespec + '.mtx.bz2'
                # open filename
                if filespec.endswith('.gz'):
                    import gzip
                    stream = gzip.open(filespec, mode)
                elif filespec.endswith('.bz2'):
                    import bz2
<<<<<<< HEAD
                    stream = bz2.BZ2File(filespec, 'r')
=======
                    stream = bz2.BZ2File(filespec, 'rb')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                else:
                    stream = open(filespec, mode)

            # open for writing
            else:
                if filespec[-4:] != '.mtx':
                    filespec = filespec + '.mtx'
                stream = open(filespec, mode)
        else:
            stream = filespec

        return stream, close_it

    #---------------------------------------------------------------------------
    @staticmethod
    def _get_symmetry(a):
        m,n = a.shape
<<<<<<< HEAD
        if m!=n:
=======
        if m != n:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            return MMFile.SYMMETRY_GENERAL
        issymm = 1
        isskew = 1
        isherm = a.dtype.char in 'FD'
        for j in range(n):
            for i in range(j+1,n):
                aij,aji = a[i][j],a[j][i]
                if issymm and aij != aji:
                    issymm = 0
                if isskew and aij != -aji:
                    isskew = 0
                if isherm and aij != conj(aji):
                    isherm = 0
                if not (issymm or isskew or isherm):
                    break
<<<<<<< HEAD
        if issymm: return MMFile.SYMMETRY_SYMMETRIC
        if isskew: return MMFile.SYMMETRY_SKEW_SYMMETRIC
        if isherm: return MMFile.SYMMETRY_HERMITIAN
=======
        if issymm:
            return MMFile.SYMMETRY_SYMMETRIC
        if isskew:
            return MMFile.SYMMETRY_SKEW_SYMMETRIC
        if isherm:
            return MMFile.SYMMETRY_HERMITIAN
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        return MMFile.SYMMETRY_GENERAL

    #---------------------------------------------------------------------------
    @staticmethod
    def _field_template(field, precision):
        return {
          MMFile.FIELD_REAL: '%%.%ie\n' % precision,
          MMFile.FIELD_INTEGER: '%i\n',
          MMFile.FIELD_COMPLEX: '%%.%ie %%.%ie\n' % (precision,precision)
        }.get(field, None)

    #---------------------------------------------------------------------------
<<<<<<< HEAD
    def __init__(self, **kwargs): self._init_attrs(**kwargs)
=======
    def __init__(self, **kwargs):
        self._init_attrs(**kwargs)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    #---------------------------------------------------------------------------
    def read(self, source):
        stream, close_it = self._open(source)

        try:
            self._parse_header(stream)
            return self._parse_body(stream)

        finally:
<<<<<<< HEAD
            if close_it: stream.close()

    #---------------------------------------------------------------------------
    def write(self, target, a, comment='', field=None, precision=None):
        stream, close_it = self._open(target, 'w')
=======
            if close_it:
                stream.close()

    #---------------------------------------------------------------------------
    def write(self, target, a, comment='', field=None, precision=None):
        stream, close_it = self._open(target, 'wb')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        try:
            self._write(stream, a, comment, field, precision)

        finally:
<<<<<<< HEAD
            if close_it: stream.close()
            else: stream.flush()
=======
            if close_it:
                stream.close()
            else:
                stream.flush()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    #---------------------------------------------------------------------------
    def _init_attrs(self, **kwargs):
        """
        Initialize each attributes with the corresponding keyword arg value
        or a default of None
        """
        attrs = self.__class__.__slots__
        public_attrs = [attr[1:] for attr in attrs]
        invalid_keys = set(kwargs.keys()) - set(public_attrs)

        if invalid_keys:
<<<<<<< HEAD
            raise ValueError, \
              'found %s invalid keyword arguments, please only use %s' % \
              (`tuple(invalid_keys)`, `public_attrs`)

        for attr in attrs: setattr(self, attr, kwargs.get(attr[1:], None))
=======
            raise ValueError('found %s invalid keyword arguments, please only use %s' %
                                (tuple(invalid_keys), public_attrs))

        for attr in attrs:
            setattr(self, attr, kwargs.get(attr[1:], None))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    #---------------------------------------------------------------------------
    def _parse_header(self, stream):
        rows, cols, entries, format, field, symmetry = \
          self.__class__.info(stream)
        self._init_attrs(rows=rows, cols=cols, entries=entries, format=format,
          field=field, symmetry=symmetry)

    #---------------------------------------------------------------------------
    def _parse_body(self, stream):
<<<<<<< HEAD
        rows, cols, entries, format, field, symm = \
          (self.rows, self.cols, self.entries, self.format, self.field, self.symmetry)
=======
        rows, cols, entries, format, field, symm = (self.rows, self.cols,
            self.entries, self.format, self.field, self.symmetry)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        try:
            from scipy.sparse import coo_matrix
        except ImportError:
            coo_matrix = None

        dtype = self.DTYPES_BY_FIELD.get(field, None)

        has_symmetry = self.has_symmetry
        is_complex = field == self.FIELD_COMPLEX
        is_skew = symm == self.SYMMETRY_SKEW_SYMMETRIC
        is_herm = symm == self.SYMMETRY_HERMITIAN
        is_pattern = field == self.FIELD_PATTERN

        if format == self.FORMAT_ARRAY:
<<<<<<< HEAD
            a = zeros((rows,cols),dtype=dtype)
=======
            a = zeros((rows,cols), dtype=dtype)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            line = 1
            i,j = 0,0
            while line:
                line = stream.readline()
<<<<<<< HEAD
                if not line or line.startswith('%'):
=======
                if not line or line.startswith(b'%'):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                    continue
                if is_complex:
                    aij = complex(*map(float,line.split()))
                else:
                    aij = float(line)
                a[i,j] = aij
<<<<<<< HEAD
                if has_symmetry and i!=j:
=======
                if has_symmetry and i != j:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                    if is_skew:
                        a[j,i] = -aij
                    elif is_herm:
                        a[j,i] = conj(aij)
                    else:
                        a[j,i] = aij
<<<<<<< HEAD
                if i<rows-1:
=======
                if i < rows-1:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                    i = i + 1
                else:
                    j = j + 1
                    if not has_symmetry:
                        i = 0
                    else:
                        i = j
<<<<<<< HEAD
            assert i in [0,j] and j==cols,`i,j,rows,cols`
=======
            if not (i in [0,j] and j == cols):
                raise ValueError("Parse error, did not read all lines.")
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        elif format == self.FORMAT_COORDINATE and coo_matrix is None:
            # Read sparse matrix to dense when coo_matrix is not available.
            a = zeros((rows,cols), dtype=dtype)
            line = 1
            k = 0
            while line:
                line = stream.readline()
<<<<<<< HEAD
                if not line or line.startswith('%'):
=======
                if not line or line.startswith(b'%'):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                    continue
                l = line.split()
                i,j = map(int,l[:2])
                i,j = i-1,j-1
                if is_complex:
                    aij = complex(*map(float,l[2:]))
                else:
                    aij = float(l[2])
                a[i,j] = aij
<<<<<<< HEAD
                if has_symmetry and i!=j:
=======
                if has_symmetry and i != j:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                    if is_skew:
                        a[j,i] = -aij
                    elif is_herm:
                        a[j,i] = conj(aij)
                    else:
                        a[j,i] = aij
                k = k + 1
<<<<<<< HEAD
            assert k==entries,`k,entries`

        elif format == self.FORMAT_COORDINATE:
            # Read sparse COOrdinate format
            
=======
            if not k == entries:
                ValueError("Did not read all entries")

        elif format == self.FORMAT_COORDINATE:
            # Read sparse COOrdinate format

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            if entries == 0:
                # empty matrix
                return coo_matrix((rows, cols), dtype=dtype)

            try:
<<<<<<< HEAD
                # fromfile works for normal files
                flat_data = fromfile(stream, sep=' ')
            except:
=======
                if not _is_fromfile_compatible(stream):
                    flat_data = fromstring(stream.read(), sep=' ')
                else:
                    # fromfile works for normal files
                    flat_data = fromfile(stream, sep=' ')
            except Exception:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                # fallback - fromfile fails for some file-like objects
                flat_data = fromstring(stream.read(), sep=' ')

                # TODO use iterator (e.g. xreadlines) to avoid reading
                # the whole file into memory

            if is_pattern:
                flat_data = flat_data.reshape(-1,2)
                I = ascontiguousarray(flat_data[:,0], dtype='intc')
                J = ascontiguousarray(flat_data[:,1], dtype='intc')
                V = ones(len(I), dtype='int8')  # filler
            elif is_complex:
                flat_data = flat_data.reshape(-1,4)
                I = ascontiguousarray(flat_data[:,0], dtype='intc')
                J = ascontiguousarray(flat_data[:,1], dtype='intc')
                V = ascontiguousarray(flat_data[:,2], dtype='complex')
                V.imag = flat_data[:,3]
            else:
                flat_data = flat_data.reshape(-1,3)
                I = ascontiguousarray(flat_data[:,0], dtype='intc')
                J = ascontiguousarray(flat_data[:,1], dtype='intc')
                V = ascontiguousarray(flat_data[:,2], dtype='float')

<<<<<<< HEAD
            I -= 1 #adjust indices (base 1 -> base 0)
            J -= 1

            if has_symmetry:
                mask = (I != J)       #off diagonal mask
=======
            I -= 1  # adjust indices (base 1 -> base 0)
            J -= 1

            if has_symmetry:
                mask = (I != J)       # off diagonal mask
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                od_I = I[mask]
                od_J = J[mask]
                od_V = V[mask]

                I = concatenate((I,od_J))
                J = concatenate((J,od_I))

                if is_skew:
                    od_V *= -1
                elif is_herm:
                    od_V = od_V.conjugate()

                V = concatenate((V,od_V))

            a = coo_matrix((V, (I, J)), shape=(rows, cols), dtype=dtype)
        else:
<<<<<<< HEAD
            raise NotImplementedError,`format`
=======
            raise NotImplementedError(format)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        return a

    #---------------------------------------------------------------------------
    def _write(self, stream, a, comment='', field=None, precision=None):

        if isinstance(a, list) or isinstance(a, ndarray) or isinstance(a, tuple) or hasattr(a,'__array__'):
            rep = self.FORMAT_ARRAY
            a = asarray(a)
            if len(a.shape) != 2:
<<<<<<< HEAD
                raise ValueError, 'expected matrix'
            rows,cols = a.shape
            entries = rows*cols
=======
                raise ValueError('Expected 2 dimensional array')
            rows,cols = a.shape
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            if field is not None:

                if field == self.FIELD_INTEGER:
                    a = a.astype('i')
                elif field == self.FIELD_REAL:
                    if a.dtype.char not in 'fd':
                        a = a.astype('d')
                elif field == self.FIELD_COMPLEX:
                    if a.dtype.char not in 'FD':
                        a = a.astype('D')

        else:
            from scipy.sparse import spmatrix
            if not isinstance(a,spmatrix):
<<<<<<< HEAD
                raise ValueError,'unknown matrix type ' + `type(a)`
            rep = 'coordinate'
            rows, cols = a.shape
            entries = a.getnnz()
=======
                raise ValueError('unknown matrix type: %s' % type(a))
            rep = 'coordinate'
            rows, cols = a.shape
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        typecode = a.dtype.char

        if precision is None:
            if typecode in 'fF':
                precision = 8
            else:
                precision = 16

        if field is None:
            kind = a.dtype.kind
            if kind == 'i':
                field = 'integer'
            elif kind == 'f':
                field = 'real'
            elif kind == 'c':
                field = 'complex'
            else:
                raise TypeError('unexpected dtype kind ' + kind)

        if rep == self.FORMAT_ARRAY:
            symm = self._get_symmetry(a)
        else:
            symm = self.SYMMETRY_GENERAL

        # validate rep, field, and symmetry
        self.__class__._validate_format(rep)
        self.__class__._validate_field(field)
        self.__class__._validate_symmetry(symm)

        # write initial header line
<<<<<<< HEAD
        stream.write('%%%%MatrixMarket matrix %s %s %s\n' % (rep,field,symm))

        # write comments
        for line in comment.split('\n'):
            stream.write('%%%s\n' % (line))

=======
        stream.write(asbytes('%%%%MatrixMarket matrix %s %s %s\n' % (rep,field,symm)))

        # write comments
        for line in comment.split('\n'):
            stream.write(asbytes('%%%s\n' % (line)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        template = self._field_template(field, precision)

        # write dense format
        if rep == self.FORMAT_ARRAY:

            # write shape spec
<<<<<<< HEAD
            stream.write('%i %i\n' % (rows,cols))
=======
            stream.write(asbytes('%i %i\n' % (rows,cols)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            if field in (self.FIELD_INTEGER, self.FIELD_REAL):

                if symm == self.SYMMETRY_GENERAL:
                    for j in range(cols):
                        for i in range(rows):
<<<<<<< HEAD
                            stream.write(template % a[i,j])
                else:
                    for j in range(cols):
                        for i in range(j,rows):
                            stream.write(template % a[i,j])
=======
                            stream.write(asbytes(template % a[i,j]))
                else:
                    for j in range(cols):
                        for i in range(j,rows):
                            stream.write(asbytes(template % a[i,j]))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            elif field == self.FIELD_COMPLEX:

                if symm == self.SYMMETRY_GENERAL:
                    for j in range(cols):
                        for i in range(rows):
                            aij = a[i,j]
<<<<<<< HEAD
                            stream.write(template % (real(aij),imag(aij)))
=======
                            stream.write(asbytes(template % (real(aij),imag(aij))))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                else:
                    for j in range(cols):
                        for i in range(j,rows):
                            aij = a[i,j]
<<<<<<< HEAD
                            stream.write(template % (real(aij),imag(aij)))

            elif field == self.FIELD_PATTERN:
                raise ValueError,'pattern type inconsisted with dense format'

            else:
                raise TypeError,'Unknown field type %s'% `field`
=======
                            stream.write(asbytes(template % (real(aij),imag(aij))))

            elif field == self.FIELD_PATTERN:
                raise ValueError('pattern type inconsisted with dense format')

            else:
                raise TypeError('Unknown field type %s' % field)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # write sparse format
        else:

            if symm != self.SYMMETRY_GENERAL:
                raise NotImplementedError('symmetric matrices not yet supported')

<<<<<<< HEAD
            coo = a.tocoo() # convert to COOrdinate format

            # write shape spec
            stream.write('%i %i %i\n' % (rows, cols, coo.nnz))
=======
            coo = a.tocoo()  # convert to COOrdinate format

            # write shape spec
            stream.write(asbytes('%i %i %i\n' % (rows, cols, coo.nnz)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            fmt = '%%.%dg' % precision

            if field == self.FIELD_PATTERN:
                IJV = vstack((coo.row, coo.col)).T
<<<<<<< HEAD
            elif field in [ self.FIELD_INTEGER, self.FIELD_REAL ]:
=======
            elif field in [self.FIELD_INTEGER, self.FIELD_REAL]:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                IJV = vstack((coo.row, coo.col, coo.data)).T
            elif field == self.FIELD_COMPLEX:
                IJV = vstack((coo.row, coo.col, coo.data.real, coo.data.imag)).T
            else:
<<<<<<< HEAD
                raise TypeError('Unknown field type %s' % `field`)

            IJV[:,:2] += 1 # change base 0 -> base 1

            savetxt(stream, IJV, fmt=fmt)

=======
                raise TypeError('Unknown field type %s' % field)

            IJV[:,:2] += 1  # change base 0 -> base 1

            savetxt(stream, IJV, fmt=fmt)


def _is_fromfile_compatible(stream):
    """
    Check whether stream is compatible with numpy.fromfile.

    Passing a gzipped file to fromfile/fromstring doesn't work
    with Python3

    """
    if sys.version_info[0] < 3:
        return True

    bad_cls = []
    try:
        import gzip
        bad_cls.append(gzip.GzipFile)
    except ImportError:
        pass
    try:
        import bz2
        bad_cls.append(bz2.BZ2File)
    except ImportError:
        pass

    bad_cls = tuple(bad_cls)
    return not isinstance(stream, bad_cls)


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#-------------------------------------------------------------------------------
if __name__ == '__main__':
    import sys
    import time
    for filename in sys.argv[1:]:
<<<<<<< HEAD
        print 'Reading',filename,'...',
        sys.stdout.flush()
        t = time.time()
        mmread(filename)
        print 'took %s seconds' % (time.time() - t)
=======
        print('Reading',filename,'...', end=' ')
        sys.stdout.flush()
        t = time.time()
        mmread(filename)
        print('took %s seconds' % (time.time() - t))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
