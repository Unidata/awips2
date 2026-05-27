Python 3 Migration Guide
========================

This is a list of many, but not all, differences between Python 2 and 3, that
you may have to address when updating your Python code to support Python 3.
Some of the differences that are covered by the 2to3 utility are not covered
here. See the 2to3 documentation at:
https://docs.python.org/3.6/library/2to3.html


Removed Modules
---------------

These modules are no longer available in Python 3. Please remove them from your
code.

* ``exceptions`` - No longer needed. The built-in exception types are
  automatically imported into the global namespace

* ``sets`` - The ``set`` type is now built-in

* ``string`` - Most utility functions have been removed (see
  https://docs.python.org/2.7/library/string.html#deprecated-string-functions).
  These functions have been changed to methods on str objects. 2to3 will not
  automatically update these function calls, they have to be fixed manually.

* ``popen2`` - Most or all functionality in this module is now included in
  ``subprocess.Popen``. See:
  https://docs.python.org/2.7/library/subprocess.html#replacing-os-popen-os-popen2-os-popen3


Renamed Modules
---------------

These modules were renamed in Python 3. Update your imports accordingly. (2to3
should take care of these automatically.)

* ``cPickle`` (import ``pickle`` instead, the module will detect the fast C
  version if that is available and will import it automatically)

* ``Queue`` (renamed to ``queue``)

* ``ConfigParser`` (renamed to ``configparser``)

* ``Tkinter`` (renamed to ``tkinter``)

* ``StringIO`` (import ``io`` instead)

* ``cStringIO`` (import ``io`` instead)


String type changes
-------------------

In Python 2, there were 2 string types: ``str``, which stored ASCII strings,
and ``unicode``, which stored ``unicode`` strings. In Python 3, there is one
string type ``str``, which uses the UTF-8 encoding by default. However, many
APIs which in Python 2 would have returned a ``str`` object now return a
``bytes`` object, which is a string of bytes. Developers will now be responsible
for decoding the bytes object into a str object in many places.

* ``pickle`` functions ``load``/``loads`` and ``dump``/``dumps`` now represent
  pickled objects as ``bytes``, not ``str``. Byte strings produced by
  ``pickle`` are never valid UTF-8 so they cannot be converted to ``str``.

* The output of processes called via the ``subprocess`` module comes out by
  default as ``bytes``, not ``str``.

* Calling the ``read`` method against the ``http.client.HTTPResponse`` object
  returned by ``urllib.request.urlopen`` returns a ``bytes`` object, not a
  ``str``.

* When using h5py, retrieving data from a string dataset will return the data
  as ``bytes``, not ``str``. (This is not the case when retrieving the data
  through Pypies as we have code to convert the bytes into strings before
  sending it back to the client. It only applies to direct use of h5py.)

* ``ElementTree.tostring`` now returns an ASCII-encoded byte string instead of a
  ``str`` unless the ``encoding`` argument is set to "unicode".


Using python-netcdf4 to manipulate NetCDF datasets
--------------------------------------------------

Previously the packages Scientific and pupynere were provided for manipulating
data stored in NetCDF datasets. These libraries are not compatible with Python
3 and have been replaced with python-netcdf4
(http://unidata.github.io/netcdf4-python/netCDF4/index.html). Most code will be
compatible with the module with just a few minor changes.

* Constructor is now named ``netCDF4.Dataset``.

* The function ``typecode`` used to retrieve the type of data stored within a
  NetCDF variable has been replaced by the property ``datatype.char``.

* The function ``getValue`` used to retrieve the data stored within a NetCDF
  variable in ``numpy.ndarray`` format has been replaced with numpy slicing.


"Gotchas" with 2to3
-------------------

These are code patterns that may be incorrectly fixed by 2to3.

* If you have a class that implements an external interface similar to that of
  a ``dict`` (provides the methods ``__getitem__``, ``__setitem__``,
  ``__delitem__``, ``has_key``) ensure your class implements ``__contains__``.
  2to3 will replace all calls to ``has_key`` with the ``in`` keyword.


Logical comparisons of mismatched types
---------------------------------------

In Python 2, logical comparisons with some mismatched types (for example,
``'a' <= 4``) were allowed. Under Python 3, most of these comparisons of
mismatched types will now throw a ``TypeError``.

* ``None`` is no longer comparable with numbers, or with anything else.
  (Previously ``None`` would always compare less than any number.) Attempting
  to evaluate an expression like ``None > 3`` will now raise an exception.
  ** Some code that relied on this behavior contained expressions like
     ``dic.get(key) > 10`` that would evaluate to False if ``dic.get(key)``
     returned None. Such code can be rewritten as
     ``dic.get(key, float('-inf')) > 10`` to effectively preserve the old
     behavior.

* ``str`` instances are no longer comparable with numbers (in Python 2, ``str``
  instances are always greater than numbers). Attempting to evaluate an
  expression like ``'a string instance' > 3`` will now raise an exception.


Other issues
------------

* Semantics of the division (``/``) operator have changed.
  ** In Python 2 the result of division would depend on the type of its
     operands--if both were integers, the operation would produce an integer
     result, dropping any fractional part. If either operator was a float, the
     result would be a float.
  ** In Python 3 the ``/`` operator always produces a float.
  ** The ``//`` floor division operator behaves the same as it did in Python 2.
     The type of the result depends on the type of the operands as it did
     previously with the ``/``, but the result is always a whole number.

* The ``file`` builtin no longer exists. Use the builtin ``open`` function
  instead, preferably as a context manager, as in:
  ``with open("filename.txt") as f:``
  so that the file will be closed automatically.

* The ``exec`` statement is gone; it is replaced by a function, also called
  ``exec``. The new ``exec`` function cannot modify the value of variables in
  the caller's local scope. ``exec`` can still modify the global (module-level)
  scope, but such use of ``exec`` is discouraged. If you need to get the value
  of variables from ``exec``'d code, pass in a dictionary as the third argument
  to ``exec``, and retrieve the variables from that dictionary after the call
  to ``exec``. More info here:
  https://docs.python.org/3/library/functions.html#exec

* ``time.mktime`` now requires a tuple as its argument. Passing in a list or
  other iterable will cause an exception.

* The argument to ``list.sort`` or the second argument to ``sorted`` is now a
  key function instead of a comparison function, and must be specified using
  the ``key=`` keyword argument. More info here:
  https://docs.python.org/3/library/functions.html#sorted
  ** Comparison functions can be automatically converted to key functions using
     ``functools.cmp_to_key``.

* ``sys.meta_path`` in Python 3 is not empty by default - it contains important
  import machinery. Do not remove the default import hooks from
  ``sys.meta_path`` or you will break all imports

* ``numpy.getbuffer`` no longer exists. Python 3's built-in ``memoryview``
  provides the same functionality. Calls such as ``numpy.getbuffer(arr)`` can
  be replaced with ``memoryview(arr)`` without further changes necessary in
  most cases.

* ``re.escape`` no longer escapes the underscore character.

* Python source files that previously mixed tabs and spaces for indentation
  will not run under Python 3. Recommend using the script reindent.py
  (https://github.com/python/cpython/blob/3.6/Tools/scripts/reindent.py) to fix
  any such files.

* ``IOError`` now subclasses ``OSError``. Some properties may have changed
  names accordingly.

* ``types.FileType`` no longer exists. (2to3 will not catch this.) If you need
  to check if an object is a file-like object, use
  ``isinstance(obj, io.IOBase)``.
