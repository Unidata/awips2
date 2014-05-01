# This module defines a class that handles I/O using
# Fortran-compatible format specifications.
#
#
# Warning: Fortran formatting is a complex business and I don't
# claim that this module works for anything complicated. It knows
# only the most frequent formatting options. Known limitations:
#
# 1) Only A, D, E, F, G, I, and X formats are supported (plus string constants
#    for output).
# 2) No direct support for complex numbers. You have to split them into
#    real and imaginary parts before output, and for input you get
#    two float numbers anyway.
# 3) No overflow check. If an output field gets too large, it will
#    take more space, instead of being replaced by stars.
#
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# With contributions from Andreas Prlic <andreas@came.sbg.ac.at>
# last revision: 2008-8-18
#

"""
Fortran-style formatted input/output

This module provides two classes that aid in reading and writing
Fortran-formatted text files.

Examples::

  Input::

    >>>s = '   59999'
    >>>format = FortranFormat('2I4')
    >>>line = FortranLine(s, format)
    >>>print line[0]
    >>>print line[1]

  prints::

    >>>5
    >>>9999


  Output::

    >>>format = FortranFormat('2D15.5')
    >>>line = FortranLine([3.1415926, 2.71828], format)
    >>>print str(line)

  prints::

    '3.14159D+00    2.71828D+00'
"""

import string

#
# The class FortranLine represents a single line of input/output,
# which can be accessed as text or as a list of items.
#
class FortranLine:

    """Fortran-style record in formatted files

    FortranLine objects represent the content of one record of a
    Fortran-style formatted file. Indexing yields the contents as
    Python objects, whereas transformation to a string (using the
    built-in function 'str') yields the text representation.

    Restrictions:

      1. Only A, D, E, F, G, I, and X formats are supported (plus string
         constants for output).

      2. No direct support for complex numbers; they must be split into
         real and imaginary parts before output.

      3. No overflow check. If an output field gets too large, it will
         take more space, instead of being replaced by stars according
         to Fortran conventions.
    """
    
    def __init__(self, line, format, length = 80):
        """
        @param line: either a sequence of Python objects, or a string
                     formatted according to Fortran rules

        @param format: either a Fortran-style format string, or a
                       L{FortranFormat} object. A FortranFormat should
                       be used when the same format string is used repeatedly,
                       because then the rather slow parsing of the string
                       is performed only once.

        @param length: the length of the Fortran record. This is relevant
                       only when data is a string; this string is then
                       extended by spaces to have the indicated length.
                       The default value of 80 is almost always correct.
        """
        if type(line) == type(''):
            self.text = line
            self.data = None
        else:
            self.text = None
            self.data = line
        if type(format) == type(''):
            self.format = FortranFormat(format)
        else:
            self.format = format
        self.length = length
        if self.text is None:
            self._output()
        if self.data is None:
            self._input()

    def __len__(self):
        """
        @returns: the number of data elements in the record
        @rtype: C{int}
        """
        return len(self.data)

    def __getitem__(self, i):
        """
        @param i: index
        @type i: C{int}
        @returns: the ith data element
        """
        return self.data[i]

    def __getslice__(self, i, j):
        """
        @param i: start index
        @type i: C{int}
        @param j: end index
        @type j: C{int}
        @returns: a list containing the ith to jth data elements
        """
        return self.data[i:j]

    def __str__(self):
        """
        @returns: a Fortran-formatted text representation of the data record
        @rtype: C{str}
        """
        return self.text

    def isBlank(self):
        """
        @returns: C{True} if the line contains only whitespace
        @rtype: C{bool}
        """
        return len(string.strip(self.text)) == 0

    def _input(self):
        text = self.text
        if len(text) < self.length: text = text + (self.length-len(text))*' '
        self.data = []
        for field in self.format:
            l = field[1]
            s = text[:l]
            text = text[l:]
            type = field[0]
            value = None
            if type == 'A':
                value = s
            elif type == 'I':
                s = string.strip(s)
                if len(s) == 0:
                    value = 0
                else:
                    # by AP
                    # sometimes a line does not match to expected format, 
                    # e.g.: pdb2myd.ent.Z chain: - model: 0 : CONECT*****
                    # catch this and set value to None
                    try:
                        value = string.atoi(s)
                    except:
                        value = None
            elif type == 'D' or type == 'E' or type == 'F' or type == 'G':
                s = string.lower(string.strip(s))
                n = string.find(s, 'd')
                if n >= 0:
                    s = s[:n] + 'e' + s[n+1:]
                if len(s) == 0:
                    value = 0.
                else:
                    try:
                        value = string.atof(s)
                    except:
                        value = None
            if value is not None:
                self.data.append(value)

    def _output(self):
        data = self.data
        self.text = ''
        for field in self.format:
            type = field[0]
            if type == "'":
                self.text = self.text + field[1]
            elif type == 'X':
                self.text = self.text + field[1]*' '
            else: # fields that use input data
                length = field[1]
                if len(field) > 2: fraction = field[2]
                value = data[0]
                data = data[1:]
                if type == 'A':
                    self.text = self.text + (value+length*' ')[:length]
                else: # numeric fields
                    if value is None:
                        s = ''
                    elif type == 'I':
                        s = `value`
                    elif type == 'D':
                        s = ('%'+`length`+'.'+`fraction`+'e') % value
                        n = string.find(s, 'e')
                        s = s[:n] + 'D' + s[n+1:]
                    elif type == 'E':
                        s = ('%'+`length`+'.'+`fraction`+'e') % value
                    elif type == 'F':
                        s = ('%'+`length`+'.'+`fraction`+'f') % value
                    elif type == 'G':
                        s = ('%'+`length`+'.'+`fraction`+'g') % value
                    else:
                        raise ValueError('Not yet implemented')
                    s = string.upper(s)
                    self.text = self.text + ((length*' ')+s)[-length:]
        self.text = string.rstrip(self.text)

#
# The class FortranFormat represents a format specification.
# It ought to work for correct specifications, but there is
# little error checking.
#
class FortranFormat:

    """
    Parsed Fortran-style format string

    FortranFormat objects can be used as arguments when constructing
    FortranLine objects instead of the plain format string. If a
    format string is used more than once, embedding it into a FortranFormat
    object has the advantage that the format string is parsed only once.
    """

    def __init__(self, format, nested = False):
        """
        @param format: a Fortran format specification
        @type format: C{str}
        @param nested: I{for internal use}
        """
        fields = []
        format = string.strip(format)
        while format and format[0] != ')':
            n = 0
            while format[0] in string.digits:
                n = 10*n + string.atoi(format[0])
                format = format[1:]
            if n == 0: n = 1
            type = string.upper(format[0])
            if type == "'":
                eof = string.find(format, "'", 1)
                text = format[1:eof]
                format = format[eof+1:]
            else:
                format = string.strip(format[1:])
            if type == '(':
                subformat = FortranFormat(format, 1)
                fields = fields + n*subformat.fields
                format = subformat.rest
                eof = string.find(format, ',')
                if eof >= 0:
                    format = format[eof+1:]
            else:
                eof = string.find(format, ',')
                if eof >= 0:
                    field = format[:eof]
                    format = format[eof+1:]
                else:
                    eof = string.find(format, ')')
                    if eof >= 0:
                        field = format[:eof]
                        format = format[eof+1:]
                    else:
                        field = format
                        format = ''
                if type == "'":
                    field = (type, text)
                else:
                    dot = string.find(field, '.')
                    if dot > 0:
                        length = string.atoi(field[:dot])
                        fraction = string.atoi(field[dot+1:])
                        field = (type, length, fraction)
                    else:
                        if field:
                            length = string.atoi(field)
                        else:
                            length = 1
                        field = (type, length)
                fields = fields + n*[field]
        self.fields = fields
        if nested:
            self.rest = format

    def __len__(self):
        return len(self.fields)

    def __getitem__(self, i):
        return self.fields[i]


# Test code

if __name__ == '__main__':
    f = FortranFormat("'!!',D10.3,F10.3,G10.3,'!!'")
    l = FortranLine([1.5707963, 3.14159265358, 2.71828], f)
    print str(l)
    f = FortranFormat("F12.0")
    l = FortranLine('2.1D2', f)
    print l[0]
