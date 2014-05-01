# This module handles input and output of PDB files.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2008-8-19
# 

"""
Parsing and writing of Protein Data Bank (PDB) files

This module provides classes that represent PDB (Protein Data Bank)
files and configurations contained in PDB files. It provides access to
PDB files on two levels: low-level (line by line) and high-level
(chains, residues, and atoms).

Caution: The PDB file format has been heavily abused, and it is
probably impossible to write code that can deal with all variants
correctly. This modules tries to read the widest possible range of PDB
files, but gives priority to a correct interpretation of the PDB
format as defined by the Brookhaven National Laboratory.

A special problem are atom names. The PDB file format specifies that
the first two letters contain the right-justified chemical element
name. A later modification allowed the initial space in hydrogen names
to be replaced by a digit. Many programs ignore all this and treat the
name as an arbitrary left-justified four-character name. This makes it
difficult to extract the chemical element accurately; most programs
write the '"CA"' for C_alpha in such a way that it actually stands for
a calcium atom. For this reason a special element field has been added
later, but only few files use it. In the absence of an element field,
the code in this module attempts to guess the element using all information
available.

The low-level routines in this module do not try to deal with the atom
name problem; they return and expect four-character atom names
including spaces in the correct positions. The high-level routines use
atom names without leading or trailing spaces, but provide and use the
element field whenever possible. For output, they use the element
field to place the atom name correctly, and for input, they construct
the element field content from the atom name if no explicit element
field is found in the file.

Except where indicated, numerical values use the same units and
conventions as specified in the PDB format description.

Example::

  >>>conf = Structure('example.pdb')
  >>>print conf
  >>>for residue in conf.residues:
  >>>    for atom in residue:
  >>>        print atom

@undocumented: atom_format
@undocumented: anisou_format
@undocumented: conect_format
@undocumented: ter_format
@undocumented: model_format
@undocumented: header_format
@undocumented: cryst1_format
@undocumented: scalen_format
@undocumented: mtrixn_format
@undocumented: generic_format
@undocumented: export_filters
@undocumented: DummyChain
"""

from Scientific.IO.TextFile import TextFile
from Scientific.IO.FortranFormat import FortranFormat, FortranLine
from Scientific.Geometry import Vector, Tensor
from Scientific import N
from PDBExportFilters import export_filters
import copy, string

#
# Fortran formats for PDB entries
#
atom_format = FortranFormat('A6,I5,1X,A4,A1,A4,A1,I4,A1,3X,3F8.3,2F6.2,' +
                            '6X,A4,2A2')
anisou_format = FortranFormat('A6,I5,1X,A4,A1,A4,A1,I4,A1,1X,6I7,2X,A4,2A2')
conect_format = FortranFormat('A6,11I5')
ter_format = FortranFormat('A6,I5,6X,A4,A1,I4,A1')
model_format = FortranFormat('A6,4X,I4')
header_format = FortranFormat('A6,4X,A40,A9,3X,A4')
cryst1_format = FortranFormat('A6,3F9.3,3F7.2,1X,A11,I4')
scalen_format = FortranFormat('A6,4X,3F10.6,5X,F10.5')
mtrixn_format = FortranFormat('A6,1X,I3,3F10.6,5X,F10.5,4X,I1')
generic_format = FortranFormat('A6,A74')

#
# Amino acid and nucleic acid residues
#
amino_acids = ['ALA', 'ARG', 'ASN', 'ASP', 'CYS', 'CYX', 'GLN', 'GLU', 'GLY',
               'HIS', 'HID', 'HIE', 'HIP', 'HSD', 'HSE', 'HSP', 'ILE', 'LEU',
               'LYS', 'MET', 'PHE', 'PRO', 'SER', 'THR', 'TRP', 'TYR', 'VAL',
               'ACE', 'NME', 'NHE']

nucleic_acids = [ 'A',  'C',  'G',  'I',  'T',  'U',
                 '+A', '+C', '+G', '+I', '+T', '+U',
                  'RA',  'RC',  'RG',  'RU',
                  'DA',  'DC',  'DG',  'DT',
                  'RA5',  'RC5',  'RG5',  'RU5',
                  'DA5',  'DC5',  'DG5',  'DT5',
                  'RA3',  'RC3',  'RG3',  'RU3',
                  'DA3',  'DC3',  'DG3',  'DT3',
                  'RAN',  'RCN',  'RGN',  'RUN',
                  'DAN',  'DCN',  'DGN',  'DTN',
                  ]

def defineAminoAcidResidue(symbol):
    """
    Make the parser recognize a particular residue type as an amino
    acid residue
    
    @param symbol: the three-letter code for an amino acid
    @type symbol: C{str}
    """
    symbol = symbol.upper()
    if symbol not in amino_acids:
        amino_acids.append(symbol)

def defineNucleicAcidResidue(symbol):
    """
    Make the parser recognize a particular residue type as an nucleic
    acid residue
    
    @param symbol: the one-letter code for a nucleic acid
    @type symbol: C{str}
    """
    symbol = symbol.upper()
    if symbol not in nucleic_acids:
        nucleic_acids.append(symbol)


#
# Low-level file object. It represents line contents as Python dictionaries.
# For output, there are additional methods that generate sequence numbers
# for everything.
#
class PDBFile:

    """
    X{PDB} file with access at the record level

    The low-level file access is handled by the module
    L{Scientific.IO.TextFile}, therefore compressed files and URLs
    (for reading) can be used as well.
    """

    def __init__(self, file_or_filename, mode = 'r', subformat = None):
        """
        @param file_or_filename: the name of the PDB file, or a file object
        @type file_or_filename: C{str} or C{file}
        @param mode: the file access mode, 'r' (read) or 'w' (write)
        @type mode: C{str}
        @param subformat: indicates a specific dialect of the PDB format.
                          Subformats are defined in
                          L{Scientific.IO.PDBExportFilters}; they are used
                          only when writing.
        @type subformat: C{str} or C{NoneType}
        """
        if isinstance(file_or_filename, str):
            self.file = TextFile(file_or_filename, mode)
        else:
            self.file = file_or_filename
        self.output = string.lower(mode[0]) == 'w'
        self.export_filter = None
        if subformat is not None:
            export = export_filters.get(subformat, None)
            if export is not None:
                self.export_filter = export()
        self.open = 1
        if self.output:
            self.data = {'serial_number': 0,
                         'residue_number': 0,
                         'chain_id': '',
                         'segment_id': ''}
            self.het_flag = 0
            self.chain_number = -1

    def readLine(self):
        """
        Return the contents of the next non-blank line (= record) The
        return value is a tuple whose first element (a string)
        contains the record type. For supported record types (HEADER,
        CRYST1, SCALEn, MTRIXn, ATOM, HETATM, ANISOU, TERM, MODEL,
        CONECT), the items from the remaining fields are put into a
        dictionary which is returned as the second tuple element. Most
        dictionary elements are strings or numbers; atom positions are
        returned as a vector, and anisotropic temperature factors are
        returned as a rank-2 tensor, already multiplied by 1.e-4.
        White space is stripped from all strings except for atom
        names, whose correct interpretation can depend on an initial
        space. For unsupported record types, the second tuple element
        is a string containing the remaining part of the record.

        @returns: the contents of one PDB record
        @rtype: C{tuple}
        """
        while 1:
            line = self.file.readline()
            if not line: return ('END','')
            if line[-1] == '\n': line = line[:-1]
            line = string.strip(line)
            if line: break
        line = string.ljust(line, 80)
        type = string.strip(line[:6])
        if type == 'ATOM' or type == 'HETATM':
            line = FortranLine(line, atom_format)
            data = {'serial_number': line[1],
                    'name': line[2],
                    'alternate': string.strip(line[3]),
                    'residue_name': string.strip(line[4]),
                    'chain_id': string.strip(line[5]),
                    'residue_number': line[6],
                    'insertion_code': string.strip(line[7]),
                    'position': Vector(line[8:11]),
                    'occupancy': line[11],
                    'temperature_factor': line[12],
                    'segment_id': string.strip(line[13]),
                    'element': string.strip(line[14]),
                    'charge': string.strip(line[15])}
            return type, data
        elif type == 'ANISOU':
            line = FortranLine(line, anisou_format)
            data = {'serial_number': line[1],
                    'name': line[2],
                    'alternate': string.strip(line[3]),
                    'residue_name': string.strip(line[4]),
                    'chain_id': string.strip(line[5]),
                    'residue_number': line[6],
                    'insertion_code': string.strip(line[7]),
                    'u': 1.e-4*Tensor([[line[8], line[11], line[12]],
                                       [line[11], line[9] , line[13]],
                                       [line[12], line[13], line[10]]]),
                    'segment_id': string.strip(line[14]),
                    'element': string.strip(line[15]),
                    'charge': string.strip(line[16])}
            return type, data
        elif type == 'TER':
            line = FortranLine(line, ter_format)
            data = {'serial_number': line[1],
                    'residue_name': string.strip(line[2]),
                    'chain_id': string.strip(line[3]),
                    'residue_number': line[4],
                    'insertion_code': string.strip(line[5])}
            return type, data
        elif type == 'CONECT':
            line = FortranLine(line, conect_format)
            data = {'serial_number': line[1],
                    'bonded': [i for i in line[2:6] if i > 0],
                    'hydrogen_bonded': [i for i in line[6:10] if i > 0],
                    'salt_bridged': [i for i in line[10:12] if i > 0]}
            return type, data
        elif type == 'MODEL':
            line = FortranLine(line, model_format)
            data = {'serial_number': line[1]}
            return type, data
        elif type == 'HEADER':
            line = FortranLine(line, header_format)
            data = {'compound': line[1],
                    'date': line[2],
                    'pdb_code': line[3]}
            return type, data
        elif type == 'CRYST1':
            line = FortranLine(line, cryst1_format)
            data = {'a': line[1],
                    'b': line[2],
                    'c': line[3],
                    'alpha': line[4],
                    'beta': line[5],
                    'gamma': line[6],
                    'space_group': line[7],
                    'z': line[8]}
            return type, data
        elif type[:-1] == 'SCALE':
            line = FortranLine(line, scalen_format)
            data = {'s1': line[1],
                    's2': line[2],
                    's3': line[3],
                    'u': line[4]}
            return type, data
        elif type[:-1] == 'MTRIX':
            line = FortranLine(line, mtrixn_format)
            data = {'serial': line[1],
                    'm1': line[2],
                    'm2': line[3],
                    'm3': line[4],
                    'v': line[5],
                    'given': line[6] == 1}
            return type, data
        else:
            return type, line[6:]

    def writeLine(self, type, data):
        """
        Write a line using record type and data dictionary in the
        same format as returned by readLine(). Default values are
        provided for non-essential information, so the data dictionary
        need not contain all entries.

        @param type: PDB record type
        @type type: C{str}
        @param data: PDB record data
        @type data: C{tuple}
        """
        if self.export_filter is not None:
            type, data = self.export_filter.processLine(type, data)
            if type is None:
                return
        line = [type]
        if type == 'ATOM' or type == 'HETATM':
            format = atom_format
            position = data['position']
            line = line + [data.get('serial_number', 1),
                           data.get('name'),
                           data.get('alternate', ''),
                           string.rjust(data.get('residue_name', ''), 3),
                           data.get('chain_id', ''),
                           data.get('residue_number', 1),
                           data.get('insertion_code', ''),
                           position[0], position[1], position[2],
                           data.get('occupancy', 0.),
                           data.get('temperature_factor', 0.),
                           data.get('segment_id', ''),
                           string.rjust(data.get('element', ''), 2),
                           data.get('charge', '')]
        elif type == 'ANISOU':
            format = anisou_format
            u = 1.e4*data['u']
            u = [int(u[0,0]), int(u[1,1]), int(u[2,2]),
                 int(u[0,1]), int(u[0,2]), int(u[1,2])]
            line = line + [data.get('serial_number', 1),
                           data.get('name'),
                           data.get('alternate', ''),
                           string.rjust(data.get('residue_name'), 3),
                           data.get('chain_id', ''),
                           data.get('residue_number', 1),
                           data.get('insertion_code', '')] \
                        + u \
                        + [data.get('segment_id', ''),
                           string.rjust(data.get('element', ''), 2),
                           data.get('charge', '')]
        elif type == 'TER':
            format = ter_format
            line = line + [data.get('serial_number', 1),
                           string.rjust(data.get('residue_name'), 3),
                           data.get('chain_id', ''),
                           data.get('residue_number', 1),
                           data.get('insertion_code', '')]
        elif type == 'CONECT':
            format = conect_format
            line = line + [data.get('serial_number')]
            line = line + (data.get('bonded', [])+4*[None])[:4]
            line = line + (data.get('hydrogen_bonded', [])+4*[None])[:4]
            line = line + (data.get('salt_bridged', [])+2*[None])[:2]
        elif type == 'MODEL':
            format = model_format
            line = line + [data.get('serial_number')]
        elif type == 'CRYST1':
            format = cryst1_format
            line = line + [data.get('a'), data.get('b'), data.get('c'),
                           data.get('alpha'), data.get('beta'),
                           data.get('gamma'),
                           data.get('space_group'),
                           data.get('z')]
        elif type[:-1] == 'SCALE':
            format = scalen_format
            line = line + [data.get('s1'), data.get('s2'), data.get('s3'),
                           data.get('u')]
        elif type[:-1] == 'MTRIX':
            format = scalen_format
            line = line + [data.get('serial'),
                           data.get('m1'), data.get('m2'), data.get('m3'),
                           data.get('v'), int(data.get('given'))]
        elif type == 'HEADER':
            format = header_format
            line = line + [data.get('compound', ''), data.get('date', ''),
                           data.get('pdb_code')]
        else:
            format = generic_format
            line = line + [data]
        self.file.write(str(FortranLine(line, format)) + '\n')

    def writeComment(self, text):
        """
        Write text into one or several comment lines.
        Each line of the text is prefixed with 'REMARK' and written
        to the file.

        @param text: the comment contents
        @type text: C{str}
        """
        while text:
            eol = string.find(text,'\n')
            if eol == -1:
                eol = len(text)
            self.file.write('REMARK %s \n' % text[:eol])
            text = text[eol+1:]

    def writeAtom(self, name, position, occupancy=0.0, temperature_factor=0.0,
                  element=''):
        """
        Write an ATOM or HETATM record using the information supplied.
        The residue and chain information is taken from the last calls to
        the methods L{nextResidue} and L{nextChain}.

        @param name: the atom name
        @type name: C{str}
        @param position: the atom position
        @type position: L{Scientific.Geometry.Vector}
        @param occupancy: the occupancy
        @type occupancy: C{float}
        @param temperature_factor: the temperature factor (B-factor)
        @type temperature_factor: C{float}
        @param element: the chemical element
        @type element: C{str}
        """
        if self.het_flag:
            type = 'HETATM'
        else:
            type = 'ATOM'
        name = string.upper(name)
        if element != '' and len(element) == 1 and name and name[0] == element:
            name = ' ' + name
        self.data['name'] = name
        self.data['position'] = position
        self.data['serial_number'] = (self.data['serial_number'] + 1) % 100000
        self.data['occupancy'] = occupancy
        self.data['temperature_factor'] = temperature_factor
        self.data['element'] = element
        self.writeLine(type, self.data)

    def nextResidue(self, name, number = None, terminus = None):
        """
        Signal the beginning of a new residue, starting with the
        next call to L{writeAtom}.

        @param name: the residue name
        @type name: C{str}
        @param number: the residue number. If C{None}, the residues
                       will be numbered sequentially, starting from 1.
        @type number: C{int} or C{NoneType}
        @param terminus: C{None}, "C", or "N". This information
                         is passed to export filters that can use this
                         information in order to use different atom or
                         residue names in terminal residues.
        """
        name  = string.upper(name)
        if self.export_filter is not None:
            name, number = self.export_filter.processResidue(name, number,
                                                             terminus)
        self.het_flag =  not (name in amino_acids or name in nucleic_acids)
        self.data['residue_name'] = name
        self.data['residue_number'] = (self.data['residue_number'] + 1) % 10000
        self.data['insertion_code'] = ''
        if number is not None:
            if isinstance(number, int):
                if number >= 0:
                    self.data['residue_number'] = number % 10000
                else:
                    self.data['residue_number'] = -((-number) % 1000)
            else:
                self.data['residue_number'] = number.number % 10000
                self.data['insertion_code'] = number.insertion_code

    def nextChain(self, chain_id = None, segment_id = ''):
        """
        Signal the beginning of a new chain.

        @param chain_id: a chain identifier. If C{None}, consecutive letters
                         from the alphabet are used.
        @type chain_id: C{str} or C{NoneType}
        @param segment_id: a chain identifier
        @type segment_id: C{str}
        """
        if chain_id is None:
            self.chain_number = (self.chain_number + 1) % len(self._chain_ids)
            chain_id = self._chain_ids[self.chain_number]
        if self.export_filter is not None:
            chain_id, segment_id = \
                      self.export_filter.processChain(chain_id, segment_id)
        self.data['chain_id'] = (chain_id+' ')[:1]
        self.data['segment_id'] = (segment_id+'    ')[:4]
        self.data['residue_number'] = 0

    _chain_ids = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    def terminateChain(self):
        """
        Signal the end of a chain.
        """
        if self.export_filter is not None:
            self.export_filter.terminateChain()
        self.data['serial_number'] = (self.data['serial_number'] + 1) % 100000
        self.writeLine('TER', self.data)
        self.data['chain_id'] = ''
        self.data['segment_id'] = ''
        
    def close(self):
        """
        Close the file. This method B{must} be called for write mode
        because otherwise the file will be incomplete.
        """
        if self.open:
            if self.output:
                self.file.write('END\n')
            self.file.close()
            self.open = 0

    def __del__(self):
        self.close()


#
# High-level object representation of PDB file contents.
#

#
# Representation of objects.
#
class Atom:

    """
    Atom in a PDB structure
    """
    
    def __init__(self, name, position, **properties):
        """
        @param name: the atom name
        @type name: C{str}
        @param position: the atom position
        @type position: L{Scientific.Geometry.Vector}
        @param properties: any other atom properties as keyword parameters.
                           These properties are stored in the atom object
                           and can be accessed by indexing, as for
                           dictionaries.
        """
        self.position = position
        self.properties = properties
        if self.properties.get('element', '') == '':
            if name[0] == ' ' or name[0] in string.digits:
                self.properties['element'] = name[1]
            elif name[1] in string.digits:
                self.properties['element'] = name[0]
            else:
                self.properties['element'] = name[0:2]
        self.name = string.strip(name)
        self.parent = None

    def __getitem__(self, item):
        """
        @param item: the name of a property, including "name" or "position"
        @type item: C{str}
        @returns: the property value
        """
        try:
            return self.properties[item]
        except KeyError:
            if item == 'name':
                return self.name
            elif item == 'position':
                return self.position
            else:
                raise KeyError("Undefined atom property: " + repr(item))

    def __setitem__(self, item, value):
        """
        @param item: the name of an existing or to be defined property
        @type item: C{str}
        @param value: the new value for the property
        """
        self.properties[item] = value

    def __str__(self):
        return self.__class__.__name__ + ' ' + self.name + \
               ' at ' + str(self.position)
    __repr__ = __str__

    def type(self):
        """
        @returns: the six-letter record type, ATOM or HETATM
        @rtype: C{str}
        """
        return 'ATOM  '

    def writeToFile(self, file):
        """
        Write an atom record to a file

        @param file: a PDB file object or a filename
        @type file: L{PDBFile} or C{str}
        """
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        file.writeAtom(self.name, self.position,
                       self.properties.get('occupancy', 0.),
                       self.properties.get('temperature_factor', 0.),
                       self.properties.get('element', ''))
        if close:
            file.close()


class HetAtom(Atom):

    """
    HetAtom in a PDB structure

    A subclass of Atom, which differs only in the return value
    of the method type().
    """

    def type(self):
        return 'HETATM'
    

class Group:

    """
    Atom group (residue or molecule) in a PDB file

    This is an abstract base class. Instances can be created using
    one of the subclasses (L{Molecule}, L{AminoAcidResidue},
    L{NucleotideResidue}).

    Group objects permit iteration over atoms with for-loops,
    as well as extraction of atoms by indexing with the
    atom name.
    """

    def __init__(self, name, atoms = None, number = None):
        """
        @param name: the name of the group
        @type name: C{str}
        @param atoms: a list of atoms (or C{None} for no atoms)
        @type atoms: C{list} or C{NoneType}
        @param number: the PDB residue number (or C{None})
        @type number: C{int} or C{NoneType}
        """
        self.name = name
        self.number = number
        self.atom_list = []
        self.atoms = {}
        if atoms:
            self.atom_list = atoms
            for a in atoms:
                self.atoms[a.name] = a

    def __len__(self):
        return len(self.atom_list)

    def __getitem__(self, item):
        """
        @param item: an integer index or an atom name
        @type item: C{int} or C{str}
        """
        if isinstance(item, int):
            return self.atom_list[item]
        else:
            return self.atoms[item]

    def __str__(self):
        s = self.__class__.__name__ + ' ' + self.name + ':\n'
        for atom in self.atom_list:
            s = s + '  ' + `atom` + '\n'
        return s
    __repr__ = __str__

    def isCompatible(self, residue_data):
        return residue_data['residue_name'] == self.name \
               and residue_data['residue_number'] == self.number

    def addAtom(self, atom):
        """
        Add an atom to the group
        
        @param atom: the atom
        @type atom: L{Atom}
        """
        self.atom_list.append(atom)
        self.atoms[atom.name] = atom
        atom.parent = self

    def deleteAtom(self, atom):
        """
        Remove an atom from the group

        @param atom: the atom to be removed
        @type atom: L{Atom}
        @raises KeyError: if the atom is not part of the group
        """
        self.atom_list.remove(atom)
        del self.atoms[atom.name]
        atom.parent = None

    def deleteHydrogens(self):
        """
        Remove all hydrogen atoms of the group
        """
        delete = []
        for a in self.atom_list:
            if a.name[0] == 'H' or (a.name[0] in string.digits
                                    and a.name[1] == 'H'):
                delete.append(a)
        for a in delete:
            self.deleteAtom(a)

    def changeName(self, name):
        """
        Set the PDB residue name

        @param name: the new name
        @type name: C{str}
        """
        self.name = name

    def writeToFile(self, file):
        """
        Write the group to a file

        @param file: a PDBFile object or a file name
        @type file: L{PDBFile} or C{str}
        """
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        file.nextResidue(self.name, self.number, None)
        for a in self.atom_list:
            a.writeToFile(file)
        if close:
            file.close()

class Molecule(Group):

    """
    Molecule in a PDB file

    B{Note:} In PDB files, non-chain molecules are treated as residues,
    there is no separate molecule definition. This module defines
    every residue as a molecule that is not an amino acid residue or a
    nucleotide residue.
    """

    pass

class Residue(Group):

    pass

class AminoAcidResidue(Residue):

    """
    Amino acid residue in a PDB file
    """

    is_amino_acid = 1

    def isCTerminus(self):
        """
        @returns: C{True} if the residue is in C-terminal configuration,
        i.e. if it has a second oxygen bound to the carbon atom of
        the peptide group. C{False} otherwise.
        @rtype: C{bool}
        """
        return self.name == 'NME' \
               or self.atoms.has_key('OXT') \
               or self.atoms.has_key('OT2')

    def isNTerminus(self):
        """
        @returns: C{True} if the residue is in N-terminal configuration,
        i.e. if it contains more than one hydrogen bound to be
        nitrogen atom of the peptide group. C{False} otherwise.
        @rtype: C{bool}
        """
        return self.atoms.has_key('1HT') or self.atoms.has_key('2HT') \
               or self.atoms.has_key('3HT')

    def addAtom(self, atom):
        Residue.addAtom(self, atom)
        if atom.name == 'CA': # Make sure it's not a calcium
            atom.properties['element'] = 'C'

    def writeToFile(self, file):
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        terminus = None
        if self.isCTerminus(): terminus = 'C'
        if self.isNTerminus(): terminus = 'N'
        file.nextResidue(self.name, self.number, terminus)
        for a in self.atom_list:
            a.writeToFile(file)
        if close:
            file.close()


class NucleotideResidue(Residue):

    """
    Nucleotide residue in a PDB file
    """

    is_nucleotide = 1

    def __init__(self, name, atoms = None, number = None):
        self.pdbname = name
        name = string.strip(name)
        if name[0] != 'D' and name[0] != 'R':
            name = 'D' + name
        Residue.__init__(self, name, atoms, number)
        for a in atoms:
            if a.name == 'O2*' or a.name == "O2'": # Ribose
                self.name = 'R' + self.name[1:]

    def isCompatible(self, residue_data):
        return (residue_data['residue_name'] == self.name or
                residue_data['residue_name'] == self.pdbname) \
               and residue_data['residue_number'] == self.number

    def addAtom(self, atom):
        Residue.addAtom(self, atom)
        if atom.name == 'O2*' or atom.name == "O2'": # Ribose
            self.name = 'R' + self.name[1:]

    def hasRibose(self):
        """
        @returns: C{True} if the residue has an atom named O2*
        @rtype: C{bool}
        """
        return self.atoms.has_key('O2*') or self.atoms.has_key("O2'")

    def hasDesoxyribose(self):
        """
        @returns: C{True} if the residue has no atom named O2*
        @rtype: C{bool}
        """
        return not self.hasRibose()

    def hasPhosphate(self):
        """
        @returns: C{True} if the residue has a phosphate group
        @rtype: C{bool}
        """
        return self.atoms.has_key('P')

    def hasTerminalH(self):
        """
        @returns: C{True} if the residue has a 3-terminal H atom
        @rtype: C{bool}
        """
        return self.atoms.has_key('H3T')

    def writeToFile(self, file):
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        terminus = None
        if not self.hasPhosphate(): terminus = '5'
        file.nextResidue(self.name[1:], self.number, terminus)
        for a in self.atom_list:
            a.writeToFile(file)
        if close:
            file.close()

class Chain:

    """Chain of PDB residues

    This is an abstract base class. Instances can be created using
    one of the subclasses (L{PeptideChain}, L{NucleotideChain}).

    Chain objects respond to len() and return their residues
    by indexing with integers.
    """

    def __init__(self, residues = None, chain_id = None, segment_id = None):
        """
        @param residues: a list of residue objects, or C{None} meaning
                         that the chain is initially empty
        @type residues: C{list} or C{NoneType}
        @param chain_id: a one-letter chain identifier or C{None}
        @type chain_id: C{str} or C{NoneType}
        @param segment_id: a multi-character segment identifier or C{None}
        @type segment_id: C{str} or C{NoneType}
        """
        if residues is None:
            self.residues = []
        else:
            self.residues = residues
        self.chain_id = chain_id
        self.segment_id = segment_id

    def __len__(self):
        """
        @returns: the number of residues in the chain
        @rtype: C{int}
        """
        return len(self.residues)

    def sequence(self):
        """
        @returns: the list of residue names
        @rtype: C{list} of C{str}
        """
        return [r.name for r in self.residues]

    def __getitem__(self, index):
        """
        @param index: an index into the chain
        @type index: C{int}
        @returns: the residue corresponding to the index
        @rtype: L{AminoAcidResidue} or L{NucleotideResidue}
        @raises IndexError: if index exceeds the chain length
        """
        return self.residues[index]

    def __getslice__(self, i1, i2):
        """
        @param i1: in index into the chain
        @type i1: C{int}
        @param i2: in index into the chain
        @type i12 C{int}
        @returns: the subchain from i1 to i2
        @rtype: L{PeptideChain} or L{NucleotideChain}
        """
        return self.__class__(self.residues[i1:i2])

    def addResidue(self, residue):
        """
        Add a residue at the end of the chain

        @param residue: the residue to be added
        @type residue: L{AminoAcidResidue} or L{NucleotideResidue}
        """
        self.residues.append(residue)

    def removeResidues(self, first, last):
        """
        Remove residues in a given index range.

        @param first: the index of the first residue to be removed
        @type first: C{int}
        @param last: the index of the first residue to be kept, or C{None}
                     meaning remove everything to the end of the chain.
        @type last: C{int} or C{NoneType}
        """
        if last is None:
            del self.residues[first:]
        else:
            del self.residues[first:last]

    def deleteHydrogens(self):
        """
        Remove all hydrogen atoms in the chain
        """
        for r in self.residues:
            r.deleteHydrogens()

    def writeToFile(self, file):
        """
        Write the chain to a file

        @param file: a PDBFile object or a file name
        @type file: L{PDBFile} or C{str}
        """
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        file.nextChain(self.chain_id, self.segment_id)
        for r in self.residues:
            r.writeToFile(file)
        file.terminateChain()
        if close:
            file.close()

class PeptideChain(Chain):

    """
    Peptide chain in a PDB file
    """

    def isTerminated(self):
        """
        @returns: C{True} if the last residue is in C-terminal configuration
        @rtype: C{bool}
        """
        return self.residues and self.residues[-1].isCTerminus()

    def isCompatible(self, chain_data, residue_data):
        return chain_data['chain_id'] == self.chain_id and \
               chain_data['segment_id'] == self.segment_id and \
               residue_data['residue_name'] in amino_acids


class NucleotideChain(Chain):

    """
    Nucleotide chain in a PDB file
    """

    def isTerminated(self):
        """
        @returns: C{True} if the last residue is in 3-terminal configuration
        @rtype: C{bool}
        @note: There is no way to perform this test with standard PDB files.
               The algorithm used works for certain non-standard files only.
        """
        return self.residues and \
               (self.residues[-1].name[-1] == '3'
                or self.residues[-1].hasTerminalH())

    def isCompatible(self, chain_data, residue_data):
        return chain_data['chain_id'] == self.chain_id and \
               chain_data['segment_id'] == self.segment_id and \
               residue_data['residue_name'] in nucleic_acids

class DummyChain(Chain):

    def __init__(self, structure, chain_id, segment_id):
        self.structure = structure
        self.chain_id = chain_id
        self.segment_id = segment_id

    def isTerminated(self):
        return 0

    def addResidue(self, residue):
        self.structure.addMolecule(residue)

    def isCompatible(self, chain_data, residue_data):
        return chain_data['chain_id'] == self.chain_id and \
               chain_data['segment_id'] == self.segment_id and \
               residue_data['residue_name'] not in amino_acids and \
               residue_data['residue_name'] not in nucleic_acids

#
# Residue number class for dealing with insertion codes
#
class ResidueNumber:

    """
    PDB residue number

    Most PDB residue numbers are simple integers, but when insertion
    codes are used a number can consist of an integer plus a letter.
    Such compound residue numbers are represented by this class.
    """

    def __init__(self, number, insertion_code):
        """
        @param number: the numeric part of the residue number
        @type number: C{int}
        @param insertion_code: the letter part of the residue number
        @type insertion_code: C{str}
        """
        self.number = number
        self.insertion_code = insertion_code

    def __cmp__(self, other):
        if isinstance(other, int):
            if self.number == other:
                return 1
            else:
                return cmp(self.number, other)
        if self.number == other.number:
            return cmp(self.insertion_code, other.insertion_code)
        else:
            return cmp(self.number, other.number)

    def __str__(self):
        return str(self.number) + self.insertion_code
    __repr__ = __str__

#
# The configuration class.
#
class Structure:

    """
    A high-level representation of the contents of a PDB file

    The components of a structure can be accessed in several ways
    ('s' is an instance of this class):

     - 's.residues' is a list of all PDB residues, in the order in
       which they occurred in the file.

     - 's.peptide_chains' is a list of PeptideChain objects, containing
       all peptide chains in the file in their original order.

     - 's.nucleotide_chains' is a list of NucleotideChain objects, containing
       all nucleotide chains in the file in their original order.

     - 's.molecules' is a list of all PDB residues that are neither
       amino acid residues nor nucleotide residues, in their original
       order.

     - 's.objects' is a list of all high-level objects (peptide chains,
       nucleotide chains, and molecules) in their original order.

     - 's.to_fractional' is the transformation from real-space coordinates
       to fractional coordinates, as read from the SCALEn records.

     - 's.from_fractional' is the transformation from fractional coordinates
       to real-space coordinates, the inverse of s.to_fractional.

     - 's.ncs_transformations' is a list of transformations that
        describe non-crystallographic symmetries, as read from the
        MTRIXn records.

     - if a CRYST1 record exists, 's.a', 's.b', 's.c', 's.alpha',
       's.beta', 's.gamma' are the parameters of the unit cell and
       's.space_group' is a string indicating the space group.
       If no CRYST1 record exists, all those values are None.
       Furthermore, 's.cs_transformations' is a list of transformations
       that describe crystallographic symmetries. If no CRYST1 record
       exists, the list is empty.

    An iteration over a Structure instance by a for-loop is equivalent
    to an iteration over the residue list.
    """

    def __init__(self, file_or_filename, model = 0, alternate_code = 'A'):
        """
        @param file_or_filename: the name of the PDB file, or a file object.
                                 Compressed files and URLs are accepted,
                                 as for class L{PDBFile}.
        @type file_or_filename: C{str} or C{file}
        @param model: the number of the model to read from a multiple-model
                      file. Only one model can be treated at a time.
        @type model: C{int}
        @param alternate_code: the version of the positions to be read
                               from a file with alternate positions.
        @type alternate_code: single-letter C{str}
        """
        if isinstance(file_or_filename, str):
            self.filename = file_or_filename
        else:
            self.filename = ''
        self.model = model
        self.alternate = alternate_code
        self.pdb_code = ''
        self.residues = []
        self.objects = []
        self.peptide_chains = []
        self.nucleotide_chains = []
        self.molecules = {}
        self.to_fractional = self.from_fractional = None
        self.ncs_transformations = []
        self.cs_transformations = []
        self.a = self.b = self.c = None
        self.alpha = self.beta = self.gamma = None
        self.space_group = None
        self.parseFile(PDBFile(file_or_filename))
        self.findSpaceGroupTransformations()

    peptide_chain_constructor = PeptideChain
    nucleotide_chain_constructor = NucleotideChain
    molecule_constructor = Molecule

    def __len__(self):
        return len(self.residues)

    def __getitem__(self, item):
        return self.residues[item]

    def deleteHydrogens(self):
        """
        Remove all hydrogen atoms
        """
        for r in self.residues:
            r.deleteHydrogens()

    def splitPeptideChain(self, number, position):
        """
        Split a peptide chain into two chains

        The two chain fragments remain adjacent in the peptide chain
        list, i.e. the numbers of all following chains increase
        by one.

        @param number: the number of the peptide chain to be split
        @type number: C{int}
        @param position: the residue index at which the chain is split.
        @type position: C{int}
        """
        self._splitChain(self.peptide_chain_constructor,
                         self.peptide_chains, number, position)
        
    def splitNucleotideChain(self, number, position):
        """
        Split a nucleotide chain into two chains

        The two chain fragments remain adjacent in the nucleotide chain
        list, i.e. the numbers of all following chains increase
        by one.

        @param number: the number of the nucleotide chain to be split
        @type number: C{int}
        @param position: the residue index at which the chain is split.
        @type position: C{int}
        """
        self._splitChain(self.nucleotide_chain_constructor,
                         self.nucleotide_chains, number, position)

    def _splitChain(self, constructor, chain_list, number, position):
        chain = chain_list[number]
        part1 = constructor(chain.residues[:position],
                            chain.chain_id, chain.segment_id)
        part2 = constructor(chain.residues[position:])
        chain_list[number:number+1] = [part1, part2]
        index = self.objects.index(chain)
        self.objects[index:index+1] = [part1, part2]

    def joinPeptideChains(self, first, second):
        """
        Join two peptide chains into a single one. The new chain occupies
        the position of the first chain, the second one is removed from
        the peptide chain list.

        @param first: the number of the first chain
        @type first: C{int}
        @param second: the number of the second chain
        @type second: C{int}
        """
        self._joinChains(self.peptide_chain_constructor,
                         self.peptide_chains, first, second)
        
    def joinNucleotideChains(self, first, second):
        """
        Join two nucleotide chains into a single one. The new chain occupies
        the position of the first chain, the second one is removed from
        the nucleotide chain list.

        @param first: the number of the first chain
        @type first: C{int}
        @param second: the number of the second chain
        @type second: C{int}
        """
        self._joinChains(self.nucleotide_chain_constructor,
                         self.nucleotide_chains, first, second)

    def _joinChains(self, constructor, chain_list, first, second):
        chain1 = chain_list[first]
        chain2 = chain_list[second]
        total = constructor(chain1.residues+chain2.residues,
                            chain1.chain_id, chain1.segment_id)
        chain_list[first] = total
        del chain_list[second]
        index = self.objects.index(chain1)
        self.objects[index] = total
        index = self.objects.index(chain2)
        del self.objects[index]

    def addMolecule(self, molecule):
        try:
            molecule_list = self.molecules[molecule.name]
        except KeyError:
            molecule_list = []
            self.molecules[molecule.name] = molecule_list
        molecule_list.append(molecule)
        self.objects.append(molecule)
    
    def deleteResidue(self, residue):
        self.residues.remove(residue)
        delete = None
        for type, mlist in self.molecules.items():
            try:
                mlist.remove(residue)
            except ValueError:
                pass
            if len(mlist) == 0:
                delete = type
        if delete is not None:
            del self.molecules[delete]
        delete = None
        for chain in self.peptide_chains + self.nucleotide_chains:
            try:
                chain.residues.remove(residue)
            except ValueError:
                pass
            if len(chain.residues) == 0:
                delete = chain
        if delete is not None:
            try:
                self.peptide_chains.remove(chain)
            except ValueError:
                pass
            try:
                self.nucleotide_chains.remove(chain)
            except ValueError:
                pass
        try:
            self.objects.remove(residue)
        except ValueError:
            pass

    def extractData(self, data):
        atom_data = {}
        for name in ['serial_number', 'name', 'position',
                     'occupancy', 'temperature_factor']:
            atom_data[name] = data[name]
        for name in ['alternate', 'charge']:
            value = data[name]
            if value:
                atom_data[name] = value
        element = data['element']
        if element != '':
            try:
                string.atoi(element)
            except ValueError:
                atom_data['element'] = element
        residue_data = {'residue_name': data['residue_name']}
        number = data['residue_number']
        insertion = data['insertion_code']
        if insertion == '':
            residue_data['residue_number'] = number
        else:
            residue_data['residue_number'] = ResidueNumber(number, insertion)
        chain_data = {}
        for name in ['chain_id', 'segment_id']:
            chain_data[name] = data[name]
        if chain_data['segment_id'] == self.pdb_code:
            chain_data['segment_id'] = ''
        return atom_data, residue_data, chain_data

    def newResidue(self, residue_data):
        name = residue_data['residue_name']
        residue_number = residue_data['residue_number']
        if name in amino_acids:
            residue = AminoAcidResidue(name, [], residue_number)
        elif name in nucleic_acids:
            residue = NucleotideResidue(name, [], residue_number)
        else:
            residue = self.molecule_constructor(name, [], residue_number)
        self.residues.append(residue)
        return residue

    def newChain(self, residue, chain_data):
        if hasattr(residue, 'is_amino_acid'):
            chain = self.peptide_chain_constructor([], chain_data['chain_id'],
                                                   chain_data['segment_id'])
            self.peptide_chains.append(chain)
            self.objects.append(chain)
        elif hasattr(residue, 'is_nucleotide'):
            chain = self.nucleotide_chain_constructor([],
                                                      chain_data['chain_id'],
                                                      chain_data['segment_id'])
            self.nucleotide_chains.append(chain)
            self.objects.append(chain)
        else:
            chain = DummyChain(self, chain_data['chain_id'],
                               chain_data['segment_id'])
        return chain

    def parseFile(self, file):
        atom = None
        residue = None
        chain = None
        read = self.model == 0
        while 1:
            type, data = file.readLine()
            if type == 'END': break
            elif type == 'HEADER':
                self.pdb_code = data['pdb_code']
            elif type == 'CRYST1':
                for name, value in data.items():
                    setattr(self, name, value)
                self.space_group = self.space_group.strip()
            elif type[:-1] == 'SCALE':
                if not hasattr(self, '_scale_matrix'):
                    self._scale_matrix = {}
                self._scale_matrix[type[-1]] = data
                if type[-1] == '3': # last line read
                    from Scientific.Geometry.Transformation \
                         import Shear, Translation
                    l1 = self._scale_matrix['1']
                    l2 = self._scale_matrix['2']
                    l3 = self._scale_matrix['3']
                    s = N.array([[l1['s1'], l1['s2'], l1['s3']],
                                 [l2['s1'], l2['s2'], l2['s3']],
                                 [l3['s1'], l3['s2'], l3['s3']]])
                    u = Vector(l1['u'], l2['u'], l3['u'])
                    self.to_fractional = Translation(u)*Shear(s)
                    self.from_fractional = self.to_fractional.inverse()
                    del self._scale_matrix
            elif type[:-1] == 'MTRIX':
                if not hasattr(self, '_ncs_matrix'):
                    self._ncs_matrix = {}
                self._ncs_matrix[type[-1]] = data
                if type[-1] == '3': # last line read
                    from Scientific.Geometry.Transformation \
                         import Rotation, Translation
                    l1 = self._ncs_matrix['1']
                    l2 = self._ncs_matrix['2']
                    l3 = self._ncs_matrix['3']
                    m = N.array([[l1['m1'], l1['m2'], l1['m3']],
                                 [l2['m1'], l2['m2'], l2['m3']],
                                 [l3['m1'], l3['m2'], l3['m3']]])
                    v = Vector(l1['v'], l2['v'], l3['v'])
                    tr = Translation(v)*Rotation(Tensor(m))
                    tr.given = data['given']
                    tr.serial = data['serial']
                    self.ncs_transformations.append(tr)
                    del self._ncs_matrix
            elif type == 'MODEL':
                read = data['serial_number'] == self.model
                if self.model == 0 and len(self.residues) == 0:
                    read = 1
            elif type == 'ENDMDL':
                read = 0
            elif read:
                if type == 'ATOM' or type == 'HETATM':
                    alt = data['alternate']
                    if alt == '' or alt == self.alternate:
                        atom_data, residue_data, chain_data = \
                                   self.extractData(data)
                        if type == 'ATOM':
                            atom = apply(Atom, (), atom_data)
                        else:
                            atom = apply(HetAtom, (), atom_data)
                        new_chain = chain is None or \
                                    not chain.isCompatible(chain_data,
                                                           residue_data)
                        new_residue = new_chain or residue is None \
                                      or not residue.isCompatible(residue_data)
                        if new_residue and chain is not None and \
                           chain.isTerminated():
                            new_chain = 1
                        if new_residue:
                            residue = self.newResidue(residue_data)
                            if new_chain:
                                chain = self.newChain(residue, chain_data)
                            chain.addResidue(residue)
                        residue.addAtom(atom)
                elif type == 'ANISOU':
                    alt = data['alternate']
                    if alt == '' or alt == self.alternate:
                        if atom is None:
                            raise ValueError("ANISOU record before " +
                                              "ATOM record")
                        atom['u'] = data['u']
                elif type == 'TERM':
                    if chain is None:
                        raise ValueError("TERM record before chain")
                    chain = None

    def findSpaceGroupTransformations(self):
        if self.space_group is not None and self.to_fractional is not None:
            from Scientific.IO.PDBSpaceGroups import \
                 getSpaceGroupTransformations
            try:
                trs = getSpaceGroupTransformations(self.space_group)
            except KeyError:
                return
            for tr in trs:
                tr = self.from_fractional*tr*self.to_fractional
                self.cs_transformations.append(tr)

    def renumberAtoms(self):
        """
        Renumber all atoms sequentially starting with 1
        """
        n = 0
        for residue in self.residues:
            for atom in residue:
                atom['serial_number'] = n
                n = n + 1

    def __repr__(self):
        s = self.__class__.__name__ + "(" + repr(self.filename)
        if self.model != 0:
            s = s + ", model=" + repr(self.model)
        if self.alternate != 'A':
            s = s + ", alternate_code = " + repr(self.alternate)
        s = s + "):\n"
        for name, list in [("Peptide", self.peptide_chains),
                           ("Nucleotide", self.nucleotide_chains)]:
            for c in list:
                s = s + "  " + name + " chain "
                if c.segment_id:
                    s = s + c.segment_id + " "
                elif c.chain_id:
                    s = s + c.chain_id + " "
                s = s + "of length " + repr(len(c)) + "\n"
        for name, list in self.molecules.items():
            s = s + "  " + repr(len(list)) + " " + name + " molecule"
            if len(list) == 1:
                s = s + "\n"
            else:
                s = s + "s\n"
        return s

    def writeToFile(self, file):
        """
        Write everything to a file

        @param file: a PDB file object or a filename
        @type file: L{PDBFile} or C{str}
        """
        close = 0
        if type(file) == type(''):
            file = PDBFile(file, 'w')
            close = 1
        for o in self.objects:
            o.writeToFile(file)
        if close:
            file.close()

if __name__ == '__main__':

    if 0:

        file = PDBFile('~/3lzt.pdb')
        copy = PDBFile('test.pdb', 'w', 'xplor')
        while 1:
            type, data = file.readLine()
            if type == 'END':
                break
            copy.writeLine(type, data)
        copy.close()

    if 1:

        s = Structure('~/Programs/MMTK/main/MMTK/Database/PDB/insulin.pdb')
        print s
