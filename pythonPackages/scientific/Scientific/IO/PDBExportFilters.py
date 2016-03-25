# Export filters for PDB output.
#

#
# A convenient base class...
#
class PDBExportFilter:

    def processLine(self, type, data):
        return type, data

    def processResidue(self, name, number, terminus):
        return name, number

    def processChain(self, chain_id, segment_id):
        return chain_id, segment_id

    def terminateChain(self):
        pass

#
# XPlor export filter

import string

class XPlorExportFilter(PDBExportFilter):

    xplor_atom_names = {' OXT': 'OT2'}

    def processLine(self, type, data):
        if type == 'TER':
            return None, data
        if type == 'ATOM' or type == 'HETATM' or type == 'ANISOU':
            name = self.xplor_atom_names.get(data['name'], data['name'])
            data['name'] = name
        return type, data


export_filters = {'xplor': XPlorExportFilter}
