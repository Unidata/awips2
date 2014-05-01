# A dictionary with default values for non-existing entries

"""
Dictionary with default values

Note: this module has become obsolete by the introduction of the get method
for standard Python dictionaries. It is maintained only in order not to
break old code that uses it.
"""

import UserDict, copy

class DictWithDefault(UserDict.UserDict):

    """
    Dictionary with default values

    Instances of this class act like standard Python dictionaries,
    except that they return a *copy* of |default| for a key that
    has no associated value.
    """

    def __init__(self, default):
        """
        @param default: the default value that is returned for a key that
                        has no associated value
        """
        self.data = {}
        self.default = default
        UserDict.UserDict.__init__(self)

    def __getitem__(self, key):
        """
        @param key: the key whose associated value is requested
        @returns: the associated value. If none is defined, the return
                  value is a copy of the default value.
        """
        try:
            item = self.data[key]
        except KeyError:
            item = copy.copy(self.default)
            self.data[key] = item
        return item

    def __delitem__(self, key):
        try:
            del self.data[key]
        except KeyError:
            pass
