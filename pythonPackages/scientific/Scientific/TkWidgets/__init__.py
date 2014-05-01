# Package Scientific.TkWidgets

"""
@undocumented: Utility*
"""

from Utility import FilenameEntry, FloatEntry, IntEntry, ButtonBar, StatusBar
from Utility import ModalDialog

import sys
if sys.modules.has_key('epydoc'):
    import Utility
    utility_name = Utility.__name__
    FilenameEntry.__module__ = 'Scientific.TkWidgets'
    FloatEntry.__module__ = 'Scientific.TkWidgets'
    IntEntry.__module__ = 'Scientific.TkWidgets'
    ButtonBar.__module__ = 'Scientific.TkWidgets'
    StatusBar.__module__ = 'Scientific.TkWidgets'
    ModalDialog.__module__ = 'Scientific.TkWidgets'
    Utility.__name__ = utility_name
    del Utility
    del utility_name
del sys
