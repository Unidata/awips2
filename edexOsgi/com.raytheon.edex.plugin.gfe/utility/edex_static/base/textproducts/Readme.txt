
Running configureTextProducts.py
--------------------------------

        Before running configureTextProducts ensure Python is properly installed.

        Try `configureTextProducts.py --help' for help on running 
        configureTextProducts

Enable Detailed Logging
-----------------------

        To enable logging to a file or change the format of the log output see
        the 'preferences\logging.conf' file.

Important Files and Directories
-------------------------------

        The following directories and files are important to the running of 
        configureTextProducts.py

        \library
                \SiteInfo.py
                \afos2awips.txt
        \preferences
                \logging.config
                \configureTextProducts.py
        \templates

Moving or Renaming Directories
------------------------------

        Alternate afos2awips and template locations can be temporarily specified
        through command line options.  For a more permanent change update the 
        'Standard Paths' section of the configureTextProducts.py script.

        When permanently moving files there are a couple of things to keep in 
        mind:

        1. The 'SiteInfo.py' and 'configureTextProducts.py' files should _not_
           be renamed or put into directories other than those with the names of 
           'preferences' or 'library'.
           
        2. The 'preferences' and 'library' directories must contain the file
           '__init__.py'
