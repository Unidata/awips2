#! /bin/bash

# Wrapper for TafDecoderTestsA2.py that sets $LD_LIBRARY_PATH and $PATH

# Query the system to see if awips2-python rpm is installed
pypath="/awips2/python"

# Check the return code
if [ ! -d /awips2/python ]
then
    echo "Please install the awips2-python rpm."
    exit 1
fi

# Add correct python to the PATH
echo "Adding $pypath/bin to PATH"
export PATH=$pypath/bin:$PATH

# Add correct python to the LD_LIBRARY_PATH
echo "Adding $pypath/lib to LD_LIBRARY_PATH"
export LD_LIBRARY_PATH=$pypath/lib:$LD_LIBRARY_PATH

# Run TafDecoderTestsA2.py
echo "Running TafDecoderTestsA2.py..."
python python/TafDecoderTestsA2.py
