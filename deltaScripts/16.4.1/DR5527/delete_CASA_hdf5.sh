#!/bin/bash
# DR5527 Removing CASA hdf5-root-folder from /awips2/edex/data/hdf5

cd /awips2/edex/data/hdf5/
rm -rf nswrc_radial/

echo "Removal of CASA 'nswrc_radial' hdf5 folder complete."