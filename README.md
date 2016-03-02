# Install AWIPS II

These scripts should be run as *root*:

## CAVE Client

Download and run `installCAVE.sh`

        wget https://raw.githubusercontent.com/Unidata/awips2/unidata_15.1.1/installCAVE.sh
        chmod 755 ./installCAVE.sh
        ./installCAVE.sh

## EDEX Server

Download and run `installEDEX.sh`:

        wget https://raw.githubusercontent.com/Unidata/awips2/unidata_15.1.1/installEDEX.sh
        chmod 755 ./installEDEX.sh
        ./installEDEX.sh

## What do these scripts do?

1. Download `http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo` to `/etc/yum.repos.d/awips2.repo`

2. Yum clean all

3. Yum groupinstall (awips2-cave|awips2-server)
