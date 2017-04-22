#! /bin/bash

# This script
#    1) Ensures existence of a source-distribution in the download area;
#    2) Modifies the package's website; and
#    3) Copies all binary-repositories to the publicly-visible repository
#       computer.
#
# The package is built in the current working directory and installed under a
# temporary directory in order to extract the documentation.
#
# Usage:
#     $0 reposRoot
#
# where:
#       reposRoot       Path to the root-directory of all binary repositories.
#                       May be absolute or relative to the current working
#                       directory. For example, "/opt/tomcat/repos".

set -e  # exit on failure

reposRoot=${1:?Root-directory of all binary repositories not specified}

# Get the static release variables.
#
. ./release-vars.sh

# Ensure that the path to the root-directory of all binary repositories is
# absolute.
#
echo $reposRoot | grep -qv '^/' && reposRoot=`pwd`/$reposRoot

# Ensure that the source repository has the source distribution,
#
if ! ssh $SOURCE_REPO_HOST test -e $ABSPATH_SOURCE_DISTRO; then
    trap "ssh $SOURCE_REPO_HOST rm -f $ABSPATH_SOURCE_DISTRO; `trap -p ERR`" ERR
    scp $SOURCE_DISTRO_NAME $SOURCE_REPO_HOST:$ABSPATH_SOURCE_DISTRO
fi

# Purge the source-repository of bug-fix versions that are older than the latest
# corresponding minor release.
#
ssh -T $SOURCE_REPO_HOST bash --login <<EOF
    set -ex # Exit on error
    cd $ABSPATH_SOURCE_REPO_DIR        
    ls -d $PKG_ID_GLOB |
        sed "s/$PKG_NAME-//" |
        sort -t. -k 1nr,1 -k 2nr,2 -k 3nr,3 |
        awk -F. '\$1!=ma||\$2!=mi{print}{ma=\$1;mi=\$2}' >versions
    for vers in \`ls -d $PKG_ID_GLOB | sed "s/$PKG_NAME-//"\`; do
        fgrep -s \$vers versions || rm -rf $PKG_NAME-\$vers
    done
EOF

# Install the package in order to obtain the documentation.
#
prefix=/tmp/$PKG_ID
./configure --prefix=$prefix >configure.log 2>&1
trap "rm -rf $prefix; `trap -p EXIT`" EXIT
make install install-info install-html >install.log 2>&1

# Copy the documentation to the package's website.
#
versionWebDirTmp=$ABSPATH_VERSION_WEB_DIR.tmp
ssh -T $WEB_HOST rm -rf $versionWebDirTmp
trap "ssh -T $WEB_HOST rm -rf $versionWebDirTmp; `trap -p ERR`" ERR
scp -Br $prefix/share $WEB_HOST:$versionWebDirTmp
ssh -T $WEB_HOST rm -rf $ABSPATH_VERSION_WEB_DIR
ssh -T $WEB_HOST mv $versionWebDirTmp $ABSPATH_VERSION_WEB_DIR

# On the web host,
#
ssh -T $WEB_HOST bash --login <<EOF
    set -ex # exit on error

    # Go to the home directory of the package.
    #
    cd $ABSPATH_PKG_WEB_DIR

    # Allow group write access to all created files.
    #
    umask 02

    # Copy the change-log to the home-directory of the website of the package.
    #
    cp $PKG_ID/doc/$PKG_NAME/CHANGE_LOG .

    # Set the hyperlink references in the top-level HTML file. For a given
    # major and minor version, keep only the latest bug-fix.
    #
    ls -d $PKG_NAME-*.*.* |
        sed "s/$PKG_NAME-//" |
        sort -t. -k 1nr,1 -k 2nr,2 -k 3nr,3 |
        awk -F. '\$1!=ma||\$2!=mi{print}{ma=\$1;mi=\$2}' >versions
    sed -n '1,/$BEGIN_VERSION_LINKS/p' index.html >index.html.new
    for vers in \`cat versions\`; do
        href=\`find $PKG_NAME-\$vers -name udunits2.html\`
        test "\$href" || href=\`find $PKG_NAME-\$vers -name udunits2.html\`
        echo "            <li><a href=\"\$href\">\$vers</a>" \
            >>index.html.new
    done
    sed -n '/$END_VERSION_LINKS/,\$p' index.html >>index.html.new
    cp index.html index.html.old
    mv index.html.new index.html

    # Delete all versions not referenced in the top-level HTML file.
    #
    for vers in \`ls -d $PKG_NAME-*.*.* | sed "s/$PKG_NAME-//"\`; do
        fgrep -s \$vers versions || rm -rf $PKG_NAME-\$vers
    done

    # Adjust the symbolic link to the current version.
    #
    rm -f $PKG_NAME-current
    ln -s $PKG_ID $PKG_NAME-current
EOF

# Copy the binary-repositories to the publicly-accessible repository
# computer.
#
cd $reposRoot
flock . rsync --archive --relative --delete * $WEB_HOST:$ABSPATH_WEB_ROOT/repos