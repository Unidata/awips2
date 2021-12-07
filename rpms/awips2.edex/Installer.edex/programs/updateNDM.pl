#!/usr/bin/perl

#This script gets installed with EDEX and a cronjob is setup to run this script daily. This script initially sets up a local git repo and is configured to just checkout the National Dataset files from Unidata's public git repo and then copies the files to /awips2/edex/data/ndm to keep the databases updated with new metar/station information.

$ndmGitPath = "rpms/awips2.edex/Installer.edex/ndm/";

#get the current branch/version of awips2
$version = `grep AWIPSII_VERSION /awips2/repo/awips2/build/buildEnvironment.sh`;
chomp $version;
@split = split(/"/,$version);

$branch = "unidata_$split[1]";

#Check if ndm location has been created
$ndmDir="/awips2/dev/ndm";
if(!-d $ndmDir)
{
  `mkdir $ndmDir`;
}

#Check if ndm git files have been checked out yet
if(!-e "$ndmDir/.git")
{
  `cd $ndmDir ; 
   git init ; 
   git remote add -f origin https://github.com/Unidata/awips2.git ; 
   git config core.sparseCheckout true ;
   echo \"$ndmGitPath\" >> .git/info/sparse-checkout ;
   git checkout -t origin/$branch
  `;
}

#Pull latest files and copy them to the AWIPS-II NDM endpoint
`cd $ndmDir ; 
 git fetch origin ;
 git reset --hard origin/$branch ;
 rsync -aP $ndmDir/$ndmGitPath /awips2/edex/data/ndm/
`;

