#!/usr/bin/env python
#
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

import os
import re
import datetime
import urllib
import sys
import string

from xml.dom import minidom
from optparse import OptionParser

if map(int, string.split(string.split(sys.version)[0], ".")) < [2, 4, 0]:
    print ("subprocess is required for this tool and is not present in versions prior to 2.4.0")
    try:
        import subprocess
    except ImportError:
        print ("subprocess module not found please install it locally or upgrade your python version")
        sys.exit(1)

import subprocess
from subprocess import Popen

TOOL_NAME="buildCreator.py"

#Default Build script
DEFAULT_BUILD="build.config"

# Path locations
DEFAULT_ROOTDIR="builder"
SOURCE_DIR="src"
PATCH_DIR="patch"
BUILD_DIR="build"
RELEASE_DIR="release"

# Command Binaries
SVN='svn'
SVN_BIN='svn'
HTTP='http'
FTP='ftp'
WGET_BIN='wget'
FILE='file'
CP_BIN='cp'
PATCH_BIN='patch'
FILE_BIN='file'
LS_BIN='ls'
TAR_BIN='tar'
BZIP2_BIN='bzip2'
UNZIP_BIN='unzip'
ECHO_BIN='echo'
SVNVERSION_BIN='svnversion'



GZIP_DATA='gzip compressed data'
BZIP2_DATA='bzip2 compressed data'
ZIP_DATA='Zip archive data'
TAR_DATA='POSIX tar archive'
DIFF_FILE="'diff' output text"

#Build Targets
DISTCLEAN='distclean'
CLEAN='clean'
RETRIEVE='retrieve'
PREPARE='prepare'
PATCH='patch'
SHOWBUILDS='showbuilds'
BUILD='build'
RELEASE='release'
FULL='full'
HELP='help'
DEFAULT_TARGET=FULL

# XML Elements toplevel
BUILDER="builder"
ENVIRONMENT="environment"
SOURCES="sources"
SOURCE="source"
PATCHES="patches"
PATCH="patch"
BUILDS="builds"
INCLUDE="include"
DEPENDENCY='dependency'
TARGETS='targets'
SCRIPT='script'

# XML Elements - Source/Patch elements
NAME="name"
TYPE="type"
URL="url"
REVISION="revision"
ROOTDIR="root"
VERSION="version"
PREFIX='prefix'
PATH='path'

PATH_SEP=os.sep

_source=None
_target=DEFAULT_BUILD
_log = True
_verbose = False
_debug = False
_ignoreErrors = False
                         
_charIndex = 0
_waitingChars = ['-', '/' , '|',  '\\']

def showUsage():
    print TOOL_NAME+" [-c|--configure <config file>] [-v| --verbose] [-q|--quiet] [-i|--ignore-errors] [<build target>] [options]"
    print "Available Targets:"
    print "     distclean [source] - Remove all or specified retrieved source"
    print "     clean [source]     - Remove all or specified source build directory"
    print "     retrieve [source]  - Retrieve all or specified source"
    print "     prepare [source]   - Prepare all or specified source : i.e. extract archives"
    print "     patch [source]     - Patch all or specified source"
    print "     showbuilds         - List all builds"
    print "     build [build]      - Perform the build scripts for all or specified build"
    print "     release [build]    - Perform the release scripts for all or specified source"
    print "     full               - Perfrom clean, retrieve, prepare,  patch, build, release for all builds (DEFAULT)"

def main():
    global _log, _verbose, _debug, _rootDir, _target, _source, _baseConfiguration, _ignoreErrors

    # Load the
    parser = OptionParser()
    parser.add_option("-c", "--config", dest="config",
                      action="store", default=DEFAULT_BUILD,
                      help="set configuration file : default = " + DEFAULT_BUILD)

    parser.add_option("-v", "--verbose", dest="verbose",
                      action="store_true", default=False, help="enable verbose output")

    parser.add_option("-d", "--debug", dest="debug",
                      action="store_true", default=False, help="enable debug output")

    parser.add_option("-q", "--quiet", dest="quiet",
                      action="store_false", default=True, help="Enable quiet ouptut")

    parser.add_option("-i", "--ignore-errors", dest="ignoreErrors",
                      action="store_true", default=False, help="Ignore errors")


    (options, args) = parser.parse_args()

    _verbose = options.verbose
    _debug = options.debug
    _log = options.quiet
    _ignoreErrors = options.ignoreErrors

    log("Logging Enabled")
    verbose("Verbose Output Enabled")
    debug("Debug Enabled")

    if (len(args) > 2):
        showUsage()
        sys.exit(1)
    else:
        # NOTE : Would be good to be able to do builder.py clean build release
        if (len(args) > 0 ):
            # Override the default target
            _target = checkTarget(args[0])
            # limit the comand to just the specified source
            if (len(args) > 1 ):
                _source = args[1]
            else:
                _source = None
        else:
            _target = FULL


    _baseConfiguration = loadBaseConfiguration(options.config)

    debug ("Loading Environment")
    prepareEnvironment(_baseConfiguration.getElementsByTagName(ENVIRONMENT)[0])

    if _target == DISTCLEAN:
        distclean()

    if _target == CLEAN or _target == FULL:
        clean()

    if _target == RETRIEVE or _target == FULL:
        try:
            retrieve()
        except KeyboardInterrupt:
            log ("User Interrupted preparation")
            sys.exit(0)

    if _target == PREPARE or _target == FULL:
        prepare()

    if _target == PATCH or _target == FULL:
        patch()

    if _target == SHOWBUILDS:
        showBuilds()

    if _target == BUILD or _target == FULL:
        build()

    if _target == RELEASE or _target == FULL:
        release()

    log("Complete")

def checkTarget(target):

    if target == HELP:
        showUsage()
        sys.exit(0)                

    if target == DISTCLEAN:
        return DISTCLEAN

    if target == CLEAN:
        return CLEAN

    if target == RETRIEVE:
        return RETRIEVE

    if target == PREPARE:
        return PREPARE

    if target == PATCH:
        return PATCH

    if target == SHOWBUILDS:
        return SHOWBUILDS

    if target == BUILD:
        return BUILD

    if target == RELEASE:
        return RELEASE

    if target == FULL:
        return FULL

    warn("Target: '"+target+"' not valid")
    showUsage()
    sys.exit(1)


################################################################################
#
# Environment
#
################################################################################
def prepareEnvironment(env):
    global _rootDir

    rootdir = env.getElementsByTagName(ROOTDIR)
    if (rootdir.length > 0):
        _rootDir = getValue(rootdir[0])
    else:
        verbose ("Using default build dir: "+DEFAULT_ROOTDIR)
        _rootDir = os.getcwd() + PATH_SEP + DEFAULT_ROOTDIR

    if _rootDir == "":
        verbose (ROOTDIR+" value is empty. Please specify a value for "+ ROOTDIR)
        attemptExit(0)

    if (os.path.exists(_rootDir)):
        verbose ("Using Existing root dir: "+_rootDir)
    else:
        mkdir(_rootDir)

################################################################################
#
# Clean Methods
#
################################################################################
def clean():
    global _source
    sources = getSourceList()

    if len(sources) > 0:
        log ("Removing built code...")
        performed = False        
        for source in sources:
            if _source != None:
                if getName(source) == _source:
                    performed = True
                    removeDir(source, BUILD_DIR)
            else:
                removeDir(source, BUILD_DIR)

        if _source == None:
            deleteDir(_rootDir + PATH_SEP + BUILD_DIR)

    builds = getBuildList()
    if len(builds) > 0:
        log ("Removing built releases...")
        for build in builds:
            if _source != None:
                if getName(build) == _source:
                    performed = True
                    removeDir(build, RELEASE_DIR)
            else:
                removeDir(build, RELEASE_DIR)
        if _source == None:
            deleteDir(_rootDir + PATH_SEP + RELEASE_DIR)

    if _source != None:
        if not performed:
            fatal("No such source:" + _source);



def distclean():
    sources  = getSourceList()

    if len(sources) > 0:
        log ("Removing source...")
        for source in sources:
            if _source != None:
                if getName(source) == _source:
                    performed = True
                    removeDir(source, SOURCE_DIR)
            else:
                removeDir(source, SOURCE_DIR)

        if _source == None:
            deleteDir(_rootDir + PATH_SEP + SOURCE_DIR)

        log ("Removing built code...")
        for source in sources:
            if _source != None:
                if getName(source) == _source:
                    performed = True
                    removeDir(source, BUILD_DIR)
            else:
                removeDir(source, BUILD_DIR)
        if _source == None:
            deleteDir(_rootDir + PATH_SEP + BUILD_DIR)

    patches =getPatchList()
    if len(patches) > 0:
        log ("Removing patches...")
        for patch in patches:
            if _source != None:
                if getName(patch) == _source:
                    performed = True
                    removeDir(patch, PATCH_DIR)
            else:
                removeDir(patch, PATCH_DIR)
        if _source == None:
            deleteDir(_rootDir + PATH_SEP + PATCH_DIR)


    builds = getBuildList()
    if len(builds) > 0:
        log ("Removing built releases...")
        for build in builds:
            if _source != None:
                if getName(build) == _source:
                    performed = True
                    removeDir(build, RELEASE_DIR)
            else:
                removeDir(build, RELEASE_DIR)
        if _source == None:
            deleteDir(_rootDir + PATH_SEP + RELEASE_DIR)


    if _source == None:
        deleteDir(_rootDir)

    if _source != None:
        if not performed:
            fatal("No such source:" + _source);



def removeDir(source, rootdir):
    name = getName(source)
    deleteDir(_rootDir + PATH_SEP + rootdir + PATH_SEP + name)

################################################################################
#
# Retrieve Methods
#
################################################################################
def retrieve():
    global _source
    sources  = getSourceList()

    # Retreive Source
    performed=False
    if len(sources) > 0:
        log ("Retrieving source...")

        mkdir(_rootDir + PATH_SEP + SOURCE_DIR)

        for source in sources:
            if _source != None:
                if getName(source) == _source:
                    performed = True
                    downloadSource(source, SOURCE_DIR)
            else:
                downloadSource(source, SOURCE_DIR)

    # Retreive Patches
    patches = getPatchList()
    if len(patches) > 0:

        log ("Retrieving patches...")

        mkdir(_rootDir + PATH_SEP + PATCH_DIR)

        for patch in patches:
            if _source != None:
                if getName(patch) == _source:
                    performed = True
                    downloadSource(patch, PATCH_DIR)
            else:
                downloadSource(patch, PATCH_DIR)

    if _source != None:
        if not performed:
            fatal("No such patch:" + _source);


################################################################################
#
# Prepare Methods
#
################################################################################

def prepare():
    verbose("Prepare")

    mkdir(_rootDir + PATH_SEP + BUILD_DIR)

    sources = getSourceList()
    performed = False
    if len(sources) > 0:
        log ("Preparing source...")
        for source in sources:
            if _source != None:
                if getName(source) == _source:
                    log_no_newline("Preparing "+getName(source)+": ")
                    performed = True
                    postProcess(source, SOURCE_DIR)
            else:
                log_no_newline("Preparing "+getName(source)+": ")
                postProcess(source, SOURCE_DIR)
        if _source != None:
            if not performed:
                fatal("No such source:" + _source);

    patches = getPatchList()
    if len(patches) > 0:
        log ("Preparing patches...")
        for patch in patches:
            if _source != None:
                if getName(patch) == _source:
                    performed = True
                    log("Preparing "+getName(patch))
                    postProcess(patch, PATCH_DIR)
            else:
                log("Preparing "+getName(patch))
                postProcess(patch, PATCH_DIR)

        if _source != None:
            if not performed:
                fatal("No such patch:" + _source);


def postProcess(item, destination):
    name = getName(item)
    type = getType(item)

    verbose("Post Processing:"+name)

    targetdir = _rootDir + PATH_SEP + destination + PATH_SEP + name

    builddir = _rootDir + PATH_SEP + BUILD_DIR + PATH_SEP + name


    if type == SVN:
       # Do we want to perform an export?

       #extractcommand=SVN_BIN+" export "+ targetdir +" "+ builddir
       # Use -v just now so we can show progress 
       extractcommand=CP_BIN+" -rvf "+ targetdir +" "+ builddir

       runCommand(extractcommand, False)

    else:
        if type == FILE or type == HTTP or type == FTP:

            mkdir(builddir)

            # Look at all the files and see if they need unpacks
            for root, dirs, files in os.walk(targetdir, topdown=False):
                for file in files:
                    command = FILE_BIN+" "+root+PATH_SEP+file

                    result = Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                            stderr=subprocess.PIPE)
                    line = result.stdout.readline()
                    firstline=line
                    while (line != "" ):
                        # process nextline
                        line=result.stdout.readline()

                    result.wait()

                    if result.returncode != 0:
                        fatal("Download (" + name + ") contained unrecognized file type:"+ firstline)


                    extractcommand=""

                    if firstline.find(GZIP_DATA) != -1:
                        extractcommand=TAR_BIN+" -vxzf "+root+PATH_SEP+file+" -C " + builddir

                    if firstline.find(BZIP2_DATA) != -1:
                        extractcommand=TAR_BIN+" -vxjf "+root+PATH_SEP+file+" -C " + builddir

                    if firstline.find(ZIP_DATA) != -1:
                       extractcommand=ZIP_BIN+" -v "+root+PATH_SEP+file+" -d "+ builddir

                    if firstline.find(TAR_DATA) != -1:
                       extractcommand=TAR_BIN+" -vxf "+root+PATH_SEP+file+" -C "+ builddir

                    if firstline.find(DIFF_FILE) != -1 or firstline.find("text"):
                       extractcommand=CP_BIN+" -r "+root+PATH_SEP+file+" "+ builddir



                    if not extractcommand=="":
                        log_no_newline("Extracting archive:" + file+": " )
                        runCommand(extractcommand, False)
                    else:
                        fatal("Download (" + name + ") contained unsupported file type:"+ firstline)



################################################################################
#
# Patch Methods
#
################################################################################
def patch():

    # Retreive Patches
    patches= getPatchList()
    if len(patches) > 0:
        performed = False
        for patch in patches:
            if _source != None:
                if getName(source) == _source:
                    performed = True
                    applyPatch(patch)
            else:
                applyPatch(patch)

        if _source != None:
            if not performed:
                fatal("No such patch:" + _source);


def applyPatch(patch):
    global _rootDir

    name = getName(patch)
    type = getType(patch)
    source = getValue(patch.getElementsByTagName(SOURCE)[0])
    if (patch.getElementsByTagName(PREFIX).length > 0):
        prefix = getValue(patch.getElementsByTagName(PREFIX)[0])
    else:
        prefix = None

    if (patch.getElementsByTagName(PATH).length > 0):
        path= getValue(patch.getElementsByTagName(PATH)[0])
    else:
        path = None


    basecommand = PATCH_BIN

    if prefix != None:
        basecommand = basecommand + " -p "+prefix

    basecommand = basecommand + " -E -d "+ _rootDir + PATH_SEP + BUILD_DIR + PATH_SEP + source + PATH_SEP

    if path != None:
        basecommand = basecommand + path

    basecommand = basecommand + " < "

    patchSource= _rootDir + PATH_SEP + PATCH_DIR + PATH_SEP + name

    for root, dirs, files in os.walk(patchSource):
    	if '.svn' in dirs:
		dirs.remove('.svn')
        files.sort()	
        for patchName in files:
	                log("Applying patch '" + name + "'("+patchName+") to " + source)
        	        runCommandShowError(basecommand + patchSource + PATH_SEP + patchName)


################################################################################
#
# build Methods
#
################################################################################
def showBuilds():
    builds = getNamesfromBuildList(getBuildList())
    if len(builds) > 0:
        log("Available Builds:")
        for build in builds:
            log(" "+build)
    else:
        log("No builds available")

#
# Given a list of build elements extract the Name values and return as a list
#
def getNamesfromBuildList(list):
    names=[]
    for item in list:
        name = getName(item)
        if name != None:
            names.append(name)
    return names

def build():
    doBuildAction(BUILD)



################################################################################
#
# Release Methods
#
################################################################################
def release():
    log ("Releasing...")
    mkdir(_rootDir + PATH_SEP + RELEASE_DIR)

    builds = getBuildList()

    for build in builds:
        if _source != None:
            if getName(build) == _source:
                mkdir(_rootDir + PATH_SEP + RELEASE_DIR + PATH_SEP + getName(build))
        else:
            mkdir(_rootDir + PATH_SEP + RELEASE_DIR + PATH_SEP + getName(build))
                    
    doBuildAction(RELEASE)



################################################################################
#
# Build Helper Methods
#
################################################################################

def doBuildAction(action):
    config = _baseConfiguration

    if len(getSourceList()) > 0:
        log("Performing "+ action.title() +"...")

        builds = getBuildList()

        performed = False
        for build in builds:
            if _source != None:
                if getName(build) == _source:
                    performed = True
                    performScript(build , action)
            else:
                performScript(build, action)

        if _source != None:
            if not performed:
                fatal("No such build:" + _source);


def performScript(build, scriptName):
    name = getName(build)

    checkDependencies(build)

    verbose("Running "+scriptName+":"+name)

    targets = build.getElementsByTagName(TARGETS)

    if targets.length > 0:
        target = targets[0].getElementsByTagName(scriptName)

        if target.length > 1:
            fatal("More than one build target specified")

        if target.length == 0:
            fatal("No build target specified")

        script = getValue(target[0].getElementsByTagName(SCRIPT)[0])

        script = peformSubstitutionsInScript(build, script)

        runScript(script)

    else:
        fatal("Build "+name+" has no build targets")


def checkDependencies(build):
    name = getName(build)
    dependencies = build.getElementsByTagName(DEPENDENCY)

    if dependencies > 0:
        for dependency in dependencies :
            sources = dependency.getElementsByTagName(SOURCE)
            if sources.length == 0:
                fatal("No sources specified in dependency block for build:"+name)
            else:
                for source in sources:
                    sourceDependency = getValue(source)
                    if not (sourceDefined(sourceDependency)):
                        fatal("Unable to build "+name+" as specifed dependency("+sourceDependency +") is not available")

def sourceDefined(name):
    for source in getSourceList():
        sourcename = getValue(source.getElementsByTagName(NAME)[0])
        if sourcename == name:
            return True
    return False


def runScript(script):
    (returncode, stdout, stderr) = runCommandWithOutput(script)

    if returncode != 0:
        for line in stdout:            
                warn(line)
        for line in stderr:            
                warn(line)

        warn("Script Failed")

        attemptExit(1)
        

################################################################################
#
# XML Helper Methods
#
################################################################################

def loadBaseConfiguration(config):
    log ("Loading configuration:" + config)
    full = minidom.parse(config)
    return full.getElementsByTagName(BUILDER)[0]

#
# Assumes that we have a <node>text</node> element and returns the text value.
#
def getValue(node):
    if node.childNodes.length > 0:
        return node.childNodes[0].data
    else:
        return ""

def getEnvironment():
    env = _baseConfiguration.getElementsByTagName(ENVIRONMENT)
    if env.length > 0:
        return env[0]
    else:
        return None

#
# Returns the value of the NAME element contained in the specified item
#
def getName(item):
    name = item.getElementsByTagName(NAME)
    if name.length > 0:
        return getValue(name[0])
	
#
# Returns the value of the TYPE element contained in the specified item
#
def getType(item):
    type = item.getElementsByTagName(TYPE)	
    if type.length > 0:
        return getValue(type[0])
	    
#
# Returns the value of the URL element contained in the specified item
#
def getURL(item):
    url = item.getElementsByTagName(URL)	
    if url.length > 0:
        return getValue(url[0])

#
# Return the list of sources in this build configuration
# If no sources are available then this is logged as a fatal error.
#
def getSourceList():
    config = _baseConfiguration
    sourceCount = config.getElementsByTagName(SOURCES).length
    if sourceCount > 0:
        return config.getElementsByTagName(SOURCES)[0].getElementsByTagName(SOURCE)
    else:
        fatal("No source elements defined.")
#
# Return the list of patches in this build configuration
#
def getPatchList():
    config = _baseConfiguration
    patchCount = config.getElementsByTagName(PATCHES).length
    if patchCount > 0:
        return config.getElementsByTagName(PATCHES)[0].getElementsByTagName(PATCH)
    else:
        return []

# Returns a list of build elements including any any included build files
# Currently nested build elements are not supported so all builds must be specified via the <include> tag.
#
def getBuildList():
    config = _baseConfiguration

    builds = config.getElementsByTagName(BUILDS)
    buildcount = builds.length

    if buildcount > 0:
        build = builds[0]
        useInclude = build.getElementsByTagName(INCLUDE).length > 0

        # If we are using includes then build a list of all the files we need to include
        if useInclude:
            return  getIncludeList(build)

        else:
           warn("Nested builds not currently supported")
    else:
        fatal("No Builds defined in config")        

#
# Look at all <include> values in the given element and return the list of inlcudes
#
def getIncludeList(build):
    includelist=[]
    for include in build.getElementsByTagName(INCLUDE):
        for item in getIncludeValue(getValue(include)):
            includelist.append(item)

    return includelist

#
# Process in the given include value.
# This is done by performing `ls <include>`
# This means includes such as 'builds/*.config' will match multiple includes and return all entries
#
# Any error in performing the ls is printed and the tool exits (unless ignore errors)
#
def getIncludeValue(include):
    debug("Loading Includes:"+include+" ")    

    command = LS_BIN+" "+include
    (returncode, stdout, stderr) = runCommandWithOutput(command)

    if returncode == 0:
        values=[]
        for line in stdout:
            include = loadIncludeFile(line)
            if not include == None:
                values.append(include)
        return values
    else:
        for line in stderr:
            warn(line)
        attemptExit(1)

#
# Given a file name parse the XML. Any trailing '\n's that the ls command may have added are removed here.
# The file is checked to ensure that it is a <builds> file
# The first <build> element is returned.
#
#  TODO: Allow multiple builds per file.
#
def loadIncludeFile(file):
     buildFile = minidom.parse(file.rstrip('\n'))

     builds = buildFile.getElementsByTagName(BUILDS)

     if builds.length != 1:
        warn("Build Configuration does not contain any <"+BUILDS+"> definitions")
     else:
        buildElements = builds[0].getElementsByTagName(BUILD)
        if not buildElements.length > 0:
            warn("Build Configuration does not contain any <"+BUILD+"> definitions")
        else:
            if buildElements.length > 0:
                build = buildElements[0]
                # getElementsByTagName is recursive so this will pick up the sub element build
                # Only use the first element
                namecount = build.getElementsByTagName(NAME).length
                if namecount > 0:
                    return build
                else:
                    return None

#
# Given the build target and a script substitute $value entries in script for values in
# the Environment
# the Source entries <source><name>
# the build <build><name>
# the release location : _rootDir + PATH_SEP + RELEASE_DIR + PATH_SEP + buildName
#
def peformSubstitutionsInScript(build, script):
    buildName = getName(build)
    sources = getSourceList()

    #Replace Build name
    script = script.replace("$build", buildName)

    #Replace release directory
    releaseDir = _rootDir + PATH_SEP + RELEASE_DIR + PATH_SEP + buildName            
    script = script.replace("$release", releaseDir)

    # Replace Source varables
    for source in sources:
        sourceName = getName(source)

        search = "$"+sourceName

        sourcePath = source.getElementsByTagName(PATH)

        replacement = _rootDir + PATH_SEP + BUILD_DIR + PATH_SEP + sourceName
        if sourcePath.length > 0:
            replacement = replacement + PATH_SEP + getValue(sourcePath[0])

        script = script.replace(search,replacement)

    # Take values from the environment script for replacement
    env = getEnvironment()
    if env != None:
        for item in env.childNodes:
            if item.nodeType == 1:

                search = "$"+item.tagName
                replace = item.childNodes[0].data

                script = script.replace(search,replace)
		
    # Perform keyword substitution replacements
    # Currently only one substitution exists so for simplisity fix it here    
    writeVersionSubstitution = script.find("$writeVersions")
    if writeVersionSubstitution != -1:
    	
	#Extract Filename
        fileNameStart = script.find("(",writeVersionSubstitution)
        fileNameEnd = script.find(")",fileNameStart)	
	fileName= script[fileNameStart+1:fileNameEnd]
	
	substitution = createVersionSubstitution(build, fileName)
		
	script = script.replace("$writeVersions(" + fileName + ")", substitution)		
	

    return script     

################################################################################
#
# Keyword Substitutions
#
################################################################################
   
#
# Use the specified build as to lookup all associated source/patches and write out their details
# to the specified file using shell redirects. redirects are to be used as the absolute filename 
# location may not be known as the name comes in via the release script
#
def createVersionSubstitution(build, filename):
    substitution = ""
    sources = getSourceList();

    dependencies = build.getElementsByTagName(DEPENDENCY)
	
    if dependencies > 0:
        substitution += "\n echo 'Source Version Information:'>> " + filename	
        for dependency in dependencies :
            depSources = dependency.getElementsByTagName(SOURCE)
            # Can assume we have dependencies as we would have failed before now
            for source in depSources:
                sourceDependency = getValue(source)	
		# We can assume source is valid.		
		for s in sources:
		    if sourceDependency == getName(s):		    	
		    	# provide header <source>:<type>:<revision>
			substitution += "\n " + ECHO_BIN + " -n '" + sourceDependency + ":" \
			         + getType(s) + ":' >> " + filename
			substitution += "\n" + getVersionCommand(s) + " >>" + filename
			# Add Source URL to Revisions file
			url = getValue(s.getElementsByTagName(URL)[0])
			substitution += "\n" + ECHO_BIN + " \"URL:" + url + "\" >> "+filename
			# Add Patches applied to this source to revisions file
			substitution += addPatchVersions(s, filename)
						   	    	     
    return substitution
    
#
# Use the specified source as to lookup all associated patches and write their details out the 
# the specified file using shell redirects. redirects are to be used as the absolute filename 
# location may not be known as the name comes in via the release script
#
def addPatchVersions(source, filename):
    substitution = ""
    
    patches = getPatchList()
    
    sourceName = getName(source)
	
    for patch in patches:
    	patchSourceName = getValue(patch.getElementsByTagName(SOURCE)[0])

        if sourceName == patchSourceName:
	    type = getType(patch)
	    substitution += "\n" + ECHO_BIN + " \"\t"+getName(patch)+":"+type + "\" >> "+filename
	    url = getValue(patch.getElementsByTagName(URL)[0])
	    substitution += "\n" + ECHO_BIN + " \"\t\tURL:" + url + "\" >> "+filename
	    if (type == SVN):
		    if (patch.getElementsByTagName(REVISION).length > 0):
                        substitution += "\n" + ECHO_BIN + " \"\t\tREVISION:"+ \
			         getValue(patch.getElementsByTagName(REVISION)[0])  + "\" >> " + filename	    
		    else:			
                        substitution += "\n" + ECHO_BIN + " -n \"\t\tREVISION: \"  >> " + filename
			substitution += "\n" + SVNVERSION_BIN + " " + _rootDir + PATH_SEP + PATCH_DIR + PATH_SEP + getName(patch) + " >> " + filename	    

			
	    if (patch.getElementsByTagName(PREFIX).length > 0):
		substitution += "\n" + ECHO_BIN + " \"\t\tPREFIX: " + \
		         getValue(patch.getElementsByTagName(PREFIX)[0]) + "\" >> " + filename
			 
	    if (patch.getElementsByTagName(PATH).length > 0):
   		substitution += "\n" + ECHO_BIN + " \"\t\tPATH: " + \
		         getValue(patch.getElementsByTagName(PATH)[0]) + "\" >> " + filename 
	    
	    global _rootDir
	    patchSource= _rootDir + PATH_SEP + PATCH_DIR + PATH_SEP + getName(patch)
	
	    #
	    # Include the list of patches files applied
	    #
	    for root, dirs, files in os.walk(patchSource):
    		if '.svn' in dirs:
			dirs.remove('.svn')
	        files.sort()	
        	for patchName in files:
        	        substitution += "\n" + ECHO_BIN + " \"\t\tFILE: " + patchName + "\" >> " + filename 
 
    
	   
		
    if (substitution != ""):
	return "\n" + ECHO_BIN + " \"\tPatches applied to " + sourceName + ":\" >> " + filename + substitution 
    else:
    	return "\n" + ECHO_BIN + " \"\tNo Patches\" >> " + filename 

      
#
# Given a source entry return the command that will provide the current version 
# of that source.
# i.e. svn source : svnversion <path to source>
#     http source : echo <URL>
#      
def getVersionCommand(source):
    global _rootDir
    type = getType(source)	
	
    versionCommand=""
    
    if type == SVN:
        versionCommand=SVNVERSION_BIN+" "+_rootDir + PATH_SEP + SOURCE_DIR + PATH_SEP + getName(source)
    else:
        if type == FILE or type == HTTP or type == FTP:
	    versionCommand = ECHO_BIN +" " + getURL(source)
	
	
    return versionCommand	

################################################################################
#
# Download Helper Methods
#
################################################################################

#
# Download the item specified in source to the given destintation
#
def downloadSource(source, destination):
    name = getName(source)
    type = getType(source)
    url =  getValue(source.getElementsByTagName(URL)[0])
    log ( "Retrieving "+ name + "("+ type +")")

    targetdir=_rootDir + PATH_SEP + destination + PATH_SEP + name

    command = ""

    mkdir(targetdir)

    if (os.listdir(targetdir)==[]):

        # Setup command for a fresh checkout
        if (type == SVN):
            command = SVN_BIN+" co "+url+" "+targetdir
            if (source.getElementsByTagName(REVISION).length > 0):
                revision =  getValue(source.getElementsByTagName(REVISION)[0])
		command = SVN_BIN+" co -r"+revision+" "+url+" "+targetdir
        else:
            if (type == HTTP):
                command = WGET_BIN+" --no-directories -P "+targetdir+" "+url
            else:
                if (type == FILE):
                    if url.startswith(HTTP):
                        command = WGET_BIN+" -P "+targetdir+" "+url
                    else:
                        if url.startswith(FTP):
                            command = WGET_BIN+" -P "+targetdir+" "+url
                        else:
                            command = CP_BIN+" "+url+" "+targetdir
    else:
        warn("Target directory(" + targetdir + ") is not empty please ensure contents are valid or run 'clean "+name+"'")

    verbose("Executing:"+command)
    log_no_newline("Retrieving "+source.nodeName+": ")

    if (type == FILE):
        runCommand(command, True)
    else:
        runCommand(command, False)

################################################################################
#
# Command Helper Methods
#
################################################################################

#
# Run command and print out last 20 lines of data on error
#
def runCommandShowError(command):
    last20 = runCommand(command, False)
    if last20 != None:
        lines=last20[0]
        lines=lines + 1
        current = 1
        while current != lines:
            log (last20[current])
            current = current + 1
        attemptExit(1)

#
# Runs the given command if showOutput is true then stdout/stderr is shown on screen
# other wise the last 20 lines of output is gathered:
#
# As command runs progress is shown
#
# return array [0] = no of elements in array. Array is fixed size 21 elements but not all are used. FIXME: this is poor
#
# TODO: Current mechanism for limiting to 20 lines is poor, potential to replace usages of this
#       method with runCommandWithOutput below
#
def runCommand(command, showOutput):
    debug("Running Command:"+command)
    try:
        if showOutput:
            # Process that shows the output
            result = Popen(command, shell=True)
        else:
            # consume the output ourselves
            result = Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)

            index=0
            last20=[""] * 21
            line = result.stdout.readline()
            while (line != "" ):
                logWaiting()

                #Record last 20 lines of output
                index = index + 1
                if index == 20:
                    index = 1

                last20[index]=line

                # process nextline
                line = result.stdout.readline()

            #
            # If we didn't get any standard or fill our buffers then fill the end of the buffer with any stderr
            #
            if index == 0 | index < 15 :
                line = result.stderr.readline()

                if index != 0:
                    index = index + 1
                    if line != "":
                        last20[index]="STDERR"
                reset = index
                while (line != "" ):
                    logWaiting()

                    #Record last 20 lines of output
                    index = index + 1
                    if index == 20:
                        index = reset

                    last20[index]=line

                    # process nextline
                    line = result.stderr.readline()

        result.wait()

        if result.returncode == 0:
            logWaitingDone()
        else:
            logWaitingFailed("Failed")
	    attemptExit(1)
            if not showOutput:
                last20[0]=index
                return last20

        return None

    except IOError:
        logWaitingFailed ("Error running command.")
        attemptExit(1)

#
# Runs the given command if showOutput is true then stdout/stderr is shown on screen
# Stdout and stderr is gathered up and returned with error code.
#
# return (result.returncode, stdout, stderr)
#
# As command runs progress is shown
#
def runCommandWithOutput(command):

    result = Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)


    # Retrieve STDOUT
    stdout=[]
    line = result.stdout.readline()
    while (line != "" ):
        logWaiting()

        stdout.append(line)

        # process nextline
        line = result.stdout.readline()

    line = result.stderr.readline()

    # Retrieve STDERR
    stderr=[]
    while (line != "" ):
        stderr.append(line)

        # process nextline
        line = result.stderr.readline()

    result.wait()

    logWaitingClear()

    return (result.returncode, stdout, stderr)


################################################################################
#
# OS Helper Methods
#
################################################################################

#
# Check _ignoreErrors value and exit if false
#
def attemptExit(code):
    if not _ignoreErrors:
        sys.exit(code)
    else:
	print ("Ignoring Errors")

#
# Check that the required binaries are present for this tool.
# Only checks the minimum set.
# Logs warning if archive tools are missing
#
def checkSystemRequirements():
    exists = checkExists(SVN_BIN)
    exists = exists & checkExists(WGET_BIN)
    exists = exists & checkExists(CP_BIN)
    exists = exists & checkExists(PATCH_BIN)
    exists = exists & checkExists(FILE_BIN)

    if not checkExists(TAR_BIN):
        warn("Unable to process tar files as tar binary does not exist:" + TAR_BIN)
    if not checkExists(BZIP2_BIN):
        warn("Unable to process bzip2 files as bzip2 binary does not exist:" + BZIP2_BIN)
    if not checkExists(UNZIP_BIN):
        warn("Unable to process zip files as unzip binary does not exist:" + UNZIP_BIN)

    if not exists:
        sys.exit(1)

#
# Helper that checks for files existence
#
def checkExists(command):
    debug_no_newline("Checking for "+command+":")
    command = LS_BIN+" "+command

    result = Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    line = result.stdout.readline()

    while (line != "" ):
        # process nextline
        line = result.stdout.readline()

    result.wait()

    if result.returncode == 0:
        debug("OK")
        return True
    else:
        debug("Missing")
        warn("Missing dependancy:"+command)
        return False


# Delete everything reachable from the directory named in 'top',
# assuming there are no symbolic links.
#
# If an attempt to delete '/' is  performed this is logged as a fatal error
#
def deleteDir(top):
    if top == '/':
        fatal("Exiting as attempt to delete '/' occured.")
    else:
        if (os.path.exists(top)):
            log_no_newline("Removing:"+top+". ")
            for root, dirs, files in os.walk(top, topdown=False):
                logWaiting()
                for name in files:
                    os.remove(os.path.join(root, name))
                for name in dirs:
                    logWaiting()
                    os.rmdir(os.path.join(root, name))

            logWaiting()
            os.rmdir(top)

            logWaitingDone()

def mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)


################################################################################
#
# Logging Helper Methods
#
################################################################################

#
# Provide a spinning -/|\
#
def logWaiting():
    global _charIndex, _waitingChars

    _charIndex = (_charIndex + 1) % len(_waitingChars)

    log_no_newline('\b')
    log_no_newline(_waitingChars[_charIndex])

#
# Clear the logWaiting symbol and end the line with ' Done'
#
def logWaitingDone():
    log_no_newline('\b')
    log(" Done")

#
# Clear the logWaiting symbol
#
def logWaitingClear():
        log_no_newline('\b')

#
# Clear the logWaiting symbol and end line with messsage
#
def logWaitingFailed(message):
    log_no_newline('\b')
    log(" "+message)

def debug(string):
    if _debug:
        log(string)

def verbose(string):
    if _verbose:
        log(string)

def log (string):
    if _log:
        print string

def warn (string):
    print string

def fatal(string):
    print string
    attemptExit(1)

def log_no_newline (string):
    if _log:
        sys.stdout.write(string)
        sys.stdout.flush()

def verbose_no_newline (string):
    if _verbose:
        sys.stdout.write(string)
        sys.stdout.flush()

def debug_no_newline (string):
    if _debug:
        sys.stdout.write(string)
        sys.stdout.flush()

if __name__ == "__main__":
    main()
