##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
# Library for accessing localization files from python.
#
# SOFTWARE HISTORY
#
# Date      Ticket#  Engineer  Description
# --------- -------- --------- --------------------------
# 08/09/17  5731     bsteffen  Initial Creation.


import urllib2 
from json import load as loadjson
from xml.etree.ElementTree import parse as parseXml
from base64 import b64encode
from StringIO import StringIO
from getpass import getuser
import dateutil.parser
import contextlib
import os
from urlparse import urlunparse, urljoin 

NON_EXISTENT_CHECKSUM = 'NON_EXISTENT_CHECKSUM'
DIRECTORY_CHECKSUM = 'DIRECTORY_CHECKSUM'

class LocalizationFileVersionConflictException(Exception):
    pass

class LocalizationFileDoesNotExistException(Exception):
    pass

class LocalizationFileIsNotDirectoryException(Exception):
    pass

class LocalizationContext(object):
    """A localization context defines the scope of a localization file.
    
    For example the base localization context includes all the default files
    installed with EDEX, while a particular user context has custom files for
    that user.
    
    A localization context consists of a level and name. The level defines what
    kind of entity this context is valid for, such as 'base', 'site', or 'user'.
    The name identifies the specific entity, for example the name of a 'user'
    level context is usually the username. The 'base' level does not have a name
    because there cannot be only one 'base' context.
    
    Attributes:
        level: the localization level
        name: the context name
    """
    def __init__(self, level="base", name=None, type="common_static"):
        if level != "base":
            assert name is not None
        self.level = level
        self.name = name
        self.type = type
    def isBase(self):
        return self.level == "base"
    def _getUrlComponent(self):
        if self.isBase():
            return self.type + '/' + "base/"
        else:
            return self.type + '/' + self.level + '/' + self.name + '/'
    def __str__(self):
        if self.isBase():
            return self.type + ".base"
        else:
            return self.type + "." + self.level + "." + self.name
    def __eq__(self, other):
        return self.level == other.level and \
               self.name == other.name and \
               self.type == other.type    
    def __hash__(self):
        return hash((self.level, self.name, self.type))

class _LocalizationOutput(StringIO):
    """A file-like object for writing a localization file.
    
    The contents being written are stored in memory and written to a
    localization server only when the writing is finished.
    
    This object should be used as a context manager, a save operation will be
    executed if the context exits with no errors. If errors occur the partial
    contents are abandoned and the server is unchanged.
    
     It is also possible to save the contents to the server with the save()
     method.
    """
    def __init__(self, manager, file):
        StringIO.__init__(self)
        self._manager = manager
        self._file = file
    def save(self):
        """Send the currently written contents to the server."""
        request = self._manager._buildRequest(self._file.context, self._file.path, method="PUT")
        
        request.add_data(self.getvalue())
        request.add_header("If-Match", self._file.checksum)
        try:   
            urllib2.urlopen(request)
        except urllib2.HTTPError as e:
            if e.code == 409:
                raise LocalizationFileVersionConflictException, e.read()
            else:
                raise e
    def __enter__(self):
        return self
    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type is None:
            self.save()
    def __str__(self):
        return '<' + self.__class__.__name__ + " for " + str(self._file) + '>'

class LocalizationFile(object):
    """A specific file stored in localization.
    
    A localization file is uniquely defined by the context and path. There can
    only be one valid file for that path and localization at a time. To access
    the contents of the file use the open method.
    
    Attributes:
        context: A LocalizationContext
        path: A path to this file
        checksum: A string representation of a checksum generated from the file contents.
        timnestamp: A datetime.datetime object indicating when the file was last modified.
    """
    def __init__(self, manager, context, path, checksum, timestamp):
        """Initialize a LocalizationFile with the given manager and attributes.
        
        Args:
            manager: A LocalizationFileManager to assist with server communication
            context: A LocalizationContext
            path: A path to this file
            checksum: A string representation of a checksum generated from the file contents.
            timnestamp: A datetime.datetime object indicating when the file was last modified.
        """
        self._manager = manager
        self.context = context
        self.path = path
        self.checksum = checksum
        self.timestamp = timestamp
    def open(self, mode='r'):
        """Open the file.
        
        This should always be called as as part of a with statement. When
        writing the content is not saved on the server until leaving the with
        statement normally, if an error occurs the server is left unchanged.
        
        Example:
            with locFile.open('w') as output:
                output.write('some content')
        
        Args:
            mode: 'r' for reading the file, 'w' for writing

        Returns:
            A file like object that can be used for reads or writes.
        """
        if mode == 'r':
            request = self._manager._buildRequest(self.context, self.path)
            response = urllib2.urlopen(request)
            # Not the recommended way of reading directories.
            if not(self.isDirectory()):
                checksum = response.headers["Content-MD5"]
                if self.checksum != checksum:
                    raise RuntimeError, "Localization checksum mismatch " + self.checksum + " " + checksum
            return contextlib.closing(response)
        elif mode == 'w':
            return _LocalizationOutput(self._manager, self)
        else:
            raise ValueError, "mode string must be 'r' or 'w' not " + str(r)
    def delete(self):
        """Delete this file from the server"""
        request = self._manager._buildRequest(self.context, self.path, method='DELETE')
        request.add_header("If-Match", self.checksum)
        try:   
            urllib2.urlopen(request)
        except urllib2.HTTPError as e:
            if e.code == 409:
                raise LocalizationFileVersionConflictException, e.read()
            else:
                raise e
    def exists(self):
        """Check if this file actually exists.
        
        Returns:
            boolean indicating existence of this file
        """
        return self.checksum != NON_EXISTENT_CHECKSUM
    def isDirectory(self):
        """Check if this file is a directory.
        
        A file must exist to be considered a directory.
        
        Returns:
            boolean indicating directorocity of this file
        """
        return self.checksum == DIRECTORY_CHECKSUM
    def getCheckSum(self):
        return self.checksum
    def getContext(self):
        return self.context
    def getPath(self):
        return self.path
    def getTimeStamp(self):
        return self.timestamp
    def __str__(self):
        return str(self.context) + "/" + self.path
    def __eq__(self, other):
        return self.context == other.context and \
               self.path == other.path and \
               self.checksum == other.checksum \
               and self.timestamp == other.timestamp
    def __hash__(self):
        return hash((self.context, self.path, self.checksum, self.timestamp))

def _getHost():
    import subprocess
    host = subprocess.check_output(
                    "source /awips2/fxa/bin/setup.env; echo $DEFAULT_HOST",
                    shell=True).strip()
    if host:
        return host
    return 'localhost'

def _getSiteFromServer(host):
    try:
        from ufpy import ThriftClient
        from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetPrimarySiteRequest
        client = ThriftClient.ThriftClient(host)
        return client.sendRequest(GetPrimarySiteRequest())
    except:
        # Servers that don't have GFE installed will not return a site
        pass

def _getSiteFromEnv():
    site = os.environ.get('FXA_LOCAL_SITE')
    if site is None:
        site = os.environ.get('SITE_IDENTIFIER');
    return site
    
def _getSite(host):
    site = _getSiteFromEnv()
    if not(site):
        site = _getSiteFromServer(host)
    return site

def _parseJsonList(manager, response, context, path):
    fileList = []
    jsonResponse = loadjson(response)
    for name, jsonData in jsonResponse.items():
        checksum = jsonData["checksum"]
        timestampString = jsonData["timestamp"]
        timestamp = dateutil.parser.parse(timestampString)
        newpath = urljoin(path, name)
        fileList.append(LocalizationFile(manager, context, newpath, checksum, timestamp))
    return fileList
        
def _parseXmlList(manager, response, context, path):
    fileList = []
    for xmlData in parseXml(response).getroot().findall('file'):
        name = xmlData.get("name")
        checksum = xmlData.get("checksum")
        timestampString = xmlData.get("timestamp")
        timestamp = dateutil.parser.parse(timestampString)
        newpath = urljoin(path, name)
        fileList.append(LocalizationFile(manager, context, newpath, checksum, timestamp))
    return fileList

class LocalizationFileManager(object):
    """Connects to a server and retrieves LocalizationFiles."""
    def __init__(self, host=None, port=9581, path="/services/localization/", contexts=None, site=None, type="common_static"):
        """Initializes a LocalizationFileManager with connection parameters and context information
        
        All arguments are optional and will use defaults or attempt to figure out appropriate values form the environment.
        
        Args:
            host: A hostname of the localization server, such as 'ec'.
            port: A port to use to connect to the localization server, usually 9581.
            path: A path to reach the localization file service on the server.
            contexts: A list of contexts to check for files, the order of the contexts will be used
                    for the order of incremental results and the priority of absolute results. 
            site: A site identifier to use for site specific contexts. This is only used if the contexts arg is None.
            type: A localization type for contexts. This is only used if the contexts arg is None.
            
        """
        if host is None:
            host = _getHost()
        if contexts is None:
            if site is None :
                site = _getSite(host)
            contexts = [LocalizationContext("base", None, type)]
            if site:
                contexts.append(LocalizationContext("configured", site, type))
                contexts.append(LocalizationContext("site", site, type))
            contexts.append(LocalizationContext("user", getuser(), type))
        netloc = host + ':' + str(port)
        self._baseUrl = urlunparse(('http', netloc, path, None, None, None))
        self._contexts = contexts
    def _buildRequest(self, context, path, method='GET'):
        url = urljoin(self._baseUrl, context._getUrlComponent())
        url = urljoin(url, path)
        request = urllib2.Request(url)
        username = getuser()
        # Currently password is ignored in the server
        # this is the defacto standard for not providing one to this service.
        password =  username
        base64string = b64encode('%s:%s' % (username, password))
        request.add_header("Authorization", "Basic %s" % base64string)
        if method != 'GET':
            request.get_method = lambda: method
        return request
    def _normalizePath(self, path):
        if path == '' or path == '/':
            path = '.'
        if path[0] == '/':
            path = path[1:]
        return path
    def _list(self, path):
        path = self._normalizePath(path)
        if path[-1] != '/':
            path += '/'
        fileList = []
        exists = False
        for context in self._contexts:
            try:
                request = self._buildRequest(context, path)
                request.add_header("Accept", "application/json, application/xml")
                response = urllib2.urlopen(request)
                exists = True
                if not(response.geturl().endswith("/")):
                    # For ordinary files the server sends a redirect to remove the slash.
                    raise LocalizationFileIsNotDirectoryException, "Not a directory: " + path
                elif response.headers["Content-Type"] == "application/xml":
                    fileList += _parseXmlList(self, response, context, path)
                else:
                    fileList += _parseJsonList(self, response, context, path)
            except urllib2.HTTPError as e:
                if e.code != 404:
                    raise e
        if not(exists):
            raise LocalizationFileDoesNotExistException, "No such file or directory: " + path
        return fileList
    def _get(self, context, path):
        path = self._normalizePath(path)
        try:
            request = self._buildRequest(context, path, method='HEAD')
            resp = urllib2.urlopen(request)
            if (resp.geturl().endswith("/")):
                checksum = DIRECTORY_CHECKSUM;
            else:
                if "Content-MD5" not in resp.headers:
                    raise RuntimeError, "Missing Content-MD5 header in response from " + resp.geturl() 
                checksum = resp.headers["Content-MD5"]
                if "Last-Modified" not in resp.headers:
                    raise RuntimeError, "Missing Last-Modified header in response from " + resp.geturl()
            timestamp = dateutil.parser.parse(resp.headers["Last-Modified"])
            return LocalizationFile(self, context, path, checksum, timestamp)
        except urllib2.HTTPError as e:
            if e.code != 404:
                raise e
            else:
                return LocalizationFile(self, context, path, NON_EXISTENT_CHECKSUM, None)
    def listAbsolute(self, path):
        """List the files in a localization directory, only a single file is returned for each unique path.
        
        If a file exists in more than one context then the highest level(furthest from base) is used.
        
        Args:
            path: A path to a directory that should be the root of the listing
            
        Returns:
            A list of LocalizationFiles
        """
        merged = dict()
        for file in self._list(path):
            merged[file.path] = file
        return sorted(merged.values(), key=lambda file: file.path)
    def listIncremental(self, path):
        """List the files in a localization directory, this includes all files for all contexts.
        
        Args:
            path: A path to a directory that should be the root of the listing

        Returns:
            A list of tuples, each tuple will contain one or more files for the
            same paths but different contexts. Each tuple will be ordered the
            same as the contexts in this manager, generally with 'base' first
            and 'user' last. 
        """
        merged = dict()
        for file in self._list(path):
            if file.path in merged:
                merged[file.path] += (file,)
            else:
                merged[file.path] = (file, )
        return sorted(merged.values(), key=lambda t: t[0].path)
    def getAbsolute(self, path):
        """Get a single localization file from the highest level context where it exists.
        
        Args:
            path: A path to a localization file
        
        Returns:
            A Localization File with the specified path or None if the file does not exist in any context.
        
        """
        for context in reversed(self._contexts):
            f = self._get(context, path)
            if f.exists():
                return f
    def getIncremental(self, path):
        """Get all the localization files that exist in any context for the provided path.

        Args:
            path: A path to a localization file
        
        Returns:
            A tuple containing all the files that exist for this path in any context. The tuple
            will be ordered the same as the contexts in this manager, generally with 'base' first
            and 'user' last. 
        """
        result = ()
        for context in self._contexts:
            f = self._get(context, path)
            if f.exists():
                result += (f,)
        return result
    def getSpecific(self, level, path):
        """Get a specific localization file at a given level, the file may not exist.
        
        The file is returned for whichever context is valid for the provided level in this manager.
        
        For writing new files this is the only way to get access to a file that
        does not exist in order to create it.
        
        Args:
            level: the name of a localization level, such as "base", "site", "user"
            path: A path to a localization file
        
        Returns:
            A Localization File with the specified path and a context for the specified level.
        """
        for context in self._contexts:
            if context.level == level:
                return self._get(context, path)
        raise ValueError, "No context defined for level " + level
    def __str__(self):
        contextsStr = '[' + ' '.join((str(c) for c in self._contexts)) + ']'
        return '<' + self.__class__.__name__ + " for " + self._baseUrl + ' ' + contextsStr + '>'
