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
import types

##############################################################################
# Class to input and output activities to file objects. The client is responsible
# for opening and closing the file object. Methods are provided to read and write
# the files. Other methods may be added later.
#
# General usage:
#   import InputOutput as IO
#   io = IO.InputOutput()
#   fh = open("myFile.txt","r")
#   io.setStream(fh)
#   text = io.read()
#
# TODO: need to provide some level of error checking
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1584          mfegan         Initial Creation.
##############################################################################
class InputOutput:
    # Initializer. 
    #
    # args:
    #   stream: the stream to perform operations on
    #
    # when stream not set, must either set stream or 
    # use single arg operations
    def __init__(self,stream=None):
        self.stream = stream

    # Reads from the input stream until an end of file 
    # marker is encountered. Returns data received.
    #
    # args:
    #   stream: an open input stream
    # throws:
    #   IOError: if stream is invalid
    #
    # When the stream is not specified, is must be set either by
    # the constructor or setStream(...)
    def read(self,stream=None):
        if stream is None:
            stream = self.stream
        return self.__read(stream)

    # reads a single line of input from the specified stream.
    # Returns the data received.
    #
    # args:
    #   stream: an open input stream
    # throws:
    #   IOError: if stream is invalid
    #
    # When the stream is not specified, is must be set either by
    # the constructor or setStream(...)
    def readln(self,stream=None):
        if stream is None:
            stream = self.stream
        return self.__readln(stream)
        
    # Writes to the specified output stream until the data has 
    # all been written. Once the data is written, the stream is
    # flushed. No formatting of the data is assumed.
    #
    # args:
    #   stream: stream to use to perform the write
    #   data:   the data to write
    # throws:
    #   IOError: if stream is invalid
    #
    # When the stream is not specified, it must be set either by
    # the constructor or setStream(...). In that case, the data
    # must be identified by name. 
    def write(self,stream=None,data=None):
        if stream is None:
            stream = self.stream
        self.__write(stream,data)
    # Writes to the specified output stream until the data has 
    # all been written. A single new line character is appended to 
    # the data once it has been written. Once the data is written,
    # the stream is flushed. No formatting of the data is assumed.
    #
    # args:
    #   stream: stream to use to perform the write
    #   data:   the data to write
    # throws:
    #   IOError: if stream is invalid
    #
    # When the stream is not specified, it must be set either by
    # the constructor or setStream(...). In that case, the data
    # must be identified by name. 
    def writeln(self,stream=None,data=None):
        if stream is None:
            stream = self.stream
        self.__write(stream,data,True)

    # sets the stream to be used for subsequent operations
    def setStream(self,stream):
        self.stream = stream

    # reads from the specified input stream until an end of file 
    # marker is encountered. Returns data received.
    #
    # args:
    #   stream: stream to use to perform the read
    # throws:
    #   IOError: if stream is invalid
    def __read(self,stream):
        # validate the input stream
        try:
            self.__validate(stream,"r")
        except:
            raise

        # read the input stream
        retVal = ""
        data = stream.readline()
        while '' != data:
            retVal += data
            data = stream.readline()
        return retVal.strip()

    # reads from the specified data stream until an eol is encountered
    def __readln(self,stream):
        try:
            self.__validate(stream,"r")
        except:
            raise
        retVal = stream.readline()
        return retVal.strip()
        
        
        
    # Writes data to the specified stream until the data has 
    # all been written. Once the data is written, a newline is written
    # (if the new line flag is True), and then the stream is flushed.
    # No formatting of the data is assumed.
    #
    # args:
    #   stream: stream to use to perform the write
    #   data:   the data to write
    #   nl:the new line flag - set to True to write new line following text
    #
    # raises:
    #    IOError: if the stream is not writable.
    def __write(self,stream,data,nl=False):
        # validate the input stream
        try:
            self.__validate(stream,"w")
        except:
            raise

        # write the data to the stream
        conv = self.__dataToString(data)
        stream.write(conv)
        if nl:
            stream.write('\n')
        stream.flush()

    # performs validation. The stream is valid provided
    #   1. it is not null,
    #   2. it is a file,
    #   3. it is not closed, and
    #   4. it has the appropriate mode.
    #
    # args:
    #   stream: the stream to check
    #   mode:   the expected mode; 'r' or 'w'
    # raises:
    #   IOError: if the stream is invalid
    #
    # Note: it might be a good idea to allow things other than files, provided
    #       they support the appropriate methods. 
    def __validate(self,stream,mode):
        if stream is None:
            raise IOError("No stream available")
        if not isinstance(stream,types.FileType):
            raise IOError("Configured stream of Invalid Type " + str(type(stream)))
        if stream.closed:
            raise IOError("Configured stream " + stream.name + "has been closed")
        if stream.mode.find(mode) == -1:
            raise IOError("Invalid stream mode " + stream.mode + " for stream " + stream.name)
    
    # Converts the input data to a string. In most cases, we don't want to simply call
    # str(...) on the data; we only do this if we can't make a reasonable string from
    # the data; e.g. if you want to output a dictionary.
    #
    # args:
    #   data: the data to convert to a string
    # return:
    #   the data converted to a string
    def __dataToString(self,data):
        sep = "\n";
        if isinstance(data,types.StringType):
            return data
        elif isinstance(data,types.ListType):
            return sep.join(data)
        elif isinstance(data,types.TupleType):
            return sep.join(data)
        return str(data)

##############################################################################
# General exception to be raised when errors occur in the InputOutput class.
##############################################################################
class InputOutputError(Exception):
    def __init__(self,value,cause=None):
          self.value = value
          self.cause = cause
    def __str__(self):
        msg = 'InputOutputError: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()