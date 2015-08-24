"""
Module to read / write wav files using numpy arrays

Functions
---------
<<<<<<< HEAD
read: Return the sample rate (in samples/sec) and data from a WAV file.

write: Write a numpy array as a WAV file.

"""
=======
`read`: Return the sample rate (in samples/sec) and data from a WAV file.

`write`: Write a numpy array as a WAV file.

"""
from __future__ import division, print_function, absolute_import

import sys
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import numpy
import struct
import warnings

<<<<<<< HEAD
=======

__all__ = [
    'WavFileWarning',
    'read',
    'write'
]


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class WavFileWarning(UserWarning):
    pass

_big_endian = False

<<<<<<< HEAD
# assumes file pointer is immediately
#  after the 'fmt ' id
=======
WAVE_FORMAT_PCM = 0x0001
WAVE_FORMAT_IEEE_FLOAT = 0x0003
WAVE_FORMAT_EXTENSIBLE = 0xfffe
KNOWN_WAVE_FORMATS = (WAVE_FORMAT_PCM, WAVE_FORMAT_IEEE_FLOAT)

# assumes file pointer is immediately
#  after the 'fmt ' id


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def _read_fmt_chunk(fid):
    if _big_endian:
        fmt = '>'
    else:
        fmt = '<'
<<<<<<< HEAD
    res = struct.unpack(fmt+'ihHIIHH',fid.read(20))
    size, comp, noc, rate, sbytes, ba, bits = res
    if (comp != 1 or size > 16):
        warnings.warn("Unfamiliar format bytes", WavFileWarning)
        if (size>16):
            fid.read(size-16)
    return size, comp, noc, rate, sbytes, ba, bits

# assumes file pointer is immediately
#   after the 'data' id
def _read_data_chunk(fid, noc, bits):
=======
    res = struct.unpack(fmt+'iHHIIHH', fid.read(20))
    size, comp, noc, rate, sbytes, ba, bits = res
    if comp not in KNOWN_WAVE_FORMATS or size > 16:
        comp = WAVE_FORMAT_PCM
        warnings.warn("Unknown wave file format", WavFileWarning)
        if size > 16:
            fid.read(size - 16)

    return size, comp, noc, rate, sbytes, ba, bits


# assumes file pointer is immediately
#   after the 'data' id
def _read_data_chunk(fid, comp, noc, bits, mmap=False):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if _big_endian:
        fmt = '>i'
    else:
        fmt = '<i'
<<<<<<< HEAD
    size = struct.unpack(fmt,fid.read(4))[0]
    if bits == 8:
        data = numpy.fromfile(fid, dtype=numpy.ubyte, count=size)
        if noc > 1:
            data = data.reshape(-1,noc)
    else:
        bytes = bits//8
        if _big_endian:
            dtype = '>i%d' % bytes
        else:
            dtype = '<i%d' % bytes
        data = numpy.fromfile(fid, dtype=dtype, count=size//bytes)
        if noc > 1:
            data = data.reshape(-1,noc)
    return data

def _read_riff_chunk(fid):
    global _big_endian
    str1 = fid.read(4)
    if str1 == 'RIFX':
        _big_endian = True
    elif str1 != 'RIFF':
=======
    size = struct.unpack(fmt, fid.read(4))[0]

    bytes = bits//8
    if bits == 8:
        dtype = 'u1'
    else:
        if _big_endian:
            dtype = '>'
        else:
            dtype = '<'
        if comp == 1:
            dtype += 'i%d' % bytes
        else:
            dtype += 'f%d' % bytes
    if not mmap:
        data = numpy.fromstring(fid.read(size), dtype=dtype)
    else:
        start = fid.tell()
        data = numpy.memmap(fid, dtype=dtype, mode='c', offset=start,
                            shape=(size//bytes,))
        fid.seek(start + size)

    if noc > 1:
        data = data.reshape(-1, noc)
    return data


def _skip_unknown_chunk(fid):
    if _big_endian:
        fmt = '>i'
    else:
        fmt = '<i'

    data = fid.read(4)
    # call unpack() and seek() only if we have really read data from file
    # otherwise empty read at the end of the file would trigger 
    # unnecessary exception at unpack() call
    # in case data equals somehow to 0, there is no need for seek() anyway
    if data:
        size = struct.unpack(fmt, data)[0]
        fid.seek(size, 1)


def _read_riff_chunk(fid):
    global _big_endian
    str1 = fid.read(4)
    if str1 == b'RIFX':
        _big_endian = True
    elif str1 != b'RIFF':
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        raise ValueError("Not a WAV file.")
    if _big_endian:
        fmt = '>I'
    else:
        fmt = '<I'
    fsize = struct.unpack(fmt, fid.read(4))[0] + 8
    str2 = fid.read(4)
<<<<<<< HEAD
    if (str2 != 'WAVE'):
        raise ValueError("Not a WAV file.")
    if str1 == 'RIFX':
=======
    if (str2 != b'WAVE'):
        raise ValueError("Not a WAV file.")
    if str1 == b'RIFX':
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        _big_endian = True
    return fsize

# open a wave-file
<<<<<<< HEAD
def read(file):
=======


def read(filename, mmap=False):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Return the sample rate (in samples/sec) and data from a WAV file

    Parameters
    ----------
<<<<<<< HEAD
    file : file
        Input wav file.
=======
    filename : string or open file handle
        Input wav file.
    mmap : bool, optional
        Whether to read data as memory mapped.
        Only to be used on real files (Default: False)

        .. versionadded:: 0.12.0
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Returns
    -------
    rate : int
        Sample rate of wav file
    data : numpy array
        Data read from wav file

    Notes
    -----
<<<<<<< HEAD

    * The file can be an open file or a filename.

    * The returned sample rate is a Python integer
    * The data is returned as a numpy array with a
      data-type determined from the file.

    """
    if hasattr(file,'read'):
        fid = file
    else:
        fid = open(file, 'rb')

    fsize = _read_riff_chunk(fid)
    noc = 1
    bits = 8
    while (fid.tell() < fsize):
        # read the next chunk
        chunk_id = fid.read(4)
        if chunk_id == 'fmt ':
            size, comp, noc, rate, sbytes, ba, bits = _read_fmt_chunk(fid)
        elif chunk_id == 'data':
            data = _read_data_chunk(fid, noc, bits)
        else:
            warnings.warn("chunk not understood", WavFileWarning)
            size = struct.unpack('I',fid.read(4))[0]
            bytes = fid.read(size)
    fid.close()
=======
    * The file can be an open file or a filename.
    * The returned sample rate is a Python integer.
    * The data is returned as a numpy array with a data-type determined
      from the file.
    * This function cannot read wav files with 24 bit data.

    """
    if hasattr(filename, 'read'):
        fid = filename
        mmap = False
    else:
        fid = open(filename, 'rb')

    try:
        fsize = _read_riff_chunk(fid)
        noc = 1
        bits = 8
        comp = WAVE_FORMAT_PCM
        while (fid.tell() < fsize):
            # read the next chunk
            chunk_id = fid.read(4)
            if chunk_id == b'fmt ':
                size, comp, noc, rate, sbytes, ba, bits = _read_fmt_chunk(fid)
                if bits == 24:
                    raise ValueError("Unsupported bit depth: the wav file "
                                     "has 24 bit data.")
            elif chunk_id == b'fact':
                _skip_unknown_chunk(fid)
            elif chunk_id == b'data':
                data = _read_data_chunk(fid, comp, noc, bits, mmap=mmap)
            elif chunk_id == b'LIST':
                # Someday this could be handled properly but for now skip it
                _skip_unknown_chunk(fid)
            else:
                warnings.warn("Chunk (non-data) not understood, skipping it.",
                              WavFileWarning)
                _skip_unknown_chunk(fid)
    finally:
        if not hasattr(filename, 'read'):
            fid.close()
        else:
            fid.seek(0)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return rate, data

# Write a wave-file
# sample rate, data
<<<<<<< HEAD
=======


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def write(filename, rate, data):
    """
    Write a numpy array as a WAV file

    Parameters
    ----------
<<<<<<< HEAD
    filename : file
        The name of the file to write (will be over-written).
    rate : int
        The sample rate (in samples/sec).
    data : ndarray
        A 1-D or 2-D numpy array of integer data-type.

    Notes
    -----
=======
    filename : string or open file handle
        Output wav file
    rate : int
        The sample rate (in samples/sec).
    data : ndarray
        A 1-D or 2-D numpy array of either integer or float data-type.

    Notes
    -----
    * The file can be an open file or a filename.

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    * Writes a simple uncompressed WAV file.
    * The bits-per-sample will be determined by the data-type.
    * To write multiple-channels, use a 2-D array of shape
      (Nsamples, Nchannels).

    """
<<<<<<< HEAD
    fid = open(filename, 'wb')
    fid.write('RIFF')
    fid.write('\x00\x00\x00\x00')
    fid.write('WAVE')
    # fmt chunk
    fid.write('fmt ')
    if data.ndim == 1:
        noc = 1
    else:
        noc = data.shape[1]
    bits = data.dtype.itemsize * 8
    sbytes = rate*(bits / 8)*noc
    ba = noc * (bits / 8)
    fid.write(struct.pack('<ihHIIHH', 16, 1, noc, rate, sbytes, ba, bits))
    # data chunk
    fid.write('data')
    fid.write(struct.pack('<i', data.nbytes))
    import sys
    if data.dtype.byteorder == '>' or (data.dtype.byteorder == '=' and sys.byteorder == 'big'):
        data = data.byteswap()
    data.tofile(fid)
    # Determine file size and place it in correct
    #  position at start of the file.
    size = fid.tell()
    fid.seek(4)
    fid.write(struct.pack('<i', size-8))
    fid.close()
=======
    if hasattr(filename, 'write'):
        fid = filename
    else:
        fid = open(filename, 'wb')

    try:
        dkind = data.dtype.kind
        if not (dkind == 'i' or dkind == 'f' or (dkind == 'u' and
                                                 data.dtype.itemsize == 1)):
            raise ValueError("Unsupported data type '%s'" % data.dtype)

        fid.write(b'RIFF')
        fid.write(b'\x00\x00\x00\x00')
        fid.write(b'WAVE')
        # fmt chunk
        fid.write(b'fmt ')
        if dkind == 'f':
            comp = 3
        else:
            comp = 1
        if data.ndim == 1:
            noc = 1
        else:
            noc = data.shape[1]
        bits = data.dtype.itemsize * 8
        sbytes = rate*(bits // 8)*noc
        ba = noc * (bits // 8)
        fid.write(struct.pack('<ihHIIHH', 16, comp, noc, rate, sbytes,
                              ba, bits))
        # data chunk
        fid.write(b'data')
        fid.write(struct.pack('<i', data.nbytes))
        if data.dtype.byteorder == '>' or (data.dtype.byteorder == '=' and
                                           sys.byteorder == 'big'):
            data = data.byteswap()
        _array_tofile(fid, data)

        # Determine file size and place it in correct
        #  position at start of the file.
        size = fid.tell()
        fid.seek(4)
        fid.write(struct.pack('<i', size-8))

    finally:
        if not hasattr(filename, 'write'):
            fid.close()
        else:
            fid.seek(0)


if sys.version_info[0] >= 3:
    def _array_tofile(fid, data):
        # ravel gives a c-contiguous buffer
        fid.write(data.ravel().view('b').data)
else:
    def _array_tofile(fid, data):
        fid.write(data.tostring())
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
