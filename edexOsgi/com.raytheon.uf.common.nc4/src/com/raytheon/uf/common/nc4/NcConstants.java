/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Constants used in Netcdf library
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class NcConstants {

    public static final Map<Integer, String> ERROR_MAP;

    /** Attribute id to put/get a global attribute. */
    public static final int NC_GLOBAL = -1;
    
    /** No Error */
    public static final int NC_NOERR = 0;

    /** Returned for all errors in the v2 API. */
    public static final int NC2_ERR = (-1);

    /**
     * Not a netcdf id.
     * 
     * The specified netCDF ID does not refer to an open netCDF dataset.
     */
    public static final int NC_EBADID = (-33);

    /** Too many netcdfs open */
    public static final int NC_ENFILE = (-34);

    /** netcdf file exists && NC_NOCLOBBER */
    public static final int NC_EEXIST = (-35);

    /** Invalid Argument */
    public static final int NC_EINVAL = (-36);

    /** Write to read only */
    public static final int NC_EPERM = (-37);



    /**
     * Operation not allowed in data mode. This is returned for netCDF classic
     * or 64-bit offset files, or for netCDF-4 files, when they have been
     * created with ::NC_CLASSIC_MODEL flag in nc_create().
     */
    public static final int NC_ENOTINDEFINE = (-38);

    /**
     * Operation not allowed in define mode.
     * 
     * The specified netCDF is in define mode rather than data mode.
     * 
     * With netCDF-4/HDF5 files, this error will not occur, unless
     * ::NC_CLASSIC_MODEL was used in nc_create().
     */
    public static final int NC_EINDEFINE = (-39);

    /**
     * Index exceeds dimension bound.
     * 
     * The specified corner indices were out of range for the rank of the
     * specified variable. For example, a negative index or an index that is
     * larger than the corresponding dimension length will cause an error.
     */
    public static final int NC_EINVALCOORDS = (-40);

    /** NC_MAX_DIMS exceeded */
    public static final int NC_EMAXDIMS = (-41);

    /** String match to name in use */
    public static final int NC_ENAMEINUSE = (-42);

    /** Attribute not found */
    public static final int NC_ENOTATT = (-43);

    /** NC_MAX_ATTRS exceeded */
    public static final int NC_EMAXATTS = (-44);

    /** Not a netcdf data type */
    public static final int NC_EBADTYPE = (-45);

    /** Invalid dimension id or name */
    public static final int NC_EBADDIM = (-46);

    /** NC_UNLIMITED in the wrong index */
    public static final int NC_EUNLIMPOS = (-47);



    /**
     * NC_MAX_VARS exceeded. Max number of variables exceeded in a classic or
     * 64-bit offset file, or an netCDF-4 file with ::NC_CLASSIC_MODEL on.
     */
    public static final int NC_EMAXVARS = (-48);

    /**
     * Variable not found. The variable ID is invalid for the specified netCDF
     * dataset.
     */
    public static final int NC_ENOTVAR = (-49);

    /** Action prohibited on NC_GLOBAL varid */
    public static final int NC_EGLOBAL = (-50);

    /** Not a netcdf file */
    
    public static final int NC_ENOTNC = (-51);

    /** In Fortran, string too short */
    public static final int NC_ESTS = (-52);

    /** NC_MAX_NAME exceeded */
    public static final int NC_EMAXNAME = (-53);

    /** NC_UNLIMITED size already in use */
    public static final int NC_EUNLIMIT = (-54);

    /** nc_rec op when there are no record vars */
    public static final int NC_ENORECVARS = (-55);

    /** Attempt to convert between text & numbers */
    public static final int NC_ECHAR = (-56);

    

    /** Start+count exceeds dimension bound.

    The specified edge lengths added to the specified corner would have
    referenced data out of range for the rank of the specified
    variable. For example, an edge length that is larger than the
    corresponding dimension length minus the corner index will cause an
    error. */
    public static final int NC_EEDGE = (-57);

    /** Illegal stride */
    public static final int NC_ESTRIDE = (-58);

    /** Attribute or variable name contains illegal characters */
    public static final int NC_EBADNAME = (-59);

    

    /** Math result not representable.

    One or more of the values are out of the range of values representable
    by the desired type. */
    public static final int NC_ERANGE = (-60);

    /** Memory allocation (malloc) failure */
    public static final int NC_ENOMEM = (-61);

    /** One or more variable sizes violate format constraints */
    public static final int NC_EVARSIZE = (-62);

    /** Invalid dimension size */
    public static final int NC_EDIMSIZE = (-63);

    /** File likely truncated or possibly corrupted */
    public static final int NC_ETRUNC = (-64);

    /** Unknown axis type. */
    public static final int NC_EAXISTYPE = (-65);

    
    /** Generic DAP error */
    public static final int NC_EDAP = (-66);

    /** Generic libcurl error */
    public static final int NC_ECURL = (-67);

    /** Generic IO error */
    public static final int NC_EIO = (-68);

    /** Attempt to access variable with no data */
    public static final int NC_ENODATA = (-69);

    /** DAP server error */
    public static final int NC_EDAPSVC = (-70);

    /** Malformed or inaccessible DAS */
    public static final int NC_EDAS = (-71);

    /** Malformed or inaccessible DDS */
    public static final int NC_EDDS = (-72);

    /** Malformed or inaccessible DATADDS */
    public static final int NC_EDATADDS = (-73);

    /** Malformed DAP URL */
    public static final int NC_EDAPURL = (-74);

    /** Malformed DAP Constraint */
    public static final int NC_EDAPCONSTRAINT = (-75);

    /** Untranslatable construct */
    public static final int NC_ETRANSLATION = (-76);

    /*
     * The following was added in support of netcdf-4. Make all netcdf-4 error
     * codes-100 so that errors can be added to netcdf-3 if needed.
     */

    /** Error at HDF5 layer. */
    public static final int NC_EHDFERR = (-101);

    /** Can't read. */
    public static final int NC_ECANTREAD = (-102);

    /** Can't write. */
    public static final int NC_ECANTWRITE = (-103);

    /** Can't create. */
    public static final int NC_ECANTCREATE = (-104);

    /** Problem with file metadata. */
    public static final int NC_EFILEMETA = (-105);

    /** Problem with dimension metadata. */
    public static final int NC_EDIMMETA = (-106);

    /** Problem with attribute metadata. */
    public static final int NC_EATTMETA = (-107);

    /** Problem with variable metadata. */
    public static final int NC_EVARMETA = (-108);

    /** Not a compound type. */
    public static final int NC_ENOCOMPOUND = (-109);

    /** Attribute already exists. */
    public static final int NC_EATTEXISTS = (-110);

    /** Attempting netcdf-4 operation on netcdf-3 file. */
    public static final int NC_ENOTNC4 = (-111);

    

    /** Attempting netcdf-4 operation on strict nc3 netcdf-4 file. */
    public static final int NC_ESTRICTNC3 = (-112);

    /** Attempting netcdf-3 operation on netcdf-4 file. */
    public static final int NC_ENOTNC3 = (-113);

    /** Parallel operation on file opened for non-parallel access. */
    public static final int NC_ENOPAR = (-114);

    /** Error initializing for parallel access. */
    public static final int NC_EPARINIT = (-115);

    /** Bad group ID. */
    public static final int NC_EBADGRPID = (-116);

    /** Bad type ID. */
    public static final int NC_EBADTYPID = (-117);

    /** Type has already been defined and may not be edited. */
    public static final int NC_ETYPDEFINED = (-118);

    /** Bad field ID. */
    public static final int NC_EBADFIELD = (-119);

    /** Bad class. */
    public static final int NC_EBADCLASS = (-120);

    /** Mapped access for atomic types only. */
    public static final int NC_EMAPTYPE = (-121);

    /** Attempt to define fill value when data already exists. */
    public static final int NC_ELATEFILL = (-122);

    /** Attempt to define var properties, like deflate, after enddef. */
    public static final int NC_ELATEDEF = (-123);

    /** Probem with HDF5 dimscales. */
    public static final int NC_EDIMSCALE = (-124);

    /** No group found. */
    public static final int NC_ENOGRP = (-125);

    /** Can't specify both contiguous and chunking. */
    public static final int NC_ESTORAGE = (-126);

    /** Bad chunksize. */
    public static final int NC_EBADCHUNK = (-127);

    /** Attempt to use feature that was not turned on when netCDF was built. */
    public static final int NC_ENOTBUILT = (-128);

    /** Error in using diskless access. */
    public static final int NC_EDISKLESS = (-129);

    /** Set read-only access for nc_open(). */
    public static final int NC_NOWRITE = 0x0000;

    /** Set read-write access for nc_open(). */
    public static final int NC_WRITE = 0x0001;

    /** Destroy existing file. Mode flag for nc_create(). */
    public static final int NC_CLOBBER = 0x0000;

    /** Don't destroy existing file. Mode flag for nc_create(). */
    public static final int NC_NOCLOBBER = 0x0004;

    /** Use diskless file. Mode flag for nc_open() or nc_create(). */
    public static final int NC_DISKLESS = 0x0008;

    /** Use diskless file with mmap. Mode flag for nc_open() or nc_create(). */
    public static final int NC_MMAP = 0x0010;

    /** Enforce classic model. Mode flag for nc_create(). */
    public static final int NC_CLASSIC_MODEL = 0x0100;

    /** Use large (64-bit) file offsets. Mode flag for nc_create(). */
    public static final int NC_64BIT_OFFSET = 0x0200;



    /**
     * deprecated The following flag currently is ignored, but use in nc_open()
     * or nc_create() may someday support use of advisory locking to prevent
     * multiple writers from clobbering a file
     */
    public static final int NC_LOCK = 0x0400;

    /** Share updates, limit cacheing.
    Use this in mode flags for both nc_create() and nc_open(). */
    public static final int NC_SHARE = 0x0800;

    /** Use netCDF-4/HDF5 format. Mode flag for nc_create(). */
    public static final int NC_NETCDF4 = 0x1000;



    /** Turn on MPI I/O.
    Use this in mode flags for both nc_create() and nc_open(). */
    public static final int NC_MPIIO = 0x2000;
    /** Turn on MPI POSIX I/O.
    Use this in mode flags for both nc_create() and nc_open(). */
    public static final int NC_MPIPOSIX = 0x4000;

    /** Use parallel-netcdf library. Mode flag for nc_open(). */
    public static final int NC_PNETCDF = 0x8000;

    /**
     * Let nc__create() or nc__open() figure out as suitable chunk size.
     */
    public static final int NC_SIZEHINT_DEFAULT = 0;

    /** signed 1 byte integer */
    public static final int NC_BYTE = 1;

    /** ISO/ASCII character */
    public static final int NC_CHAR = 2;

    /** signed 2 byte integer */
    public static final int NC_SHORT = 3;

    /** signed 4 byte integer */
    public static final int NC_INT = 4;

    /** single precision floating point number */
    public static final int NC_FLOAT = 5;

    /** double precision floating point number */
    public static final int NC_DOUBLE = 6;

    /** signed 8-byte int */
    public static final int NC_INT64 = 10;


    static {
        HashMap<Integer, String> map = new HashMap<Integer, String>();
        map.put(NC_NOERR, "No Error ");
        map.put(NC2_ERR, "Returned for all errors in the v2 API. ");

        map.put(NC_EBADID, "Not a netcdf id. ");
        map.put(NC_ENFILE, "Too many netcdfs open ");
        map.put(NC_EEXIST, "netcdf file exists && NC_NOCLOBBER ");
        map.put(NC_EINVAL, "Invalid Argument ");
        map.put(NC_EPERM, "Write to read only ");

        map.put(NC_ENOTINDEFINE, "Operation not allowed in data mode. ");
        map.put(NC_EINDEFINE, "Operation not allowed in define mode. ");

        map.put(NC_EINVALCOORDS, "Index exceeds dimension bound. ");
        map.put(NC_EMAXDIMS, "NC_MAX_DIMS exceeded ");
        map.put(NC_ENAMEINUSE, "String match to name in use ");
        map.put(NC_ENOTATT, "Attribute not found ");
        map.put(NC_EMAXATTS, "NC_MAX_ATTRS exceeded ");
        map.put(NC_EBADTYPE, "Not a netcdf data type ");
        map.put(NC_EBADDIM, "Invalid dimension id or name ");
        map.put(NC_EUNLIMPOS, "NC_UNLIMITED in the wrong index ");

        map.put(NC_EMAXVARS, "NC_MAX_VARS exceeded. ");

        map.put(NC_ENOTVAR, "Variable not found. ");
        map.put(NC_EGLOBAL, "Action prohibited on NC_GLOBAL varid ");
        map.put(NC_ENOTNC, "Not a netcdf file ");
        map.put(NC_ESTS, "In Fortran, string too short ");
        map.put(NC_EMAXNAME, "NC_MAX_NAME exceeded ");
        map.put(NC_EUNLIMIT, "NC_UNLIMITED size already in use ");
        map.put(NC_ENORECVARS, "nc_rec op when there are no record vars ");
        map.put(NC_ECHAR, "Attempt to convert between text & numbers ");

        map.put(NC_EEDGE, "Start+count exceeds dimension bound. ");
        map.put(NC_ESTRIDE, "Illegal stride ");
        map.put(NC_EBADNAME,
                "Attribute or variable name contains illegal characters ");

        map.put(NC_ERANGE,
                "One or more of the values are out of the range of values representable by the desired type. ");
        map.put(NC_ENOMEM, "Memory allocation (malloc) failure ");
        map.put(NC_EVARSIZE,
                "One or more variable sizes violate format constraints ");
        map.put(NC_EDIMSIZE, "Invalid dimension size ");
        map.put(NC_ETRUNC, "File likely truncated or possibly corrupted ");
        map.put(NC_EAXISTYPE, "Unknown axis type. ");

        map.put(NC_EDAP, "Generic DAP error ");
        map.put(NC_ECURL, "Generic libcurl error ");
        map.put(NC_EIO, "Generic IO error ");
        map.put(NC_ENODATA, "Attempt to access variable with no data ");
        map.put(NC_EDAPSVC, "DAP server error ");
        map.put(NC_EDAS, "Malformed or inaccessible DAS ");
        map.put(NC_EDDS, "Malformed or inaccessible DDS ");
        map.put(NC_EDATADDS, "Malformed or inaccessible DATADDS ");
        map.put(NC_EDAPURL, "Malformed DAP URL ");
        map.put(NC_EDAPCONSTRAINT, "Malformed DAP Constraint");
        map.put(NC_ETRANSLATION, "Untranslatable construct ");

        map.put(NC_EHDFERR, "Error at HDF5 layer. ");
        map.put(NC_ECANTREAD, "Can't read. ");
        map.put(NC_ECANTWRITE, "Can't write. ");
        map.put(NC_ECANTCREATE, "Can't create. ");
        map.put(NC_EFILEMETA, "Problem with file metadata. ");
        map.put(NC_EDIMMETA, "Problem with dimension metadata. ");
        map.put(NC_EATTMETA, "Problem with attribute metadata. ");
        map.put(NC_EVARMETA, "Problem with variable metadata. ");
        map.put(NC_ENOCOMPOUND, "Not a compound type. ");
        map.put(NC_EATTEXISTS, "Attribute already exists. ");
        map.put(NC_ENOTNC4, "Attempting netcdf-4 operation on netcdf-3 file. ");

        map.put(NC_ESTRICTNC3,
                "Attempting netcdf-4 operation on strict nc3 netcdf-4 file. ");
        map.put(NC_ENOTNC3, "Attempting netcdf-3 operation on netcdf-4 file. ");
        map.put(NC_ENOPAR,
                "Parallel operation on file opened for non-parallel access. ");
        map.put(NC_EPARINIT, "Error initializing for parallel access. ");
        map.put(NC_EBADGRPID, "Bad group ID. ");
        map.put(NC_EBADTYPID, "Bad type ID. ");
        map.put(NC_ETYPDEFINED,
                "Type has already been defined and may not be edited. ");
        map.put(NC_EBADFIELD, "Bad field ID. ");
        map.put(NC_EBADCLASS, "Bad class. ");
        map.put(NC_EMAPTYPE, "Mapped access for atomic types only. ");
        map.put(NC_ELATEFILL,
                "Attempt to define fill value when data already exists. ");
        map.put(NC_ELATEDEF,
                "Attempt to define var properties, like deflate, after enddef. ");
        map.put(NC_EDIMSCALE, "Probem with HDF5 dimscales. ");
        map.put(NC_ENOGRP, "No group found. ");
        map.put(NC_ESTORAGE, "Can't specify both contiguous and chunking. ");
        map.put(NC_EBADCHUNK, "Bad chunksize. ");
        map.put(NC_ENOTBUILT,
                "Attempt to use feature that was not turned on when netCDF was built. ");
        map.put(NC_EDISKLESS, "Error in using diskless  access. ");

        ERROR_MAP = Collections.unmodifiableMap(map);
    }
}
