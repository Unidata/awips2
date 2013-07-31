/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.format;

import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.nc4.NcVariable;
import com.raytheon.uf.common.nc4.NetcdfException;

/**
 * Abstraction for data type specific writing from data record to netcdf file
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class NcWriter<T extends NcVariable> {

    public abstract Class<T> getVarClass();

    /**
     * Write data record to variable
     * 
     * @param var
     * @param irecord
     * @throws NetcdfException
     */
    public abstract void write(NcVariable var, int[] start, IDataRecord irecord)
            throws NetcdfException;

    /**
     * Converts the dimensions of the record to netcdf shape. This will be
     * reverse from the data record sizes;
     * 
     * @param record
     * @return
     */
    protected int[] getShape(IDataRecord record) {
        long[] sizes = record.getSizes();
        return new int[] { 1, 1, (int) sizes[1], (int) sizes[0] };
    }

    /**
     * Factory method for ncwriters
     * 
     * @param record
     * @return
     * @throws Exception
     *             if the data record's data type is not supported by the netcdf
     *             library
     */
    public static NcWriter<? extends NcVariable> create(IDataRecord record)
            throws Exception {
        if (record instanceof ByteDataRecord) {
            return new ByteNcWriter();
        } else if (record instanceof ShortDataRecord) {
            return new ShortNcWriter();
        } else if (record instanceof IntegerDataRecord) {
            return new IntNcWriter();
        } else if (record instanceof FloatDataRecord) {
            return new FloatNcWriter();
        } else {
            throw new Exception("Unsupported data record type: "
                    + record.getClass());
        }
    }
}
