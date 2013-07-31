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

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.nc4.NcVariable;
import com.raytheon.uf.common.nc4.NcVariable.IntVariable;
import com.raytheon.uf.common.nc4.NetcdfException;

/**
 * TODO Add Description
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
public class IntNcWriter extends NcWriter<IntVariable> {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.format.NcWriter#getVarClass()
     */
    @Override
    public Class<IntVariable> getVarClass() {
        return IntVariable.class;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.format.NcWriter#write(com.raytheon.uf.common
     * .nc4.NcVariable, com.raytheon.uf.common.datastorage.records.IDataRecord)
     */
    @Override
    public void write(NcVariable var, int[] start, IDataRecord irecord)
            throws NetcdfException {
        IntegerDataRecord record = (IntegerDataRecord) irecord;
        IntVariable variable = (IntVariable) var;
        int[] shape = getShape(record);
        variable.putVar(start, shape, record.getIntData());
    }

}
