/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.datastorage.hdf5;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Provides a proxy for HDF5 to be extended for various types of requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class HDF5OpManager {

    protected static final Map<Type, AbstractHDFRead> readTypes;
    static {
        readTypes = new HashMap<Type, AbstractHDFRead>();
        readTypes.put(Type.POINT, new PointSelectionRead());
        readTypes.put(Type.ALL, new WholeDatasetSelectionRead());
        readTypes.put(Type.SLAB, new SlabSelectionRead());
        readTypes.put(Type.XLINE, new LineSelectionRead.XLineSelectionRead());
        readTypes.put(Type.YLINE, new LineSelectionRead.YLineSelectionRead());
    }

    public static IDataRecord read(Request request, int file_id, String group,
            String dataset, float scaleFactor) throws StorageException {
        AbstractHDFRead readImpl = readTypes.get(request.getType());
        if (readImpl == null) {
            throw new IllegalArgumentException("Unsupported request type: "
                    + request.getType());
        }

        return readImpl.read(request, file_id, group, dataset, scaleFactor);
    }

}
