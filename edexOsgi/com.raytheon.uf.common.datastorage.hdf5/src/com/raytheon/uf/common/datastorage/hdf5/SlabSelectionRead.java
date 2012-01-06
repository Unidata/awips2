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

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;

/**
 * Read a "stripe" of data. This is much more efficient than reading by points
 * as it tells the HDF5 library to do a continuous read.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SlabSelectionRead extends AbstractHDFRead {

    @Override
    protected int calculateSize(int dataspace, int sz, Request request,
            long[] dims, long[] originalDims, int totalSize)
            throws HDF5LibraryException {
        int[] minIndex = request.getMinIndexForSlab();
        int[] maxIndex = request.getMaxIndexForSlab();

        if (minIndex != null && maxIndex != null) {
            dims[0] = maxIndex[1] - minIndex[1];
            dims[1] = maxIndex[0] - minIndex[0];
            totalSize = (int) ((dims[0]) * dims[1]);
        } else {
            H5.H5Sget_simple_extent_dims(dataspace, dims, (long[]) null);
            for (long dim : dims) {
                totalSize *= (int) dim;
            }
        }
        return totalSize;
    }

    @Override
    protected void selectSet(int memspace, int dataspace, Request request,
            long[] dims, long[] originalDatasetDims) throws HDF5Exception,
            HDF5LibraryException, StorageException {
        long[] offset = new long[2];
        long[] count = new long[2];

        int[] minIndex = request.getMinIndexForSlab();
        int[] maxIndex = request.getMaxIndexForSlab();
        
        if ((minIndex[0] > maxIndex[0]) || (minIndex[1] > maxIndex[1])) {
            throw new StorageException("Minimum slab dimension exceeds Maximum dimension, request failed", null);
        }

        offset[0] = minIndex[1];
        offset[1] = minIndex[0];
        count[0] = maxIndex[1] - minIndex[1];
        count[1] = maxIndex[0] - minIndex[0];

        H5.H5Sselect_hyperslab(dataspace, HDF5Constants.H5S_SELECT_SET, offset,
                null, count, null);
    }

}
