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
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Implements a set of line (one dimensional) reads
 * 
 * Initially, this supports reading along either the X or Y axis in a two
 * dimensional dataset. It could be extended to accomodate selection from
 * n-dimensional datasets or selection along a diagonal.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009            chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class LineSelectionRead extends AbstractHDFRead {

    @Override
    protected int calculateSize(int dataspace, int sz, Request request,
            long[] dims, long[] originalDims, int totalSize)
            throws HDF5LibraryException {

        int[] points = request.getIndices();

        return points.length;
    }

    protected abstract void pruneIndices(Request request, long[] originalDims);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.hdf5.AbstractHDFRead#read(com.raytheon
     * .uf.common.datastorage.Request, int, java.lang.String, java.lang.String,
     * float)
     */
    @Override
    public IDataRecord read(Request request, int file_id, String group,
            String dataset, float scaleFactor) throws StorageException {
        return super.read(request, file_id, group, dataset, scaleFactor);
    }

    public static class YLineSelectionRead extends LineSelectionRead {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.datastorage.hdf5.LineSelectionRead#pruneIndices
         * (com.raytheon.uf.common.datastorage.Request, long[])
         */
        @Override
        protected void pruneIndices(Request request, long[] originalDims) {
            int[] indices = request.getIndices();
            int[] prunedIndices = new int[indices.length];
            int k = 0;
            for (int i = 0; i < indices.length; i++) {
                if (indices[i] < originalDims[0] && indices[i] >= 0) {
                    prunedIndices[k] = indices[i];
                    k++;
                }
            }

            indices = new int[k];
            System.arraycopy(prunedIndices, 0, indices, 0, k);
            request.setIndices(indices);

        }

        @Override
        protected void selectSet(int memspace, int dataspace, Request request,
                long[] dims, long[] originalDatasetDims) throws HDF5Exception,
                HDF5LibraryException, StorageException {
            int[] points = request.getIndices();

            long[] memOffset = new long[] { 0, 0 };
            for (int i = 0; i < points.length; i++) {
                long[] start = new long[] { points[i], 0 };
                long[] count = new long[] { 1, originalDatasetDims[1] };

                if (i == 0) {
                    H5.H5Sselect_hyperslab(dataspace,
                            HDF5Constants.H5S_SELECT_SET, start, null, count,
                            null);
                    H5.H5Sselect_hyperslab(memspace,
                            HDF5Constants.H5S_SELECT_SET, memOffset, null,
                            count, null);
                } else {
                    memOffset[0] += 1;
                    H5.H5Sselect_hyperslab(dataspace,
                            HDF5Constants.H5S_SELECT_OR, start, null, count,
                            null);
                    H5.H5Sselect_hyperslab(memspace,
                            HDF5Constants.H5S_SELECT_OR, memOffset, null,
                            count, null);
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.datastorage.hdf5.LineSelectionRead#calculateSize
         * (int, int, com.raytheon.uf.common.datastorage.Request, long[], int)
         */
        @Override
        protected int calculateSize(int dataspace, int sz, Request request,
                long[] dims, long[] originalDims, int totalSize)
                throws HDF5LibraryException {
            pruneIndices(request, originalDims);
            dims[0] = request.getIndices().length;
            return (int) (super.calculateSize(dataspace, sz, request, dims,
                    originalDims, totalSize) * dims[1]);
        }

    }

    public static class XLineSelectionRead extends LineSelectionRead {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.datastorage.hdf5.LineSelectionRead#pruneIndices
         * (com.raytheon.uf.common.datastorage.Request, long[])
         */
        @Override
        protected void pruneIndices(Request request, long[] originalDims) {
            int[] indices = request.getIndices();
            int[] prunedIndices = new int[indices.length];
            int k = 0;
            for (int i = 0; i < indices.length; i++) {
                if (indices[i] < originalDims[1] && indices[i] >= 0) {
                    prunedIndices[k] = indices[i];
                    k++;
                }
            }

            indices = new int[k];
            System.arraycopy(prunedIndices, 0, indices, 0, k);
            request.setIndices(indices);

        }

        @Override
        protected void selectSet(int memspace, int dataspace, Request request,
                long[] dims, long[] originalDatasetDims) throws HDF5Exception,
                HDF5LibraryException, StorageException {
            int[] points = request.getIndices();

            long[] memOffset = new long[] { 0, 0 };
            for (int i = 0; i < points.length; i++) {
                long[] start = new long[] { 0, points[i] };
                long[] count = new long[] { originalDatasetDims[0], 1 };

                if (i == 0) {
                    H5.H5Sselect_hyperslab(dataspace,
                            HDF5Constants.H5S_SELECT_SET, start, null, count,
                            null);
                    H5.H5Sselect_hyperslab(memspace,
                            HDF5Constants.H5S_SELECT_SET, memOffset, null,
                            count, null);
                } else {
                    memOffset[1] += 1;
                    H5.H5Sselect_hyperslab(dataspace,
                            HDF5Constants.H5S_SELECT_OR, start, null, count,
                            null);
                    H5.H5Sselect_hyperslab(memspace,
                            HDF5Constants.H5S_SELECT_OR, memOffset, null,
                            count, null);
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.datastorage.hdf5.LineSelectionRead#calculateSize
         * (int, int, com.raytheon.uf.common.datastorage.Request, long[], int)
         */
        @Override
        protected int calculateSize(int dataspace, int sz, Request request,
                long[] dims, long[] originalDims, int totalSize)
                throws HDF5LibraryException {
            pruneIndices(request, originalDims);
            dims[1] = request.getIndices().length;
            return (int) (super.calculateSize(dataspace, sz, request, dims,
                    originalDims, totalSize) * dims[0]);
        }

    }

}
