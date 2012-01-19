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

import java.awt.Point;

import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;

/**
 * Selects data from an hdf dataset along a discrete set of points
 * 
 * Currently this supports either 1D or 2D selection (the java.awt.Point class
 * is used, which is only at max two dimensional). In one dimensional cases, the
 * y value is ignored.
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

public class PointSelectionRead extends AbstractHDFRead {

    @Override
    protected int calculateSize(int dataspace, int sz, Request request,
            long[] dims, long[] originalDims, int totalSize)
            throws HDF5LibraryException {
        Point[] points = request.getPoints();

        dims[0] = points.length;
        if (sz > 1) {
            dims[1] = 1;
        }
        for (int i = 0; i < dims.length; i++) {
            totalSize *= dims[i];
        }
        return totalSize;
    }

    @Override
    protected void selectSet(int memspace, int dataspace, Request request,
            long[] dims, long[] originalDatasetDims) throws HDF5Exception,
            HDF5LibraryException, StorageException {
        long[][] coords;
        Point[] points = request.getPoints();

        if (dims.length == 2) {
            coords = new long[points.length * (int) dims[1]][dims.length];
        } else {
            coords = new long[points.length][dims.length];
        }

        for (int i = 0; i < points.length; i++) {

            if (dims.length == 2) {
                coords[i][0] = points[i].y;
                coords[i][1] = points[i].x;
            } else {
                coords[i][0] = points[i].x;
            }
        }

        // Below is a performance optimization due to
        // limitations in hdf5
        byte[] coordsAsBytes = longToBytes(coords);
        invokeH5Sselect_elements(dataspace, HDF5Constants.H5S_SELECT_SET,
                coords.length, coordsAsBytes);
    }

}
