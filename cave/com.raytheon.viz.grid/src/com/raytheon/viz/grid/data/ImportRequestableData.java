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
package com.raytheon.viz.grid.data;

import java.nio.FloatBuffer;
import java.util.Arrays;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.geospatial.interpolation.BicubicInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.PrecomputedGridReprojection;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.data.AliasRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.grid.util.SliceUtil;

/**
 * Import from one data source to another. Handles temporal and geolocational
 * interpolation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010 #4473      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ImportRequestableData extends AliasRequestableData {
    protected AbstractRequestableData sourceRecord2;

    public ImportRequestableData(ImportRequestableData that) {
        super(that);
        sourceRecord2 = that.sourceRecord2;
        this.dataTime = that.dataTime;
    }

    public ImportRequestableData(AbstractRequestableData beforeRecord,
            AbstractRequestableData afterRecord, DataTime dataTime) {
        super(beforeRecord);
        sourceRecord2 = afterRecord;
        this.dataTime = dataTime;
    }

    @Override
    public Object getDataValue(Object arg) throws DataCubeException {
        Request req = Request.ALL;
        if (arg instanceof Request) {
            req = (Request) arg;
        }
        Object rval = getDataAndConvert(sourceRecord, Request.ALL);

        if (sourceRecord2 != null) {
            Object interp2 = getDataAndConvert(sourceRecord2, Request.ALL);
            // do time interpolation
            long millis1 = sourceRecord.getDataTime().getValidTime()
                    .getTimeInMillis();
            long millis2 = sourceRecord2.getDataTime().getValidTime()
                    .getTimeInMillis();
            float w1 = millis2 - millis1;
            float w2 = (millis2 - dataTime.getValidTime().getTimeInMillis())
                    / w1;
            w1 = 1 - w2;
            if ((rval instanceof FloatDataRecord)
                    && (interp2 instanceof FloatDataRecord)) {
                // multiply in place so rval will hold correct value after
                // calculation
                interpolate(((FloatDataRecord) rval).getFloatData(),
                        ((FloatDataRecord) interp2).getFloatData(), w2, w1);
            } else if ((rval instanceof FloatDataRecord[])
                    && (interp2 instanceof FloatDataRecord)) {
                FloatDataRecord[] recs = (FloatDataRecord[]) rval;
                FloatDataRecord[] recs2 = (FloatDataRecord[]) interp2;
                for (int i = 0; (i < recs.length) && (i < recs2.length); i++) {
                    interpolate(recs[i].getFloatData(),
                            recs2[i].getFloatData(), w2, w1);
                }
            } else if (rval instanceof IDataRecord[]) {
                IDataRecord[] recs = (IDataRecord[]) rval;
                IDataRecord[] recs2 = (IDataRecord[]) interp2;
                for (int i = 0; (i < recs.length) && (i < recs2.length); i++) {
                    if ((recs[i] instanceof FloatDataRecord)
                            && (recs2[i] instanceof FloatDataRecord)) {
                        interpolate(((FloatDataRecord) recs[i]).getFloatData(),
                                ((FloatDataRecord) recs2[i]).getFloatData(),
                                w2, w1);
                    }
                }
            }
        }

        GridCoverage sourceGrid = (GridCoverage) sourceRecord.getSpace();
        GridCoverage destGrid = (GridCoverage) getSpace();
        Interpolation interpolation = new BicubicInterpolation();
        try {
            if (rval instanceof FloatDataRecord) {
                FloatDataRecord fdr = remapGrid(sourceGrid, destGrid,
                        (FloatDataRecord) rval, interpolation);
                rval = SliceUtil.slice(fdr, req);
            } else if (rval instanceof FloatDataRecord[]) {
                FloatDataRecord[] recs = (FloatDataRecord[]) rval;
                for (int i = 0; i < recs.length; i++) {
                    FloatDataRecord fdr = remapGrid(sourceGrid, destGrid,
                            recs[i], interpolation);
                    recs[i] = SliceUtil.slice(fdr, req);
                }
            } else if (rval instanceof IDataRecord[]) {
                IDataRecord[] recs = (IDataRecord[]) rval;
                for (int i = 0; i < recs.length; i++) {
                    if (recs[i] instanceof FloatDataRecord) {
                        FloatDataRecord fdr = remapGrid(sourceGrid, destGrid,
                                (FloatDataRecord) recs[i], interpolation);
                        recs[i] = SliceUtil.slice(fdr, req);
                    }
                }
            }
        } catch (VizException e) {
            throw new DataCubeException(e);
        }

        return rval;
    }

    /**
     * remap gridded data to a new grid coverage
     * 
     * @param sourceGrid
     *            source grid coverage
     * @param destGrid
     *            destination grid coverage
     * @param fdr
     *            source float data record
     * @param interpolation
     *            interpolation algorithm
     * @return destination float data record
     * @throws VizException
     */
    protected FloatDataRecord remapGrid(GridCoverage sourceGrid,
            GridCoverage destGrid, FloatDataRecord fdr,
            Interpolation interpolation) throws VizException {

        try {
            GridGeometry2D sourceGeometry = sourceGrid.getGridGeometry();
            GridGeometry2D destGeometry = destGrid.getGridGeometry();

            GridReprojection interp = PrecomputedGridReprojection
                    .getReprojection(sourceGeometry, destGeometry);

            DataSource source = new GeographicDataSource(FloatBuffer.wrap(fdr
                    .getFloatData()), sourceGeometry);

            FloatBufferWrapper dest = new FloatBufferWrapper(
                    destGeometry.getGridRange2D());

            interp.reprojectedGrid(interpolation, source, dest);

            FloatDataRecord rval = new FloatDataRecord(fdr.getName(),
                    fdr.getGroup(), dest.getArray(),
                    destGeometry.getDimension(), new long[] { dest.getNx(),
                            dest.getNy() });

            return rval;
        } catch (Exception e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Performs the following calc in place at arr1: arr1 = arr1 * val1 + arr2 *
     * val2
     * 
     * @param arr1
     * @param arr2
     * @param val1
     * @param val2
     */
    protected void interpolate(float[] arr1, float[] arr2, float val1,
            float val2) {
        if (arr1.length == arr2.length) {
            for (int i = 0; i < arr1.length; i++) {
                arr1[i] = (arr1[i] * val1) + (arr2[i] * val2);
            }
        } else {
            // world implodes
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AliasRequestableData#getDependencies
     * ()
     */
    @Override
    public List<AbstractRequestableData> getDependencies() {
        return Arrays.asList(sourceRecord, sourceRecord2);
    }

}
