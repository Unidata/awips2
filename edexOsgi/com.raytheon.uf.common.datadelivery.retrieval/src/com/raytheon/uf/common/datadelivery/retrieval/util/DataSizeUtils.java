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
package com.raytheon.uf.common.datadelivery.retrieval.util;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.PointDataSet;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Data Structure for calculating Data Set Size.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012    1002     mpduff     Initial creation
 * Aug 12, 2012  1022      djohnson   Stop coordinates on GriddedCoverage from being corrupted.
 * Oct 31, 2012  1278      mpduff     Clarified a Javadoc comment.
 * Dec 10, 2012  1259      bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * Jun 11, 2013  2021      dhladky    WFS semi-scientific sizing.
 * Jun 14, 2013  2108      mpduff     Abstracted the class.
 * Sept 09, 2013 2351      dhladky    Fixed incorrect calculation for default pointdata overhead
 * Nov 20, 2013   2554     dhladky    Generics
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class DataSizeUtils<DS extends DataSet<?, ?>> {

    /**
     * Factory method to get a DataSizeUtils.
     * 
     * @param dataSet
     *            The data set
     * @param subscription
     *            The subscription
     * @return The correct implementation of DataSizeUtils
     */
    public static DataSizeUtils<?> getInstance(DataSet<?, ?> dataSet,
            Subscription<?, ?> subscription) {
        DataSizeUtils<?> dsu = null;
        if (subscription.getDataSetType() == DataType.GRID) {
            dsu = new GriddedDataSizeUtils((GriddedDataSet) dataSet);
        } else if (subscription.getDataSetType() == DataType.POINT) {
            dsu = new PointDataSizeUtils((PointDataSet) dataSet);
        }

        return dsu;
    }

    /**
     * Get data size.
     * 
     * @param ra
     *            RetrievalAttribute
     * @param st
     *            ServiceType
     * @return dataset size
     */
    public static long calculateSize(RetrievalAttribute<?, ?> ra, ServiceType st) {

        if (st == ServiceType.OPENDAP) {
            if (ra.getCoverage() instanceof GriddedCoverage) {
                GriddedCoverage griddedCov = (GriddedCoverage) ra.getCoverage();
                int nx = griddedCov.getGridCoverage().getNx();
                int ny = griddedCov.getGridCoverage().getNy();

                long l = st.getRequestBytesPerParameterPerLevel(nx * ny);

                l = l / bytesPerKilobyte;
                return l;
            } else {
                throw new IllegalStateException(
                        "Couldn't calculate the retrieval size for a retrieval of type "
                                + st.name() + "!");
            }

        } else if (st == ServiceType.WFS) {

            ReferencedEnvelope re = ra.getCoverage().getRequestEnvelope();
            Coordinate ur = EnvelopeUtils.getUpperRightLatLon(re);
            Coordinate ll = EnvelopeUtils.getLowerLeftLatLon(re);
            double lonSpan = Math.abs(ll.x - ur.x);
            double latSpan = Math.abs(ll.y - ur.y);
            PointTime time = (PointTime) ra.getTime();
            long l = st.getRequestBytesPerLatLonBoxAndTime(latSpan, lonSpan,
                    time.getInterval());
            return l;
        } else {
            throw new IllegalStateException(
                    "Couldn't calculate the retrieval size for a retrieval of type "
                            + st.name() + "!");
        }
    }

    /** Bytes per Kilobyte */
    protected static final int bytesPerKilobyte = 1024;

    /** Data Set Object */
    protected DS dataSet;

    /** Data Set Size */
    protected final long size = 0;

    /** Full Data Set Size in bytes */
    protected long fullSize = -999;

    /**
     * Returns the estimated full data set size in bytes.
     * 
     * @return full data set size in bytes
     */
    public abstract long getFullSizeInBytes();

    /**
     * Get the data set size for the provided subscription.
     * 
     * @param subscription
     *            Subscription for calculating the size
     * @return Data size in bytes
     */
    protected abstract long getDataSetSizeInBytes(Subscription<?, ?> subscription);

    /**
     * @return the dataSet
     */
    public DS getDataSet() {
        return dataSet;
    }

    /**
     * Returns the estimated data set size in kB.
     * 
     * @param subscription
     *            The subscription to size
     * 
     * @return the size in kB
     */
    public long getDataSetSizeInKb(Subscription<?, ?> subscription) {
        return getDataSetSizeInBytes(subscription) / bytesPerKilobyte;
    }

    /**
     * Returns the estimated full dataset size in kB.
     * 
     * @return full data set size in kB
     */
    public long getFullSizeInKb() {
        return getFullSizeInBytes() / bytesPerKilobyte;
    }

    /**
     * @return the size
     */
    public long getSize() {
        return size;
    }
}
