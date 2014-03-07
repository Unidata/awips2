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
package com.raytheon.uf.edex.database.plugin;

import java.awt.Rectangle;

import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.filter.FillValueFilter;
import com.raytheon.uf.common.numeric.filter.InverseFillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Utility for storing downscaled data to datastore
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 19, 2013  2393     bclement    Initial creation
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class DownscaleStoreUtil {

    /**
     * Interface for creating IDataRecords for interpolation levels
     */
    public static interface IDataRecordCreator {

        /**
         * Create new data record for level
         * 
         * @param data
         * @param downScaleLevel
         * @param size
         * @return
         * @throws StorageException
         */
        public IDataRecord create(Object data, int downScaleLevel,
                Rectangle size) throws StorageException;

        /**
         * @return fill value
         */
        public double getFillValue();

        public boolean isSigned();
    }

    /**
     * Create and add interpolated levels from dataSource.
     * 
     * @param dataStore
     * @param downScaler
     * @param dataSource
     * @param creator
     * @return number of levels not including the base level
     * @throws StorageException
     */
    public static <T extends PluginDataObject> int storeInterpolated(
            IDataStore dataStore, GridDownscaler downScaler,
            BufferWrapper dataSource, IDataRecordCreator creator)
            throws StorageException {
        // default to batch storage
        return storeInterpolated(dataStore, downScaler, dataSource, creator,
                false);
    }

    /**
     * Create and add interpolated levels from dataSource.
     * 
     * @param dataStore
     * @param downScaler
     * @param dataSource
     * @param creator
     * @param storeAfterEach
     *            if true, call store method on dataStore after each level is
     *            created
     * @return number of levels not including the base level
     * @throws StorageException
     */
    public static <T extends PluginDataObject> int storeInterpolated(
            IDataStore dataStore, GridDownscaler downScaler,
            BufferWrapper dataWrapper, IDataRecordCreator creator,
            boolean storeAfterEach) throws StorageException {

        // How many interpolation levels do we need for this data?
        int levels = downScaler.getNumberOfDownscaleLevels();

        // How many interpolation levels do we need for this data? Includes
        // the base level!
        // Subtract one for the base level data.
        int downScaleLevels = levels - 1;
        if (DataStoreFactory.isInterpolated(levels)) {
            for (int level = 0; level < downScaleLevels; level++) {
                int downScaleLevel = level + 1;
                Rectangle size = downScaler.getDownscaleSize(downScaleLevel);

                BufferWrapper destWrapper = BufferWrapper
                        .create(dataWrapper.getPrimitiveType(),
                                size.width, size.height);

                DataSource dataSource = dataWrapper;
                if (creator.isSigned() == false) {
                    if (dataSource instanceof ByteBufferWrapper) {
                        dataSource = UnsignedFilter
                                .apply((ByteBufferWrapper) dataSource);
                    } else if (dataSource instanceof ShortBufferWrapper) {
                        dataSource = UnsignedFilter
                                .apply((ShortBufferWrapper) dataSource);
                    }
                }
                dataSource = FillValueFilter.apply(dataSource,
                        creator.getFillValue());
                DataDestination dataDest = InverseFillValueFilter.apply(
                        (DataDestination) destWrapper, creator.getFillValue());
                try {
                    // Downscale from previous level
                    downScaler.downscale(downScaleLevel - 1, downScaleLevel,
                            dataSource, dataDest);
                    Object data = destWrapper.getArray();
                    if (data == null) {
                        throw new StorageException(
                                "Unable to get downscaled data from destination type: "
                                        + destWrapper.getClass(), null);
                    }
                    IDataRecord dr = creator.create(data, downScaleLevel, size);
                    dataStore.addDataRecord(dr);
                    if (storeAfterEach) {
                        dataStore.store();
                    }
                    // Set source to current level
                    dataWrapper = destWrapper;
                } catch (TransformException e) {
                    throw new StorageException(
                            "Error creating downscaled data", null, e);
                }
            }
        }
        return downScaleLevels;
    }

}
