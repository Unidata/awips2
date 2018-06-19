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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Grib decoder post processor interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/30/10      5875        bphillip    Initial Creation
 * Oct 07, 2015 3756        nabowle     Switch interface to abstract class,
 *                                      rename from IDecoderPostProcessor, add
 *                                      getMessageData(), getType(), and 
 *                                      relevant enum,
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public abstract class DecoderPostProcessor {

    /**
     * Processes the provided record to see if it needs to be post processed
     *
     * @param record
     *            The record to examine to determine if it needs to be post
     *            processed
     * @return The array of grib records including any created during post
     *         processing
     * @throws GribException
     */
    public abstract GridRecord[] process(GridRecord record)
            throws GribException;

    /** Get the type of post processor. Defaults to {@link #PRE_PERSIST}. */
    public PostProcessorType getType() {
        return PostProcessorType.PRE_PERSIST;
    }

    /**
     * Gets the message data for a record. If the record's messagedata is null,
     * the data will be retrieved from the data store, set on the record, and
     * returned.
     *
     * @param record
     *            The record to get the message data for.
     * @return
     * @throws GribException
     */
    protected float[] getMessageData(GridRecord record) throws GribException {
        float[] data = (float[]) record.getMessageData();
        if (data == null) {
            GridDao dao = null;
            try {
                dao = new GridDao();
                record.setMessageData(((FloatDataRecord) dao.getHDF5Data(
                        record, -1)[0]).getFloatData());
                data = (float[]) record.getMessageData();
            } catch (PluginException e) {
                throw new GribException("Error populating grib data", e);
            }
        }
        return data;
    }

    /**
     * Defines the types of IDecoderPostProcessors.
     *
     * It is expected that {@link #POST_PERSIST} processors do not return the
     * input record.
     */
    public static enum PostProcessorType {
        /**
         * Processors that process a GridRecord that was just decoded and before
         * being persisted.
         */
        PRE_PERSIST,

        /**
         * Processors that process a GridRecord after it was persisted. It is
         * expected these processors do not return the input record.
         */
        POST_PERSIST
    }
}
