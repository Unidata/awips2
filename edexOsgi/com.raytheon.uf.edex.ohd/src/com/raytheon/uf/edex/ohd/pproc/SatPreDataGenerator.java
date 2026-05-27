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
package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Filters URIs for Satellite Precip and generates an xmrg file if the filter
 * matches. Note: this class must remain thread safe.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            snaples     Initial creation
 * Feb 12, 2010 4635       snaples     Updated to match more generically.
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * May 05, 2014 2060       njensen     Cleanup
 * Sep 15, 2017 6407       bkowal      Updated to use {@link IPrecipDataCreator}.
 * Oct 06, 2017 6407       bkowal      Renamed.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class SatPreDataGenerator {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final PrecipDataCreatorFactory dataFactory;

    public SatPreDataGenerator() throws Exception {
        dataFactory = new PrecipDataCreatorFactory();
    }

    /**
     * Get the current time. ESB uses this to get the time for product header
     * information.
     *
     * @return The current time as a Long.
     */
    public Long getQueueTime() {
        return new Long(System.currentTimeMillis());
    }

    public PluginDataObject[] process(DataURINotificationMessage msg)
            throws Exception {
        if (msg instanceof DataURINotificationMessage) {
            for (String data : msg.getDataURIs()) {
                final PluginDataObject pdo = DataURIUtil
                        .createPluginDataObject(data);
                IPrecipDataCreator<? extends PluginDataObject> dataCreator = dataFactory
                        .lookupCreator(pdo.getClass());
                if (dataCreator == null) {
                    statusHandler
                            .error("Failed to find a precip data creation class for PluginDataObject: "
                                    + pdo.getClass().getName() + ".");
                    continue;
                }
                statusHandler.info("Generating " + PrecipField.SATPRE.name()
                        + " precip data for: " + data + " using: "
                        + dataCreator.getClass().getName() + " ...");
                return dataCreator.build(pdo);
            }
        }

        return null;
    }
}