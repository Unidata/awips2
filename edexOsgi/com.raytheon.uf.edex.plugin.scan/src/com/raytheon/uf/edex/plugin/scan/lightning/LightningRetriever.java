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
package com.raytheon.uf.edex.plugin.scan.lightning;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Utility for retrieving a lightning record. Originally extracted from
 * ScanUtils.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LightningRetriever {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(LightningRetriever.class);

    private LightningRetriever() {

    }

    /**
     * Populate the Lightning record
     * 
     * @param uri
     * @return
     */
    public static BinLightningRecord getLightningRecord(String uri) {
        BinLightningRecord lightRec = null;
        try {
            lightRec = new BinLightningRecord(uri);
            PluginDao ld = PluginFactory.getInstance().getPluginDao(
                    lightRec.getPluginName());
            lightRec = (BinLightningRecord) ld.getMetadata(uri);
            IDataStore dataStore = ld.getDataStore(lightRec);
            lightRec.retrieveFromDataStore(dataStore);
        } catch (Exception e) {
            logger.error("Error retrieving lightning record", e);
        }

        return lightRec;
    }

}
