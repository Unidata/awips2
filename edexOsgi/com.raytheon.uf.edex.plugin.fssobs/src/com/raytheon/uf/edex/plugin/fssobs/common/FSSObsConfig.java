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
package com.raytheon.uf.edex.plugin.fssobs.common;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.fssobs.FSSObsGenerator;
import com.raytheon.uf.edex.plugin.fssobs.FSSObsUtils;

/**
 * FSSObs Configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2010            skorolev    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 04, 2014 3220       skorolev    Removed cwa and monitorUse from data set.
 * Sep 18, 2015 3873       skorolev    Removed identical constant definitions.
 * Dec 02, 2015 3873       dhladky     Logging change.
 * Dec 14, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsConfig {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsConfig.class);

    /**
     * Generator
     */
    private FSSObsGenerator fssgen = null;

    /**
     * FSSObs data
     */
    private FSSObsRecord tableRow;

    /** The logger */
    protected transient final Logger logger = LoggerFactory
            .getLogger(getClass());

    public FSSObsConfig(URIGenerateMessage genMessage, FSSObsGenerator generator)
            throws Exception {

        this.fssgen = generator;
    }

    /**
     * Gets FSSObs data table row.
     * 
     * @param uri
     * @return tableRow
     */
    public FSSObsRecord getTableRow(String uri) {
        String dt = uri.substring(1)
                .substring(0, uri.substring(1).indexOf("/"));

        try {
            
            if (dt.equals("obs")) {
                tableRow = FSSObsUtils.getRecordFromMetar(uri);
            } else if (dt.equals("sfcobs")) {
                tableRow = FSSObsUtils.getRecordFromMaritime(uri);
            } else if (dt.equals("ldadmesonet")) {
                tableRow = FSSObsUtils.getRecordFromMesowest(uri);
            }

        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM, "Could not create type: "
                    + dt + " URI: " + uri, e);
        }

        if (tableRow.getRelativeHumidity() == ObConst.MISSING) {
            Float RH = FSSObsUtils.getRH(tableRow.getDewpoint(),
               tableRow.getTemperature());
            tableRow.setRelativeHumidity(RH);
        }
        float[] snowData = FSSObsUtils.getSnowData(tableRow);
        if ((tableRow.getTemperature() != ObConst.MISSING)
                && (tableRow.getDewpoint() != ObConst.MISSING)) {
            // TODO to check if this is correct. calcdpd() in Meteolib
            tableRow.setDewpointDepr(tableRow.getTemperature()
                    - tableRow.getDewpoint());
        } else {
            tableRow.setDewpointDepr(ObConst.MISSING);
        }
        tableRow.setSnincrHourly(snowData[0]);
        tableRow.setSnincrTotal(snowData[1]);
        tableRow.setSnowDepth(snowData[2]);
        tableRow.setWindChill(snowData[3]);
        tableRow.setFrostbiteTime(snowData[4]);
        tableRow.setPlatformId(tableRow.getLocation().getStationId());

        return tableRow;

    }

    /**
     * Gets CPG generator
     * 
     * @return cpg generator
     */
    public CompositeProductGenerator getGenerator() {
        return fssgen;
    }

    /**
     * Sets FSSObs data table row.
     * 
     * @param tableRow
     *            the tableRow to set
     */
    public void setTableRow(FSSObsRecord tableRow) {
        this.tableRow = tableRow;
    }

}
