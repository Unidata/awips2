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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
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
    protected transient final Log logger = LogFactory.getLog(getClass());

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
        if (dt.equals("obs")) {
            try {
                tableRow = FSSObsUtils.getRecordFromMetar(uri);

            } catch (PluginException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else if (dt.equals("sfcobs")) {
            try {
                tableRow = FSSObsUtils.getRecordFromMaritime(uri);
            } catch (PluginException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else if (dt.equals("ldadmesonet")) {
            try {
                tableRow = FSSObsUtils.getRecordFromMesowest(uri);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        if (tableRow.getRelativeHumidity() == FSSObsUtils.MISSING) {
            Float RH = FSSObsUtils.getRH(tableRow.getDewpoint(),
                    tableRow.getTemperature());
            tableRow.setRelativeHumidity(RH);
        }
        float[] snowData = FSSObsUtils.getSnowData(tableRow);
        if ((tableRow.getTemperature() != FSSObsUtils.MISSING)
                && (tableRow.getDewpoint() != FSSObsUtils.MISSING)) {
            // TODO to check if this is correct. calcdpd() in Meteolib
            tableRow.setDewpointDepr(tableRow.getTemperature()
                    - tableRow.getDewpoint());
        } else {
            tableRow.setDewpointDepr(FSSObsUtils.MISSING);
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
