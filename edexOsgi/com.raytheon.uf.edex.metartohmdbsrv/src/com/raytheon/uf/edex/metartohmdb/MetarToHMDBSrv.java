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
package com.raytheon.uf.edex.metartohmdb;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.metartohmdb.dao.HMDBReport;
import com.raytheon.uf.edex.metartohmdb.dao.HMDBRptDao;

/**
 * Stores MetarRecords as HMDBReport in database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ????                                Initial creation
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class MetarToHMDBSrv {

    private Log logger = LogFactory.getLog(getClass());

    private HMDBRptDao dao = null;

    // If a required resource is not available,
    // Set failSave to true. This will "short circuit" processing.
    private boolean failSafe = false;

    /**
     * Construct an instance of this transformer.
     */
    public MetarToHMDBSrv() {

        try {
            dao = new HMDBRptDao();
        } catch (Exception e) {
            logger.error("HMDBRptDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
    }

    /**
     * 
     * @param objects
     */
    public void process(PluginDataObject[] objects, Headers headers) {
        // If required resouces are not available, exit now!
        if (failSafe) {
            return;
        }
        logger.debug("Inside MetarToHMDBSrv.process()");
        logger.debug(String.format("Received %d metar observations",
                objects.length));

        for (PluginDataObject report : objects) {
            if (report instanceof MetarRecord) {
                try {
                    writeObs((MetarRecord) report, headers);
                } catch (Exception e) {
                    logger.error("Error writing to rpt table", e);
                }
            }
        }
    }

    /**
     * 
     * @param report
     */
    private void writeObs(MetarRecord report, Headers headers) {

        if (dao == null) {
            return;
        }

        HMDBReport rpt = new HMDBReport();

        // Get the report data. This contains both the
        // WMOHeader and METAR report, so we need to split them.
        String rptData = report.getReport();
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader hdr = new WMOHeader(rptData.getBytes(), fileName);
        if (hdr.isValid()) {
            rpt.setWmo_dd(hdr.getWmoHeader().substring(0, 6));
            // if the report data is longer than 255 characters,
            // truncate it.
            String obsData = rptData.substring(hdr.getMessageDataStart());
            if (obsData.length() > 255) {
                obsData = obsData.substring(0, 254);
            }
            rpt.setReport(rptData.substring(hdr.getMessageDataStart()));
        } else {
            // wmo_dd is a not_null field, so if we can't find it, exit now.
            return;
        }
        rpt.setDate(report.getTimeObs());
        // We don't have the origin time available,
        // so use the observation time.
        rpt.setOrigin(report.getTimeObs());
        rpt.setNominal(report.getRefHour());

        rpt.setReport_type(report.getReportType());

        rpt.setIcao_loc_id(report.getStationId());

        rpt.setLat(report.getLatitude());
        rpt.setLon(report.getLongitude());

        if (!dao.storeToTable(rpt)) {
            logger.error("Error writing data to hmdb");
        }
    }
}
