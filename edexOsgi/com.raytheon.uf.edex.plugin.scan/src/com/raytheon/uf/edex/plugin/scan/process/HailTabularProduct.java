package com.raytheon.uf.edex.plugin.scan.process;

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

import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * 
 * Process incoming Hail tabular product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/07/2009   2037      dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HailTabularProduct extends RadarProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** Hail Tabular prod ID */
    public static final String hi = "59";

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public HailTabularProduct(String uri, ScanTables tableType,
            ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public void process() throws Exception {
        RadarRecord rec = null;
        try {
            rec = getRecord();
        } catch (Exception ve) {
            ve.printStackTrace();
        }
        if (rec != null) {
            filter.getRadarData().setRadarRecord(hi, rec);
        }
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {
        return null;
    }

    @Override
    public boolean getAllowNew() {
        return false;
    }

    /**
     * Hail Tabular Data pattern
     * 
     * @return
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile(uriSeparator + RADAR + uriSeparator + wildCard
                + uriSeparator + icao + uriSeparator + hi + uriSeparator
                + tiltAngle + uriSeparator + tiltAngle);
    }

    /**
     * Gets the Hail SQL
     * 
     * @param icao
     * @return
     */
    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + hi
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

}