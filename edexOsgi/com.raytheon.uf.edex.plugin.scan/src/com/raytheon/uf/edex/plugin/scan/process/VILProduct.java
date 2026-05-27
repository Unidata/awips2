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
package com.raytheon.uf.edex.plugin.scan.process;

import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 *
 * Process incoming VIL product
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------
 * May 11, 2009  2037     dhladky   Initial Creation.
 * Apr 04, 2018  6696     randerso  Code cleanup
 *
 * </pre>
 *
 * @author dhladky
 */

public class VILProduct extends RadarProduct {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /** Vertically Integrated Liquid prod ID */
    public static final String vil = "57";

    /**
     *
     * @param uri
     * @param tableType
     * @param filter
     */
    public VILProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public boolean getAllowNew() {
        return false;
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {
        return null;
    }

    @Override
    public void process() throws Exception {
        RadarRecord rec = null;
        rec = getRecord();

        if (rec != null) {
            filter.getRadarData().setRadarRecord(vil, rec);
        }
    }

    /**
     * Gets the VIL URI Pattern
     *
     * @param icao
     * @param tiltAngle
     *
     * @return the dataURI pattern
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile("^" + DataURI.SEPARATOR + RADAR
                + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR + icao
                + DataURI.SEPARATOR + vil + DataURI.SEPARATOR + tiltAngle
                + DataURI.SEPARATOR + layer);
    }

    /**
     * Gets the SQL
     *
     * @param icao
     * @param tiltAngle
     * @param interval
     * @return the SQL query string
     */
    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + vil
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

}
