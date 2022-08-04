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

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * Process incoming UA product
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 04, 2018  6696     randerso  Code cleanup
 *
 * </pre>
 *
 * @author randerso
 */
public class UAProduct extends ScanProduct {

    /*** data type **/
    public static final String UA = "bufrua";

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constructor
     *
     * @param uri
     * @param tableType
     * @param filter
     */
    public UAProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    /**
     * @return the VerticalSounding record
     */
    public VerticalSounding getSoundingRecord() {
        VerticalSounding vs = null;
        try {
            filter.setSoundingRecord(UA, uri);
            vs = filter.getSoundingRecord(UA);
        } catch (Exception e) {
            statusHandler.error("Error getting sounding record", e);
        }
        return vs;
    }

    @Override
    public void process() throws Exception {

        VerticalSounding rec = getSoundingRecord();

        if (rec != null) {
            filter.getSoundingData().setSoundingRecord(UA, rec);
        }
    }

    @Override
    public boolean getAllowNew() {
        return false;
    }

    @Override
    public void setDataType() {
        this.dataType = UA;
    }

    // UA data dosne't use these methods

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {
        return null;
    }

    @Override
    public PersistablePluginDataObject getRecord() throws PluginException {
        return null;
    }

    @Override
    public ScanTableDataRow setSpatial(ScanTableDataRow row, String key,
            PersistableDataObject<?> pdo) {
        return null;
    }

    /**
     * UA URI Pattern, Tornadoes
     *
     * @param wmo
     * @return the dataURI Pattern
     */
    public static Pattern getPattern(String wmo) {
        return Pattern.compile("^" + DataURI.SEPARATOR + UA + DataURI.SEPARATOR
                + wildCard + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR
                + wildCard + DataURI.SEPARATOR + wmo + DataURI.SEPARATOR
                + wildCard + DataURI.SEPARATOR + wildCard);

        // /bufrua/2011-10-07_00:00:00.0/2022/null/IUSZ52_KWBC_070040/72634/44.90833/-84.71944
    }

    /**
     * The SQL UA
     *
     * @param wmo
     * @param interval
     * @return the SQL query string
     */
    public static String getSQL(String wmo, int interval) {
        return "select datauri from " + UA + " where stationid = \'" + wmo
                + "\'" + " and reftime > (now()- interval \'" + interval
                + " hours\')";
    }

}
