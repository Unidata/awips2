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
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

public class UAProduct extends ScanProduct {

    /*** data type **/

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * 
     */
    public UAProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    public VerticalSounding getSoundingRecord() {
        VerticalSounding vs = null;
        try {
            filter.setSoundingRecord(ScanProduct.UA, uri);
            vs = filter.getSoundingRecord(ScanProduct.UA);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return vs;
    }

    @Override
    public void process() throws Exception {

        VerticalSounding rec = null;
        try {
            rec = getSoundingRecord();
        } catch (Exception pe) {
            pe.printStackTrace();
        }
        if (rec != null) {
            filter.getSoundingData().setSoundingRecord(UA, rec);
        }

        /*
         * try { // get most recent set of UAObs data for all levels and types
         * // 120 minutes ArrayList<String> uris = record.getRecords(UAProduct
         * .getSQL(record.getStationWMO(), 120), UA); UAObs[] uaObs = new
         * UAObs[uris.size()]; int i = 0; for (String uri : uris) { uaObs[i++] =
         * new UAObs(uri); //System.out.println("Adding VerticalSounding..."+i);
         * }
         * 
         * UAObsAdapter adapt = new UAObsAdapter(); adapt.setObjects(uaObs);
         * 
         * for (VerticalSounding vs : adapt.createSoundings()) { vs.toString();
         * // all UA does is update the environmental data object if (vs !=
         * null) { SoundingData ed = new SoundingData(vs);
         * monitor.setSoundingData(ed); ed.toString(); } }
         * 
         * } catch (Exception e) { throw new VizException(
         * "ScanMonitor: Unable to retrieve UA Records " + uri, e); }
         */
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
            PersistableDataObject pdo) {
        return null;
    }

    /**
     * UA URI Pattern, Tornadoes
     * 
     * @return
     */
    public static Pattern getPattern(String wmo) {
        return Pattern.compile("^" + uriSeparator + UA + uriSeparator
                + wildCard + uriSeparator + wildCard + uriSeparator + wildCard
                + uriSeparator + wmo + uriSeparator + wildCard + uriSeparator
                + wildCard);

        // /bufrua/2011-10-07_00:00:00.0/2022/null/IUSZ52_KWBC_070040/72634/44.90833/-84.71944
    }

    /**
     * The SQL UA
     * 
     * @param wmo
     * @return
     */
    public static String getSQL(String wmo, int interval) {
        return "select datauri from " + UA + " where stationid = \'" + wmo
                + "\'" + " and reftime > (now()- interval \'" + interval
                + " hours\')";
    }

}
