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

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.raytheon.uf.edex.plugin.scan.lightning.LightningRetriever;

/**
 * 
 * Process incoming Lightning product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/05/2009   2037       dhladky     Initial Creation.
 * 05/14/2014   3133       njensen     Use LightingRetriever instead of ScanUtils
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class LightningProduct extends ScanProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    // special one with brackets for lightning
    private static String wildCard = "[\\w\\[\\]\\-_:.]+";

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public LightningProduct(String uri, ScanTables tableType,
            ScanURIFilter filter) {
        super(uri, tableType, filter);
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

    @Override
    public BinLightningRecord getRecord() throws Exception {

        BinLightningRecord lightRec = null;
        try {
            lightRec = LightningRetriever.getLightningRecord(uri);
        } catch (Exception e) {
            e.printStackTrace();
        }

        return lightRec;
    }

    @Override
    public void process() throws Exception {

        BinLightningRecord rec = null;
        rec = getRecord();
        filter.setLightningData(rec);
    }

    @Override
    public ScanTableDataRow setSpatial(ScanTableDataRow row, String key,
            PersistableDataObject pdo) {
        return null;
    }

    @Override
    public void setDataType() {
        this.dataType = ScanProduct.BINLIGHTNING;
    }

    /**
     * Lightning URI Pattern
     * 
     * @return
     */
    public static Pattern getPattern() {
        return Pattern.compile("^" + uriSeparator + BINLIGHTNING + uriSeparator
                + wildCard + uriSeparator + wildCard + uriSeparator + wildCard);

    }

    /**
     * Gets the SQL
     * 
     * @return
     */
    public static String getSQL(int interval) {
        return "select datauri from " + BINLIGHTNING
                + " where reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

}