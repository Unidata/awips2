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
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * Storm Relative Helicity Product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010 5098       grichard    Initial creation
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class HELIProduct extends GridProduct {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static final String heli = SCANRunSiteConfigurationManager.DATA_TYPE.HELI
            .getType();

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public HELIProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public boolean getAllowNew() {
        return false;
    }

    @Override
    public PersistablePluginDataObject getRecord() throws PluginException,
            Exception {
        GridRecord grib = null;
        try {
            filter.setGridRecord(HELIProduct.heli, uri);
            // statusHandler.handle(Priority.INFO, "MATCHED " + heli
            // + " MODEL URI: " + uri);
            grib = filter.getGridRecord(HELIProduct.heli);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return grib;
    }

    @Override
    public void process() throws Exception {

        GridRecord rec = null;
        try {
            rec = (GridRecord) getRecord();
        } catch (Exception pe) {
            pe.printStackTrace();
        }
        if (rec != null) {
            filter.setGridRecord(heli, rec);
        }
    }

    @Override
    public void setDataType() {
        this.dataType = GRID;

    }

    @Override
    public ScanTableDataRow setSpatial(ScanTableDataRow row, String key,
            PersistableDataObject pdo) {
        return null;
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {

        return null;
    }

    /**
     * HELI URI Pattern
     * 
     * @return
     */
    public static Pattern getPattern(String model) {
        return getGridPattern(model, "Heli", "SFC", "0.0",
                Level.getInvalidLevelValueAsString());
    }

    /**
     * The SQL HELI
     * 
     * @param interval
     * @return
     */
    public static String getSQL(int interval, String model) {
        return getGridSQL(interval, model, "Heli", "SFC", "0.0",
                Level.getInvalidLevelValueAsString());
    }
}
