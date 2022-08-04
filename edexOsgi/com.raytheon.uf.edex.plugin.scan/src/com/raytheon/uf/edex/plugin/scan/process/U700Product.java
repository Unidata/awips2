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
import com.raytheon.uf.common.dataplugin.scan.ScanException;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * U-Wind Component Product at 700 mb
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2010 5098       grichard    Initial creation
 * Apr 24, 2014 2060       njensen     Updates for removal of grid dataURI column
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class U700Product extends GridProduct {

    private static final long serialVersionUID = 1L;

    public static final String U700 = SCANRunSiteConfigurationManager.DATA_TYPE.U700
            .getType();

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public U700Product(String uri, ScanTables tableType, ScanURIFilter filter) {
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
            filter.setGridRecord(U700, uri);
            // statusHandler.handle(Priority.INFO, "MATCHED " + U700
            // + " MODEL URI: " + uri);
            grib = filter.getGridRecord(U700);
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
            filter.setGridRecord(U700, rec);
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

        // if (filter.getModelData() != null) {
        // ((CellTableDataRow) row).setCape(filter.getModelData().getValue(
        // U700, new Coordinate(row.getLon(), row.getLat())));
        // }
        // return row;
    }

    /**
     * CAPE URI Pattern
     * 
     * @return
     */
    public static Pattern getPattern(String model) {
        return getGridPattern(model, "uW", "MB", "700.0",
                Level.getInvalidLevelValueAsString());
    }

    /**
     * Retrieves the record for u700
     * 
     * @return
     * @throws ScanException
     */
    public static GridRecord getGridRecord(int interval, String model)
            throws ScanException {
        return getGridRecord(interval, model, "uW", "MB", "700.0",
                Level.getInvalidLevelValueAsString());
    }

}
