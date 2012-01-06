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
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * Geopotential Height Product at 1000 mb
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2010 5098       grichard    Initial creation
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class GH1000Product extends ScanProduct {

    private static final long serialVersionUID = 1L;

    public static final String GH1000 = SCANRunSiteConfigurationManager.DATA_TYPE.GH1000
            .getType();

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public GH1000Product(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public boolean getAllowNew() {
        return false;
    }

    @Override
    public PersistablePluginDataObject getRecord() throws PluginException,
            Exception {
        GribRecord grib = null;
        try {
            filter.setGribRecord(GH1000, uri);
            // statusHandler.handle(Priority.INFO, "MATCHED " + GH1000
            // + " MODEL URI: " + uri);
            grib = filter.getGribRecord(GH1000);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return grib;
    }

    @Override
    public void process() throws Exception {

        GribRecord rec = null;
        try {
            rec = (GribRecord) getRecord();
        } catch (Exception pe) {
            pe.printStackTrace();
        }
        if (rec != null) {
            filter.setGribRecord(GH1000, rec);
        }
    }

    @Override
    public void setDataType() {
        this.dataType = GRIB;

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
        // GH1000, new Coordinate(row.getLon(), row.getLat())));
        // }
        // return row;
    }

    /**
     * CAPE URI Pattern
     * 
     * @return
     */
    public static Pattern getPattern(String model) {
        return Pattern.compile("^" + uriSeparator + GRIB + uriSeparator
                + wildCard + uriSeparator + model + uriSeparator + "GH"
                + uriSeparator + "MB" + uriSeparator + "1000.0" + uriSeparator
                + Level.getInvalidLevelValueAsString() + uriSeparator + "null"
                + uriSeparator + "null" + uriSeparator + "0");
    }

    /**
     * The SQL
     * 
     * @return
     */
    public static String getSQL(int interval, String model) {
        return "select datauri from grib where modelinfo_id = (select id from grib_models where parameterabbreviation = \'"
                + "GH"
                + "\' and modelname = \'"
                + model
                + "\' and level_id = (select id from level where masterlevel_name = 'MB' and levelonevalue = '1000.0' and leveltwovalue = "
                + "\'"
                + Level.getInvalidLevelValueAsString()
                + "\'"
                + ")) and reftime > (now()- interval \'"
                + interval
                + " minutes\') order by forecasttime desc" + " limit 1";
    }

}
