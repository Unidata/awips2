package com.raytheon.uf.common.dataplugin.scan.data;

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

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * TVS Table Data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/11/2009   1981       dhladky    Initial Creation.
 * 02/01/13     1569       D. Hladky   removed XML where not needed
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@SuppressWarnings({ "unchecked", "rawtypes" })
@DynamicSerialize
public class TVSTableData<T extends ScanTableDataRow> extends ScanTableData {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public TVSTableData() {
        tableName = ScanTables.TVS;
    }

    @Override
    public void configureThresholds() {
        // TODO Auto-generated method stub

    }

    @Override
    public ScanTableData copyMap(ScanTableData table) {
        for (Object id : getTableData().keySet()) {
            TVSTableDataRow row = (TVSTableDataRow) getTableData().get(id);
            table.addRow((String) id, row.copy());
        }
        return table;
    }

    @Override
    public ScanTableData copy() {
        TVSTableData table = new TVSTableData();
        table.setTrueAngle(this.getTrueAngle());
        table.setVolScanTime(this.getVolScanTime());
        table.setFeatureIds(this.getFeatureIds());
        return (TVSTableData) copyMap(table);
    }

}
