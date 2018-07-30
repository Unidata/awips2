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
package com.raytheon.uf.common.dataplugin.scan.data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * SCAN Table Data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/29/2009   2037       dhladky     Initial creation
 * 02/01/2013   1569       D. Hladky   removed XML where not needed
 * 05/12/2014   3133       njensen     Remove unused field
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

@DynamicSerialize
public abstract class ScanTableData<T extends ScanTableDataRow> implements
        Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    public ScanTables tableName = null;

    @DynamicSerializeElement
    public Integer vcp = null;

    @DynamicSerializeElement
    public Double trueAngle = null;

    @DynamicSerializeElement
    public Boolean lastElevationAngle;

    @DynamicSerializeElement
    public Date volScanTime = null;

    @DynamicSerializeElement
    public ConcurrentHashMap<String, T> tableData = null;

    @DynamicSerializeElement
    public List<String> featureIds = null;

    public ScanTableData() {
        tableData = new ConcurrentHashMap<String, T>();
        featureIds = new ArrayList<String>();
    }

    /**
     * Add another row
     * 
     * @param id
     * @param scanTableDataRow
     */
    public void addRow(String id, T row) {
        tableData.put(id, row);
    }

    /**
     * Update the row
     * 
     * @param key
     * @param T
     */
    public void updateRow(String key, T row) {
        if (tableData.containsKey(row)) {
            synchronized (tableData) {
                addRow(key, row);
            }
        }
    }

    /**
     * Remove an entry
     * 
     * @param key
     */
    public void removeRow(String key) {
        if (tableData.containsKey(key)) {
            synchronized (tableData) {
                tableData.remove(key);
            }
        }
    }

    /**
     * Get the row
     * 
     * @param key
     * @return
     */
    public T getRow(String key) {
        T row = null;
        if (tableData.containsKey(key)) {
            row = tableData.get(key);
        }
        return row;
    }

    /**
     * Get the HashMap
     * 
     * @return
     */
    public ConcurrentHashMap<String, T> getTableData() {
        return tableData;
    }

    /**
     * Set the HashMap
     * 
     * @return
     */
    public void setTableData(ConcurrentHashMap<String, T> tableData) {
        this.tableData = tableData;
    }

    /**
     * set up thresholds map according to table params
     */
    public abstract void configureThresholds();

    /**
     * make a copy
     */
    public abstract ScanTableData<?> copy();

    /**
     * copy the map
     */
    public abstract ScanTableData<?> copyMap(ScanTableData<?> table);

    public ScanTables getTableName() {
        return tableName;
    }

    public void setTableName(ScanTables tableName) {
        this.tableName = tableName;
    }

    /**
     * true angle
     * 
     * @return
     */
    public Double getTrueAngle() {
        return trueAngle;
    }

    /**
     * sets true angle
     * 
     * @param trueAngle
     */
    public void setTrueAngle(Double trueAngle) {
        this.trueAngle = trueAngle;
    }

    public Boolean getLastElevationAngle() {
        if (lastElevationAngle == null) {
            return false;
        }
        return lastElevationAngle;
    }

    public void setLastElevationAngle(Boolean lastElevationAngle) {
        this.lastElevationAngle = lastElevationAngle;
    }

    /**
     * Volume scan time
     * 
     * @return
     */
    public Date getVolScanTime() {
        return volScanTime;
    }

    /**
     * sets the volume scan time
     * 
     * @param volScanTime
     */
    public void setVolScanTime(Date volScanTime) {
        this.volScanTime = volScanTime;
    }

    public List<String> getFeatureIds() {
        return featureIds;
    }

    public void setFeatureIds(List<String> featureIds) {
        this.featureIds = featureIds;
    }

    public Integer getVcp() {
        return vcp;
    }

    public void setVcp(Integer vcp) {
        this.vcp = vcp;
    }

}
