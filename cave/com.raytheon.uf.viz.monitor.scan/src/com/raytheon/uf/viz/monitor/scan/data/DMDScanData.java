package com.raytheon.uf.viz.monitor.scan.data;

/**
 * 
 * DMDScanData
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010   4288         dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

import java.util.Date;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeMap;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.serialization.ISerializableObject;

public class DMDScanData implements ISerializableObject {

    private HashMap<Long, ScanTableData<?>> data = null;

    private Long startTime = null;

    public DMDScanData() {
        data = new HashMap<Long, ScanTableData<?>>();
    }

    public DMDScanData(Long startTime) {
        data = new HashMap<Long, ScanTableData<?>>();
        setStartTime(startTime);
    }

    public HashMap<Long, ScanTableData<?>> getData() {
        return data;
    }

    public void setData(HashMap<Long, ScanTableData<?>> data) {
        this.data = data;
    }

    /**
     * gets the DMD by time
     * 
     * @param time
     * @return
     */
    public ScanTableData<?> getTableData(Long time) {
        if (data.size() > 0) {
            Set<Long> dataTimeSet = this.getKeys();
           long max = 0;
            for (Long millis: dataTimeSet) {
                if (millis > max) {
                    max = millis;
                }
            }

            return data.get(max);
        }
        
        return null;
    }

    public void addData(Double angle, long dataTime, ScanTableData<?> tableData) {
//        System.out.println("DMDScanData.addData(): Adding data...  " + angle + "  " + new Date(dataTime));
//        System.out.println("    ---- angle = " + angle);
//        System.out.println("    ---- dataTime = " + dataTime + "  " + new Date(dataTime));
//        System.out.println("    ---- getStartTime() = " 
//                + new Date(getStartTime()));
//        System.out.println("    ---- offset = " + (dataTime - getStartTime()) + "\n\n");

        data.put(dataTime, tableData);
    }

    /**
     * Gets the keys
     * 
     * @return
     */
    public Set<Long> getKeys() {
        return data.keySet();
    }

    /**
     * contains key check
     * 
     * @param angle
     * @return
     */
    public boolean containsKey(Double angle) {
        if (data.containsKey(angle)) {
            return true;
        }
        return false;
    }

    /**
     * Gets a Vertical Column for the time height display graph
     * 
     * @param tableCol
     * @param dmdIdent
     * @param angle
     * @return
     */
    public TreeMap<Long, DMDTableDataRow> getTimeHeightData(
            SCANConfigEnums.DMDTable tableCol, String dmdIdent) {

        TreeMap<Long, DMDTableDataRow> graphData = new TreeMap<Long, DMDTableDataRow>();
       
        for (Long dataTime : getKeys()) {
            DMDTableDataRow dtrd = (DMDTableDataRow) getTableData(dataTime).getRow(dmdIdent);
            
            // if it is an "EXT" feature, should not access the time-height data for trend draw.
            if ((dtrd == null) || dtrd.getStatus().equalsIgnoreCase("EXT")) {
                continue;
            }
            graphData.put(dataTime.longValue(), dtrd);
        }

        return graphData;
    }

    public Long getStartTime() {
        return startTime;
    }

    public void setStartTime(Long startTime) {
        this.startTime = startTime;
    }

}
