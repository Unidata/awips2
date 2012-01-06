package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;

/**
 * Drawable implementation for FFMP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/23/10     4494        D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPDrawable {

    private ConcurrentMap<String, HashMap<Object, RGB>> colorMaps = new ConcurrentHashMap<String, HashMap<Object, RGB>>(
            10, 0.75f, 5);

    private ConcurrentMap<String, FFMPTableData> tableData = new ConcurrentHashMap<String, FFMPTableData>(
            10, 0.75f, 5);

    private boolean isDirty = true;

    private FIELDS field = null;

    private String huc = null;

    private String shadedHuc = null;

    private Object centerAggrKey = null;

    private boolean isMaintainLayer = false;

    private boolean isParent = true;

    private boolean isWorstCase = true;

    private DataTime time = null;

    private PixelExtent ext = null;

    private double dtime = 0.0;

    protected HashMap<Long, Float> worstCaseHash = new HashMap<Long, Float>();

    private ConcurrentMap<String, Set<Long>> basins = new ConcurrentHashMap<String, Set<Long>>(
            10, 0.75f, 5);

    private String guidType = null;

    public FFMPDrawable(Collection<DomainXML> domains) {
        setValidDomains(domains);
    }

    public void setValidDomains(Collection<DomainXML> domains) {
        Set<String> cwasToRemove = new HashSet<String>(basins.keySet());
        for (DomainXML domain : domains) {
            cwasToRemove.remove(domain.getCwa());
        }

        for (String cwa : cwasToRemove) {
            disposeCwa(cwa);
        }

        tableData.clear();
        worstCaseHash.clear();
    }

    /**
     * Disposes of currently drawn shapes.
     */
    public void dispose() {

        disposeImage();

        huc = null;
        tableData.clear();
        worstCaseHash.clear();
    }

    /**
     * Disposes of currently drawn shapes.
     */
    public void disposeImage() {
        for (String cwa : basins.keySet()) {
            disposeCwa(cwa);
        }
    }

    protected void disposeCwa(String cwa) {
        HashMap<Object, RGB> colorMap = colorMaps.remove(cwa);
        if (colorMap != null) {
            colorMap.clear();
            colorMap = null;
        }

        basins.remove(cwa);
        // tableData.clear();
        worstCaseHash.clear();
    }

    public void setColorMap(String cwa, HashMap<Object, RGB> newMap) {
        HashMap<Object, RGB> oldMap = colorMaps.remove(cwa);
        if (oldMap != null) {
            oldMap.clear();
            oldMap = null;
        }
        colorMaps.put(cwa, newMap);
    }

    public HashMap<Object, RGB> getColorMap(String cwa) {
        if (colorMaps.containsKey(cwa)) {
            return colorMaps.get(cwa);
        }
        return null;
    }

    public void setDirty(boolean isDirty) {
        this.isDirty = isDirty;
    }

    public DataTime getTime() {
        return time;
    }

    public void setTime(DataTime time) {
        this.time = time;
    }

    public FIELDS getField() {
        return field;
    }

    public void setField(FIELDS field) {
        this.field = field;
    }

    public String getHuc() {
        return huc;
    }

    public void setHuc(String huc) {
        this.huc = huc;
    }

    public Object getCenterAggrKey() {
        return centerAggrKey;
    }

    public void setCenterAggrKey(Object centerAggrKey) {
        this.centerAggrKey = centerAggrKey;
    }

    public boolean isMaintainLayer() {
        return isMaintainLayer;
    }

    public void setMaintainLayer(boolean isMaintainLayer) {
        this.isMaintainLayer = isMaintainLayer;
    }

    public boolean isParent() {
        return isParent;
    }

    public void setParent(boolean isParent) {
        this.isParent = isParent;
    }

    public boolean isDirty() {
        return isDirty;
    }

    public boolean genCwa(String cwa) {
        return colorMaps.containsKey(cwa);
    }

    public boolean isWorstCase() {
        return isWorstCase;
    }

    public void setWorstCase(boolean isWorstCase) {
        this.isWorstCase = isWorstCase;
    }

    public PixelExtent getExt() {
        return ext;
    }

    public void setExt(PixelExtent ext) {
        this.ext = ext;
    }

    public Set<Long> getBasins(String cwa) {
        return basins.get(cwa);
    }

    public void setBasins(String cwa, Set<Long> basins) {
        this.basins.put(cwa, basins);
    }

    public void setTableData(String thuc, FFMPTableData tData) {
        tableData.put(thuc, tData);
    }

    public FFMPTableData getTableData(String thuc) {
        if (tableData.containsKey(thuc)) {
            return tableData.get(thuc);
        } else {
            return null;
        }
    }

    public void removeTable(String huc) {
        if (tableData.containsKey(huc)) {
            tableData.remove(huc);
        }
    }

    /**
     * Add a value to worst case hash
     * 
     * @param aggPfaf
     * @param value
     */
    protected void addWorstCase(Long aggPfaf, Float value) {
        worstCaseHash.put(aggPfaf, value);
    }

    /**
     * Clear the worst case hash
     */
    public void clearWorstCase() {
        worstCaseHash.clear();
    }

    /**
     * Get a value from the worst case hash
     * 
     * @param aggPfaf
     * @return
     */
    protected double getWorstCaseValue(Long aggPfaf) {
        return worstCaseHash.get(aggPfaf);
    }

    /**
     * check to see if aggregate exists in hash
     * 
     * @param aggPfaf
     * @return
     */
    protected boolean checkWorstCase(Long aggPfaf) {
        return worstCaseHash.containsKey(aggPfaf);
    }

    protected HashMap<Long, Float> getWorstCaseHash() {
        return worstCaseHash;
    }

    public void setShadedHuc(String shadedHuc) {
        this.shadedHuc = shadedHuc;
    }

    public String getShadedHuc() {
        return shadedHuc;
    }

    public void setDrawTime(double dtime) {
        this.dtime = dtime;
    }

    public double getDrawTime() {
        return dtime;
    }

    public void clearTables() {
        tableData.clear();
    }

    /**
     * @return the guidType
     */
    public String getGuidType() {
        return guidType;
    }

    /**
     * @param guidType
     *            the guidType to set
     */
    public void setGuidType(String guidType) {
        this.guidType = guidType;
    }
}
