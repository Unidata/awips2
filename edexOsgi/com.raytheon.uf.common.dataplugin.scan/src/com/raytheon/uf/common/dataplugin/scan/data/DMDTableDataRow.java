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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * SCAN DMD Table Data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 29, 2009   2037    dhladky     Initial creation
 * 02/01/13     1569        D. Hladky   removed XML where not needed
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */
@DynamicSerialize
public class DMDTableDataRow extends ScanTableDataRow {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private StringBuffer displayString = null;

    public DMDTableDataRow(DataTime time) {
        super(time);
    }

    public DMDTableDataRow() {

    }

    /** storm ID **/
    @DynamicSerializeElement
    public String strmID = "N/A";

    /** Low level rotational velocity **/
    @DynamicSerializeElement
    public Double llVr = 0.0;

    /** low level gate to gate velocity **/
    @DynamicSerializeElement
    public Double llgtg = 0.0;

    /** base of circulation height kft **/
    @DynamicSerializeElement
    public Double base = 0.0;

    /** depth of circulation depth kft **/
    @DynamicSerializeElement
    public Double depth = 0.0;

    /** relative depth % **/
    @DynamicSerializeElement
    public Double relDepth = 0.0;

    /** low level diameter **/
    @DynamicSerializeElement
    public Double llDiam = 0.0;

    /** max rotational velocity **/
    @DynamicSerializeElement
    public Double maxVr = 0.0;

    /** height of max rotational velocity **/
    @DynamicSerializeElement
    public Double htMxVr = 0.0;

    /** low level convergence **/
    @DynamicSerializeElement
    public Double llConv = 0.0;

    /** mid level convergence **/
    @DynamicSerializeElement
    public Double mlConv = 0.0;

    /** Low level shear **/
    @DynamicSerializeElement
    public Double llShear = 0.0;

    /** tvs Tornado Vortex Sig **/
    @DynamicSerializeElement
    public String tvs = "N/A";

    /** MSI mean strength index **/
    @DynamicSerializeElement
    public Integer msi = 0;

    /** age of storm **/
    @DynamicSerializeElement
    public Integer age = 0;

    /** zero level elevation **/
    @DynamicSerializeElement
    public String elev0 = "N/A";

    /** status **/
    @DynamicSerializeElement
    public String status = "N/A";

    /** rank **/
    @DynamicSerializeElement
    public String rank = "N/A";

    /** rankType **/
    @DynamicSerializeElement
    public String rankType = "";

    /** overlap **/
    @DynamicSerializeElement
    public boolean overlap = false;

    /** fcst lat **/
    @DynamicSerializeElement
    public ArrayList<Double> fcstLat = null;

    /** fsct lon **/
    @DynamicSerializeElement
    public ArrayList<Double> fcstLon = null;

    @DynamicSerializeElement
    public ArrayList<Double> pastLat = null;

    /** fsct lon **/
    @DynamicSerializeElement
    public ArrayList<Double> pastLon = null;

    /** timeHeight_height **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightHeight = null;

    /** timeHeight_diam **/
    @DynamicSerializeElement
    @XmlElement
    public ArrayList<Double> timeHeightDiam = null;

    /** timeHeight_rotvel **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightRotvel = null;

    /** timeHeight_shear **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightShear = null;

    /** timeHeight_gtgmax **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightGtgMax = null;

    /** timeHeight_rank **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightRank = null;

    /** angles **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightElevationAngles = null;

    /** indexes **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightElevationIndexes = null;

    /** times **/
    @DynamicSerializeElement
    public ArrayList<Double> timeHeightTimes = null;

    /**
     * Get storm ID
     * 
     * @return
     */
    public String getStrmID() {
        return strmID;
    }

    /**
     * Set the storm ID
     * 
     * @param strmID
     */
    public void setStrmID(String strmID) {
        this.strmID = strmID;
    }

    /**
     * Get low level velocity
     * 
     * @return
     */
    public Double getLlVr() {
        return llVr;
    }

    /**
     * Set low level velocity
     * 
     * @param llVr
     */
    public void setLlVr(Double llVr) {
        this.llVr = llVr;
    }

    /**
     * Get low level diameter
     * 
     * @return
     */
    public Double getLlDiam() {
        return llDiam;
    }

    /**
     * Set low level diameter
     * 
     * @param llDiam
     */
    public void setLlDiam(Double llDiam) {
        this.llDiam = llDiam;
    }

    /**
     * Get lowl level gate to gate velocity
     * 
     * @return
     */
    public Double getLlgtg() {
        return llgtg;
    }

    /**
     * set low level gate to gate velocity
     * 
     * @param llgtg
     */
    public void setLlgtg(Double llgtg) {
        this.llgtg = llgtg;
    }

    /**
     * get the base height
     * 
     * @return
     */
    public Double getBase() {
        return base;
    }

    /**
     * Set the base height
     * 
     * @param base
     */
    public void setBase(Double base) {
        this.base = base;
    }

    /**
     * Get the depth of rotation
     * 
     * @return
     */
    public Double getDepth() {
        return depth;
    }

    /**
     * Set the depth of rotation
     * 
     * @param depth
     */
    public void setDepth(Double depth) {
        this.depth = depth;
    }

    /**
     * get realtive depth as %
     * 
     * @return
     */
    public Double getRelDepth() {
        return relDepth;
    }

    /**
     * Set relative depth as %
     * 
     * @param relDepth
     */
    public void setRelDepth(Double relDepth) {
        this.relDepth = relDepth;
    }

    /**
     * get Max Rotational Velocity
     * 
     * @return
     */
    public Double getMaxVr() {
        return maxVr;
    }

    /**
     * Set max rotational velocity
     * 
     * @param maxVr
     */
    public void setMaxVr(Double maxVr) {
        this.maxVr = maxVr;
    }

    /**
     * get Low Level Convergence
     * 
     * @return
     */
    public Double getLlConv() {
        return llConv;
    }

    /**
     * Set Low Level Convergence
     * 
     * @param maxVr
     */
    public void setLlConv(Double llConv) {
        this.llConv = llConv;
    }

    /**
     * get Mid Level Convergence
     * 
     * @return
     */
    public Double getMlConv() {
        return mlConv;
    }

    /**
     * Set Mid Level Convergence
     * 
     * @param maxVr
     */
    public void setMlConv(Double mlConv) {
        this.mlConv = mlConv;
    }

    /**
     * get Low Level Shear
     * 
     * @return
     */
    public Double getLlShear() {
        return llShear;
    }

    /**
     * Set Low Level Shear
     * 
     * @param maxVr
     */
    public void setLlShear(Double llShear) {
        this.llShear = llShear;
    }

    /**
     * get height of max rotational velocity
     * 
     * @return
     */
    public Double getHtMxVr() {
        return htMxVr;
    }

    /**
     * set height of max rotational velocity
     * 
     * @param htMxVr
     */
    public void setHtMxVr(Double htMxVr) {
        this.htMxVr = htMxVr;
    }

    /**
     * Get the TVS value
     * 
     * @return
     */
    public String getTvs() {
        return tvs;
    }

    /**
     * Set the TVS value
     * 
     * @param tvs
     */
    public void setTvs(String tvs) {
        this.tvs = tvs;
    }

    /**
     * Get the Mean Strength Index MSI
     * 
     * @return
     */
    public Integer getMsi() {
        return msi;
    }

    /**
     * Set the Mean Strength Index MSI
     * 
     * @param msi
     */
    public void setMsi(Integer msi) {
        this.msi = msi;
    }

    /**
     * Get the Age as an integer of minutes
     * 
     * @return
     */
    public Integer getAge() {
        return age;
    }

    /**
     * Set the Age as an integer of minutes
     * 
     * @param msi
     */
    public void setAge(Integer age) {
        this.age = age;
    }

    public String getElev0() {
        return elev0;
    }

    public void setElev0(String elev0) {
        this.elev0 = elev0;
    }

    public String getLowestElev(String tilts) {
        String result = "N";
        String[] elevations;

        elevations = tilts.split(",");

        for (String elev : elevations) {
            try {
                if (Integer.parseInt(elev) == 0) {
                    result = "Y";
                    break;
                }
            } catch (NumberFormatException e) {
                // System.out.println("SCAN DMD elev0 tilt: bad format");
            }
        }

        return result;
    }

    /**
     * gets the status
     * 
     * @return
     */
    public String getStatus() {
        return status;
    }

    /**
     * sets the status
     * 
     * @param status
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Get the rank
     * 
     * @return
     */
    public String getRank() {
        return rank;
    }

    /**
     * Sets the rank
     * 
     * @param rank
     */
    public void setRank(String rank) {
        this.rank = rank;
    }

    /**
     * Get the rank type
     * 
     * @return
     */
    public String getRankType() {
        return rankType;
    }

    /**
     * Sets the rank type
     * 
     * @param rankType
     */
    public void setRankType(String rankType) {
        this.rankType = rankType;
    }

    /**
     * Get the overlap
     * 
     * @return
     */
    public boolean getOverlap() {
        return overlap;
    }

    /**
     * Sets the overlap
     * 
     * @param overlap
     */
    public void setOverlap(boolean overlap) {
        this.overlap = overlap;
    }

    /**
     * latitude forecast
     * 
     * @return
     */
    public ArrayList<Double> getFcstLat() {
        return fcstLat;
    }

    /**
     * Set the forecast lat
     * 
     * @param llDiam
     */
    public void setFcstLat(ArrayList<Double> fcstLat) {
        this.fcstLat = fcstLat;
    }

    /**
     * latitude forecast
     * 
     * @return
     */
    public ArrayList<Double> getFcstLon() {
        return fcstLon;
    }

    /**
     * Sets the foecast lon
     * 
     * @param llDiam
     */
    public void setFcstLon(ArrayList<Double> fcstLon) {
        this.fcstLon = fcstLon;
    }

    /**
     * latitude past
     * 
     * @return
     */
    public ArrayList<Double> getPastLat() {
        return pastLat;
    }

    /**
     * Set the past lat
     * 
     * @param pastlat
     */
    public void setPastLat(ArrayList<Double> pastLat) {
        this.pastLat = pastLat;
    }

    /**
     * latitude past
     * 
     * @return
     */
    public ArrayList<Double> getPastLon() {
        return pastLon;
    }

    /**
     * Sets the past lon
     * 
     * @param past
     */
    public void setPastLon(ArrayList<Double> pastLon) {
        this.pastLon = pastLon;
    }

    /**
     * time height graph params
     * 
     * @return
     */

    public ArrayList<Double> getTimeHeightHeight() {
        return timeHeightHeight;
    }

    public void setTimeHeightHeight(ArrayList<Double> timeHeightHeight) {
        this.timeHeightHeight = timeHeightHeight;
    }

    public ArrayList<Double> getTimeHeightDiam() {
        return timeHeightDiam;
    }

    public void setTimeHeightDiam(ArrayList<Double> timeHeightDiam) {
        this.timeHeightDiam = timeHeightDiam;
    }

    public ArrayList<Double> getTimeHeightRotvel() {
        return timeHeightRotvel;
    }

    public void setTimeHeightRotvel(ArrayList<Double> timeHeightRotvel) {
        this.timeHeightRotvel = timeHeightRotvel;
    }

    public ArrayList<Double> getTimeHeightShear() {
        return timeHeightShear;
    }

    public void setTimeHeightShear(ArrayList<Double> timeHeightShear) {
        this.timeHeightShear = timeHeightShear;
    }

    public ArrayList<Double> getTimeHeightGtgMax() {
        return timeHeightGtgMax;
    }

    public void setTimeHeightGtgMax(ArrayList<Double> timeHeightGtgMax) {
        this.timeHeightGtgMax = timeHeightGtgMax;
    }

    public ArrayList<Double> getTimeHeightRank() {
        return timeHeightRank;
    }

    public void setTimeHeightRank(ArrayList<Double> timeHeightRank) {
        this.timeHeightRank = timeHeightRank;
    }

    public ArrayList<Double> getTimeHeightElevationIndexes() {
        return timeHeightElevationIndexes;
    }

    public void setTimeHeightElevationIndexes(
            ArrayList<Double> timeHeightElevationIndexes) {
        this.timeHeightElevationIndexes = timeHeightElevationIndexes;
    }

    public ArrayList<Double> getTimeHeightElevationAngles() {
        return timeHeightElevationAngles;
    }

    public void setTimeHeightElevationAngles(
            ArrayList<Double> timeHeightElevationAngles) {
        this.timeHeightElevationAngles = timeHeightElevationAngles;
    }

    public ArrayList<Double> getTimeHeightTimes() {
        return timeHeightTimes;
    }

    public void setTimeHeightTimes(ArrayList<Double> timeHeightTimes) {
        this.timeHeightTimes = timeHeightTimes;
    }

    /**
     * Gets the value by column
     * 
     * @param column
     * @return
     */
    @Override
    public Double getValue(String column) {
        Double value = null;
        if (column.equals(SCANConfigEnums.DMDTable.AZM.getColName())) {
            value = azm;
        } else if (column.equals(SCANConfigEnums.DMDTable.RNG.getColName())) {
            value = rng;
        } else if (column.equals(SCANConfigEnums.DMDTable.STRANK.getColName())) {
            // Since stRank can have letters we must strip
            // out the potential characters.
            StringBuilder sb = new StringBuilder();
            char[] rankChars = rank.toCharArray();

            for (int i = 0; i < rankChars.length; i++) {
                if (String.valueOf(rankChars[i]).matches("[0-9.]+") == true) {
                    sb.append(rankChars[i]);
                }
            }
            if (!sb.toString().isEmpty()) {
                value = Double.valueOf(sb.toString()).doubleValue();
            } else {
                value = 0.0;
            }
        } else if (column.equals(SCANConfigEnums.DMDTable.MSI.getColName())) {
            value = msi.doubleValue();
        } else if (column.equals(SCANConfigEnums.DMDTable.BASE.getColName())) {
            value = base;
        } else if (column.equals(SCANConfigEnums.DMDTable.DEPTH.getColName())) {
            value = depth;
        } else if (column.equals(SCANConfigEnums.DMDTable.RELDEP.getColName())) {
            value = relDepth;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLDIAM.getColName())) {
            value = llDiam;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLVR.getColName())) {
            value = llVr;
        } else if (column.equals(SCANConfigEnums.DMDTable.MAXVR.getColName())) {
            value = maxVr;
        } else if (column.equals(SCANConfigEnums.DMDTable.HTMXVR.getColName())) {
            value = htMxVr;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLSHR.getColName())) {
            value = llShear;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLGTG.getColName())) {
            value = llgtg;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLCONV.getColName())) {
            value = llConv;
        } else if (column.equals(SCANConfigEnums.DMDTable.MLCONV.getColName())) {
            value = mlConv;
        } else if (column.equals(SCANConfigEnums.DMDTable.DIR.getColName())) {
            value = dir;
        } else if (column.equals(SCANConfigEnums.DMDTable.SPD.getColName())) {
            value = spd;
        }
        return value;
    }

    /**
     * used for debugging / screen mouse-over
     */
    @Override
    public String toString() {

        if (displayString != null) {
            return displayString.toString();
        }

        displayString = new StringBuffer("ident: " + ident + "    ");

        if (strmID != null) {
            displayString.append("Storm ID: " + strmID + "\n");
        } else {
            displayString.append("Storm ID: N/A \n");
        }

        if (status != null) {
            displayString.append("status: " + status + "\n");
        } else {
            displayString.append("status: N/A \n");
        }

        if (base > 0) {
            displayString.append("base: " + formatNumber(base) + "    ");
        } else {
            displayString.append("base: N/A    ");
        }

        if (depth > 0) {
            displayString.append("depth: " + formatNumber(depth) + "\n");
        } else {
            displayString.append("depth: N/A \n");
        }

        if (rank != null) {
            displayString.append("stRank: " + rank + "    ");
        } else {
            displayString.append("stRank: N/A      ");
        }

        if (msi > 0) {
            displayString.append("msi: " + msi + "\n");
        } else {
            displayString.append("msi: N/A \n");
        }

        if (llVr >= 0) {
            displayString.append("llVr: " + formatNumber(llVr) + "\n");
        } else {
            displayString.append("llVr: N/A \n");
        }

        if (llgtg >= 0) {
            displayString.append("llgtg: " + formatNumber(llgtg) + "\n");
        } else {
            displayString.append("llgtg: N/A \n");
        }

        if (llConv >= 0) {
            displayString.append("llConv: " + formatNumber(llConv) + "\n");
        } else {
            displayString.append("llConv: N/A \n");
        }

        return displayString.toString();
    }

    private String formatNumber(Double dbl) {
        String rv = "N/A";

        if (dbl.isNaN()) {
            return rv;
        }

        try {
            rv = String.valueOf(Math.round(dbl));
        } catch (Exception e) {
            return "N/A";
        }

        return rv;
    }

    @Override
    public ScanTableDataRow copy() {
        DMDTableDataRow row = new DMDTableDataRow(this.getTime());
        row = (DMDTableDataRow) copyCommon(row);
        row.setStrmID(this.getStrmID());
        row.setLlVr(this.getLlVr());
        row.setLlgtg(this.getLlgtg());
        row.setBase(this.getBase());
        row.setDepth(this.getDepth());
        row.setRelDepth(this.getRelDepth());
        row.setLlDiam(this.getLlDiam());
        row.setMaxVr(this.getMaxVr());
        row.setHtMxVr(this.getHtMxVr());
        row.setLlConv(this.getLlConv());
        row.setMlConv(this.getMlConv());
        row.setLlShear(this.getLlShear());
        row.setTvs(this.getTvs());
        row.setMsi(this.getMsi());
        row.setAge(this.getAge());
        row.setElev0(this.getElev0());
        row.setStatus(this.getStatus());
        row.setRank(this.getRank());
        row.setRankType(this.getRankType());
        row.setOverlap(this.getOverlap());
        row.setFcstLon(this.getFcstLon());
        row.setFcstLat(this.getFcstLat());
        row.setPastLon(this.getPastLon());
        row.setPastLat(this.getPastLat());
        row.setTimeHeightHeight(this.getTimeHeightHeight());
        row.setTimeHeightDiam(this.getTimeHeightDiam());
        row.setTimeHeightGtgMax(this.getTimeHeightGtgMax());
        row.setTimeHeightRank(this.getTimeHeightRank());
        row.setTimeHeightRotvel(this.getTimeHeightRotvel());
        row.setTimeHeightShear(this.getTimeHeightShear());
        row.setTimeHeightElevationIndexes(this.getTimeHeightElevationIndexes());
        row.setTimeHeightElevationAngles(this.getTimeHeightElevationAngles());
        row.setTimeHeightTimes(this.getTimeHeightTimes());

        return row;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub

    }

}
