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

import java.util.List;

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
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------
 * Apr 29, 2009  2037     dhladky   Initial creation
 * Feb 01, 2013  1569     dhladky   removed XML where not needed
 * Apr 03, 2018  6696     randerso  Code cleanup
 *
 * </pre>
 *
 * @author dhladky
 *
 */
@DynamicSerialize
public class DMDTableDataRow extends ScanTableDataRow {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private String displayString = null;

    /** storm ID **/
    @DynamicSerializeElement
    private String strmID = "N/A";

    /** Low level rotational velocity **/
    @DynamicSerializeElement
    private Double llVr = 0.0;

    /** low level gate to gate velocity **/
    @DynamicSerializeElement
    private Double llgtg = 0.0;

    /** base of circulation height kft **/
    @DynamicSerializeElement
    private Double base = 0.0;

    /** depth of circulation depth kft **/
    @DynamicSerializeElement
    private Double depth = 0.0;

    /** relative depth % **/
    @DynamicSerializeElement
    private Double relDepth = 0.0;

    /** low level diameter **/
    @DynamicSerializeElement
    private Double llDiam = 0.0;

    /** max rotational velocity **/
    @DynamicSerializeElement
    private Double maxVr = 0.0;

    /** height of max rotational velocity **/
    @DynamicSerializeElement
    private Double htMxVr = 0.0;

    /** low level convergence **/
    @DynamicSerializeElement
    private Double llConv = 0.0;

    /** mid level convergence **/
    @DynamicSerializeElement
    private Double mlConv = 0.0;

    /** Low level shear **/
    @DynamicSerializeElement
    private Double llShear = 0.0;

    /** tvs Tornado Vortex Sig **/
    @DynamicSerializeElement
    private String tvs = "N/A";

    /** MSI mean strength index **/
    @DynamicSerializeElement
    private Integer msi = 0;

    /** age of storm **/
    @DynamicSerializeElement
    private Integer age = 0;

    /** zero level elevation **/
    @DynamicSerializeElement
    private String elev0 = "N/A";

    /** status **/
    @DynamicSerializeElement
    private String status = "N/A";

    /** rank **/
    @DynamicSerializeElement
    private String rank = "N/A";

    /** rankType **/
    @DynamicSerializeElement
    private String rankType = "";

    /** overlap **/
    @DynamicSerializeElement
    private boolean overlap = false;

    /** fcst lat **/
    @DynamicSerializeElement
    private List<Double> fcstLat = null;

    /** fsct lon **/
    @DynamicSerializeElement
    private List<Double> fcstLon = null;

    @DynamicSerializeElement
    private List<Double> pastLat = null;

    /** fsct lon **/
    @DynamicSerializeElement
    private List<Double> pastLon = null;

    /** timeHeight_height **/
    @DynamicSerializeElement
    private List<Double> timeHeightHeight = null;

    /** timeHeight_diam **/
    @DynamicSerializeElement
    @XmlElement
    private List<Double> timeHeightDiam = null;

    /** timeHeight_rotvel **/
    @DynamicSerializeElement
    private List<Double> timeHeightRotvel = null;

    /** timeHeight_shear **/
    @DynamicSerializeElement
    private List<Double> timeHeightShear = null;

    /** timeHeight_gtgmax **/
    @DynamicSerializeElement
    private List<Double> timeHeightGtgMax = null;

    /** timeHeight_rank **/
    @DynamicSerializeElement
    private List<Double> timeHeightRank = null;

    /** angles **/
    @DynamicSerializeElement
    private List<Double> timeHeightElevationAngles = null;

    /** indexes **/
    @DynamicSerializeElement
    private List<Double> timeHeightElevationIndexes = null;

    /** times **/
    @DynamicSerializeElement
    private List<Double> timeHeightTimes = null;

    /**
     * Default constructor for serialization
     */
    public DMDTableDataRow() {

    }

    /**
     * Constructor
     *
     * @param time
     */
    public DMDTableDataRow(DataTime time) {
        super(time);
    }

    /**
     * @return the storm ID
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
     * @return the low level velocity
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
     * @return the low level diameter
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
     * @return the lowl level gate to gate velocity
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
     * @return the base height
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
     * @return the depth of rotation
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
     * @return the realtive depth as %
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
     * @return the Max Rotational Velocity
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
     * @return the Low Level Convergence
     */
    public Double getLlConv() {
        return llConv;
    }

    /**
     * Set Low Level Convergence
     *
     * @param llConv
     */
    public void setLlConv(Double llConv) {
        this.llConv = llConv;
    }

    /**
     * @return the Mid Level Convergence
     */
    public Double getMlConv() {
        return mlConv;
    }

    /**
     * Set Mid Level Convergence
     *
     * @param mlConv
     */
    public void setMlConv(Double mlConv) {
        this.mlConv = mlConv;
    }

    /**
     * @return the Low Level Shear
     */
    public Double getLlShear() {
        return llShear;
    }

    /**
     * Set Low Level Shear
     *
     * @param llShear
     */
    public void setLlShear(Double llShear) {
        this.llShear = llShear;
    }

    /**
     * @return the height of max rotational velocity
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
     * @return the TVS value
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
     * @return the Mean Strength Index MSI
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
     * @return the Age as an integer of minutes
     */
    public Integer getAge() {
        return age;
    }

    /**
     * Set the Age as an integer of minutes
     *
     * @param age
     */
    public void setAge(Integer age) {
        this.age = age;
    }

    /**
     * @return the elev0
     */
    public String getElev0() {
        return elev0;
    }

    /**
     * Set the elev0
     *
     * @param elev0
     */
    public void setElev0(String elev0) {
        this.elev0 = elev0;
    }

    /**
     * Get the lowest elevation from the tilts string
     *
     * @param tilts
     * @return the lowest elevation
     */
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
     * @return the status
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
     * @return the rank
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
     * @return the rank type
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
     * @return the overlap
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
     * @return the latitude forecast
     */
    public List<Double> getFcstLat() {
        return fcstLat;
    }

    /**
     * Set the forecast lat
     *
     * @param fcstLat
     */
    public void setFcstLat(List<Double> fcstLat) {
        this.fcstLat = fcstLat;
    }

    /**
     * @return the latitude forecast
     */
    public List<Double> getFcstLon() {
        return fcstLon;
    }

    /**
     * Sets the forecast lon
     *
     * @param fcstLon
     */
    public void setFcstLon(List<Double> fcstLon) {
        this.fcstLon = fcstLon;
    }

    /**
     * @return the past latitude
     */
    public List<Double> getPastLat() {
        return pastLat;
    }

    /**
     * Set the past latitude
     *
     * @param pastLat
     */
    public void setPastLat(List<Double> pastLat) {
        this.pastLat = pastLat;
    }

    /**
     * @return the past longitude
     */
    public List<Double> getPastLon() {
        return pastLon;
    }

    /**
     * Sets the past longitude
     *
     * @param pastLon
     */
    public void setPastLon(List<Double> pastLon) {
        this.pastLon = pastLon;
    }

    /**
     * @return the time height graph params
     */

    public List<Double> getTimeHeightHeight() {
        return timeHeightHeight;
    }

    /**
     * Set the time height graph params
     *
     * @param timeHeightHeight
     */
    public void setTimeHeightHeight(List<Double> timeHeightHeight) {
        this.timeHeightHeight = timeHeightHeight;
    }

    /**
     * @return the time height diam
     */
    public List<Double> getTimeHeightDiam() {
        return timeHeightDiam;
    }

    /**
     * Set the time height diam
     *
     * @param timeHeightDiam
     */
    public void setTimeHeightDiam(List<Double> timeHeightDiam) {
        this.timeHeightDiam = timeHeightDiam;
    }

    /**
     * @return the time height rotvel
     */
    public List<Double> getTimeHeightRotvel() {
        return timeHeightRotvel;
    }

    /**
     * Set the time height rotvel
     *
     * @param timeHeightRotvel
     */
    public void setTimeHeightRotvel(List<Double> timeHeightRotvel) {
        this.timeHeightRotvel = timeHeightRotvel;
    }

    /**
     * @return the time height shear
     */
    public List<Double> getTimeHeightShear() {
        return timeHeightShear;
    }

    /**
     * Set the time height shear
     *
     * @param timeHeightShear
     */
    public void setTimeHeightShear(List<Double> timeHeightShear) {
        this.timeHeightShear = timeHeightShear;
    }

    /**
     * @return the time height gtg max
     */
    public List<Double> getTimeHeightGtgMax() {
        return timeHeightGtgMax;
    }

    /**
     * Set the time height gtg max
     *
     * @param timeHeightGtgMax
     */
    public void setTimeHeightGtgMax(List<Double> timeHeightGtgMax) {
        this.timeHeightGtgMax = timeHeightGtgMax;
    }

    /**
     * @return the time height gtg max
     */
    public List<Double> getTimeHeightRank() {
        return timeHeightRank;
    }

    /**
     * Set the time height gtg max
     *
     * @param timeHeightRank
     */
    public void setTimeHeightRank(List<Double> timeHeightRank) {
        this.timeHeightRank = timeHeightRank;
    }

    /**
     * @return the time height elevation indexes
     */
    public List<Double> getTimeHeightElevationIndexes() {
        return timeHeightElevationIndexes;
    }

    /**
     * Set the time height elevation indexes
     *
     * @param timeHeightElevationIndexes
     */
    public void setTimeHeightElevationIndexes(
            List<Double> timeHeightElevationIndexes) {
        this.timeHeightElevationIndexes = timeHeightElevationIndexes;
    }

    /**
     * @return the time height elevation angles
     */
    public List<Double> getTimeHeightElevationAngles() {
        return timeHeightElevationAngles;
    }

    /**
     * Set the
     *
     * @param timeHeightElevationAngles
     */
    public void setTimeHeightElevationAngles(
            List<Double> timeHeightElevationAngles) {
        this.timeHeightElevationAngles = timeHeightElevationAngles;
    }

    /**
     * @return the time height times
     */
    public List<Double> getTimeHeightTimes() {
        return timeHeightTimes;
    }

    /**
     * Set the time height times
     *
     * @param timeHeightTimes
     */
    public void setTimeHeightTimes(List<Double> timeHeightTimes) {
        this.timeHeightTimes = timeHeightTimes;
    }

    /**
     * Gets the value by column
     *
     * @param column
     * @return the value
     */
    @Override
    public Double getValue(String column) {
        Double value = null;
        if (column.equals(SCANConfigEnums.DMDTable.AZM.getColName())) {
            value = azm;
        } else if (column.equals(SCANConfigEnums.DMDTable.RNG.getColName())) {
            value = rng;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.STRANK.getColName())) {
            // Since stRank can have letters we must strip
            // out the potential characters.
            StringBuilder sb = new StringBuilder();
            char[] rankChars = rank.toCharArray();

            for (int i = 0; i < rankChars.length; i++) {
                if (String.valueOf(rankChars[i]).matches("[0-9.]+")) {
                    sb.append(rankChars[i]);
                }
            }
            if (!sb.toString().isEmpty()) {
                value = Double.valueOf(sb.toString());
            } else {
                value = 0.0;
            }
        } else if (column.equals(SCANConfigEnums.DMDTable.MSI.getColName())) {
            value = msi.doubleValue();
        } else if (column.equals(SCANConfigEnums.DMDTable.BASE.getColName())) {
            value = base;
        } else if (column.equals(SCANConfigEnums.DMDTable.DEPTH.getColName())) {
            value = depth;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.RELDEP.getColName())) {
            value = relDepth;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.LLDIAM.getColName())) {
            value = llDiam;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLVR.getColName())) {
            value = llVr;
        } else if (column.equals(SCANConfigEnums.DMDTable.MAXVR.getColName())) {
            value = maxVr;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.HTMXVR.getColName())) {
            value = htMxVr;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLSHR.getColName())) {
            value = llShear;
        } else if (column.equals(SCANConfigEnums.DMDTable.LLGTG.getColName())) {
            value = llgtg;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.LLCONV.getColName())) {
            value = llConv;
        } else if (column
                .equals(SCANConfigEnums.DMDTable.MLCONV.getColName())) {
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

        if (displayString == null) {

            StringBuilder sb = new StringBuilder("ident: ").append(ident)
                    .append("    ");

            if (strmID != null) {
                sb.append("Storm ID: ").append(strmID).append("\n");
            } else {
                sb.append("Storm ID: N/A \n");
            }

            if (status != null) {
                sb.append("status: ").append(status).append("\n");
            } else {
                sb.append("status: N/A \n");
            }

            if (base > 0) {
                sb.append("base: ").append(formatNumber(base)).append("    ");
            } else {
                sb.append("base: N/A    ");
            }

            if (depth > 0) {
                sb.append("depth: ").append(formatNumber(depth)).append("\n");
            } else {
                sb.append("depth: N/A \n");
            }

            if (rank != null) {
                sb.append("stRank: ").append(rank).append("    ");
            } else {
                sb.append("stRank: N/A      ");
            }

            if (msi > 0) {
                sb.append("msi: ").append(msi).append("\n");
            } else {
                sb.append("msi: N/A \n");
            }

            if (llVr >= 0) {
                sb.append("llVr: ").append(formatNumber(llVr)).append("\n");
            } else {
                sb.append("llVr: N/A \n");
            }

            if (llgtg >= 0) {
                sb.append("llgtg: ").append(formatNumber(llgtg)).append("\n");
            } else {
                sb.append("llgtg: N/A \n");
            }

            if (llConv >= 0) {
                sb.append("llConv: ").append(formatNumber(llConv)).append("\n");
            } else {
                sb.append("llConv: N/A \n");
            }
            displayString = sb.toString();
        }

        return displayString;
    }

    private String formatNumber(Double dbl) {
        String rv = "N/A";
        if (!dbl.isNaN()) {
            rv = Long.toString(Math.round(dbl));
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
