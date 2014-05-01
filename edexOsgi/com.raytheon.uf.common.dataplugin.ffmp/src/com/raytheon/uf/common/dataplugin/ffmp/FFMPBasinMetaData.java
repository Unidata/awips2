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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Holds geometry info for FFMP basins.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05Aug09      2521       dhladky     Setup
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class FFMPBasinMetaData implements ISerializableObject {

    /** default display name for basin **/
    @DynamicSerializeElement
    private String streamName = "XXXX";

    /** default display name for basin **/
    @DynamicSerializeElement
    private String hucName = "XXXX";

    /** basin id(key) in GIS **/
    @DynamicSerializeElement
    private Integer basinId = 0;

    /** basin id(key) in GIS **/
    @DynamicSerializeElement
    private Double area = 0.00;

    /** county **/
    @DynamicSerializeElement
    private String county;

    /** state **/
    @DynamicSerializeElement
    private String state;

    /** state **/
    @DynamicSerializeElement
    private String rfc;

    /** state **/
    @DynamicSerializeElement
    private String cwa;

    /** is it the primary cwa **/
    @DynamicSerializeElement
    private boolean primaryCwa = false;

    @DynamicSerializeElement
    public Long pfaf = null;

    @DynamicSerializeElement
    public Long aggregatedPfaf = null;

    /** aggregated pfafs if applicable **/
    @DynamicSerializeElement
    private ArrayList<Long> aggregatedPfafs = null;

    /** stream pfafs if applicable **/
    @DynamicSerializeElement
    private ArrayList<Integer> streamPfafs = null;

    public FFMPBasinMetaData() {

    }

    /**
     * @return the pfaf_id
     */
    public Long getPfaf() {
        return pfaf;
    }

    /**
     * @param basin_id
     *            the basin_id to set
     */
    public void setPfaf(Long pfaf) {
        this.pfaf = pfaf;
    }

    /**
     * @return the cwa
     */
    public String getCwa() {
        return cwa;
    }

    /**
     * @param cwa
     *            to set
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    /**
     * Set the stream name
     * 
     * @param streamName
     */
    public void setStreamName(String streamName) {
        this.streamName = streamName;
    }

    /**
     * Set the stream name
     * 
     * @param streamName
     */
    public String getStreamName() {
        return streamName;
    }

    /**
     * Set the county name
     * 
     * @param county
     */
    public void setCounty(String county) {
        this.county = county;
    }

    /**
     * Set the county name
     * 
     * @param county
     */
    public String getCounty() {
        return county;
    }

    /**
     * Set the state name
     * 
     * @param state
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * Set the state name
     * 
     * @param state
     */
    public String getState() {
        return state;
    }

    /**
     * Set the rfc name
     * 
     * @param rfc
     */
    public void setRfc(String rfc) {
        this.rfc = rfc;
    }

    /**
     * Set the rfc name
     * 
     * @param rfc
     */
    public String getRfc() {
        return rfc;
    }

    /**
     * Set the HUC name
     * 
     * @param HUCName
     */
    public void setHucName(String hucName) {
        this.hucName = hucName;
    }

    /**
     * Set the HUC name
     * 
     * @param HUCName
     */
    public String getHucName() {
        return hucName;
    }

    /**
     * @return the basin_id
     */
    public Integer getBasinId() {
        return basinId;
    }

    /**
     * @param basin_id
     *            the basin_id to set
     */
    public void setBasinId(Integer basinId) {
        this.basinId = basinId;
    }

    /**
     * Adds a pfaf for aggregations
     * 
     * @param pfaf
     */
    public void addPfaf(Long pfaf_id) {
        aggregatedPfafs.add(pfaf_id);
    }

    /**
     * Gets the list of aggregated pfafs for higher HUC levels
     * 
     * @return
     */
    public ArrayList<Long> getAggregatedPfafs() {
        return aggregatedPfafs;
    }

    /**
     * required setter
     * 
     * @param pfafs
     */
    public void setAggregatedPfafs(ArrayList<Long> aggregatedPfafs) {
        this.aggregatedPfafs = aggregatedPfafs;
    }

    /**
     * Gets the pfaf to which this is aggregated into if applicable
     * 
     * @return
     */
    public Long getAggregatedPfaf() {
        return aggregatedPfaf;
    }

    /**
     * required setter
     * 
     * @param pfaf
     */
    public void setAggregatedPfaf(Long aggregatedPfaf) {
        this.aggregatedPfaf = aggregatedPfaf;
    }

    public String toString() {
        StringBuffer buff = new StringBuffer();
        buff.append("PFAF ID: " + pfaf + "\n");
        buff.append("CWA : " + cwa + "\n");
        buff.append("State : " + state + "\n");
        buff.append("RFC : " + rfc + "\n");
        buff.append("basinID : " + basinId + "\n");
        buff.append("Area : " + area + "\n");
        buff.append("HUC NAME : " + hucName + "\n");
        buff.append("Stream Name : " + streamName + "\n");
        buff.append("County : " + county + "\n");
        return buff.toString();
    }

    public void setPrimaryCwa(boolean primaryCwa) {
        this.primaryCwa = primaryCwa;
    }

    public boolean isPrimaryCwa() {
        return primaryCwa;
    }

    public void setArea(Double area) {
        this.area = area;
    }

    public Double getArea() {
        return area;
    }

    public void setStreamPfafs(ArrayList<Integer> streamPfafs) {
        this.streamPfafs = streamPfafs;
    }

    public ArrayList<Integer> getStreamPfafs() {
        return streamPfafs;
    }

    public void addStreamPfaf(Integer streamPfaf) {
        if (streamPfafs == null) {
            streamPfafs = new ArrayList<Integer>();
        }
        streamPfafs.add(streamPfaf);
    }

    /**
     * gets rid of junk stream pfafs
     */
    public void removeZeros() {
        ArrayList<Integer> removes = new ArrayList<Integer>();
        for (int streamPfaf : streamPfafs) {
            if (streamPfaf == 0) {
                removes.add(streamPfaf);
            }
        }
        streamPfafs.removeAll(removes);
    }

}
