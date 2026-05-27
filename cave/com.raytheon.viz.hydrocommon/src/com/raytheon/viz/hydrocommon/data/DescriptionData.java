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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;

/**
 * This class contains the description data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02 Dec 2008              lvenable    Initial creation
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class DescriptionData extends HydroDBData
{
    /**
     * The location ID.
     */
    private String lid;
    
    /**
     * Stream bed.
     */
    private String streamBed;
    
    /**
     * Divert information.
     */
    private String divert;
    
    /**
     * Remark.
     */
    private String remark;
    
    /**
     * Ice/Freezing.
     */
    private String ice;
    
    /**
     * Proximity.
     */
    private String proximity;
    
    /**
     * Reach information.
     */
    private String reach;
    
    /**
     * Res/Regulation.
     */
    private String regulation;
    
    /**
     * Topo information.
     */
    private String topo;
    
    /**
     * Constructor.
     */
    public DescriptionData()
    {
        lid = "";
        streamBed = "";
        divert = "";
        remark = "";
        ice = "";
        proximity = "";
        reach = "";
        regulation = "";
        topo = "";
    }
    
    /**
     * Constructor.
     * @param data Result data.
     * @param dataMap Column to Index map.
     */
    public DescriptionData(QueryResultRow data, Map<String, Integer> dataMap)
    {
        setLid(getDBValue("lid", data, dataMap, "ZZZZZ"));
        setStreamBed(getDBValue("bed", data, dataMap, ""));
        setDivert(getDBValue("divert", data, dataMap, ""));
        setRemark(getDBValue("remark", data, dataMap, ""));
        setIce(getDBValue("ice", data, dataMap, ""));
        setProximity(getDBValue("proximity", data, dataMap, ""));
        setReach(getDBValue("reach", data, dataMap, ""));
        setRegulation(getDBValue("res", data, dataMap, ""));
        setTopo(getDBValue("topo", data, dataMap, ""));
    }

    /**
     * Get the location ID.
     * @return The location ID.
     */
    public String getLid()
    {
        return lid;
    }

    /**
     * Set the location ID.
     * @param lid The location ID.
     */
    public void setLid(String lid)
    {
        this.lid = lid;
    }

    /**
     * Get the stream bed.
     * @return The stream bed information.
     */
    public String getStreamBed()
    {
        return streamBed;
    }

    /**
     * Set the stream bed information.
     * @param streamBed Stream bed information.
     */
    public void setStreamBed(String streamBed)
    {
        this.streamBed = streamBed;
    }

    /**
     * Get the divert information.
     * @return The divert information.
     */
    public String getDivert()
    {
        return divert;
    }

    /**
     * Set the divert information.
     * @param divert The divert information.
     */
    public void setDivert(String divert)
    {
        this.divert = divert;
    }

    /**
     * Get the remarks.
     * @return The remarks.
     */
    public String getRemark()
    {
        return remark;
    }

    /**
     * Set the remarks.
     * @param remark The remarks.
     */
    public void setRemark(String remark)
    {
        this.remark = remark;
    }

    /**
     * Get the ice/freezing information.
     * @return The ice/freezing information.
     */
    public String getIce()
    {
        return ice;
    }

    /**
     * Set the ice/freezing information.
     * @param ice
     */
    public void setIce(String ice)
    {
        this.ice = ice;
    }

    /**
     * Get the proximity.
     * @return The proximity.
     */
    public String getProximity()
    {
        return proximity;
    }

    /**
     * Set the proximity.
     * @param proximity The proximity.
     */
    public void setProximity(String proximity)
    {
        this.proximity = proximity;
    }

    /**
     * Get the reach information.
     * @return The reach information.
     */
    public String getReach()
    {
        return reach;
    }

    /**
     * Set the reach information.
     * @param reach the reach information.
     */
    public void setReach(String reach)
    {
        this.reach = reach;
    }

    /**
     * Get the res/regulation.
     * @return The res/regulation information.
     */
    public String getRegulation()
    {
        return regulation;
    }

    /**
     * Set the res/regulation.
     * @param regulation The res/regulation information.
     */
    public void setRegulation(String regulation)
    {
        this.regulation = regulation;
    }

    /**
     * Get the topo information.
     * @return The topo information.
     */
    public String getTopo()
    {
        return topo;
    }

    /**
     * Set the topo information.
     * @param topo The topo information.
     */
    public void setTopo(String topo)
    {
        this.topo = topo;
    }
}
