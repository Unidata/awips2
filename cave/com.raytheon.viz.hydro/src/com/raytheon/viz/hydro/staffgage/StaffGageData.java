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

package com.raytheon.viz.hydro.staffgage;

/**
 * This class containing staff gage data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 18 Nov 2008  1268       dhladky     Made it work.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class StaffGageData
{
    /**
     * City name.
     */
    private String name = StaffGageDlg.MISSING;
    
    /**
     * Latitude and Longitude.
     */
    private String latLon = StaffGageDlg.MISSING;
    
    /**
     * Basin name.
     */
    private String basin = StaffGageDlg.MISSING;
    
    /**
     * Elevation.
     */
    private String elevation = StaffGageDlg.MISSING;
    
    /**
     * Stream name.
     */
    private String stream = StaffGageDlg.MISSING;
    
    /**
     * Tidal elevation.
     */
    private String tidal = StaffGageDlg.MISSING;
    
    /**
     * County name.
     */
    private String county = StaffGageDlg.MISSING;
    
    /**
     * State.
     */
    private String state = StaffGageDlg.MISSING;
    
    /**
     * Date of the record value.
     */
    private String recordDate = StaffGageDlg.MISSING;
    
    /**
     * Record stage value.
     */
    private String recordStage = StaffGageDlg.MISSING;
    
    /**
     * Record flow value.
     */
    private String recordFlow = StaffGageDlg.MISSING;
    
    /**
     * Major category stage value.
     */
    private String majorCatStage = StaffGageDlg.MISSING;
    
    /**
     * Major category flow value.
     */
    private String majorCatFlow = StaffGageDlg.MISSING;
    
    /**
     * Moderate category stage value.
     */
    private String modCatStage = StaffGageDlg.MISSING;
    
    /**
     * Moderate category flow value.
     */
    private String modCatFlow = StaffGageDlg.MISSING;
    
    /**
     * Minor category stage value.
     */
    private String minorCatStage = StaffGageDlg.MISSING;
    
    /**
     * Minor category flow value.
     */
    private String minorCatFlow = StaffGageDlg.MISSING;
    
    /**
     * Flood stage.
     */
    private String floodStage = StaffGageDlg.MISSING;
    
    /**
     * Flood flow.
     */
    private String floodFlow = StaffGageDlg.MISSING;
    
    /**
     * Action stage.
     */
    private String actionStage = StaffGageDlg.MISSING;
    
    /**
     * Action flow.
     */
    private String actionFlow = StaffGageDlg.MISSING;
    
    /**
     * Bank full stage value.
     */
    private String bankfullStage = StaffGageDlg.MISSING;
    
    /**
     * Zero datum value.
     */
    private String zeroDatum = StaffGageDlg.MISSING;
    
    /**
     * Constructor.
     */
    public StaffGageData()
    {        
    }

    /**
     * Get the city name.
     * @return The city name.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Set the city name.
     * @param name The city name.
     */
    public void setName(String name)
    {
        this.name = name;
    }

    /**
     * Get the Lat/Lon value.
     * @return Lat/Lon value.
     */
    public String getLatLon()
    {
        return latLon;
    }

    /**
     * Set the Lat/Lon value.
     * @param latLon The Lat/Lon value.
     */
    public void setLatLon(String latLon)
    {
        this.latLon = latLon;
    }

    /**
     * Get the basin name.
     * @return The basin name.
     */
    public String getBasin()
    {
        return basin;
    }

    /**
     * Set the basin name.
     * @param basin The basin name.
     */
    public void setBasin(String basin)
    {
        this.basin = basin;
    }

    /**
     * Get the elevation.
     * @return The elevation.
     */
    public String getElevation()
    {
        return elevation;
    }

    /**
     * Set the elevation.
     * @param elevation The elevation value.
     */
    public void setElevation(String elevation)
    {
        this.elevation = elevation;
    }

    /**
     * Get the stream name.
     * @return The stream name.
     */
    public String getStream()
    {
        return stream;
    }

    /**
     * Set the stream name.
     * @param stream The stream name.
     */
    public void setStream(String stream)
    {
        this.stream = stream;
    }

    /**
     * Get the tidal elevation.
     * @return The tidal elevation.
     */
    public String getTidal()
    {
        return tidal;
    }

    /**
     * Set the tidal elevation.
     * @param tidal The tidal elevation.
     */
    public void setTidal(String tidal)
    {
        this.tidal = tidal;
    }

    /**
     * Get the county name.
     * @return The county name.
     */
    public String getCounty()
    {
        return county;
    }

    /**
     * Set the county name.
     * @param county The county name.
     */
    public void setCounty(String county)
    {
        this.county = county;
    }

    /**
     * Get the state abbreviation.
     * @return The state abbreviation .
     */
    public String getState()
    {
        return state;
    }

    /**
     * Set the state abbreviation.
     * @param state The state abbreviation.
     */
    public void setState(String state)
    {
        this.state = state;
    }

    /**
     * Get the record date.
     * @return The record date.
     */
    public String getRecordDate()
    {
        return recordDate;
    }

    /**
     * Set the record date.
     * @param recordDate The record date.
     */
    public void setRecordDate(String recordDate)
    {
        this.recordDate = recordDate;
    }

    /**
     * Set the record stage value.
     * @return The record stage value.
     */
    public String getRecordStage()
    {
        return recordStage;
    }

    /**
     * Set the record stage value.
     * @param recordStage The record stage value.
     */
    public void setRecordStage(String recordStage)
    {
        this.recordStage = recordStage;
    }

    /**
     * Get the record flow.
     * @return The record flow.
     */
    public String getRecordFlow()
    {
        return recordFlow;
    }

    /**
     * Set the record flow.
     * @param recordFlow The record flow.
     */
    public void setRecordFlow(String recordFlow)
    {
        this.recordFlow = recordFlow;
    }

    /**
     * Get the major category stage value.
     * @return The major category stage value.
     */
    public String getMajorCatStage()
    {
        return majorCatStage;
    }

    /**
     * Set the major category stage value.
     * @param majorCatStage The major category stage value.
     */
    public void setMajorCatStage(String majorCatStage)
    {
        this.majorCatStage = majorCatStage;
    }

    /**
     * Get the major category flow value.
     * @return The major category flow value.
     */
    public String getMajorCatFlow()
    {
        return majorCatFlow;
    }

    /**
     * Set the major category flow value.
     * @param majorCatStage The major category flow value.
     */
    public void setMajorCatFlow(String majorCatFlow)
    {
        this.majorCatFlow = majorCatFlow;
    }

    /**
     * Get the moderate category stage value.
     * @return The moderate category moderate value.
     */
    public String getModCatStage()
    {
        return modCatStage;
    }

    /**
     * Set the moderate category stage value.
     * @param modCatStage The moderate category moderate value.
     */
    public void setModCatStage(String modCatStage)
    {
        this.modCatStage = modCatStage;
    }

    /**
     * Get the moderate category flow value.
     * @return The moderate category flow value.
     */
    public String getModCatFlow()
    {
        return modCatFlow;
    }

    /**
     * Set the moderate category flow value.
     * @param modCatFlow The moderate category flow value.
     */
    public void setModCatFlow(String modCatFlow)
    {
        this.modCatFlow = modCatFlow;
    }

    /**
     * Get the minor category stage value.
     * @return Minor category stage value.
     */
    public String getMinorCatStage()
    {
        return minorCatStage;
    }

    /**
     * Set the minor category stage value.
     * @param minorCatStage Minor category stage value.
     */
    public void setMinorCatStage(String minorCatStage)
    {
        this.minorCatStage = minorCatStage;
    }

    /**
     * Get the minor category flow value.
     * @return Minor category flow value.
     */
    public String getMinorCatFlow()
    {
        return minorCatFlow;
    }

    /**
     * Set the minor category flow value.
     * @param minorCatFlow Minor category flow value.
     */
    public void setMinorCatFlow(String minorCatFlow)
    {
        this.minorCatFlow = minorCatFlow;
    }

    /**
     * Get the flood stage value.
     * @return The flood stage value.
     */
    public String getFloodStage()
    {
        return floodStage;
    }

    /**
     * Set the flood stage value.
     * @param floodStage The flood stage value.
     */
    public void setFloodStage(String floodStage)
    {
        this.floodStage = floodStage;
    }

    /**
     * Get the flood flow value.
     * @return The flood flow value.
     */
    public String getFloodFlow()
    {
        return floodFlow;
    }

    /**
     * Set the flood flow value.
     * @param floodFlow The flood flow value.
     */
    public void setFloodFlow(String floodFlow)
    {
        this.floodFlow = floodFlow;
    }

    /**
     * Get the action stage value.
     * @return The action stage value.
     */
    public String getActionStage()
    {
        return actionStage;
    }

    /**
     * Set the action stage value.
     * @param actionStage The action stage value.
     */
    public void setActionStage(String actionStage)
    {
        this.actionStage = actionStage;
    }

    /**
     * Get the action flow value.
     * @return The action flow value.
     */
    public String getActionFlow()
    {
        return actionFlow;
    }

    /**
     * Set the action flow value.
     * @param actionFlow The action flow value.
     */
    public void setActionFlow(String actionFlow)
    {
        this.actionFlow = actionFlow;
    }

    /**
     * Get the bank full stage value.
     * @return The bank full stage value.
     */
    public String getBankfullStage()
    {
        return bankfullStage;
    }

    /**
     * Set the bank full stage value.
     * @param bankfullStage The bank full stage value.
     */
    public void setBankfullStage(String bankfullStage)
    {
        this.bankfullStage = bankfullStage;
    }

    /**
     * Get the zero datum value.
     * @return The zero datum value.
     */
    public String getZeroDatum()
    {
        return zeroDatum;
    }

    /**
     * Set the zero datum value.
     * @param zeroDatum The zero datum value.
     */
    public void setZeroDatum(String zeroDatum)
    {
        this.zeroDatum = zeroDatum;
    }
}
