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
package com.raytheon.viz.hydrobase.data;

/**
 * Geo area data object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2009 2772       mpduff     Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GeoAreaData {
    /** The area id */
    private String areaId = null;
    
    /** The area name */
    private String name = null;
    
    /** The area boundary type */
    private String boundaryType = null;
    
    /** The area interior latitude */
    private double interiorLat;
    
    /** The area interior Longitude */
    private double interiorLon;
    
    /** Latitude array */
    private double[] lat;
    
    /** Longitude array */
    private double[] lon;
    
    /** Number of points contained in this object */
    private int numberPoints;
    
    /** Save the block of data flag */
    private boolean saveDataBlock = false;

    /**
     * @return the areaId
     */
    public String getAreaId() {
        return areaId;
    }

    /**
     * @param areaId the areaId to set
     */
    public void setAreaId(String areaId) {
        this.areaId = areaId;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the boundaryType
     */
    public String getBoundaryType() {
        return boundaryType;
    }

    /**
     * @param boundaryType the boundaryType to set
     */
    public void setBoundaryType(String boundaryType) {
        this.boundaryType = boundaryType;
    }

    /**
     * @return the interiorLat
     */
    public double getInteriorLat() {
        return interiorLat;
    }

    /**
     * @param interiorLat the interiorLat to set
     */
    public void setInteriorLat(double interiorLat) {
        this.interiorLat = interiorLat;
    }

    /**
     * @return the interiorLon
     */
    public double getInteriorLon() {
        return interiorLon;
    }

    /**
     * @param interiorLon the interiorLon to set
     */
    public void setInteriorLon(double interiorLon) {
        this.interiorLon = interiorLon;
    }

    /**
     * @return the lat
     */
    public double[] getLat() {
        return lat;
    }

    /**
     * @param lat the lat to set
     */
    public void setLat(double[] lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public double[] getLon() {
        return lon;
    }

    /**
     * @param lon the lon to set
     */
    public void setLon(double[] lon) {
        this.lon = lon;
    }

    /**
     * @return the numberPoints
     */
    public int getNumberPoints() {
        return numberPoints;
    }

    /**
     * @param numberPoints the numberPoints to set
     */
    public void setNumberPoints(int numberPoints) {
        this.numberPoints = numberPoints;
    }

    /**
     * @return the saveDataBlock
     */
    public boolean isSaveDataBlock() {
        return saveDataBlock;
    }

    /**
     * @param saveDataBlock the saveDataBlock to set
     */
    public void setSaveDataBlock(boolean saveDataBlock) {
        this.saveDataBlock = saveDataBlock;
    }
}
