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
package com.raytheon.viz.warngen.gis;

import java.util.List;

/**
 * 
 * AffectedAreas
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 11, 2007 #601        chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class AffectedAreas {
    /** The name of the area affected */
    protected String name;

    /**
     * The portion of the region that is affected
     * 
     * If the entire area is affected this will be null
     */
    protected List<String> partOfArea;

    /**
     * The partOfArea to suppress (e.g. "ns")
     */
    protected String suppress;

    /** The notation of the area affected (e.g. "COUNTY") */
    protected String areaNotation;

    /** The plural notation of the area affected (e.g. "COUNTIES") */
    protected String areasNotation;

    /** The portion of the parent region that is affected, if applicable */
    protected List<String> partOfParentRegion;

    /** The name of the parent region that is affected, if applicable */
    protected String parentRegion;

    /** The FIPS county number */
    protected String fips;

    /** The state Abbreviation */
    protected String stateabbr;

    /** A list of the points that are affected in the area, if applicable */
    protected String[] points;

    /**
     * the time zone the area is in - P, M, C, E (capitalization not enforced)
     */
    protected String timezone;

    protected double size;

    /**
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the areaNotation
     */
    public String getAreaNotation() {
        return areaNotation;
    }

    /**
     * @return the areasNotation
     */
    public String getAreasNotation() {
        return areasNotation;
    }

    /**
     * @return the partOfParentRegion
     */
    public List<String> getPartOfParentRegion() {
        return partOfParentRegion;
    }

    /**
     * @return the parentRegion
     */
    public String getParentRegion() {
        return parentRegion;
    }

    /**
     * @return the points
     */
    public String[] getPoints() {
        return points;
    }

    /**
     * @return the partOfArea
     */
    public List<String> getPartOfArea() {
        return partOfArea;
    }

    /**
     * @return the fips
     */
    public String getFips() {
        return fips;
    }

    /**
     * @param fips
     *            the fips to set
     */
    public void setFips(String fips) {
        this.fips = fips;
    }

    /**
     * @return the stateabbr
     */
    public String getStateabbr() {
        return stateabbr;
    }

    /**
     * @param stateabbr
     *            the stateabbr to set
     */
    public void setStateabbr(String stateabbr) {
        this.stateabbr = stateabbr;
    }

    /**
     * 
     * @return timezone
     */
    public String getTimezone() {
        return timezone;
    }

    /**
     * 
     * @param timezone
     */
    public void setTimezone(String timezone) {
        this.timezone = timezone;
    }

    public double getSize() {
        return size;
    }
}