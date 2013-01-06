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
package com.raytheon.uf.common.gridcoverage.subgrid;

/**
 * A sub grid definition
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 08, 2009 3177       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class SubGrid {

    /** The model name */
    private String modelName;

    /** the upper left x the subgrid will start with */
    private int upperLeftX;

    /** the upper left y the subgrid will start with */
    private int upperLeftY;

    /** the width of the sub grid */
    private int nX;

    /** the height of the sub grid */
    private int nY;

    /** lower left latitude of the subgrid area */
    private double lowerLeftLat;

    /** lower left longitude of the subgrid area */
    private double lowerLeftLon;

    /** upper right latitude of the subgrid area */
    private double upperRightLat;

    /** upper right longitude of the subgrid area */
    private double upperRightLon;

    public int getUpperLeftX() {
        return upperLeftX;
    }

    public void setUpperLeftX(int upperLeftX) {
        this.upperLeftX = upperLeftX;
    }

    public int getUpperLeftY() {
        return upperLeftY;
    }

    public void setUpperLeftY(int upperLeftY) {
        this.upperLeftY = upperLeftY;
    }

    public int getNX() {
        return nX;
    }

    public void setNX(int nx) {
        nX = nx;
    }

    public int getNY() {
        return nY;
    }

    public void setNY(int ny) {
        nY = ny;
    }

    public double getLowerLeftLat() {
        return lowerLeftLat;
    }

    public void setLowerLeftLat(double lowerLeftLat) {
        this.lowerLeftLat = lowerLeftLat;
    }

    public double getLowerLeftLon() {
        return lowerLeftLon;
    }

    public void setLowerLeftLon(double lowerLeftLon) {
        this.lowerLeftLon = lowerLeftLon;
    }

    public double getUpperRightLat() {
        return upperRightLat;
    }

    public void setUpperRightLat(double upperRightLat) {
        this.upperRightLat = upperRightLat;
    }

    public double getUpperRightLon() {
        return upperRightLon;
    }

    public void setUpperRightLon(double upperRightLon) {
        this.upperRightLon = upperRightLon;
    }

    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @param modelName
     *            the modelName to set
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

}