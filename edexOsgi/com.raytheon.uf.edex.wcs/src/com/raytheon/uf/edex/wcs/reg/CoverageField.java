/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.reg;

import java.util.List;

/**
 * 4D coverage for a specific field value
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class CoverageField {

    private String name;

    private String standardName;

    private final List<TemporalCube> timeCube;

    private final CoverageDimensions dimensions;

    private String units;

    private Number paddingValue;

    /**
     * @param timeCube
     */
    public CoverageField(List<TemporalCube> timeCube, CoverageDimensions dims) {
        this.timeCube = timeCube;
        this.dimensions = dims;
    }

    /**
     * @return the timeCube
     */
    public List<TemporalCube> getTimeCube() {
        return timeCube;
    }

    /**
     * @return the units
     */
    public String getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(String units) {
        this.units = units;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the standardName
     */
    public String getStandardName() {
        return standardName;
    }

    /**
     * @param standardName
     *            the standardName to set
     */
    public void setStandardName(String standardName) {
        this.standardName = standardName;
    }

    /**
     * May be null
     * 
     * @return the paddingValue
     */
    public Number getPaddingValue() {
        return paddingValue;
    }

    /**
     * @param paddingValue
     *            the paddingValue to set
     */
    public void setPaddingValue(Number paddingValue) {
        this.paddingValue = paddingValue;
    }

    /**
     * @return the dimensions
     */
    public CoverageDimensions getDimensions() {
        return dimensions;
    }

}
