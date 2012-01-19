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
package com.raytheon.viz.grid.gridcache;

/**
 * DataCacheCube Metadata.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2009 3579       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class DataCubeMetadata {
    /**
     * The model name.
     */
    private String modelName = null;

    /**
     * The parameter.
     */
    private String parameter = null;

    /**
     * The level type.
     */
    private String levelType = null;

    /**
     * The reference time.
     */
    private long refTime;

    /**
     * The forecast hour.
     */
    private int fcstHr;
    
    /**
     * Number of values in the x direction.
     */
    private int nx;
    
    /**
     * Number of values in the y direction.
     */
    private int ny;
    
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

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the levelType
     */
    public String getLevelType() {
        return levelType;
    }

    /**
     * @param levelType
     *            the levelType to set
     */
    public void setLevelType(String levelType) {
        this.levelType = levelType;
    }

    /**
     * @return the refTime
     */
    public long getRefTime() {
        return refTime;
    }

    /**
     * @param refTime
     *            the refTime to set
     */
    public void setRefTime(long refTime) {
        this.refTime = refTime;
    }

    /**
     * @return the fcstHr
     */
    public int getFcstHr() {
        return fcstHr;
    }

    /**
     * @param fcstHr
     *            the fcstHr to set
     */
    public void setFcstHr(int fcstHr) {
        this.fcstHr = fcstHr;
    }

    /**
     * @return the nx
     */
    public int getNx() {
        return nx;
    }

    /**
     * @param nx the nx to set
     */
    public void setNx(int nx) {
        this.nx = nx;
    }

    /**
     * @return the ny
     */
    public int getNy() {
        return ny;
    }

    /**
     * @param ny the ny to set
     */
    public void setNy(int ny) {
        this.ny = ny;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if ((obj == null) || (obj.getClass() != this.getClass())) {
            return false;
        }

        DataCubeMetadata cube = (DataCubeMetadata) obj;
        if (!getModelName().equals(cube.getModelName())) {
            return false;
        }

        if (!getParameter().equals(cube.getParameter())) {
            return false;
        }

        if (!getLevelType().equals(cube.getLevelType())) {
            return false;
        }

        if (getRefTime() != cube.getRefTime()) {
            return false;
        }

        if (getFcstHr() != cube.getFcstHr()) {
            return false;
        }
        
        return true;
    }
}
