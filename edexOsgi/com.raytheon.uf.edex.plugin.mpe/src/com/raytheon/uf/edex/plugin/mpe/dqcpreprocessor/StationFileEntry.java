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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

/**
 * Container for the contents of a single line in the station file.
 * Representative of a single station as it is used by the DQC PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class StationFileEntry {

    private String lid;

    private String shef;

    private String pe;

    private String ts;

    private String source;

    private float lat;

    private float lon;

    private int elevation;

    private int tipWeighFlag;

    private String name;

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the shef
     */
    public String getShef() {
        return shef;
    }

    /**
     * @param shef
     *            the shef to set
     */
    public void setShef(String shef) {
        this.shef = shef;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    /**
     * @return the lat
     */
    public float getLat() {
        return lat;
    }

    /**
     * @param lat
     *            the lat to set
     */
    public void setLat(float lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public float getLon() {
        return lon;
    }

    /**
     * @param lon
     *            the lon to set
     */
    public void setLon(float lon) {
        this.lon = lon;
    }

    /**
     * @return the elevation
     */
    public int getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(int elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the tipWeighFlag
     */
    public int getTipWeighFlag() {
        return tipWeighFlag;
    }

    /**
     * @param tipWeighFlag
     *            the tipWeighFlag to set
     */
    public void setTipWeighFlag(int tipWeighFlag) {
        this.tipWeighFlag = tipWeighFlag;
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + elevation;
        result = prime * result + Float.floatToIntBits(lat);
        result = prime * result + ((lid == null) ? 0 : lid.hashCode());
        result = prime * result + Float.floatToIntBits(lon);
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((pe == null) ? 0 : pe.hashCode());
        result = prime * result + ((shef == null) ? 0 : shef.hashCode());
        result = prime * result + tipWeighFlag;
        result = prime * result + ((ts == null) ? 0 : ts.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        StationFileEntry other = (StationFileEntry) obj;
        if (elevation != other.elevation)
            return false;
        if (Float.floatToIntBits(lat) != Float.floatToIntBits(other.lat))
            return false;
        if (lid == null) {
            if (other.lid != null)
                return false;
        } else if (!lid.equals(other.lid))
            return false;
        if (Float.floatToIntBits(lon) != Float.floatToIntBits(other.lon))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (pe == null) {
            if (other.pe != null)
                return false;
        } else if (!pe.equals(other.pe))
            return false;
        if (shef == null) {
            if (other.shef != null)
                return false;
        } else if (!shef.equals(other.shef))
            return false;
        if (tipWeighFlag != other.tipWeighFlag)
            return false;
        if (ts == null) {
            if (other.ts != null)
                return false;
        } else if (!ts.equals(other.ts))
            return false;
        return true;
    }
}