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
package com.raytheon.edex.plugin.gfe.paraminfo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Class holding parameter information pertaining to grid parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2010            bphillip     Initial creation
 * Sep 12, 2012  #1117     dgilling     Create field to hold list of
 *                                      valid levels for each parameter.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ParameterInfo {

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String short_name;

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String long_name;

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String units;

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String udunits;

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String uiname;

    @XmlElement
    private float[] valid_range;

    @XmlElement
    private float fillValue;

    @XmlElement
    private int n3D;

    @XmlElement
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    private String levelsDesc;

    @XmlElementWrapper(name = "levels", required = false)
    @XmlElement(name = "level")
    @XmlJavaTypeAdapter(value = CollapsedStringAdapter.class, type = String.class)
    private List<String> levels = new ArrayList<String>();

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("Short Name:").append(short_name).append("\n");
        buf.append("Long Name:").append(long_name).append("\n");
        buf.append("Units:").append(units).append("\n");
        buf.append("Udunits:").append(udunits).append("\n");
        buf.append("UiName:").append(uiname).append("\n");
        buf.append("Valid Range:").append(Arrays.toString(valid_range))
                .append("\n");
        buf.append("Fill Value:").append(fillValue).append("\n");
        buf.append("n3D:").append(n3D).append("\n");
        buf.append("Levels Description:").append(levelsDesc).append("\n");
        return buf.toString();
    }

    public ParameterInfo() {

    }

    public ParameterInfo(String parameterName) {
        this.short_name = parameterName;
    }

    /**
     * @return the long_name
     */
    public String getLong_name() {
        return long_name;
    }

    /**
     * @param long_name
     *            the long_name to set
     */
    public void setLong_name(String long_name) {
        this.long_name = long_name;
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
     * @return the udunits
     */
    public String getUdunits() {
        return udunits;
    }

    /**
     * @param udunits
     *            the udunits to set
     */
    public void setUdunits(String udunits) {
        this.udunits = udunits;
    }

    /**
     * @return the uiname
     */
    public String getUiname() {
        return uiname;
    }

    /**
     * @param uiname
     *            the uiname to set
     */
    public void setUiname(String uiname) {
        this.uiname = uiname;
    }

    /**
     * @return the valid_range
     */
    public float[] getValid_range() {
        return valid_range;
    }

    /**
     * @param valid_range
     *            the valid_range to set
     */
    public void setValid_range(float[] valid_range) {
        this.valid_range = valid_range;
    }

    /**
     * @return the fillValue
     */
    public float getFillValue() {
        return fillValue;
    }

    /**
     * @param fillValue
     *            the fillValue to set
     */
    public void setFillValue(float fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * @return the n3D
     */
    public int getN3D() {
        return n3D;
    }

    /**
     * @param n3d
     *            the n3D to set
     */
    public void setN3D(int n3d) {
        n3D = n3d;
    }

    /**
     * @return the levels
     */
    public String getLevelsDesc() {
        return levelsDesc;
    }

    /**
     * @param levels
     *            the levels to set
     */
    public void setLevelsDesc(String levels) {
        this.levelsDesc = levels;
    }

    /**
     * @return the short_name
     */
    public String getShort_name() {
        return short_name;
    }

    /**
     * @param short_name
     *            the short_name to set
     */
    public void setShort_name(String short_name) {
        this.short_name = short_name;
    }

    public void setLevels(List<String> levels) {
        this.levels = levels;
    }

    public List<String> getLevels() {
        return levels;
    }
}
