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

package com.raytheon.viz.core.style.image;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * 
 * Contains the scaling attributes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2007            chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataScale {

    public static enum Type {
        LINEAR, LOG
    };

    private Type scaleType = Type.LINEAR;

    private Type levelScale = Type.LINEAR;

    @XmlElement
    private Double minValue;

    @XmlElement
    private Double maxValue;

    @XmlElement
    private Double minValue2;

    @XmlElement
    private Double maxValue2;

    @XmlAttribute
    private boolean mirror;

    @XmlAttribute
    private boolean adaptive;

    public DataScale() {

    }

    /**
     * @return the scaleType
     */
    public Type getScaleType() {
        return scaleType;
    }

    /**
     * @param scaleType
     *            the scaleType to set
     */
    public void setScaleType(Type scaleType) {
        this.scaleType = scaleType;
    }

    /**
     * @return the minValue
     */
    public Double getMinValue() {
        return minValue;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public void setMinValue(Double minValue) {
        this.minValue = minValue;
    }

    /**
     * @return the maxValue
     */
    public Double getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public void setMaxValue(Double maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the minLogValue2
     */
    public Double getMinValue2() {
        return minValue2;
    }

    /**
     * @param minValue1
     *            the minValue1 to set
     */
    public void setMinValue2(Double minValue2) {
        this.minValue2 = minValue2;
    }

    /**
     * @return the maxValue2
     */
    public Double getMaxValue2() {
        return maxValue2;
    }

    /**
     * @param maxValue2
     *            the maxValue2 to set
     */
    public void setMaxValue2(Double maxValue2) {
        this.maxValue2 = maxValue2;
    }

    @XmlAttribute(name = "scale")
    public void setTypeAsString(String typeStr) {
        this.scaleType = Type.valueOf(typeStr);
    }

    public String getTypeAsString() {
        return this.scaleType.toString();
    }

    public Type getLevelScale() {
        return levelScale;
    }

    public void setLevelScale(Type levelScale) {
        this.levelScale = levelScale;
    }

    public String getLevelScaleAsString() {
        return levelScale.toString();
    }

    @XmlAttribute(name = "levelScale")
    public void setLevelScaleAsString(String levelScale) {
        this.levelScale = Type.valueOf(levelScale);
    }

    public boolean isMirror() {
        return mirror;
    }

    public void setMirror(boolean mirror) {
        this.mirror = mirror;
    }

    public boolean isAdaptive() {
        return adaptive;
    }

    public void setAdaptable(boolean adaptive) {
        this.adaptive = adaptive;
    }

}
