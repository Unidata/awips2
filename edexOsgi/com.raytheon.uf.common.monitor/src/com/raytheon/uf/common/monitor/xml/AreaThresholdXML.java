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
package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Class containing the XML data specifying the threshold key, red, and yellow
 * values.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Dec 15, 2009  3963     lvenable  Initial creation
 * Jan 04, 2016  5115     skorolev  moved from com.raytheon.uf.viz.monitor.xml
 * May 09, 2019  7689     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AreaThresholdXML {
    @XmlAttribute(name = "key")
    private String key;

    @XmlAttribute(name = "red")
    private double red;

    @XmlAttribute(name = "yellow")
    private double yellow;

    /**
     * Nullary constructor
     */
    public AreaThresholdXML() {
    }

    /**
     * Full constructor
     *
     * @param key
     *            value id
     * @param red
     *            red threshold value
     * @param yellow
     *            yellow threshold value
     */
    public AreaThresholdXML(String key, double red, double yellow) {
        this.key = key;
        this.red = red;
        this.yellow = yellow;
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    private AreaThresholdXML(AreaThresholdXML other) {
        this(other.key, other.red, other.yellow);
    }

    /**
     * @return the value id
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the value id
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the red threshold value
     */
    public double getRed() {
        return red;
    }

    /**
     * @param red
     *            the red threshold value
     */
    public void setRed(double red) {
        this.red = red;
    }

    /**
     * @return the yellow threshold value
     */
    public double getYellow() {
        return yellow;
    }

    /**
     * @param yellow
     *            the yellow threshold value
     */
    public void setYellow(double yellow) {
        this.yellow = yellow;
    }

    /**
     * @return an independent copy of this AreaThresholdXML
     */
    public AreaThresholdXML copy() {
        return new AreaThresholdXML(this);
    }
}
