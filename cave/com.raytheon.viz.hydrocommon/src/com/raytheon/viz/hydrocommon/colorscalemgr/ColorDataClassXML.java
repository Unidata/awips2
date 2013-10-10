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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * Individual data class xml elements.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2010 4671       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ColorDataClassXML {

    @XmlElements({ @XmlElement(name = "threshold", type = ColorThresholdXML.class) })
    private ArrayList<ColorThresholdXML> thresholdList = new ArrayList<ColorThresholdXML>();

    @XmlAttribute(name = "useName")
    private String colorUseName;

    @XmlAttribute(name = "dbUseName")
    private String dbColorUseName;

    /**
     * @return the colorUseName
     */
    public String getColorUseName() {
        return colorUseName;
    }

    /**
     * @param colorUseName
     *            the colorUseName to set
     */
    public void setColorUseName(String colorUseName) {
        this.colorUseName = colorUseName;
    }

    /**
     * @return the dbColorUseName
     */
    public String getDbColorUseName() {
        return dbColorUseName;
    }

    /**
     * @param dbColorUseName
     *            the dbColorUseName to set
     */
    public void setDbColorUseName(String dbColorUseName) {
        this.dbColorUseName = dbColorUseName;
    }

    /**
     * @return the thresholdList
     */
    public ArrayList<ColorThresholdXML> getThresholdList() {
        return thresholdList;
    }

    /**
     * @param thresholdList
     *            the thresholdList to set
     */
    public void setThresholdList(ArrayList<ColorThresholdXML> thresholdList) {
        this.thresholdList = thresholdList;
    }

}
