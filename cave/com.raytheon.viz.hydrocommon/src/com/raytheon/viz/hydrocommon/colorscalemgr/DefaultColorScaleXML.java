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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * HydroviewDefaultColorScale.xml java object. Holds the default color scale
 * data for the Color Scale Manager.
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

@XmlRootElement(name = "defaultColorScale")
@XmlAccessorType(XmlAccessType.NONE)
public class DefaultColorScaleXML {

    @XmlElements({ @XmlElement(name = "colorDataClass", type = ColorDataClassXML.class) })
    private ArrayList<ColorDataClassXML> colorDataClassList = new ArrayList<ColorDataClassXML>();

    /**
     * @return the colorDataClassList
     */
    public ArrayList<ColorDataClassXML> getColorDataClassList() {
        return colorDataClassList;
    }

    /**
     * @param colorDataClass
     *            the colorDataClassList to set
     */
    public void setColorDataClassList(
            ArrayList<ColorDataClassXML> colorDataClassList) {
        this.colorDataClassList = colorDataClassList;
    }
}
