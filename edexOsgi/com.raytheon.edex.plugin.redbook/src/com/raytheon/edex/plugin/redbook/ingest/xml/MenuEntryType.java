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
package com.raytheon.edex.plugin.redbook.ingest.xml;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

/**
 * NDM Menu Entry Types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2014   2855     mpduff      Initial creation
 * Mar 19, 2014   2860     mpduff      Add DataURI as a type.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlType(name = "type")
@XmlEnum
public enum MenuEntryType {
    /** Menu Item type */
    @XmlEnumValue("productButton")
    ProductButton("productButton"),

    /** Menu Separator type */
    @XmlEnumValue("separator")
    Separator("separator"),

    /** Submenu type */
    @XmlEnumValue("submenu")
    Submenu("submenu"),

    /** Substitute xml element type */
    @XmlEnumValue("substitute")
    Substitute("substitute"),

    /** Menu Title type */
    @XmlEnumValue("title")
    Title("title"),

    /** Data URI type */
    @XmlEnumValue("dataUri")
    DataUri("dataUri");

    /** Menu entry type */
    private String entryType;

    /**
     * Private constructor.
     * 
     * @param entryType
     */
    private MenuEntryType(String entryType) {
        this.entryType = entryType;
    }

    /**
     * Get the type.
     * 
     * @return The type
     */
    public String getType() {
        return entryType;
    }
}
