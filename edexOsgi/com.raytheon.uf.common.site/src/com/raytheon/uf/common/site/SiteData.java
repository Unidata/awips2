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
package com.raytheon.uf.common.site;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * AWIPS 2 site data object holding information regarding the site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013    1040    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SiteData {
    private static final String WFO_STRING = "wfo";

    private static final String RFC_STRING = "rfc";

    private static final String RO_STRING = "ro";

    private static final String NC_STRING = "nc";

    /**
     * Site Data Types
     */
    @XmlEnum
    public enum SiteDataType {
        @XmlEnumValue(WFO_STRING)
        WFO, @XmlEnumValue(RFC_STRING)
        RFC, @XmlEnumValue(RO_STRING)
        RO, @XmlEnumValue(NC_STRING)
        NC;
    }

    /** Site id */
    private String id;

    /** Site type (cwa, rfc, etc) */
    private SiteDataType type;

    /**
     * Default constructor.
     */
    public SiteData() {

    }

    /**
     * Constructor.
     * 
     * @param id
     *            Site id
     * @param type
     *            Site type
     */
    public SiteData(String id, SiteDataType type) {
        this.id = id;
        this.type = type;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the type
     */
    public SiteDataType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(SiteDataType type) {
        this.type = type;
    }
}
