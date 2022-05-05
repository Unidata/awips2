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
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * A domain represents a radar's coverage, similar to a range ring
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * Jul 10, 2018 6695       njensen     Overrode toString()
 *
 * </pre>
 *
 */
@XmlRootElement(name = "FFMPRunXML")
@XmlAccessorType(XmlAccessType.NONE)
public class DomainXML {

    @XmlAttribute(name = "primary")
    private boolean primary;

    @XmlAttribute(name = "cwa")
    private String cwa;

    public boolean isPrimary() {
        return primary;
    }

    public void setPrimary(boolean primary) {
        this.primary = primary;
    }

    public String getCwa() {
        return cwa;
    }

    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    @Override
    public String toString() {
        return "DomainXML [primary=" + primary + ", cwa=" + cwa + "]";
    }

}
