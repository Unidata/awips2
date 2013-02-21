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
package com.raytheon.uf.viz.datadelivery.subscription.subset.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TimeXML that supports a date range.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Moved in range specific code from TimeXML.
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "rangeDateTime")
public class DateRangeTimeXML extends TimeXML {

    @XmlElement(name = "rangeStart", type = String.class)
    protected String rangeStart;

    @XmlElement(name = "rangeEnd", type = String.class)
    protected String rangeEnd;
   
    /**
     * @return the rangeStart
     */
    public String getRangeStart() {
        return rangeStart;
    }

    /**
     * @param rangeStart
     *            the rangeStart to set
     */
    public void setRangeStart(String rangeStart) {
        this.rangeStart = rangeStart;
    }

    /**
     * @return the rangeEnd
     */
    public String getRangeEnd() {
        return rangeEnd;
    }

    /**
     * @param rangeEnd
     *            the rangeEnd to set
     */
    public void setRangeEnd(String rangeEnd) {
        this.rangeEnd = rangeEnd;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNonLatestData() {
        return "Range: " + rangeStart + " to " + rangeEnd;
    }
}