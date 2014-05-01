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
 * Point time xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013    223     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class PointTimeXML extends TimeXML {
    @XmlElement
    private int dataRetrievalInterval;

    /**
     * @return the dataRetrievalInterval
     */
    public int getDataRetrievalInterval() {
        return dataRetrievalInterval;
    }

    /**
     * @param dataRetrievalInterval
     *            the dataRetrievalInterval to set
     */
    public void setDataRetrievalInterval(int dataRetrievalInterval) {
        this.dataRetrievalInterval = dataRetrievalInterval;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreviewString() {
        return "Data Retrieval Interval: " + dataRetrievalInterval;
    }

}
