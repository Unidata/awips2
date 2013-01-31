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
package com.raytheon.uf.common.dataaccess.impl;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataaccess.geom.IGeometryRequest;
import com.raytheon.uf.common.serialization.adapters.JTSEnvelopeAdapter;
import com.vividsolutions.jts.geom.Envelope;

/**
 * A default IGeometryRequest that can be used for most IGeometryRequests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012            njensen     Initial creation
 * Jan 30, 2013 #1555     bkowal      Added XML Annotations
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DefaultGeometryRequest extends AbstractDataRequest implements
        IGeometryRequest {

    @XmlJavaTypeAdapter(value = JTSEnvelopeAdapter.class)
    protected Envelope envelope;

    @XmlElement(name="locationName")
    protected String[] locationNames;

    @Override
    public void setEnvelope(Envelope env) {
        this.envelope = env;
    }

    @Override
    public Envelope getEnvelope() {
        return envelope;
    }

    @Override
    public void setLocationNames(String... locationNames) {
        this.locationNames = locationNames;

    }

    @Override
    public String[] getLocationNames() {
        return locationNames;
    }

}
