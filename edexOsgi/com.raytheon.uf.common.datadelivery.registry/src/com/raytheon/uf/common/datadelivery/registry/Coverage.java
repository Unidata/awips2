package com.raytheon.uf.common.datadelivery.registry;

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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.ReferencedEnvelopeAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Coverage XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2011    191      dhladky     Initial creation
 * Dec 10, 2012   1259      bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 *
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@XmlSeeAlso({ GriddedCoverage.class, LatLonGridCoverage.class })
public class Coverage implements ISerializableObject, Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -4989602744566078018L;

	public Coverage() {

    }

    /**
     * Envelope describing the total area where data is available for this
     * coverage.
     */
    @XmlElement
    @DynamicSerializeElement
    @XmlJavaTypeAdapter(value = ReferencedEnvelopeAdapter.class)
    private ReferencedEnvelope envelope;

    /**
     * Envelope describing the area where data is being requested. This envelope
     * may use any projection and will need to be converted to the same crs as
     * the data before processing any requests.
     */
    @XmlElement
    @DynamicSerializeElement
    @XmlJavaTypeAdapter(value = ReferencedEnvelopeAdapter.class)
    private ReferencedEnvelope requestEnvelope;

    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }

    public ReferencedEnvelope getRequestEnvelope() {
        return requestEnvelope;
    }

    public void setRequestEnvelope(ReferencedEnvelope requestEnvelope) {
        this.requestEnvelope = requestEnvelope;
    }

    public String getProjection() {
        return envelope.getCoordinateReferenceSystem().getName().toString();
    }

    /**
     * Get the subset upper left coordinate
     * 
     * @return The subset upper left coordinate
     */
    public Coordinate getRequestUpperLeft() {
        if (requestEnvelope == null) {
            return null;
        }
        return EnvelopeUtils.getUpperLeftLatLon(requestEnvelope);
    }

    /**
     * Get the subset lower right coordinate
     * 
     * @return The subset lower right coordinate
     */
    public Coordinate getRequestLowerRight() {
        if (requestEnvelope == null) {
            return null;
        }
        return EnvelopeUtils.getLowerRightLatLon(requestEnvelope);
    }

    /**
     * Dataset upper left
     * 
     * @return dataset upper left coordinate
     */
    public Coordinate getUpperLeft() {
        if (envelope == null) {
            return null;
        }
        return EnvelopeUtils.getUpperLeftLatLon(envelope);
    }

    /**
     * Dataset Lower Right
     * 
     * @return dataset lower right coordinate
     */
    public Coordinate getLowerRight() {
        if (envelope == null) {
            return null;
        }
        return EnvelopeUtils.getLowerRightLatLon(envelope);
    }

}
