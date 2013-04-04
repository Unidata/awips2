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
package com.raytheon.uf.viz.datadelivery.common.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.serialization.adapters.ReferencedEnvelopeAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Area Filter Settings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            mpduff     Initial creation.
 * Jun  7, 2012    684     jpiatt     Added region name.
 * Dec 10, 2012   1259     bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "Area")
@XmlAccessorType(XmlAccessType.NONE)
public class AreaXML implements IDisplayXml {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AreaXML.class);

    @XmlElement
    @DynamicSerializeElement
    @XmlJavaTypeAdapter(value = ReferencedEnvelopeAdapter.class)
    protected ReferencedEnvelope envelope;

    @XmlElement(name = "regionName", type = String.class)
    protected String regionName;

    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }

    /**
     * Get region name.
     * 
     * @return the region name
     */
    public String getRegionName() {
        return regionName;
    }

    /**
     * Set region name.
     * 
     * @param regionName the name to set
     */
    public void setRegionName(String regionName) {
        this.regionName = regionName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString
     * ()
     */
    @Override
    public String getDisplayXmlString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Type: Area\n");

        if (envelope != null && !envelope.isNull()) {
            Coordinate ul = EnvelopeUtils.getUpperLeftLatLon(envelope);
            Coordinate lr = EnvelopeUtils.getLowerRightLatLon(envelope);
            sb.append("     Upper Left:\n");
            sb.append("         North Latitude: " + ul.y + "\n");
            sb.append("         West Longitude: " + ul.x + "\n");
            sb.append("     Lower Right:\n");
            sb.append("         South Latitude: " + lr.y + "\n");
            sb.append("         East Longitude: " + lr.x + "\n");
        } else {
            sb.append("     Upper Left:\n");
            sb.append("         North Latitude: \n");
            sb.append("         West Longitude: \n");
            sb.append("     Lower Right:\n");
            sb.append("         South Latitude: \n");
            sb.append("         East Longitude: \n");
        }
        
        return sb.toString();
    }
}
