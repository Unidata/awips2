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
package com.raytheon.uf.common.xmpp.iq;

import java.util.ArrayList;
import java.util.List;

import org.jivesoftware.smack.packet.IQ;

import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.XmlBuilder;
import com.raytheon.uf.common.xmpp.XmlBuilder.Pair;

/**
 * Info Query packet for feed venue configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2014 3288       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeedVenueConfig extends IQ {

    public static enum VenueType {
        /* SINGLE is a normal room, AGGREGATE is multiple rooms viewed as one */
        SINGLE, AGGREGATE;
    }

    public static final String FEED_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:feed";

    public static final String FEED_VENUE_SUBDOMAIN_ATTRIBUTE = "subdomain";

    public static final String FEED_VENUE_NAME_ATTRIBUTE = "name";

    public static final String FEED_VENUE_TYPE_ATTRIBUTE = "type";

    private String subdomain;

    private String name;

    private VenueType venueType;

    /**
     * 
     */
    public FeedVenueConfig() {
    }

    /**
     * @param iq
     */
    public FeedVenueConfig(IQ iq) {
        super(iq);
    }

    /**
     * Create a query packet
     * 
     * @return
     */
    public static FeedVenueConfig createGet() {
        FeedVenueConfig rval = new FeedVenueConfig();
        rval.setType(Type.GET);
        return rval;
    }

    /**
     * @param subdomain
     * @param name
     * @param type
     */
    public FeedVenueConfig(String subdomain, String name, VenueType type) {
        this.subdomain = subdomain;
        this.name = name;
        this.venueType = type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.packet.IQ#getChildElementXML()
     */
    @Override
    public String getChildElementXML() {
        XmlBuilder builder = new XmlBuilder();
        List<Pair> attributes = new ArrayList<XmlBuilder.Pair>(3);
        addAttributeIfPresent(attributes, FEED_VENUE_SUBDOMAIN_ATTRIBUTE, name);
        addAttributeIfPresent(attributes, FEED_VENUE_SUBDOMAIN_ATTRIBUTE,
                subdomain);
        addAttributeIfPresent(attributes, FEED_VENUE_TYPE_ATTRIBUTE, venueType);
        builder.appendTag(PacketConstants.QUERY_ELEMENT_NAME, FEED_QUERY_XMLNS,
                attributes, true);
        return builder.toXml();
    }

    /**
     * Add a new pair object to list if value is non-null. Attribute value will
     * be the object's toString() value
     * 
     * @param attributes
     * @param attributeName
     * @param valueObject
     */
    private static void addAttributeIfPresent(List<Pair> attributes,
            String attributeName, Object valueObject) {
        if (valueObject != null) {
            attributes.add(new Pair(attributeName, valueObject.toString()));
        }
    }

    /**
     * @return the subdomain
     */
    public String getSubdomain() {
        return subdomain;
    }

    /**
     * @param subdomain
     *            the subdomain to set
     */
    public void setSubdomain(String subdomain) {
        this.subdomain = subdomain;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the venueType
     */
    public VenueType getVenueType() {
        return venueType;
    }

    /**
     * @param venueType
     *            the venueType to set
     */
    public void setVenueType(VenueType venueType) {
        this.venueType = venueType;
    }

}
