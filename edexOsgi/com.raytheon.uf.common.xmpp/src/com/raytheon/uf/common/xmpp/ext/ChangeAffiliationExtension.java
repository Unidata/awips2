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
package com.raytheon.uf.common.xmpp.ext;

import java.util.Arrays;

import org.jivesoftware.smackx.pubsub.Affiliation;
import org.jivesoftware.smackx.pubsub.Node;
import org.jivesoftware.smackx.pubsub.NodeExtension;
import org.jivesoftware.smackx.pubsub.PubSubElementType;

import com.raytheon.uf.common.xmpp.XmlBuilder;
import com.raytheon.uf.common.xmpp.XmlBuilder.Pair;

/**
 * Packet extension for changing a user's affiliation with a pubsub topic.
 * Follows specification at
 * http://xmpp.org/extensions/xep-0060.html#owner-affiliations-modify
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2014 2751       bclement     Initial creation
 * Feb 27, 2013 2756       bclement     moved to common.xmpp from collaboration
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ChangeAffiliationExtension extends NodeExtension {

    private static final String affiliationName = "affiliation";

    private static final String jidAttribute = "jid";

    private final String id;

    private final Affiliation.Type type;


    /**
     * @param n
     *            topic node
     * @param id
     *            userid
     * @param type
     *            new affiliation
     */
    public ChangeAffiliationExtension(Node n, String id, Affiliation.Type type) {
        super(PubSubElementType.AFFILIATIONS, n.getId());
        this.id = id;
        this.type = type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smackx.pubsub.NodeExtension#toXML()
     */
    @Override
    public String toXML() {
        XmlBuilder builder = new XmlBuilder();
        builder.startTag(getElementName(),
                Arrays.asList(new Pair("node", getNode())));
        builder.selfClosingTag(affiliationName, Arrays.asList(new Pair(
                jidAttribute, this.id),
                new Pair(affiliationName, this.type.toString())));
        builder.endTag(getElementName());
        return builder.toXml();
    }

}
