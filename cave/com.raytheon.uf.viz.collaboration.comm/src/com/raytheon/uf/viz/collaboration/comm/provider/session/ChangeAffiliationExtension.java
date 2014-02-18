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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import org.jivesoftware.smackx.pubsub.Affiliation;
import org.jivesoftware.smackx.pubsub.Node;
import org.jivesoftware.smackx.pubsub.NodeExtension;
import org.jivesoftware.smackx.pubsub.PubSubElementType;

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
        StringBuilder builder = new StringBuilder("<");
        builder.append(getElementName());
        builder.append(" node='");
        builder.append(getNode());
        builder.append("'><").append(affiliationName).append(" ");
        builder.append(jidAttribute).append("='");
        builder.append(this.id);
        builder.append("' ").append(affiliationName).append("='");
        builder.append(this.type.toString());
        builder.append("'/></");
        builder.append(getElementName());
        builder.append(">");
        return builder.toString();
    }

}
