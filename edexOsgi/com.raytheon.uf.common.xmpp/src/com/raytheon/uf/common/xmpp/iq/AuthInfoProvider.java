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

import java.io.IOException;

import org.jivesoftware.smack.provider.IQProvider;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.PacketConstants;

/**
 * Custom XMPP IQ packet parsing for public key authentication
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AuthInfoProvider extends BaseProvider<AuthInfo> implements
        IQProvider {

    /**
     * @param extensionTagName
     */
    public AuthInfoProvider() {
        super(PacketConstants.QUERY_ELEMENT_NAME);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.provider.IQProvider#parseIQ(org.xmlpull.v1.
     * XmlPullParser)
     */
    @Override
    public AuthInfo parseIQ(XmlPullParser parser) throws Exception {
        return parse(parser);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.xmpp.BaseProvider#parseInternal(org.xmlpull.v1
     * .XmlPullParser)
     */
    @Override
    protected AuthInfo parseInternal(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        String encodedKey = null;
        String algorithm = null;
        String userid = null;
        String sessionid = null;


        do {
            String tagName = parser.getName();
            switch (parser.getEventType()) {
            case XmlPullParser.START_TAG:
                if (PacketConstants.QUERY_ELEMENT_NAME.equals(tagName)) {
                    sessionid = parser.getAttributeValue(null,
                            AuthInfo.SESSION_ATTRIBUTE);
                    userid = parser.getAttributeValue(null,
                            AuthInfo.ID_ATTRIBUTE);
                } else if (AuthInfo.PUBKEY_ELEMENT_NAME.equals(tagName)) {
                    algorithm = parser.getAttributeValue(null,
                            AuthInfo.ALG_ATTRIBUTE);
                    encodedKey = getText(parser);
                }
                break;
            }
            parser.next();
        } while (!atEndOfPacket(parser));
        return new AuthInfo(encodedKey, algorithm, userid, sessionid);
    }

}
