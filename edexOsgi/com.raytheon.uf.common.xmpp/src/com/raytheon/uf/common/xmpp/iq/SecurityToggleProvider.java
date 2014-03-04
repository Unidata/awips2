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

import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.provider.IQProvider;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.iq.SecurityToggle.Mode;

/**
 * Custom XMPP IQ packet parser for security toggle
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SecurityToggleProvider extends BaseProvider<SecurityToggle>
        implements IQProvider {

    /**
     * @param tagName
     */
    public SecurityToggleProvider() {
        super(PacketConstants.QUERY_ELEMENT_NAME);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.common.xmpp.BaseProvider#parseInternal(org.xmlpull.v1.XmlPullParser)
     */
    @Override
    protected SecurityToggle parseInternal(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        Mode mode = null;

        do {
            String tagName = parser.getName();
            switch (parser.getEventType()) {
            case XmlPullParser.START_TAG:
                if (PacketConstants.QUERY_ELEMENT_NAME.equals(tagName)) {
                    String modeStr = parser.getAttributeValue(null,
                            SecurityToggle.MODE_ATTRIBUTE);
                    if (modeStr != null && !modeStr.trim().isEmpty()) {
                        mode = Mode.valueOf(modeStr.trim().toUpperCase());
                    }
                }
                break;
            }
            parser.next();
        } while (!atEndOfPacket(parser));
        return new SecurityToggle(mode);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.provider.IQProvider#parseIQ(org.xmlpull.v1.
     * XmlPullParser)
     */
    @Override
    public IQ parseIQ(XmlPullParser parser) throws Exception {
        return super.parse(parser);
    }

}
