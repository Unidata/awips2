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

import org.jivesoftware.smack.packet.IQ;

import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.XmlBuilder;

/**
 * Custom XMPP IQ packet for HTTP server configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * Mar 04, 2014 2756       bclement     fixed null return from getChildElementXML
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class HttpInfo extends IQ {

    public static final String INFO_ELEMENT = "httpinfo";

    public static final String URL_ELEMENT = "url";

    public static final String QUERY_XMLNS = "urn:uf:viz:collaboration:iq:http";

    private String url;

    /**
     * 
     */
    public HttpInfo() {
    }

    /**
     * @param iq
     */
    public HttpInfo(IQ iq) {
        super(iq);
    }

    /**
     * @param url
     *            bare HTTP url of data server
     */
    public HttpInfo(String url) {
        this.url = url;
    }

    @Override
    public String getChildElementXML() {
        XmlBuilder builder = new XmlBuilder();
        builder.startTag(PacketConstants.QUERY_ELEMENT_NAME, QUERY_XMLNS);
        builder.startTag(INFO_ELEMENT, PacketConstants.COLLAB_XMLNS);
        builder.startTag(URL_ELEMENT).appendText(url);
        builder.endTag(URL_ELEMENT);
        builder.endTag(INFO_ELEMENT);
        builder.endTag(PacketConstants.QUERY_ELEMENT_NAME);
        return builder.toXml();
    }

    /**
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * @param url
     *            the url to set
     */
    public void setUrl(String url) {
        this.url = url;
    }

}
