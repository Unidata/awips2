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
package com.raytheon.openfire.plugin.configuration.collaboration.iq;

import java.util.Arrays;

import org.apache.commons.lang.StringUtils;
import org.dom4j.Element;
import org.jivesoftware.openfire.IQHandlerInfo;
import org.jivesoftware.openfire.auth.UnauthorizedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;


/**
 * IQ handler for dataservers to store URL configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class HttpAddressHandler extends AbstractConfigHandler {

    private static final Logger log = LoggerFactory
            .getLogger(HttpAddressHandler.class);

    public static final String HTTP_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:http";

    public static final String HTTP_ELEMENT_NAME = "httpinfo";

    public static final String URL_ELEMENT_NAME = "url";

    public HttpAddressHandler() {
        super("Collaboration Dataserver HTTP Address Handler", Arrays
                .asList(HTTP_QUERY_XMLNS), new IQHandlerInfo(
                QUERY_ELEMENT_NAME, HTTP_QUERY_XMLNS));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.openfire.plugin.configuration.collaboration.AbstractHandler
     * #handleGet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleGet(IQ packet) throws UnauthorizedException {
        String dataserver = getPrimaryDataServer();
        if (dataserver == null) {
            // no dataserver configured, return empty response
            return IQ.createResultIQ(packet);
        }
        JID target = new JID(dataserver);
        return retrieve(packet, target.getNode(),
                createElement(HTTP_ELEMENT_NAME, COLLAB_XMLNS));
    }

    /**
     * Set http address for dataserver user with id.
     * 
     * @param id
     * @param url
     */
    public void setHttpAddress(String id, String url) {
        Element httpElem = createElement(HTTP_ELEMENT_NAME, COLLAB_XMLNS);
        Element urlElement = createElement(URL_ELEMENT_NAME, COLLAB_XMLNS);
        urlElement.setText(url);
        urlElement.setParent(null);
        httpElem.add(urlElement);
        JID target = new JID(id);
        store(target.getNode(), httpElem);
    }

    /**
     * Remove http address for dataserver user with id.
     * 
     * @param id
     * @param url
     */
    public void removeHttpAddress(String id, String url) {
        JID target = new JID(id);
        store(target.getNode(), createElement(HTTP_ELEMENT_NAME, COLLAB_XMLNS));
    }

    /**
     * Get http address for dataserver user with id.
     * 
     * @param id
     * @return
     */
    public String getHttpAddress(String id) {
        JID target = new JID(id);
        Element respElem = storage.get(target.getNode(),
                createElement(HTTP_ELEMENT_NAME, COLLAB_XMLNS));
        Element urlElem;
        try {
            urlElem = getChildElement(respElem, URL_ELEMENT_NAME, COLLAB_XMLNS);
        } catch (Exception e) {
            // missing url element, no url configured
            return null;
        }
        String rval = urlElem.getText();
        if (StringUtils.isBlank(rval)) {
            return null;
        }
        return rval.trim();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.openfire.plugin.configuration.collaboration.AbstractHandler
     * #handleSet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleSet(IQ packet) throws UnauthorizedException {
        JID from = packet.getFrom();
        if (!isDataServerUser(from)) {
            String msg = "User not authorized for service: " + from.toFullJID();
            log.debug(msg);
            throw new UnauthorizedException(msg);
        }
        Element queryElem = packet.getChildElement();
        Element infoElem;
        Element urlElem;
        try {
            infoElem = getChildElement(queryElem, HTTP_ELEMENT_NAME,
                    COLLAB_XMLNS);
            urlElem = getChildElement(infoElem, URL_ELEMENT_NAME, COLLAB_XMLNS);
        } catch (Exception e) {
            log.debug("Missing Required Element in packet: " + packet.toXML(),
                    e);
            return createError(packet, new PacketError(Condition.bad_request,
                    PacketError.Type.modify, e.getLocalizedMessage()));
        }
        String url = urlElem.getText();
        if (StringUtils.isBlank(url)) {
            log.debug("Missing HTTP URL Text");
            return createError(packet, new PacketError(Condition.bad_request,
                    PacketError.Type.modify, "Missing Required Text Body"));
        }
        store(packet.getFrom().getNode(), infoElem);
        return IQ.createResultIQ(packet);
    }

}
