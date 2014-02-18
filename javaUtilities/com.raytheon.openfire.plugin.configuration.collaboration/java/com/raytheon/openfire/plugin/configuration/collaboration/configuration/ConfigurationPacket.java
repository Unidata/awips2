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
package com.raytheon.openfire.plugin.configuration.collaboration.configuration;

import org.apache.commons.lang.StringUtils;
import org.dom4j.Element;
import org.jivesoftware.util.JiveGlobals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.Message;
import org.xmpp.packet.PacketExtension;


/**
 * Packet extension for collaboration configuration messages to clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2013            bclement     Initial creation
 * Feb 14, 2014 2756       bclement     moved preferences to separate file
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class ConfigurationPacket extends PacketExtension {

    private static final Logger log = LoggerFactory
            .getLogger(ConfigurationPacket.class);

    // property keys

    public static final String XMLNS_KEY = "plugin.collaboration.packet.xmlns";

    public static final String ELEMENT_KEY = "plugin.collaboration.packet.element";

    public static final String ATTRIBUTES_KEY = "plugin.collaboration.packet.attributes";

    public static final String LEGACY_KEY = "plugin.collaboration.packet.legacy";

    // property value defaults

    public static final String XMLNS_DEFAULT = "urn:uf:viz:collaboration";

    public static final String ELEMENT_DEFAULT = "SessionData";

    public static final String ATTRIBUTES_DEFAULT = "payloadtype=Config,encoding=STRING";

    // legacy

    private static final String LEGACY_PREAMBLE = "[[CONFIG#";

    private static final String LEGACY_SUFFIX = "]]";

    /**
     * @param element
     */
    public ConfigurationPacket(Element element) {
        super(element);
    }


    /**
     * @param body
     *            configuration payload for packet
     */
    public ConfigurationPacket(String body) {
        super(create(body));
    }

    /**
     * Create packet extension element
     * 
     * @param body
     *            configuration payload for packet
     * @return
     */
    public static Element create(String body) {
        String element = PacketPreferences.get(ELEMENT_KEY, ELEMENT_DEFAULT);
        String xmlns = PacketPreferences.get(XMLNS_KEY, XMLNS_DEFAULT);
        Element rval = docFactory.createElement(element, xmlns);
        String attributes = PacketPreferences.get(ATTRIBUTES_KEY,
                ATTRIBUTES_DEFAULT);
        for (String keyval : StringUtils.split(attributes, ',')) {
            String[] separated = StringUtils.split(keyval, '=');
            if (separated.length != 2) {
                log.error("Malformed key-value pair in configuration: "
                        + keyval);
                continue;
            }
            rval.addAttribute(separated[0].trim(), separated[1].trim());
        }
        rval.addText(body);
        return rval;
    }

    /**
     * Create extended message packet with configuration payload
     * 
     * @param body
     *            configuration payload for packet
     * @return
     */
    public static Message createMessage(String body) {
        Message rval = new Message();
        rval.addExtension(new ConfigurationPacket(body));
        // pre 14.3 message format support
        if (JiveGlobals.getBooleanProperty(LEGACY_KEY, true)) {
            rval.setBody(LEGACY_PREAMBLE + body + LEGACY_SUFFIX);
        }
        return rval;
    }

}
