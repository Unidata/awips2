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

import org.dom4j.Element;
import org.dom4j.tree.DefaultAttribute;
import org.jivesoftware.openfire.IQHandlerInfo;
import org.jivesoftware.openfire.auth.UnauthorizedException;
import org.jivesoftware.util.JiveGlobals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.ConfigurationPacket;

/**
 * IQ handler for dataservers to query for security enforcing status
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2014  2756      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SecurityToggleHandler extends AbstractConfigHandler {

    public static enum Mode {
        // the openfire build requires the semicolon or it won't compile
        ENABLED, DISABLED, UPDATED;
    }

    public static final String TOGGLE_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:security:toggle";

    public static final String MODE_ATTRIBUTE = "mode";

    private static final Logger log = LoggerFactory
            .getLogger(SecurityToggleHandler.class);

    /**
     * 
     */
    public SecurityToggleHandler() {
        super("Security Toggle Status Handler", Arrays
                .asList(TOGGLE_QUERY_XMLNS), new IQHandlerInfo(
                AbstractConfigHandler.QUERY_ELEMENT_NAME, TOGGLE_QUERY_XMLNS));
    }

    /**
     * Create a packet to send to dataservers when security toggle updates. The
     * client should respond by querying for the new toggle status.
     * 
     * @return
     */
    public static IQ createUpdatePacket() {
        Element query = AbstractConfigHandler.createElement("query",
                TOGGLE_QUERY_XMLNS);
        query.add(new DefaultAttribute(MODE_ATTRIBUTE, Mode.UPDATED.toString()));
        IQ rval = new IQ();
        rval.setChildElement(query);
        rval.setType(Type.set);
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.
     * AbstractConfigHandler#handleGet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleGet(IQ packet) throws UnauthorizedException {
        boolean legacyEnabled = JiveGlobals.getBooleanProperty(
                ConfigurationPacket.LEGACY_KEY, true);

        Mode mode;
        if (legacyEnabled) {
            // legacy mode does not support security
            mode = Mode.DISABLED;
        } else {
            mode = Mode.ENABLED;
        }
        Element query = packet.getChildElement();
        query.setParent(null);
        query.add(new DefaultAttribute(MODE_ATTRIBUTE, mode.toString()));
        IQ rval = IQ.createResultIQ(packet);
        rval.setChildElement(query);
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.
     * AbstractConfigHandler#handleSet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleSet(IQ packet) throws UnauthorizedException {
        log.debug("Received unsupported packet type: " + packet.getType());
        throw new UnauthorizedException(
                "Security toggle status can only be queried");
    }

}
