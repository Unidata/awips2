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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.jivesoftware.smack.packet.IQ;

import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.XmlBuilder;
import com.raytheon.uf.common.xmpp.XmlBuilder.Pair;

/**
 * Custom IQ packet for remote security settings control. XMPP clients can query
 * the server for security status at any time. The XMPP server can also send out
 * update packets that signal the client to requery the server for an updated
 * security status.
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
public class SecurityToggle extends IQ {

    public static final String TOGGLE_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:security:toggle";

    public static final String MODE_ATTRIBUTE = "mode";

    public static enum Mode {
        ENABLED, DISABLED, UPDATED
    }

    private Mode mode;

    /**
     * 
     */
    public SecurityToggle() {
    }

    /**
     * @param mode
     */
    public SecurityToggle(Mode mode) {
        this.mode = mode;
    }

    /**
     * @param iq
     */
    public SecurityToggle(IQ iq) {
        super(iq);
    }

    /**
     * Create a query packet
     * 
     * @return
     */
    public static SecurityToggle createGet() {
        SecurityToggle rval = new SecurityToggle();
        rval.setType(Type.GET);
        return rval;
    }

    /**
     * Create a setter packet
     * 
     * @param m
     * @return
     */
    public static SecurityToggle createSet(Mode m) {
        SecurityToggle rval = new SecurityToggle(m);
        rval.setType(Type.SET);
        return rval;
    }

    /* (non-Javadoc)
     * @see org.jivesoftware.smack.packet.IQ#getChildElementXML()
     */
    @Override
    public String getChildElementXML() {
        XmlBuilder builder = new XmlBuilder();
        List<Pair> attributes;
        if ( mode != null){
            attributes = Arrays.asList(new Pair(MODE_ATTRIBUTE, mode.toString()));
        } else {
            attributes = Collections.<Pair>emptyList();
        }
        builder.appendTag(PacketConstants.QUERY_ELEMENT_NAME,
                TOGGLE_QUERY_XMLNS, attributes, true);
        return builder.toXml();
    }

    /**
     * @return the mode
     */
    public Mode getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(Mode mode) {
        this.mode = mode;
    }

}
