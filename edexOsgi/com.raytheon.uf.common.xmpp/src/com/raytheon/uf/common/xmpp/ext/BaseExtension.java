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

import org.jivesoftware.smack.packet.PacketExtension;

/**
 * Base packet extension class. Used for custom XMPP packet payloads.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class BaseExtension implements PacketExtension {

    protected final String elementName;

    protected final String namespace;

    /**
     * @param elementName
     * @param namespace
     */
    public BaseExtension(String elementName, String namespace) {
        this.elementName = elementName;
        this.namespace = namespace;
    }

    /* (non-Javadoc)
     * @see org.jivesoftware.smack.packet.PacketExtension#getElementName()
     */
    @Override
    public String getElementName() {
        return elementName;
    }

    /* (non-Javadoc)
     * @see org.jivesoftware.smack.packet.PacketExtension#getNamespace()
     */
    @Override
    public String getNamespace() {
        return namespace;
    }

}
