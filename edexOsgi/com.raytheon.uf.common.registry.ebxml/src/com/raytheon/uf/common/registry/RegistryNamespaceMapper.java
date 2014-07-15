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
package com.raytheon.uf.common.registry;

import java.util.Iterator;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * 
 * Namespace mapper to be used with the RegistryJaxbManager to correctly map
 * registry namespaces
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/8/2013     1692        bphillip    Initial implementation
 * Jul 15, 2014 3373        bclement    removed warning
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryNamespaceMapper extends NamespacePrefixMapper implements
        NamespaceContext {

    /**
     * Creates a new RegistryNamespaceMapper
     */
    public RegistryNamespaceMapper() {

    }

    @Override
    public String getPreferredPrefix(String namespaceURI, String suggestion,
            boolean requirePrefix) {
        String prefix = EbxmlNamespaces.NAMESPACE_PREFIX_MAP.get(namespaceURI);
        return prefix == null ? suggestion : prefix;

    }

    @Override
    public String getNamespaceURI(String prefix) {
        String namespaceURI = EbxmlNamespaces.PREFIX_NAMESPACE_MAP.get(prefix);
        return namespaceURI == null ? XMLConstants.NULL_NS_URI : namespaceURI;
    }

    @Override
    public String getPrefix(String namespaceURI) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<?> getPrefixes(String namespaceURI) {
        throw new UnsupportedOperationException();
    }

}
