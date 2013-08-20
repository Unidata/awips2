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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryNamespaceMapper extends NamespacePrefixMapper implements
        NamespaceContext {

    /** Map which maps namespaceURIs to prefixes */
    public static final Map<String, String> NAMESPACE_PREFIX_MAP = new HashMap<String, String>(
            7);

    /** Map which maps prefixes to namespaceURIs */
    public static final Map<String, String> PREFIX_NAMESPACE_MAP = new HashMap<String, String>(
            7);

    static {
        NAMESPACE_PREFIX_MAP.put("urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0",
                "rim");
        PREFIX_NAMESPACE_MAP.put("rim",
                "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0");

        NAMESPACE_PREFIX_MAP.put("urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0",
                "lcm");
        PREFIX_NAMESPACE_MAP.put("lcm",
                "urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0");

        NAMESPACE_PREFIX_MAP.put("urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0",
                "spi");
        PREFIX_NAMESPACE_MAP.put("spi",
                "urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0");

        NAMESPACE_PREFIX_MAP.put(
                "urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0", "query");
        PREFIX_NAMESPACE_MAP.put("query",
                "urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0");

        NAMESPACE_PREFIX_MAP.put("urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0",
                "rs");
        PREFIX_NAMESPACE_MAP.put("rs",
                "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0");

        NAMESPACE_PREFIX_MAP.put("http://www.w3.org/2005/08/addressing",
                "addressing");
        PREFIX_NAMESPACE_MAP.put("addressing",
                "http://www.w3.org/2005/08/addressing");

        NAMESPACE_PREFIX_MAP.put("http://www.w3.org/1999/xlink", "xlink");
        PREFIX_NAMESPACE_MAP.put("xlink", "http://www.w3.org/1999/xlink");
    }

    /**
     * Creates a new RegistryNamespaceMapper
     */
    public RegistryNamespaceMapper() {

    }

    @Override
    public String getPreferredPrefix(String namespaceURI, String suggestion,
            boolean requirePrefix) {
        String prefix = NAMESPACE_PREFIX_MAP.get(namespaceURI);
        return prefix == null ? suggestion : prefix;

    }

    @Override
    public String getNamespaceURI(String prefix) {
        String namespaceURI = PREFIX_NAMESPACE_MAP.get(prefix);
        return namespaceURI == null ? XMLConstants.NULL_NS_URI : namespaceURI;
    }

    @Override
    public String getPrefix(String namespaceURI) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator getPrefixes(String namespaceURI) {
        throw new UnsupportedOperationException();
    }

}
