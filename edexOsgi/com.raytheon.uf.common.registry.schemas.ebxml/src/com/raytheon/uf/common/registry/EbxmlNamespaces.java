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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * 
 * Namespaces used by the EBXML 4.0 spec. This list consists of namespaces
 * listed in section 1.6 of OASIS ebXML RegRep Version 4.0, Part 0: Overview
 * Document
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/15/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class EbxmlNamespaces {

    /** Map which maps namespaceURIs to prefixes */
    public static final Map<String, String> NAMESPACE_PREFIX_MAP;

    /** Map which maps prefixes to namespaceURIs */
    public static final Map<String, String> PREFIX_NAMESPACE_MAP;

    public static final String OASIS_PREFIX = "urn:oasis:names:tc:ebxml-regrep:";

    public static final String OASIS_XSD_PREFIX = OASIS_PREFIX + "xsd:";

    public static final String OASIS_WSDL_PREFIX = OASIS_PREFIX + "wsdl:";

    public static final String VERSION = ":4.0";

    /**
     * EbXML RegRep Part 3: XML Schema file xsd/lcm.xsd
     * <p>
     * Schema used by the LifecycleManager interface
     */
    public static final String LCM = "lcm";

    public static final String LCM_URI = OASIS_XSD_PREFIX + LCM + VERSION;

    /**
     * EbXML RegRep Part 3: XML Schema file xsd/query.xsd
     * <p>
     * Schema used by the QueryManager interface
     */
    public static final String QUERY = "query";

    public static final String QUERY_URI = OASIS_XSD_PREFIX + QUERY + VERSION;

    /**
     * ebXML RegRep Part 3: XML Schema file xsd/rim.xsd
     * <p>
     * Schema used for information model objects specified by [re-
     * grep-rim-v4.0].
     */
    public static final String RIM = "rim";

    public static final String RIM_URI = OASIS_XSD_PREFIX + RIM + VERSION;

    /**
     * ebXML RegRep Part 3: XML Schema file xsd/rs.xsd
     * <p>
     * Common schema used by registry protocols defined by [re- grep-rs-v4.0].
     */
    public static final String RS = "rs";

    public static final String RS_URI = OASIS_XSD_PREFIX + RS + VERSION;

    /**
     * ebXML RegRep Part 3: XML Schema file xsd/spi.xsd
     * <p>
     * Schema used by the service provider interfaces defined by [re-
     * grep-rs-v4.0].
     */
    public static final String SPI = "spi";

    public static final String SPI_URI = OASIS_XSD_PREFIX + SPI + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/Notification-
     * ListenerBindings.wsdl
     * <p>
     * WSDL binding definitions for NotificationListeners defined by
     * [regrep-rs-v4.0].
     */
    public static final String NL_BIND = "nl-bind";

    public static final String NL_BIND_URI = OASIS_WSDL_PREFIX
            + "NotificationListener:bindings" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/Notification-
     * ListenerInterfaces.wsdl
     * <p>
     * WSDL interface definitions for NotificationListeners defined by
     * [regrep-rs-v4.0].
     */
    public static final String NL_INT = "nl-int";

    public static final String NL_INT_URI = OASIS_WSDL_PREFIX
            + "NotificationListener:interfaces" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/Notification-
     * ListenerServices.wsdl
     * <p>
     * WSDL service definitions for NotificationListeners defined by
     * [regrep-rs-v4.0].
     */
    public static final String NL_SERV = "nl-serv";

    public static final String NL_SERV_URI = OASIS_WSDL_PREFIX
            + "NotificationListener:services" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/re- grep-server-binding.wsdl
     * <p>
     * WSDL binding definitions for interfaces defined by [re- grep-rs-v4.0].
     */
    public static final String RR_BIND = "rr-bind";

    public static final String RR_BIND_URI = OASIS_WSDL_PREFIX
            + "registry:bindings" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/regrep-server-in- terface.wsdl
     * <p>
     * WSDL interface definitions for interfaces defined by [re- grep-rs-v4.0].
     */
    public static final String RR_INT = "rr-int";

    public static final String RR_INT_URI = OASIS_WSDL_PREFIX
            + "registry:interfaces" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/re- grep-server-service.wsdl
     * <p>
     * WSDL service definitions for services defined by [re- grep-rs-v4.0].
     */
    public static final String RR_SERV = "rr-serv";

    public static final String RR_SERV_URI = OASIS_WSDL_PREFIX
            + "registry:services" + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/regrep-server-binding.wsdl
     * <p>
     * WSDL binding definitions for service provider interfaces for server
     * plugins defined by [regrep-rs-v4.0].
     */
    public static final String SPI_ = "spi-";

    public static final String SPI__URI = OASIS_WSDL_PREFIX + "spi:bindings"
            + VERSION;

    /**
     * ebXML RegRep Part 4: WSDL file wsdl/1.1/regrep-server-in- terface.wsdl
     * <p>
     * WSDL interface definitions for service provider interfaces for server
     * plugin defined by [regrep-rs-v4.0].
     */
    public static final String SPI_INT = "spi-int";

    public static final String SPI_INT_URI = OASIS_WSDL_PREFIX
            + "spi:interfaces" + VERSION;

    /**
     * A normative XML Schema [XML Schema Part 1], [XML Schema Part 2] document
     * for the "http://www.w3.org/2003/05/soap-encod- ing" namespace can be
     * found at http://www.w3.org/2003/05/soap-encoding.
     */
    public static final String ENC = "enc";

    public static final String ENC_URI = "http://www.w3.org/2003/05/soap-encoding";

    /**
     * SOAP Version 1.2 Part 1. A normative XML Schema [XML Schema Part 1], [XML
     * Schema Part 2] document for the
     * "http://www.w3.org/2003/05/soap-envel- ope" namespace can be found at
     * http://www.w3.org/2003/05/soap-envelope.
     */
    public static final String ENV = "env";

    public static final String ENV_URI = "http://www.w3.org/2003/05/soap-envelope";

    /**
     * WSDL namespace for WSDL MIME binding.
     */
    public static final String MIME = "mime";

    public static final String MIME_URI = "http://schemas.xmlsoap.org/wsdl/mime/";

    /**
     * WSDL 1.1 namespace defined by WSDL 1.1 specification.
     */
    public static final String WSDL = "wsdl";

    public static final String WSDL_URI = "http://schemas.xmlsoap.org/wsdl/";

    /**
     * XACML 2.0 Core: eXtensible Access Control Markup Language (XACML) Version
     * 2.0
     */
    public static final String XACML = "xacml";

    public static final String XACML_URI = "urn:oasis:names:tc:xacml:2.0:policy:schema:os";

    /**
     * XACML 2.0 Core: eXtensible Access Control Markup Language (XACML) Version
     * 2.0
     */
    public static final String XACMLC = "xacmlc";

    public static final String XACMLC_URI = "urn:oasis:names:tc:xacml:2.0:context:schema:os";

    /**
     * XML Linking Language (XLink) Version 1.1
     */
    public static final String XLINK = "xlink";

    public static final String XLINK_URI = "http://www.w3.org/1999/xlink";

    /**
     * XML Schema [XML Schema Part 1], [XML Schema Part 2] specification
     */
    public static final String XS = "xs";

    public static final String XS_URI = "http://www.w3.org/2001/XMLSchema";

    /**
     * W3C XML Schema specification [XML Schema Part 1], [XML Schema Part 2].
     */
    public static final String XSI = "xsi";

    public static final String XSI_URI = "http://www.w3.org/2001/XMLSchema-instance";

    /**
     * Addressing namespace
     */
    public static final String ADDRESSING = "addressing";

    public static final String ADDRESSING_URI = "http://www.w3.org/2005/08/addressing";

    static {
        Map<String, String> prefixNamespaceMap = new HashMap<String, String>(22);
        Map<String, String> namespacePrefixMap = new HashMap<String, String>(22);
        prefixNamespaceMap.put(LCM, LCM_URI);
        prefixNamespaceMap.put(QUERY, QUERY_URI);
        prefixNamespaceMap.put(RIM, RIM_URI);
        prefixNamespaceMap.put(RS, RS_URI);
        prefixNamespaceMap.put(SPI, SPI_URI);
        prefixNamespaceMap.put(NL_BIND, NL_BIND_URI);
        prefixNamespaceMap.put(NL_INT, NL_INT_URI);
        prefixNamespaceMap.put(NL_SERV, NL_SERV_URI);
        prefixNamespaceMap.put(RR_BIND, RR_BIND_URI);
        prefixNamespaceMap.put(RR_INT, RR_INT_URI);
        prefixNamespaceMap.put(RR_SERV, RR_SERV_URI);
        prefixNamespaceMap.put(SPI_, SPI__URI);
        prefixNamespaceMap.put(SPI_INT, SPI_INT_URI);
        prefixNamespaceMap.put(ENC, ENC_URI);
        prefixNamespaceMap.put(ENV, ENV_URI);
        prefixNamespaceMap.put(MIME, MIME_URI);
        prefixNamespaceMap.put(WSDL, WSDL_URI);
        prefixNamespaceMap.put(XACML, XACML_URI);
        prefixNamespaceMap.put(XACMLC, XACMLC_URI);
        prefixNamespaceMap.put(XLINK, XLINK_URI);
        prefixNamespaceMap.put(XS, XS_URI);
        prefixNamespaceMap.put(XSI, XSI_URI);
        for (String prefix : prefixNamespaceMap.keySet()) {
            namespacePrefixMap.put(prefixNamespaceMap.get(prefix), prefix);
        }
        PREFIX_NAMESPACE_MAP = Collections.unmodifiableMap(prefixNamespaceMap);
        NAMESPACE_PREFIX_MAP = Collections.unmodifiableMap(namespacePrefixMap);
    }
}
