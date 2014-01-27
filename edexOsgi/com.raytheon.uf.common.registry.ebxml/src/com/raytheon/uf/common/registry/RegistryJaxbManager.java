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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * 
 * Jaxb Manager tailor for use specifically with registry objects. This jaxb
 * manager uses a namespace mapper to correctly map registry object namespaces
 * according to the spec
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
public class RegistryJaxbManager extends JAXBManager {

    /** The namespace mapper property name on the marshaller */
    private static final String NAMESPACE_PREFIX_MAPPER_PROPERTY = "com.sun.xml.bind.namespacePrefixMapper";

    protected NamespacePrefixMapper namespaceMapper;

    /**
     * Creates a new RegistryJaxbManager. Hidden from public use
     * 
     * @throws JAXBException
     */
    protected RegistryJaxbManager() throws JAXBException {
        super();
    }

    /**
     * Creates a new RegistryJaxbManager with the given namespace mapper
     * 
     * @param namespaceMapper
     *            The namespace mapper
     * @param formattedOutput
     *            If the xml produced is formatted
     * @throws JAXBException
     *             If errors occur creating the jaxb context
     */
    public RegistryJaxbManager(RegistryNamespaceMapper namespaceMapper)
            throws JAXBException {
        super(
                oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class,
                com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse.class);
        this.namespaceMapper = namespaceMapper;
    }

    @Override
    protected Marshaller getMarshaller() throws JAXBException {
        Marshaller m = marshallers.poll();
        if (m == null) {
            m = getJaxbContext().createMarshaller();
            if (namespaceMapper != null) {
                m.setProperty(NAMESPACE_PREFIX_MAPPER_PROPERTY, namespaceMapper);
            }
        }
        return m;
    }

}
