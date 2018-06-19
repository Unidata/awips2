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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.jaxb.JaxbMarshallerStrategy;
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
 * Jul 15, 2014 3373       bclement     jaxb manager changes
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryJaxbManager extends JAXBManager {

    /** The namespace mapper property name on the marshaller */
    private static final String NAMESPACE_PREFIX_MAPPER_PROPERTY = "com.sun.xml.bind.namespacePrefixMapper";

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
                createStrategy(namespaceMapper),
                oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class,
                oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class,
                com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse.class);
    }

    private static JaxbMarshallerStrategy createStrategy(
            final NamespacePrefixMapper namespaceMapper) {
        return new JaxbMarshallerStrategy() {
            @Override
            protected Marshaller createMarshaller(JAXBContext context)
                    throws JAXBException {
                Marshaller rval = super.createMarshaller(context);
                if (namespaceMapper != null) {
                    rval.setProperty(NAMESPACE_PREFIX_MAPPER_PROPERTY,
                            namespaceMapper);
                }
                return rval;
            }
        };
    }

}
