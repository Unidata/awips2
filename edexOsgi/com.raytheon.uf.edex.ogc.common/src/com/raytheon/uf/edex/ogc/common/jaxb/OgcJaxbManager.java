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
 */
package com.raytheon.uf.edex.ogc.common.jaxb;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.sun.xml.bind.api.JAXBRIContext;
import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * Cache and utility class for OGC JAXB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2011            bclement    Initial creation
 * Aug 18, 2013  #2097     dhladky     extended JAXBManager
 * Jul 15, 2014  3373      bclement    rewritten to use JAXBManager
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcJaxbManager extends JAXBManager {

	protected static final String JAXB_NAMESPACE_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

    private final OgcMarshallerStrategy marshStrategy;

    /**
     * @param classes
     * @throws JAXBException
     */
    public OgcJaxbManager(Class<?>[] classes) throws JAXBException {
        this(new OgcMarshallerStrategy(), classes);
    }

    /**
     * @param mapper
     *            mapping of namespaces to namespace prefixes
     * @param classes
     * @throws JAXBException
     */
    public OgcJaxbManager(NamespacePrefixMapper mapper, Class<?>[] classes)
            throws JAXBException {
        this(createStrategy(mapper), classes);
    }

    /**
     * @see JAXBManager#JAXBManager(boolean,
     *      com.raytheon.uf.common.serialization.jaxb.JaxbMarshallerStrategy,
     *      Class...)
     * @param strategy
     * @param classes
     * @throws JAXBException
     */
    public OgcJaxbManager(OgcMarshallerStrategy strategy, Class<?>[] classes)
            throws JAXBException {
        super(strategy, classes);
        this.marshStrategy = strategy;
    }

    /**
     * Create a marshaller strategy that uses the provided namespace prefix
     * mapping
     * 
     * @param mapper
     * @return
     */
    private static OgcMarshallerStrategy createStrategy(
            final NamespacePrefixMapper mapper) {
        return new OgcMarshallerStrategy() {
            @Override
            protected Marshaller createMarshaller(JAXBContext context)
                    throws JAXBException {
                Marshaller rval = super.createMarshaller(context);
                if (mapper != null) {
                    rval.setProperty(JAXB_NAMESPACE_MAPPER, mapper);
                }
                return rval;
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.serialization.JAXBManager#getJaxbConfig()
     */
    @Override
    protected Map<String, Object> getJaxbConfig() throws JAXBException {
        Map<String, Object> jaxbConfig = new HashMap<String, Object>();
        TransientAnnotationReader reader = new TransientAnnotationReader();
        try {
            reader.addTransientField(Throwable.class
                    .getDeclaredField("stackTrace"));
            reader.addTransientMethod(Throwable.class
                    .getDeclaredMethod("getStackTrace"));
        } catch (Exception e) {
            throw new JAXBException("Unable to add transient members", e);
        }
        jaxbConfig.put(JAXBRIContext.ANNOTATION_READER, reader);
        return jaxbConfig;
	}

    /**
     * @see OgcMarshallerStrategy#unmarshal(JAXBContext, Node)
     * @param node
     * @return
     * @throws JAXBException
     */
    public Object unmarshal(Node node) throws JAXBException {
        JAXBContext ctx = getJaxbContext();
        return marshStrategy.unmarshal(ctx, node);
    }

    /**
     * @see OgcMarshallerStrategy#marshalToNode(JAXBContext, Object)
     * @param obj
     * @return
     * @throws JAXBException
     * @throws ParserConfigurationException
     */
    public Node marshalToNode(Object obj) throws JAXBException,
            ParserConfigurationException {
        JAXBContext ctx = getJaxbContext();
        return marshStrategy.marshalToNode(ctx, obj);
    }

}
