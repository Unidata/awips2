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
package com.raytheon.uf.edex.ogc.common.jaxb;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.raytheon.uf.common.serialization.jaxb.PooledJaxbMarshallerStrategy;

/**
 * Marshaller strategy for OGC XML operations. Includes w3c dom operations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2014 3373       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcMarshallerStrategy extends PooledJaxbMarshallerStrategy {

    /**
     * 
     */
    public OgcMarshallerStrategy() {
        super();
    }

    /**
     * @param poolSize
     * @param pooledObjectSizeLimit
     */
    public OgcMarshallerStrategy(int poolSize, int pooledObjectSizeLimit) {
        super(poolSize, pooledObjectSizeLimit);
    }

    /**
     * Unmarshal object from w3c node.
     * 
     * @param ctx
     * @param node
     * @return
     * @throws JAXBException
     */
    public Object unmarshal(JAXBContext ctx, Node node) throws JAXBException {
        Unmarshaller msh = createUnmarshaller(ctx);
        Object obj = msh.unmarshal(node);
        if (obj instanceof JAXBElement<?>) {
            obj = ((JAXBElement<?>) obj).getValue();
        }
        return obj;
    }

    /**
     * Marshal object to w3c node
     * 
     * @param ctx
     * @param obj
     * @return
     * @throws JAXBException
     * @throws ParserConfigurationException
     */
    public Node marshalToNode(JAXBContext ctx, Object obj)
            throws JAXBException, ParserConfigurationException {
        Marshaller msh = createMarshaller(ctx);
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.newDocument();
        msh.marshal(obj, doc);
        return doc.getFirstChild();
    }

}
