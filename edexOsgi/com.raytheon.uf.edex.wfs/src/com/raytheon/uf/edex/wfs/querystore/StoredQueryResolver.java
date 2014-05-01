/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.querystore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.opengis.wfs.v_2_0_0.CreateStoredQueryType;
import net.opengis.wfs.v_2_0_0.GetFeatureType;
import net.opengis.wfs.v_2_0_0.ObjectFactory;
import net.opengis.wfs.v_2_0_0.ParameterExpressionType;
import net.opengis.wfs.v_2_0_0.QueryExpressionTextType;
import net.opengis.wfs.v_2_0_0.QueryType;
import net.opengis.wfs.v_2_0_0.StoredQueryDescriptionType;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;

/**
 * Convert stored queries to get feature requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class StoredQueryResolver {

    protected final WfsRegistryImpl registry;

    protected static final ObjectFactory wfsFactory = new ObjectFactory();

    protected static final net.opengis.filter.v_2_0_0.ObjectFactory filtFactory = new net.opengis.filter.v_2_0_0.ObjectFactory();

	protected final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    public static class ParameterNode {
        public String name;

        public QName type;

        public Node node;

        public ParameterNode(String name, QName type, Node node) {
            this.name = name;
            this.type = type;
            this.node = node;
        }
    }

    public StoredQueryResolver(WfsRegistryImpl registry) {
        this.registry = registry;
    }

    /**
     * Merge parameters with stored query packaged in CreateStoredQuery xml
     * document
     * 
     * @param createStoredQueryStr
     * @param parameters
     *            map of parameter name to xml string that replaces that name in
     *            the stored query
     * @return
     * @throws Exception
     */
    public List<QueryType> resolve(String createStoredQueryStr,
            Map<String, String> parameters) throws OgcException {
        String resolved = replaceAll(createStoredQueryStr, parameters);
        try {
            CreateStoredQueryType holder = (CreateStoredQueryType) registry
                    .unmarshal(resolved);
            StoredQueryDescriptionType desc = holder.getStoredQueryDefinition()
                    .get(0);
            return resolve(desc, parameters);
        } catch (JAXBException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /**
     * @param desc
     * @param parameters
     * @return
     * @throws Exception
     */
    public List<QueryType> resolve(StoredQueryDescriptionType desc,
            Map<String, String> parameters) throws OgcException {
        List<QueryType> rval = new ArrayList<QueryType>();
        List<QueryExpressionTextType> texts = desc.getQueryExpressionText();
        for (QueryExpressionTextType text : texts) {
            List<Object> content = text.getContent();
            for (Object obj : content) {
                if (obj instanceof Node) {
                    try {
                        obj = registry.unmarshal((Node) obj);
                    } catch (JAXBException e) {
                        throw new OgcException(Code.InternalServerError, e);
                    }
                }
                if (obj instanceof JAXBElement<?>) {
                    obj = ((JAXBElement<?>) obj).getValue();
                }
                if (obj instanceof QueryType) {
                    rval.add((QueryType) obj);
                } else {
                    log.error("Unsupported query type: " + obj.getClass());
                }
            }
        }
        return rval;
    }

    /**
     * Merge parameters with stored query packaged in CreateStoredQuery xml
     * document
     * 
     * @param createStoredQueryStr
     * @param parameters
     *            map of parameter name to xml string that replaces that name in
     *            the stored query
     * @return
     */
    private String replaceAll(String createStoredQueryStr,
            Map<String, String> parameters){
        StringBuilder rval = new StringBuilder(createStoredQueryStr);
        int index = 0;
        while (true) {
            index = rval.indexOf("${", index);
            if (index == -1) {
                break;
            }
            int end = rval.indexOf("}", index);
            if (end == -1) {
                break;
            }
            int nextOpen = rval.indexOf("${", index + 2);
            if (nextOpen > 0 && end > nextOpen) {
                index = index + 2;
                continue;
            }
            String key = rval.substring(index + 2, end);
            String value = parameters.get(key);
            if (value != null) {
                rval.replace(index, end + 1, value);
                index = index + value.length();
            } else {
                index = end + 1;
            }
        }
        return rval.toString();
    }

    /**
     * Create a template Node for stored query that is Filter 2.0 value
     * referenced operator.
     * 
     * @param operator
     *            name of Filter 2.0 operator
     * @param valueRef
     *            feature field being referenced by operator
     * @param parameter
     *            template placeholder for operand that will be replaced when
     *            stored query is resolved
     * @return
     * @throws ParserConfigurationException
     */
    public Node createReferenceOpNode(String operator, String valueRef,
            String parameter) throws ParserConfigurationException {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.newDocument();
        Element opNode = doc.createElementNS(OgcNamespace.FES20, "fes:"
                + operator);
        Element valRefNode = doc.createElementNS(OgcNamespace.FES20,
                "fes:ValueReference");
        Text refText = doc.createTextNode(valueRef);
        valRefNode.appendChild(refText);
        opNode.appendChild(valRefNode);
        Text paramNode = doc.createTextNode("${" + parameter + "}");
        opNode.appendChild(paramNode);
        return opNode;
    }

    /**
     * Add parameters to stored query description filters. The parameters will
     * be in a logical 'and' with each of the query's top level filter elements
     * 
     * @param desc
     * @param parameters
     * @return
     * @throws JAXBException
     * @throws ParserConfigurationException
     */
    public StoredQueryDescriptionType addAndParams(
            StoredQueryDescriptionType desc, List<ParameterNode> parameters)
            throws JAXBException, ParserConfigurationException {
        List<ParameterExpressionType> pexprs = desc.getParameter();
        List<QueryExpressionTextType> texts = desc.getQueryExpressionText();
        for (ParameterNode param : parameters) {
            pexprs.add(createParamExpr(param.name, param.type));
            for (QueryExpressionTextType text : texts) {
                addAndParam(text, param);
            }
        }
        return desc;
    }

    /**
     * Add parameter to stored query expression text filters. The parameter will
     * be in a logical 'and' with each of the query's top level filter elements
     * 
     * @param text
     * @param param
     * @throws JAXBException
     * @throws ParserConfigurationException
     */
    public void addAndParam(QueryExpressionTextType text, ParameterNode param)
            throws JAXBException, ParserConfigurationException {
        List<Object> content = text.getContent();
        List<Object> newContents = new ArrayList<Object>(content.size());
        for (Object obj : content) {
            if (obj instanceof JAXBElement<?>) {
                obj = ((JAXBElement<?>) obj).getValue();
            }
            if (obj instanceof QueryType) {
                newContents.add(addAnd((QueryType) obj, param));
            } else if (obj instanceof Element) {
                Element elem = (Element) obj;
                newContents.addAll(addAndToElement(elem, param));
            }
        }
        text.unsetContent();
        text.setContent(newContents);
    }

    /**
     * @param elem
     * @param param
     * @return
     * @throws ParserConfigurationException
     */
    private List<Object> addAndToElement(Element elem, ParameterNode param)
            throws ParserConfigurationException {
        List<Object> rval = new ArrayList<Object>();
        String name = elem.getLocalName();
        if (name.equalsIgnoreCase("Query")) {
            rval.add(addAnd(elem, param));
        }
        return rval;
    }

    /**
     * Add parameter to stored query filters. The parameter will be in a logical
     * 'and' with each of the query's top level filter elements
     * 
     * @param qt
     * @param param
     * @return
     * @throws JAXBException
     * @throws ParserConfigurationException
     */
    protected Object addAnd(QueryType qt, ParameterNode param)
            throws JAXBException,
            ParserConfigurationException {
        Element qe = (Element) registry.marshalToNode(wfsFactory
                .createQuery(qt));
        return addAnd(qe, param);
    }

    /**
     * Add parameter to stored query filters. The parameter will be in a logical
     * 'and' with each of the query's top level filter elements
     * 
     * @param queryElement
     *            Query node
     * @param param
     * @return
     * @throws ParserConfigurationException
     */
    protected Object addAnd(Element queryElement, ParameterNode param)
            throws ParserConfigurationException {
        Node clonedQuery = queryElement.cloneNode(false);
        NodeList filters = queryElement.getElementsByTagNameNS(
                OgcNamespace.FES20, "Filter");
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.newDocument();
        Node importedClonedQuery = doc.importNode(clonedQuery, true);
        if (filters.getLength() == 0) {
            // no existing filter, skip creating 'and', just add parameter
            Element filter = doc.createElementNS(OgcNamespace.FES20,
                    "fes:Filter");
            Node paramNode = doc.importNode(param.node, true);
            filter.appendChild(paramNode);
            importedClonedQuery.appendChild(filter);
            return importedClonedQuery;
        }
        Node filter = filters.item(0);
        Node clonedFilter = filter.cloneNode(false);
        Node importedClonedFilter = doc.importNode(clonedFilter, true);
        importedClonedQuery.appendChild(importedClonedFilter);
        Element andElem = doc.createElementNS(OgcNamespace.FES20, "fes:And");
        importedClonedFilter.appendChild(andElem);
        Node paramNode = doc.importNode(param.node, true);
        andElem.appendChild(paramNode);
        for (Node child = filter.getFirstChild(); child != null; child = child
                .getNextSibling()) {
            Node importNode = doc.importNode(child, true);
            andElem.appendChild(importNode);
        }
        return importedClonedQuery;
    }

    /**
     * Create a new parameter expression type
     * 
     * @param name
     * @param type
     * @return
     */
    protected ParameterExpressionType createParamExpr(String name,
            QName type) {
        ParameterExpressionType rval = new ParameterExpressionType();
        rval.setName(name);
        rval.setType(type);
        return rval;
    }

    /**
     * Convert jaxb get feature request to stored query description object
     * 
     * @param id
     * @param featureRequest
     * @return
     */
    public StoredQueryDescriptionType convert(String id,
            GetFeatureType featureRequest) {
        StoredQueryDescriptionType desc = new StoredQueryDescriptionType();
        desc.setId(id);
        QueryExpressionTextType textType = new QueryExpressionTextType();
        List<JAXBElement<?>> expElems = featureRequest
                .getAbstractQueryExpression();
        List<Object> queries = extract(expElems);
        textType.setContent(queries);
        desc.setQueryExpressionText(Arrays.asList(textType));
        return desc;
    }

    /**
     * Extract jaxb element values to list
     * 
     * @param expElems
     * @return
     */
    private List<Object> extract(List<JAXBElement<?>> expElems) {
        ArrayList<Object> rval = new ArrayList<Object>(expElems.size());
        for (JAXBElement<?> elem : expElems) {
            rval.add(elem);
        }
        return rval;
    }

}
