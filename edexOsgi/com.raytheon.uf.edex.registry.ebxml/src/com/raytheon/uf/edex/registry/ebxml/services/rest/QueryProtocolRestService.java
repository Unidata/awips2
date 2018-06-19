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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.JAXBException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.beanutils.ConstructorUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.RegistryJaxbManager;
import com.raytheon.uf.common.registry.RegistryNamespaceMapper;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.Languages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.rest.IQueryProtocolRestService;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.QueryDefinitionDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * 
 * REST service implementation for the Query Protocol according to
 * specifications in Section 12.2 of the ebRS specification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/19/2013    1931        bphillip    Initial implementation
 * 5/21/2013    2022        bphillip    Added interface and moved constants
 * 10/8/2013    1682        bphillip    Refactored to use parameter definitions from the registry
 * 10/30/2013   1538        bphillip    Changed root REST service path
 * 12/04/2015   4993        bphillip    Modified query output to more user readable format using xslt
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/search")
@Service
@Transactional
public class QueryProtocolRestService implements IQueryProtocolRestService {

    /** The logger */
    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /** The local query manager */
    private QueryManager queryManager;

    /** Jaxb Manager for marshalling the response */
    private JAXBManager responseJaxb;

    /** Data access object for query definitions */
    private QueryDefinitionDao queryDefinitionDao;

    /**
     * Creates a new QueryProtocolRestService instance
     * 
     * @throws JAXBException
     *             If errors occur while initializing the JAXBManager
     */
    public QueryProtocolRestService() throws JAXBException {
        responseJaxb = new RegistryJaxbManager(new RegistryNamespaceMapper());
    }

    @GET
    @Produces({ "text/html" })
    public String executeQuery(@Context
    UriInfo info) throws JAXBException, MsgRegistryException {

        String xml = "";
        /*
         * Extract out the canonical query parameters
         */
        MultivaluedMap<String, String> queryParameters = info
                .getQueryParameters();

        String queryId = getValue(queryParameters, QueryRequest.QUERY_ID,
                CanonicalQueryTypes.GET_OBJECT_BY_ID);
        BigInteger depth = getValue(queryParameters, QueryRequest.DEPTH,
                QueryRequest.DEFAULT_DEPTH);
        String format = getValue(queryParameters, QueryRequest.FORMAT,
                QueryRequest.DEFAULT_RESPONSE_FORMAT);
        Boolean federated = getValue(queryParameters, QueryRequest.FEDERATED,
                Boolean.FALSE);
        String federation = getValue(queryParameters, QueryRequest.FEDERATION,
                "");
        Boolean matchOlderVersions = getValue(queryParameters,
                QueryRequest.MATCH_OLDER_VERSIONS, Boolean.FALSE);
        BigInteger startIndex = getValue(queryParameters,
                QueryRequest.START_INDEX, QueryRequest.DEFAULT_START_INDEX);
        String lang = getValue(queryParameters, QueryRequest.LANG,
                Languages.EN_US);
        BigInteger maxResults = getValue(queryParameters,
                QueryRequest.MAX_RESULTS, QueryRequest.DEFAULT_MAX_RESULTS);
        String responseOption = getValue(queryParameters,
                QueryRequest.RESPONSE_OPTION, QueryReturnTypes.REGISTRY_OBJECT);
        Boolean returnRequest = getValue(queryParameters,
                QueryRequest.RETURN_REQUEST, Boolean.FALSE);
        Boolean formatResponse = getValue(queryParameters,
                QueryRequest.FORMAT_RESPONSE, Boolean.FALSE);

        /*
         * Create the query request object
         */
        QueryRequest restQueryRequest = new QueryRequest();
        restQueryRequest.setId(RegistryUtil.generateRegistryObjectId());
        restQueryRequest.setComment("Query received from REST Endpoint");
        ResponseOptionType responseOptionObj = new ResponseOptionType();
        responseOptionObj.setReturnType(responseOption);
        responseOptionObj.setReturnComposedObjects(true);

        QueryType queryType = new QueryType();
        queryType.setQueryDefinition(queryId);

        restQueryRequest.setDepth(depth);
        restQueryRequest.setFormat(format);
        restQueryRequest.setFederated(federated);
        restQueryRequest.setFederation(federation);
        restQueryRequest.setMatchOlderVersions(matchOlderVersions);
        restQueryRequest.setStartIndex(startIndex);
        restQueryRequest.setLang(lang);
        restQueryRequest.setMaxResults(maxResults);
        restQueryRequest.setQuery(queryType);
        restQueryRequest.setResponseOption(responseOptionObj);

        List<ParameterType> parameters = queryDefinitionDao
                .getParametersForQuery(queryId);

        for (ParameterType param : parameters) {
            String value = queryParameters.getFirst(param.getParameterName());
            if (isSpecified(value)) {
                RegistryQueryUtil.addSlotToQuery(value, param, queryType);
            }
        }

        QueryResponse response = null;
        if (returnRequest) {
            xml = responseJaxb.marshalToXml(restQueryRequest);
        } else {
            response = queryManager.executeQuery(restQueryRequest);
            if (formatResponse) {
                for (RegistryObjectType obj : response.getRegistryObjects()) {
                    /*
                     * The content slot must be removed since having an xml
                     * document inside another xml document confuses the xslt
                     * transformer. The content slot is removed here but the
                     * change is not persisted to the registry, so the original
                     * object remains intact.
                     */
                    obj.getSlot().remove(obj.getSlotByName("content"));
                }
            }
            xml = responseJaxb.marshalToXml(response);
        }
        return formatResponse ? xsltTransformResponse(xml) : xml;
    }

    /**
     * Transforms the query response using XSLT to a more user friendly format
     * 
     * @param xml
     *            The xml to format
     * @return The formatted output
     * @throws JAXBException
     */
    private String xsltTransformResponse(String xml) throws JAXBException {

        try (InputStream is = new ByteArrayInputStream(xml.getBytes())) {
            OutputStream outputStream = new ByteArrayOutputStream();
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer(new StreamSource(
                    System.getProperty("ebxml.registry.webserver.home")
                            + "/registry/formatRegistryObject.xsl"));
            Source text = new StreamSource(is);
            transformer.transform(text, new StreamResult(outputStream));
            return outputStream.toString();
        } catch (Exception e) {
            statusHandler.error(
                    "Error applying XSLT transform to the following xml:\n"
                            + xml, e);
            return xml;
        }

    }

    /**
     * Gets the value of a given url argument
     * 
     * @param queryParameters
     *            The parameters received for the query
     * @param parameterName
     *            The paremeter name to get
     * @param defaultValue
     *            The default value for this parameter to return if the
     *            parameter is not found in the query parameters
     * @return The value of the parameter or default value if not found
     * @throws MsgRegistryException
     */
    @SuppressWarnings("unchecked")
    private <T extends Object> T getValue(
            MultivaluedMap<String, String> queryParameters,
            String parameterName, T defaultValue) throws MsgRegistryException {
        String value = queryParameters.getFirst(parameterName);
        try {
            return (T) (isSpecified(value) ? ConstructorUtils
                    .invokeConstructor(defaultValue.getClass(), value)
                    : defaultValue);
        } catch (Exception e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error constructing Query!", e);
        }

    }

    private boolean isSpecified(String value) {
        return !(value == null || value.isEmpty());
    }

    public void setQueryManager(QueryManager queryManager) {
        this.queryManager = queryManager;
    }

    public void setQueryDefinitionDao(QueryDefinitionDao queryDefinitionDao) {
        this.queryDefinitionDao = queryDefinitionDao;
    }

}
