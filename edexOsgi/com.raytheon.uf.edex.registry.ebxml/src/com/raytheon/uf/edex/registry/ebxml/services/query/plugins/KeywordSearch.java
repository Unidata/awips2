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
package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * The canonical query KeyWordSearch allows clients to find RegistryObjects and
 * RepositoryItems that contain text that matches keywords identified by
 * specified search patterns.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>keywords</i></b> -- A space separated list of keywords to search for
 * <p>
 * The value of the keywords parameter may consist of multiple terms where each
 * term is separated by one or more spaces<br>
 * 
 * Example: ebxml regrep Semantics: Matches objects containing either “ebxml” or
 * “regrep”
 * <p>
 * 
 * · A term may be enclosed in double-quotes to include white space characters
 * as a literal value.<br>
 * 
 * Example: “ebxml regrep” Semantics: Matches objects containing “ebxml regrep”
 * <p>
 * · Terms may be specified using wildcard characters where '*' matches one or
 * more characters and “?” matches a single character.<br>
 * 
 * Example: eb?ml reg*
 * <p>
 * · Terms may be combined using boolean operators “AND”, “OR” and “NOT”.
 * Absence of a boolean operator between terms implies an implicit OR operator
 * between them.<br>
 * 
 * · Example: ebxml AND regrep Semantics: Matches objects containing “ebxml” and
 * “regrep”
 * <p>
 * Example: ebxml NOT regrep Semantics: Matches objects containing “ebxml” and
 * not containg “regrep”<br>
 * 
 * Example: ebxml OR regrep Semantics: Matches objects containing “ebxml” or
 * “regrep”<br>
 * 
 * Example: ebxml regrep Semantics: Matches objects containing “ebxml” or
 * “regrep”
 * <p>
 * · Terms may be grouped together using “(“ at the beginning and “)” at the end
 * of the group. Grouping allowing boolean operators to be applied to a group of
 * terms as a whole and enables more flexible searches.<br>
 * 
 * Example: ebxml AND (registry OR regrep) Semantics: Matches objects containing
 * both “ebxml” and either “registry” or “regrep”
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class KeywordSearch extends RegistryQueryPlugin {

    /** The logger */
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KeywordSearch.class);

    /** Pattern used to tokenize keywords */
    private static final Pattern KEYWORD_TOKENIZER_PATTERN = Pattern
            .compile("\".*?\"");

    /** The default conjunction of 'OR' */
    private static final String DEFAULT_CONJUNCTION = "OR";

    /** Token used for parsing the keyword string */
    private static final String QUOTE_PLACEHOLDER = "-QUOTE-";

    /** Query used to get registry objects fitting the given set of keywords */
    private static final String REGISTRY_OBJECT_QUERY = ""
            + "SELECT obj FROM RegistryObjectType obj "
            + "INNER JOIN obj.name.localizedString as names "
            + "INNER JOIN obj.description.localizedString as descriptions "
            + "LEFT OUTER JOIN obj.slot as Slot "
            + "LEFT OUTER JOIN Slot.slotValue as SlotValues WHERE ";

    /** Query used to get person objects fitting the given set of keywords */
    private static final String PERSON_QUERY = "SELECT person FROM PersonType person "
            + "LEFT OUTER JOIN person.emailAddress as emailAddress "
            + "LEFT OUTER JOIN person.postalAddress as postalAddress WHERE ";

    /** Query used to get organizations fitting the given set of keywords */
    private static final String ORGANIZATION_QUERY = "SELECT org FROM OrganizationType org "
            + "LEFT OUTER JOIN org.emailAddress as emailAddress "
            + "LEFT OUTER JOIN org.postalAddress as postalAddress WHERE ";

    /** Map of predefined queries for the object types */
    private static final Map<String, List<String>> QUERY_MAP = new HashMap<String, List<String>>();

    static {
        QUERY_MAP.put(
                REGISTRY_OBJECT_QUERY,
                Arrays.asList(new String[] { "names.value",
                        "descriptions.value", "Slot.name",
                        "SlotValues.stringValue" }));
        QUERY_MAP.put(PERSON_QUERY, Arrays.asList(new String[] {
                "person.personName.firstName", "person.personName.middleName",
                "person.personName.lastName", "emailAddress.address",
                "postalAddress.city", "postalAddress.country",
                "postalAddress.postalCode", "postalAddress.stateOrProvince",
                "postalAddress.street" }));
        QUERY_MAP.put(ORGANIZATION_QUERY, Arrays.asList(new String[] {
                "emailAddress.address", "postalAddress.city",
                "postalAddress.country", "postalAddress.postalCode",
                "postalAddress.stateOrProvince", "postalAddress.street" }));
    }

    /** And token */
    private static final String AND = "AND";

    /** And not token */
    private static final String AND_NOT = "AND NOT";

    /** Or token */
    private static final String OR = "OR";

    /** OR not token */
    private static final String OR_NOT = "OR NOT";

    /** NOT token */
    private static final String NOT = "NOT";

    /** Open peren token */
    private static final String OPEN_PEREN = "(";

    /** Close peren token */
    private static final String CLOSE_PEREN = ")";

    /** Empty String */
    private static final String EMPTY_STRING = "";

    /** Space */
    private static final String SPACE = " ";

    /** Quotation mark */
    private static final String QUOTE = "\"";

    /** Static array of valid conjunction combinations */
    private static final List<String> CONJUNCTIONS = Arrays
            .asList(new String[] { AND, AND_NOT, OR, OR_NOT, NOT });

    /** Data access object for accessing registry objects */
    private RegistryObjectDao registryObjectDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String keywords = queryType.getSlotValue(QueryConstants.KEYWORDS);
        List<String> tokens = tokenizeKeywords(keywords);
        List<String> queryStrings = new ArrayList<String>();
        for (String prefix : QUERY_MAP.keySet()) {
            StringBuilder query = new StringBuilder();
            query.append(prefix);
            List<String> values = QUERY_MAP.get(prefix);
            for (int i = 0; i < values.size(); i++) {
                query.append(assembleQuery(values.get(i), tokens)).toString();
                if (i != values.size() - 1) {
                    query.append(OR);
                }
            }
            queryStrings.add(query.toString());
        }

        List<RegistryObjectType> results = new ArrayList<RegistryObjectType>();
        for (String query : queryStrings) {
            List<RegistryObjectType> result = registryObjectDao
                    .executeHQLQuery(query);
            results.addAll(result);

        }
        return createResponse(results);

    }

    /**
     * Assembles the query based on the given list of keyword tokens
     * 
     * @param indexName
     *            The name of the index
     * @param tokens
     *            The keyword tokens used to assmeble the query
     * @return The assembled query
     */
    private String assembleQuery(String indexName, List<String> tokens) {
        StringBuilder query = new StringBuilder();
        query.append(SPACE).append(OPEN_PEREN);

        boolean negateNext = false;
        for (String token : tokens) {
            if (isConjunction(token)) {
                if (token.contains(NOT)) {
                    negateNext = true;
                }
                query.append(SPACE).append(token.replace(NOT, EMPTY_STRING));
            } else if (isPeren(token)) {
                query.append(SPACE);
                if (negateNext) {
                    query.append(SPACE).append(NOT).append(SPACE);
                }
                query.append(token);
            } else {
                query.append(SPACE).append(indexName);
                if (negateNext) {
                    negateNext = false;
                    query.append(SPACE).append(NOT);
                }
                query.append(" LIKE '%").append(token).append("%'");
            }
        }
        query.append(CLOSE_PEREN).append(SPACE);
        return query.toString();
    }

    /**
     * Tokenizes the keywords. Handles quotes and conjunctions as per the EBXML
     * 4.0 spec
     * 
     * @param keywords
     *            The keywords to parse
     * @return The tokenized keywords
     */
    private List<String> tokenizeKeywords(String keywords) {
        Matcher matcher = KEYWORD_TOKENIZER_PATTERN.matcher(keywords);
        List<String> quotedStrings = new ArrayList<String>();
        while (matcher.find()) {
            quotedStrings.add(matcher.group());
        }
        for (String quote : quotedStrings) {
            keywords = keywords.replace(quote, QUOTE_PLACEHOLDER);
        }
        keywords = keywords.replaceAll(AND_NOT, "AND_NOT")
                .replaceAll(OR_NOT, "OR_NOT")
                .replaceAll("\\" + OPEN_PEREN, OPEN_PEREN + SPACE)
                .replaceAll("\\" + CLOSE_PEREN, SPACE + CLOSE_PEREN);

        List<String> keywordTokens = Arrays.asList(keywords.split(SPACE));
        CollectionUtil.removeAllInstancesOfObject(keywordTokens, EMPTY_STRING);

        for (int i = 0; i < keywordTokens.size(); i++) {
            String token = keywordTokens.get(i);
            if (token.equals(QUOTE_PLACEHOLDER)) {
                keywordTokens
                        .add(i,
                                quotedStrings.remove(0).replaceAll(QUOTE,
                                        EMPTY_STRING));
                keywordTokens.remove(i + 1);
            } else if (token.equals("AND_NOT")) {
                keywordTokens.add(i, AND_NOT);
                keywordTokens.remove(i + 1);
            } else if (token.equals("OR_NOT")) {
                keywordTokens.add(i, OR_NOT);
                keywordTokens.remove(i + 1);
            }
        }

        for (int i = 0; i < keywordTokens.size(); i++) {
            String currentWord = keywordTokens.get(i);
            String nextWord = null;
            try {
                nextWord = keywordTokens.get(i + 1);
            } catch (IndexOutOfBoundsException e) {
            }

            if (nextWord != null && !isPeren(currentWord)
                    && !isConjunction(currentWord) && !isClosePeren(nextWord)
                    && !isConjunction(nextWord)) {
                keywordTokens.add(i + 1, DEFAULT_CONJUNCTION);
            }
        }
        return keywordTokens;
    }

    /**
     * Checks if the given word is in the list of conjunctions
     * 
     * @param word
     *            The word to check
     * @return True if the word is a conjunction, else false
     */
    private boolean isConjunction(String word) {
        return CONJUNCTIONS.contains(word.trim().toUpperCase());
    }

    /**
     * Checks if the given word is a perenthesis
     * 
     * @param word
     *            The word to check
     * @return True if the given word is a perenthesis
     */
    private boolean isPeren(String word) {
        return isOpenPeren(word) || isClosePeren(word);
    }

    /**
     * Checks if the given word is an open peren
     * 
     * @param word
     *            The word to check
     * @return True if the given word is an open peren
     */
    private boolean isOpenPeren(String word) {
        return OPEN_PEREN.equals(word);
    }

    /**
     * Checks if the given word is a closed peren
     * 
     * @param word
     *            The word to check
     * @return True if the given word is a closed peren
     */
    private boolean isClosePeren(String word) {
        return CLOSE_PEREN.equals(word);
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }
}
