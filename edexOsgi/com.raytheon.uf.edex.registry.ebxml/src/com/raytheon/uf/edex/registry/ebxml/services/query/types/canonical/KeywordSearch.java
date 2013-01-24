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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.cataloger.IndexEntry;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class KeywordSearch extends CanonicalEbxmlQuery {

    /** The logger */
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KeywordSearch.class);

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "KeywordSearch";

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.KEYWORDS);
    }

    @Override
    protected <T extends RegistryObjectType> List<T> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao();
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        // The client did not specify the required parameter
        if (parameters.isEmpty()
                || !parameters.containsParameter(QueryConstants.KEYWORDS)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        String keywords = parameters.getFirstParameter(QueryConstants.KEYWORDS);
        String queryString = generateQuery(keywords);
        List<IndexEntry> entries = new RegistryDao(IndexEntry.class)
                .executeHQLQuery(queryString);
        List<String> ids = new ArrayList<String>();
        for (IndexEntry entry : entries) {
            ids.add(entry.getParentId());
        }
        if (!ids.isEmpty()) {
            return registryObjectDao.getById(ids);
        } else {
            return Collections.emptyList();
        }
    }

    /**
     * Generates the HQL query from the provided set of keywords.
     * 
     * @param keywords
     *            The keywords provided by the client
     * @return The corresponding HQL query derived from the set of keywords
     *         provided
     */
    private String generateQuery(String keywords) {
        keywords = keywords.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ")
                .replaceAll("\\*", "%");
        List<String> tokenArray = splitString(keywords);
        List<String> tokens = new ArrayList<String>();
        for (String str : tokenArray) {
            if (!str.trim().isEmpty()) {
                tokens.add(str.trim());
            }
        }

        boolean modified = true;
        String previous = null;
        String current = null;
        String next = null;
        while (modified) {
            modified = false;
            for (int i = 0; i < tokens.size(); i++) {
                if (i == 0) {
                    previous = null;
                } else {
                    previous = tokens.get(i - 1);
                }
                current = tokens.get(i);
                if (i == tokens.size() - 1) {
                    continue;
                } else {
                    next = tokens.get(i + 1);
                }
                if (isTerm(current)) {
                    if (isOpenParen(next) || isTerm(next)) {
                        tokens.add(i + 1, "OR");
                        modified = true;
                        break;
                    }
                } else if (isClosingParen(current)) {
                    if (isTerm(next)) {
                        tokens.add(i + 1, "OR");
                        modified = true;
                        break;
                    }
                } else if (current.equalsIgnoreCase("NOT") && previous != null) {
                    if (!isJunction(previous)) {
                        tokens.add(i, "AND");
                        modified = true;
                        break;
                    }
                }
            }
        }

        StringBuilder hql = new StringBuilder();
        hql.append("from ").append(IndexEntry.class.getName())
                .append(" x where ");
        for (String token : tokens) {
            if (isJunction(token) || isParen(token)) {
                hql.append(" ").append(token).append(" ");
            } else {
                hql.append(" x.value like '%").append(token).append("%'");
            }
        }
        return hql.toString();
    }

    /**
     * Splits the string on spaces. Quoted substrings are not separated, but
     * instead are treated as separate tokens
     * 
     * @param source
     *            The string to split
     * @return The string broken apart by spaces while maintaining quoted items
     */
    private List<String> splitString(String source) {
        List<String> tokens = new ArrayList<String>();
        char delimiter = ' ';
        char quote = '"';
        boolean quoted = false;
        String currentWord = "";
        char currentChar = 0;
        for (int i = 0; i < source.length(); i++) {
            currentChar = source.charAt(i);
            if (currentChar == quote) {
                if (quoted) {
                    quoted = false;
                    tokens.add(currentWord);
                    currentWord = "";
                } else {
                    quoted = true;
                }
            } else if (quoted) {
                currentWord += currentChar;
            } else if (currentChar == delimiter) {
                if (!currentWord.isEmpty()) {
                    tokens.add(currentWord);
                }
                currentWord = "";
            } else {
                currentWord += currentChar;
            }
        }
        if (!currentWord.isEmpty()) {
            tokens.add(currentWord);
        }
        return tokens;
    }

    /**
     * Checks if the provided string is a closing parenthesis
     * 
     * @param str
     *            The string to check
     * @return True if this string is a closing parenthesis
     */
    private boolean isClosingParen(String str) {
        return str != null && str.equals(")");
    }

    /**
     * Checks if the provided string is a open parenthesis
     * 
     * @param str
     *            The string to check
     * @return True if this string is a open parenthesis
     */
    private boolean isOpenParen(String str) {
        return str != null && str.equals("(");
    }

    /**
     * Checks if the provided string is a parenthesis
     * 
     * @param str
     *            The string to check
     * @return True if this string is a parenthesis
     */
    private boolean isParen(String str) {
        return isClosingParen(str) || isOpenParen(str);
    }

    /**
     * Checks if the provided string is a conjunction, i.e. AND, OR, NOT
     * 
     * @param str
     *            The string to check
     * @return True if this string is a conjunction
     */
    private boolean isJunction(String str) {
        return str != null
                && (str.equalsIgnoreCase("and") || str.equalsIgnoreCase("or") || str
                        .equalsIgnoreCase("not"));
    }

    /**
     * The string is considered a 'term' if it is not a perenthesis and it is
     * not a conjunction
     * 
     * @param str
     *            The string to check
     * @return True if this string is a 'term'
     */
    private boolean isTerm(String str) {
        return !isParen(str) && !isJunction(str);
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }
}
