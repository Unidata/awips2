package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;

public abstract class RegistryQueryPlugin implements QueryManager {

    protected String queryDefinition;

    /**
     * Data access object for classification nodes;
     */
    protected ClassificationNodeDao classificationNodeDao;

    /**
     * Gets the id of the classification node designated by the provided path
     * 
     * @param classificationNodePath
     *            The classification node path
     * @return The id of the classification node designated by the provided path
     */
    protected String getClassificationNodeIdFromPath(
            String classificationNodePath) {
        if (classificationNodePath == null) {
            return null;
        }
        ClassificationNodeType node = classificationNodeDao
                .getByPath(classificationNodePath);
        if (node == null) {
            return null;
        }
        return node.getId();
    }

    /**
     * Gets the ids of the classification nodes designated by the provided set
     * of paths
     * 
     * @param classificationNodePaths
     *            The classification node paths
     * @return The ids of the classification nodes designated by the provided
     *         set of paths
     */
    protected List<String> getClassificationNodeIdFromPath(
            List<String> classificationNodePaths) {
        if (CollectionUtil.isNullOrEmpty(classificationNodePaths)) {
            return null;
        }
        List<String> ids = new ArrayList<String>();
        for (String path : classificationNodePaths) {
            ids.add(getClassificationNodeIdFromPath(path));
        }

        return ids;
    }

    /**
     * Gets the conjunction based on the value of the matchOnAny parameter
     * commonly given to many queries
     * 
     * @param matchOnAny
     *            The value of the match on any parameter
     * @return "OR" if matchOnAny is true, else "AND"
     */
    protected String getConjunction(Boolean matchOnAny) {
        return matchOnAny == null || !matchOnAny.booleanValue() ? "AND" : "OR";
    }

    /**
     * Assembles the given list of query clauses into a query predicate
     * 
     * @param query
     *            The query to append to
     * @param conjunction
     *            The conjunction to use for joining the clauses
     * @param clauses
     *            The clauses to assemble
     */
    protected void assembleClauses(StringBuilder query, String conjunction,
            List<String> clauses) {
        // Strip all the nulls out of the list
        CollectionUtil.removeNulls(clauses);

        // Assemble the query
        for (int i = 0; i < clauses.size(); i++) {
            query.append(clauses.get(i));
            if (i != clauses.size() - 1) {
                query.append(conjunction);
            }
        }
    }

    protected <T extends Object> QueryResponse createResponse(
            Collection<T> responseObjects) {
        QueryResponse response = new QueryResponse();
        for (Object obj : responseObjects) {
            response.getRegistryObjects().add((RegistryObjectType) obj);
        }
        return response;
    }

    public String getQueryDefinition() {
        return queryDefinition;
    }

    public void setQueryDefinition(String queryDefinition) {
        this.queryDefinition = queryDefinition;
    }

    public void setClassificationNodeDao(
            ClassificationNodeDao classificationNodeDao) {
        this.classificationNodeDao = classificationNodeDao;
    }

}
