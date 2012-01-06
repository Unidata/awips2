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

package com.raytheon.edex.uengine.tasks.query;

import java.util.List;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.uengine.tasks.catalog.CatalogSearcher;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Catalog task derived from original uEngine Catalog task. Provides a catalog
 * inventory of the index.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 4, 2007                      njensen             Initial Creation
 * </PRE>
 * 
 */
public class Catalog extends ScriptTask {
	private String queryType;
	
	private DatabaseQuery query;

	/**
	 * Constructor
	 * 
	 * @param aQueryType
	 *            the query type, either CatalogSearcher.TYPE_DISTINCT_VALUE or
	 *            CatalogSearcher.TYPE_FULL_DOCUMENT
	 */
	public Catalog(String aQueryType) {
	    query = new DatabaseQuery((String)null);
		queryType = aQueryType;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {
		ResponseMessageCatalog response = null;
		if (queryType == null
				|| !(queryType.equals(CatalogSearcher.TYPE_DISTINCT_VALUE) || (queryType
						.equals(CatalogSearcher.TYPE_FULL_DOCUMENT)))) {
			throw new MicroEngineException("Invalid query type: " + queryType);
		}

		try {
		    response = CatalogSearcher.search(query);
		} catch (EdexException e1) {
			throw new MicroEngineException(e1);
		}

		return response;

	}

	public String getQueryType() {
		return queryType;
	}

	public void setQueryType(String aQueryType) {
		queryType = aQueryType;
	}

	public void addQueryField(String aQueryField) {
	    query.addDistinctParameter(aQueryField);
	}
	
	public void addReturnedField(String aQueryField){
	    query.addReturnedField(aQueryField);
	}

	/**
	 * Adds a constraint to the catalog request.
	 * 
	 * @param aName
	 *            the name of the constraint
	 * @param aValue
	 *            the value of the constraint
	 */
	public void addConstraint(String field, String value) {
	    query.addQueryParam(field,value);
	}
	
	public void addConstraint(String field, String value, String operand){
	    query.addQueryParam(field,value,QueryParam.translateOperand(operand));
	}

	public List<String> getQueryField() {
	    return null;
		//return query.getDistinctParameters();
	}
	
	public List<String> getReturnedFields(){
	    return query.getReturnedFieldNames();
	}

}
