/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.reg;

import java.util.LinkedList;
import java.util.List;

import org.hibernate.criterion.Criterion;

import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.ogc.common.OgcTimeRange;
import com.raytheon.uf.edex.wfs.request.SortBy;

/**
 * Holds query information for WFS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsQuery {

	protected Criterion criterion;

    protected List<QualifiedName> typeNames = new LinkedList<QualifiedName>();

	protected int maxResults;

	protected List<SortBy> sortBys = new LinkedList<SortBy>();

	protected List<String> propertyNames = new LinkedList<String>();
	
	protected OgcTimeRange timeRange;

    /**
	 * 
	 */
	public WfsQuery() {
	}

    /**
     * @param criterion
     * @param maxResults
     * @param sortBys
     * @param propertyNames
     */
	public WfsQuery(Criterion criterion, int maxResults, List<SortBy> sortBys,
			List<String> propertyNames, OgcTimeRange timeRange) {
		this.criterion = criterion;
		this.maxResults = maxResults;
		this.sortBys = sortBys;
		this.propertyNames = propertyNames;
		this.timeRange = timeRange;
	}

    /**
     * @return
     */
	public Criterion getCriterion() {
		return criterion;
	}

    /**
     * @param criterion
     */
	public void setCriterion(Criterion criterion) {
		this.criterion = criterion;
	}

    /**
     * @return
     */
	public int getMaxResults() {
		return maxResults;
	}

    /**
     * @param maxResults
     */
	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

    /**
     * @return
     */
	public List<SortBy> getSortBys() {
		return sortBys;
	}

    /**
     * @param sortBys
     */
	public void setSortBys(List<SortBy> sortBys) {
		this.sortBys = sortBys;
	}

    /**
     * @return
     */
	public List<String> getPropertyNames() {
		return propertyNames;
	}

    /**
     * @param propertyNames
     */
	public void setPropertyNames(List<String> propertyNames) {
		this.propertyNames = propertyNames;
	}

    /**
     * @return the typeNames
     */
    public List<QualifiedName> getTypeNames() {
        return typeNames;
    }

    /**
     * @param typeNames
     *            the typeNames to set
     */
    public void setTypeNames(List<QualifiedName> typeNames) {
        this.typeNames = typeNames;
    }

}
