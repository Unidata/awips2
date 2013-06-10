package com.raytheon.uf.edex.wfs.reg;

import java.util.LinkedList;
import java.util.List;

import org.hibernate.criterion.Criterion;

import com.raytheon.uf.edex.ogc.common.OgcTimeRange;
import com.raytheon.uf.edex.wfs.request.SortBy;

public class WfsQuery {

	protected Criterion criterion;

	protected int maxResults;

	protected List<SortBy> sortBys = new LinkedList<SortBy>();

	protected List<String> propertyNames = new LinkedList<String>();
	
	protected OgcTimeRange timeRange;

	public WfsQuery() {
		// TODO Auto-generated constructor stub
	}

	public WfsQuery(Criterion criterion, int maxResults, List<SortBy> sortBys,
			List<String> propertyNames, OgcTimeRange timeRange) {
		this.criterion = criterion;
		this.maxResults = maxResults;
		this.sortBys = sortBys;
		this.propertyNames = propertyNames;
		this.timeRange = timeRange;
	}

	public Criterion getCriterion() {
		return criterion;
	}

	public void setCriterion(Criterion criterion) {
		this.criterion = criterion;
	}

	public int getMaxResults() {
		return maxResults;
	}

	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

	public List<SortBy> getSortBys() {
		return sortBys;
	}

	public void setSortBys(List<SortBy> sortBys) {
		this.sortBys = sortBys;
	}

	public List<String> getPropertyNames() {
		return propertyNames;
	}

	public void setPropertyNames(List<String> propertyNames) {
		this.propertyNames = propertyNames;
	}

}
