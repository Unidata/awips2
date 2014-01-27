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
package com.raytheon.uf.edex.wfs.request;

import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import net.opengis.filter.v_2_0_0.FilterType;
import net.opengis.wfs.v_2_0_0.QueryType;

import com.raytheon.uf.edex.ogc.common.OgcTimeRange;
import com.raytheon.uf.edex.wfs.request.SortBy.Order;

/**
 * Wrapper object for WFS feature query information from a get feature request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureQuery {

	public enum QFilterType {
		XML, BBOX, FIDS, XMLOBJ, NONE
	}

	protected String srsName;

	protected List<QualifiedName> typeNames = new LinkedList<QualifiedName>();

	protected QFilterType filterType = QFilterType.NONE;

	protected Object filter;

	protected List<SortBy> sortBys = new LinkedList<SortBy>();

	protected List<String> propertyNames = new LinkedList<String>();
	
	protected OgcTimeRange timeRange;

    /**
     * 
     */
    public FeatureQuery() {
    }

    /**
     * @param qt
     */
    public FeatureQuery(QueryType qt) {
        if (qt.isSetAbstractSelectionClause()) {
            JAXBElement<?> elem = qt.getAbstractSelectionClause();
            FilterType filter = (FilterType) elem.getValue();
            this.setFilter(filter, QFilterType.XMLOBJ);
        }
        if (qt.isSetAbstractSortingClause()) {
            net.opengis.filter.v_2_0_0.SortByType sortBy = (net.opengis.filter.v_2_0_0.SortByType) qt
                    .getAbstractSortingClause().getValue();
            for (net.opengis.filter.v_2_0_0.SortPropertyType prop : sortBy
                    .getSortProperty()) {
                String name = prop.getValueReference();
                Order o = (prop.getSortOrder() == net.opengis.filter.v_2_0_0.SortOrderType.DESC ? Order.Descending
                        : Order.Ascending);
                this.addSortBy(new SortBy(name, o));
            }
        }
        String srsName = qt.getSrsName();
        if (srsName != null) {
            this.setSrsName(srsName);
        }
        if (qt.isSetTypeNames()) {
            for (QName q : qt.getTypeNames()) {
                this.addTypeName(new QualifiedName(q));
            }
        }
    }

	public void addPropertyName(String propertyName) {
		this.propertyNames.add(propertyName);
	}

	public void addTypeName(QualifiedName typeName) {
		this.typeNames.add(typeName);
	}

	public void addSortBy(SortBy sortBy) {
		this.sortBys.add(sortBy);
	}

	/**
	 * @return the propertyNames
	 */
	public List<String> getPropertyNames() {
		return propertyNames;
	}

	/**
	 * @param propertyNames
	 *            the propertyNames to set
	 */
	public void setPropertyNames(List<String> propertyNames) {
		this.propertyNames = propertyNames;
	}

	/**
	 * @return the srsName
	 */
	public String getSrsName() {
		return srsName;
	}

	/**
	 * @param srsName
	 *            the srsName to set
	 */
	public void setSrsName(String srsName) {
		this.srsName = srsName;
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

	/**
	 * @return the filter
	 */
	public Object getFilter() {
		return filter;
	}

	/**
	 * @param filter
	 *            the filter to set
	 */
	public void setFilter(Object filter, QFilterType type) {
		this.filter = filter;
		this.filterType = type;
	}

	/**
	 * @return the filterType
	 */
	public QFilterType getFilterType() {
		return filterType;
	}

	/**
	 * @return the sortBys
	 */
	public List<SortBy> getSortBys() {
		return sortBys;
	}

	/**
	 * @param sortBys
	 *            the sortBys to set
	 */
	public void setSortBys(List<SortBy> sortBys) {
		this.sortBys = sortBys;
	}

    public OgcTimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(OgcTimeRange timeRange) {
        this.timeRange = timeRange;
    }
}
