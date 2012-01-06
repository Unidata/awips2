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
package com.raytheon.uf.common.geospatial;

import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * ISpatialQuery
 * 
 * Defines an interface for performing client-side spatial queries
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 7, 2007              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface ISpatialQuery {

    public static enum SearchMode {
        CONTAINS, INTERSECTS, WITHIN, CLOSEST
    };

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param exclusive
     *            whether the filter is used as an exclusive or inclusive
     *            feature
     * @param mode
     *            the search mode used for the geospatial component
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, String> filter, boolean exclusive,
            SearchMode mode) throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param theGeomField
     *            the geometry field to use. Allows access to low res data if
     *            requested. Default is the_geom.
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param exclusive
     *            whether the filter is used as an exclusive or inclusive
     *            feature
     * @param mode
     *            the search mode used for the geospatial component
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry geometry, Map<String, String> filter,
            boolean exclusive, SearchMode mode) throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param theGeomField
     *            the geometry field to use. Allows access to low res data if
     *            requested. Default is the_geom.
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param exclusive
     *            whether the filter is used as an exclusive or inclusive
     *            feature
     * @param mode
     *            the search mode used for the geospatial component
     * @param limit
     *            the max number of results to return
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, String> filter, boolean exclusive,
            SearchMode mode, Integer limit) throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param theGeomField
     *            the geometry field to use. Allows access to low res data if
     *            requested. Default is the_geom.
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param exclusive
     *            whether the filter is used as an exclusive or inclusive
     *            feature
     * @param mode
     *            the search mode used for the geospatial component
     * @param limit
     *            the max number of results to return
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry geometry, Map<String, String> filter,
            boolean exclusive, SearchMode mode, Integer limit)
            throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param mode
     *            the search mode used for the geospatial component
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, RequestConstraint> filter,
            SearchMode mode) throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param theGeomField
     *            the geometry field to use. Allows access to low res data if
     *            requested. Default is the_geom.
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param mode
     *            the search mode used for the geospatial component
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry geometry,
            Map<String, RequestConstraint> filter, SearchMode mode)
            throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param mode
     *            the search mode used for the geospatial component
     * @param mode
     *            the max number of results to return
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, RequestConstraint> filter,
            SearchMode mode, Integer limit) throws SpatialException;

    /**
     * Perform a GIS style query
     * 
     * @param dataSet
     *            the dataset to search
     * @param theGeomField
     *            the geometry field to use. Allows access to low res data if
     *            requested. Default is the_geom.
     * @param attributes
     *            list of attributes to be returned. "the_geom" is always
     *            returned and need not be listed. This list may be null or
     *            empty if only the geometry is desired.
     * @param geometry
     *            the geometry to use to narrow the gis search
     * @param filter
     *            the non-geospatial elements to use in narrowing
     * @param mode
     *            the search mode used for the geospatial component
     * @param mode
     *            the max number of results to return
     * @return a set of features
     * @throws VizException
     */
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry geometry,
            Map<String, RequestConstraint> filter, SearchMode mode,
            Integer limit) throws SpatialException;

    /**
     * A direct Db call
     * 
     * @param sql
     * @return
     * @throws SpatialException
     */
    public Object[] dbRequest(String sql, String dbname)
            throws SpatialException;
}