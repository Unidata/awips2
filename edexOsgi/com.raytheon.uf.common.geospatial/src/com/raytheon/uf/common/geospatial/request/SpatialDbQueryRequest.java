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
package com.raytheon.uf.common.geospatial.request;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Query for tables involving spatial data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class SpatialDbQueryRequest extends DbQueryRequest {

    @DynamicSerializeElement
    private SearchMode searchMode;

    @DynamicSerializeElement
    private Geometry geometry;

    @DynamicSerializeElement
    private String schema = "mapdata";

    @DynamicSerializeElement
    private String table;

    @DynamicSerializeElement
    private String geometryField = "the_geom";

    @DynamicSerializeElement
    private boolean returnGeometry = true;

    public SpatialDbQueryRequest() {
        // Default to maps db
        setDatabase("maps");
    }

    /**
     * @return the schema
     */
    public String getSchema() {
        return schema;
    }

    /**
     * @param schema
     *            the schema to set
     */
    public void setSchema(String schema) {
        this.schema = schema;
    }

    /**
     * @return the table
     */
    public String getTable() {
        return table;
    }

    /**
     * @param table
     *            the table to set
     */
    public void setTable(String table) {
        this.table = table;
    }

    /**
     * @return the geometryField
     */
    public String getGeometryField() {
        return geometryField;
    }

    /**
     * @return the returnGeometry
     */
    public boolean isReturnGeometry() {
        return returnGeometry;
    }

    /**
     * @param returnGeometry
     *            the returnGeometry to set
     */
    public void setReturnGeometry(boolean returnGeometry) {
        this.returnGeometry = returnGeometry;
    }

    /**
     * @param geometryField
     *            the geometryField to set
     */
    public void setGeometryField(String geometryField) {
        this.geometryField = geometryField;
    }

    /**
     * @return the searchMode
     */
    public SearchMode getSearchMode() {
        return searchMode;
    }

    /**
     * @param searchMode
     *            the searchMode to set
     */
    public void setSearchMode(SearchMode searchMode) {
        this.searchMode = searchMode;
    }

    /**
     * @return the geometry
     */
    public Geometry getGeometry() {
        return geometry;
    }

    /**
     * @param geometry
     *            the geometry to set
     */
    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

}
