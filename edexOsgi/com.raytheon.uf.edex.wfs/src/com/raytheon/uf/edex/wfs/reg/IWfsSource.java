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
 * 
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 */
package com.raytheon.uf.edex.wfs.reg;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Source interface for WFS data adapters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IWfsSource {

    /**
     * @return list of all feature types provided by source
     */
    public List<WfsFeatureType> listFeatureTypes();

    /**
     * @return a list of aliases allowed for feature types for this source
     */
    public List<WfsFeatureType> getAliases();
    
    /**
     * @param feature
     *            name of feature to describe
     * @return XSD describing feature
     * @throws WfsException
     */
    public String describeFeatureType(QualifiedName feature)
            throws WfsException;

    /**
     * @param feature
     * @return field name of spatial geometry relative to root entity
     */
    public String getFeatureSpatialField(QualifiedName feature);

    /**
     * @param feature
     * @return field name of vertical height relative to root entity or null for
     *         surface
     */
    public String getFeatureVerticalField(QualifiedName feature);

    /**
     * @param feature
     * @return field name of unique ID field for feature
     */
    public String getFeatureIdField(QualifiedName feature);

    /**
     * @param feature
     * @return root entity of feature record object
     */
    public Class<?> getFeatureEntity(QualifiedName feature);

    /**
     * The XML schema returned from describe feature can be independent from the
     * layout of the feature entity class. This method returns a mapping of
     * dotted field paths in the XSD to dotted field paths in the feature
     * entity.
     * 
     * @return
     */
    public Map<String, String> getFieldMap();

    /**
     * Query for features
     * 
     * @param feature
     * @param q
     * @param options
     * @return
     * @throws WfsException
     */
    public WfsQueryResults query(QualifiedName feature, WfsQuery q,
            WfsQueryOptions options) throws WfsException;

    /**
     * Performs a distinct query for an entity field
     * 
     * @param feature
     * @param q
     * @return
     * @throws WfsException
     */
    public List<String> distinct(QualifiedName feature, WfsQuery q)
            throws WfsException;

    /**
     * @param feature
     * @param q
     * @return number of features that the query matches
     * @throws WfsException
     */
    public long count(QualifiedName feature, WfsQuery q) throws WfsException;

    /**
     * @return unique key for this source
     */
    public String getKey();

    /**
     * @return additional class for JAXB context
     */
    public Class<?>[] getJaxbClasses();

    /**
     * @param c
     *            extension class
     * @return extension object for class, null if none registered
     */
    public <T> T getExtension(Class<T> c);

}
