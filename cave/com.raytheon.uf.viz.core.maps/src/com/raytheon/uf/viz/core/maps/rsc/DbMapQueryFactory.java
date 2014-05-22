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
package com.raytheon.uf.viz.core.maps.rsc;

import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Factory class to retrieve appropriate DbMapQuery implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2011            bsteffen     Initial creation
 * Apr 9, 2014   #2997    randerso     Added queryWithinGeometry
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DbMapQueryFactory {

    public interface DbMapQuery {
        public QueryResult queryWithinGeometry(Geometry geom,
                List<String> columns, List<String> additionalConstraints)
                throws VizException;

        public List<String> getColumnNamesWithoutGeometries()
                throws VizException;

        public String getGeometryType() throws VizException;

        public List<Double> getLevels() throws VizException;

    }

    private static DbMapQueryFactory instance;

    public static DbMapQuery getMapQuery(String table, String geomField) {
        return getInstance().getMapQueryInternal(table, geomField);
    }

    protected static DbMapQueryFactory getInstance() {
        if (instance == null) {
            instance = new DbMapQueryFactory();
        }
        return instance;
    }

    protected static void setCustomInstance(DbMapQueryFactory factory) {
        instance = factory;
    }

    protected DbMapQueryFactory() {

    }

    protected DbMapQuery getMapQueryInternal(String table, String geomField) {
        return new DefaultDbMapQuery(table, geomField);
    }

}
