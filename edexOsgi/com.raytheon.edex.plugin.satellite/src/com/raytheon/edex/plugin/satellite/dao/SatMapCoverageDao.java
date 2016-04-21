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

package com.raytheon.edex.plugin.satellite.dao;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;
import org.hibernate.exception.ConstraintViolationException;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * The dao implementation associated with the SatelliteMapCoverage class used
 * for all database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jul 24, 2007  353      bphillip  Initial Check in    
 * Jun 27, 2012  798      jkorman   Corrected id query type.
 * Oct 02, 2013  2333     mschenke  Removed unused code
 * Nov 05, 2014  3788     bsteffen  add getOrCreateCoverage
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SatMapCoverageDao extends CoreDao {

    /**
     * Creates a new SatelliteMapCoverageDao
     */
    public SatMapCoverageDao() {
        super(DaoConfig.forClass(SatMapCoverage.class));
    }

    /**
     * Convenience method to retrieve a SatelliteMapCoverage from the database
     * given a map ID
     * 
     * @param mapId
     *            The Map ID
     * @return A SatelliteMapCoverage object with the corresponding ID. Null if
     *         not found.
     */
    public SatMapCoverage queryByMapId(Integer mapId) {
        return (SatMapCoverage) this.queryById(mapId);
    }

    /**
     * Convenience method to retrieve a SatelliteMapCoverage from the database
     * given a map ID
     * 
     * @param mapId
     *            The Map ID
     * @return A SatelliteMapCoverage object with the corresponding ID. Null if
     *         not found.
     */
    public SatMapCoverage getOrCreateCoverage(SatMapCoverage coverage) {
        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();
            SatMapCoverage result = query(coverage, sess);
            if (result != null) {
                return result;
            } else {
                try {
                    sess.save(coverage);
                    trans.commit();
                    return coverage;
                } catch (ConstraintViolationException e) {
                    trans.rollback();
                    trans = sess.beginTransaction();
                    /*
                     * To support multithreading/clustering its possible it
                     * could have been created elsewhere between the query and
                     * the save, so requery and throw an exception if it is not
                     * found.
                     */
                    result = query(coverage, sess);
                    trans.commit();
                    if (result == null) {
                        logger.error(
                                "Unable to create entry in satellite_coverage table.",
                                e);
                        return coverage;
                    } else {
                        return result;
                    }
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing session", e);
                }
            }
        }
    }

    private SatMapCoverage query(SatMapCoverage coverage, Session sess) {
        Criteria crit = sess.createCriteria(coverage.getClass());

        crit.add(Restrictions.eq("nx", coverage.getNx()));
        crit.add(Restrictions.eq("ny", coverage.getNy()));
        crit.add(Restrictions.eq("dx", coverage.getDx()));
        crit.add(Restrictions.eq("dy", coverage.getDy()));
        crit.add(Restrictions.eq("minX", coverage.getMinX()));
        crit.add(Restrictions.eq("minY", coverage.getMinY()));
        crit.add(Restrictions.eq("crsWKT", coverage.getCrsWKT()));
        return (SatMapCoverage) crit.uniqueResult();
    }

    /**
     * Retrieves all satellite map coverage windows for a given type. <br>
     * These types include IR, Visible, and Water Vapor
     * 
     * @param type
     *            The type of satellite image
     * @return A list of satellite map coverage areas for the given type
     */
    @SuppressWarnings("unchecked")
    public List<SatMapCoverage> queryByType(String type) throws DataAccessLayerException{
        return (List<SatMapCoverage>) queryBySingleCriteria("type", type);
    }

    /**
     * Retrieves all satellite map coverage windows with the given dimensions
     * 
     * @param nx
     *            The horizontal dimension in number of points
     * @param ny
     *            The vertical dimension in number of points
     * @return A list of satellite maps matching the given dimensions
     */
    @SuppressWarnings("unchecked")
    public List<SatMapCoverage> queryByDimension(Integer nx, Integer ny) throws DataAccessLayerException{
        List<String> fields = new ArrayList<String>();
        List<Object> values = new ArrayList<Object>();

        fields.add("nx");
        values.add(String.valueOf(nx));
        fields.add("ny");
        values.add(String.valueOf(ny));

        return (List<SatMapCoverage>) queryByCriteria(fields, values);
    }

}
