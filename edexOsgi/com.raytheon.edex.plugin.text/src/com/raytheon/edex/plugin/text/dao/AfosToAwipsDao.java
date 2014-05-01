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

package com.raytheon.edex.plugin.text.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwipsId;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * The DAO implementation of the AfosToAwips component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * Date          Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 28, 2009 2924         rjpeter    Initial creation
 * Nov 16, 2009 3336         njensen  Added lookupAfosId(String, String, String)
 * 02apr2013    15564   mgamazaychikov Ensured afosid to be 9 characters space-padded long
 * 
 * </pre>
 * 
 * @author garmendariz
 * @version 1
 */

public class AfosToAwipsDao extends CoreDao {
    public final static int WMOTTAAII_LENGTH = 6;

    public final static int WMOCCCC_LENGTH = 4;

    public final static int AFOSID_LENGTH = 9;

    private final static int MAX_FIELD_LENGTH = 3;

    private Log logger = LogFactory.getLog(getClass());

    public AfosToAwipsDao() {
        super(DaoConfig.forClass("fxa", AfosToAwips.class));
    }

    /**
     * @param ttaaii
     *            the WMO id to look up
     * @param cccc
     *            the 4-char site id
     * @return A container with the AfosToAwips objects found.
     * @throws DataAccessLayerException
     */
    public AfosWmoIdDataContainer lookupAfosId(String ttaaii, String cccc)
            throws DataAccessLayerException {
        Session sess = null;
        AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();

        /**
         * Method inner class for use with Session.doWork(). Performs the query
         * with a connection provided by the session.
         * 
         * @author wldougher
         * 
         */
        class LookupAfosIdWork implements Work {
            private String ttaaii;

            private String cccc;

            private AfosWmoIdDataContainer rval;

            /**
             * Constructor.
             * 
             * @param ttaaii
             *            the WMO id
             * @param cccc
             *            the 4-char site id
             */
            private LookupAfosIdWork(String ttaaii, String cccc) {
                if (ttaaii != null) {
                    ttaaii = ttaaii.toUpperCase();
                }
                this.ttaaii = ttaaii;

                if (cccc != null) {
                    cccc = cccc.toUpperCase();
                }
                this.cccc = cccc;
            }

            /**
             * @see org.hibernate.jdbc.Work#execute(java.sql.Connection)
             */
            public void execute(Connection c) throws SQLException {
                PreparedStatement ps = null;
                rval = new AfosWmoIdDataContainer();
                try {
                    ps = c.prepareStatement("SELECT afosid FROM afos_to_awips WHERE wmottaaii = ? AND wmocccc = ?");

                    ps.setString(1, ttaaii);
                    ps.setString(2, cccc);
                    ResultSet rs = ps.executeQuery();

                    while (rs.next()) {
                        AfosToAwips id = new AfosToAwips();
                        // make sure that the afosid is 9 characters space-padded long 
						String afosid = String.format("%-9s", rs.getString(1).trim());
                        id.setAfosid(afosid);
                        id.setWmottaaii(ttaaii);
                        id.setWmocccc(cccc);
                        rval.add(id);
                    }

                } finally {
                    if (ps != null) {
                        ps.close();
                    }
                }
            }
        }

        LookupAfosIdWork lookupAfosIdWork = new LookupAfosIdWork(ttaaii, cccc);

        try {
            sess = getSessionFactory().openSession();
            sess.doWork(lookupAfosIdWork);
            rval = lookupAfosIdWork.rval;
        } catch (HibernateException e) {
            logger.error("Error occurred looking up AFOS ID", e);
            rval.setErrorMessage(e.getMessage());
        } finally {
            if (sess != null) {
                sess.close();
            }
        }

        return rval;
    }

    /**
     * @param afosid
     * @return A container with the AfosToAwips objects found.
     * @throws DataAccessLayerException
     */
    public AfosWmoIdDataContainer lookupWmoId(String afosid)
            throws DataAccessLayerException {
        Session sess = null;
        AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();

        /**
         * Method inner class for use with Session.doWork(). Performs the query
         * with a connection provided by the session.
         * 
         * @author wldougher
         * 
         */
        class LookupWmoIdWork implements Work {
            private String afosid;

            private AfosWmoIdDataContainer rval;

            /**
             * @param afosid
             */
            private LookupWmoIdWork(String afosid) {
                if (afosid != null) {
                    afosid = StringUtils.rightPad(afosid.toUpperCase(),
                            AFOSID_LENGTH);
                }
                this.afosid = afosid;
            }

            /**
             * @see org.hibernate.jdbc.Work#execute(java.sql.Connection)
             */
            public void execute(Connection c) throws SQLException {
                PreparedStatement ps = null;
                rval = new AfosWmoIdDataContainer();

                try {

                    ps = c.prepareStatement("SELECT wmottaaii, wmocccc FROM afos_to_awips WHERE afosid = ?");
                    ps.setString(1, afosid);
                    ResultSet rs = ps.executeQuery();

                    while (rs.next()) {
                        AfosToAwips id = new AfosToAwips();
                        id.setAfosid(afosid);
                        id.setWmottaaii(rs.getString(1));
                        id.setWmocccc(rs.getString(2));
                        rval.add(id);
                    }
                } finally {
                    if (ps != null) {
                        ps.close();
                    }
                }
            }
        }

        LookupWmoIdWork lookupWmoIdWork = new LookupWmoIdWork(afosid);

        try {
            sess = getSessionFactory().openSession();
            sess.doWork(lookupWmoIdWork);
            rval = lookupWmoIdWork.rval;
        } catch (HibernateException e) {
            logger.error("Error occurred looking up WMO ID", e);
            rval.setErrorMessage(e.getMessage());
        } finally {
            if (sess != null) {
                sess.close();
            }
        }

        return rval;
    }

    public AfosWmoIdDataContainer lookupAfosId(String cccc, String nnn,
            String xxx) {
        Session sess = null;
        AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();

        /**
         * Method inner class for use with Session.doWork(). Performs the query
         * with a connection provided by the session.
         * 
         * @author wldougher
         * 
         */
        class LookupWork implements Work {
            private String cccc;

            private String nnn;

            private String xxx;

            private AfosWmoIdDataContainer rval;

            /**
             * Constructor.
             * 
             * @param cccc
             *            the site id
             * @param nnn
             *            first part of the awips id
             * @param xxx
             *            other part of the awips id
             */
            private LookupWork(String cccc, String nnn, String xxx) {
                if (cccc != null) {
                    cccc = cccc.toUpperCase();
                }
                LookupWork.this.cccc = cccc;

                if (nnn != null) {
                    nnn = StringUtils.rightPad(nnn.toUpperCase(),
                            MAX_FIELD_LENGTH);
                }
                LookupWork.this.nnn = nnn;

                if (xxx != null) {
                    xxx = StringUtils.rightPad(xxx.toUpperCase(),
                            MAX_FIELD_LENGTH);
                }
                LookupWork.this.xxx = xxx;
            }

            /*
             * (non-Javadoc)
             * 
             * @see org.hibernate.jdbc.Work#execute(java.sql.Connection)
             */
            public void execute(Connection c) throws SQLException {
                PreparedStatement ps = null;
                String awipsId = "%" + nnn + xxx;
                rval = new AfosWmoIdDataContainer();

                try {

                    if (cccc == null) {
                        ps = c.prepareStatement("SELECT afosid, wmottaaii, wmocccc FROM afos_to_awips WHERE afosid like ?");
                        ps.setString(1, awipsId);
                    } else if (nnn == null && xxx == null) {
                        ps = c.prepareStatement("SELECT afosid, wmottaaii FROM afos_to_awips WHERE wmocccc = ?");
                        ps.setString(1, cccc);
                    } else {
                        ps = c.prepareStatement("SELECT afosid, wmottaaii FROM afos_to_awips WHERE wmocccc = ? and afosid like ?");
                        ps.setString(1, cccc);
                        ps.setString(2, awipsId);
                    }

                    ResultSet rs = ps.executeQuery();
                    while (rs.next()) {
                        AfosToAwips id = new AfosToAwips();
                        id.setAfosid(rs.getString(1));
                        id.setWmottaaii(rs.getString(2));

                        if (cccc == null) {
                            id.setWmocccc(rs.getString(3));
                        } else {
                            id.setWmocccc(cccc);
                        }

                        rval.add(id);
                    }
                } finally {
                    if (ps != null) {
                        ps.close();
                    }
                }
            }
        }

        LookupWork lookupWork = new LookupWork(cccc, nnn, xxx);

        try {
            sess = getSessionFactory().openSession();
            sess.doWork(lookupWork);
            rval = lookupWork.rval;

        } catch (HibernateException e) {
            logger.error("Error occurred looking up AFOS ID", e);
            rval.setErrorMessage(e.getMessage());
        } finally {
            if (sess != null) {
                sess.close();
            }
        }

        return rval;
    }

    public AfosWmoIdDataContainer getAllRecords() {
        AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(AfosToAwips.class);
            List<?> vals = crit.list();

            for (Object val : vals) {
                rval.add((AfosToAwips) val);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred looking up all AFOS WMOID records.", e);

            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e);
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
        return rval;
    }

    public boolean addRecord(AfosToAwipsId record) {
        boolean success = true;
        AfosToAwips storeRecord = new AfosToAwips(record);
        try {
            persist(storeRecord);
        } catch (Exception e) {
            logger.error("Error occurred creating AfosToAwipsId record.", e);
            success = false;
        }
        return success;
    }

    public boolean removeRecord(AfosToAwipsId record) {
        boolean success = true;
        Session sess = null;
        Transaction trans = null;
        AfosToAwips storeRecord = new AfosToAwips(record);
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();
            sess.delete(storeRecord);
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred deleting AfosToAwipsId record.", e);
            success = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e);
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
        return success;
    }

}
