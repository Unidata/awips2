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

package com.raytheon.uf.edex.plugin.level.dao;

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelContainer;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.MasterLevelContainer;

/**
 * The DAO implementation of the Level component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * Date          Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 10, 2009             rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1
 */

public class LevelDao extends DefaultPluginDao {
    private Log logger = LogFactory.getLog(getClass());

    public LevelDao() throws PluginException {
        this("level");
    }

    public LevelDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        // no op
    }

    public void purgeAllData() throws PluginException {
        // no op
    }

    public MasterLevel lookupMasterLevel(MasterLevel level, boolean createLevel) {
        MasterLevel rval = null;

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(MasterLevel.class);

            Criterion nameCrit = Restrictions.eq("name", level.getName());
            crit.add(nameCrit);
            List<?> vals = crit.list();

            if (vals.size() > 0) {
                rval = (MasterLevel) vals.get(0);
            } else if (createLevel) {
                sess.saveOrUpdate(level);
                rval = level;
            }

            trans.commit();
        } catch (Exception e) {
            logger.error(
                    "Error occurred looking up MasterLevel[" + level.getName()
                            + "]", e);

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

    public Level lookupLevel(Level level) {
        Level rval = null;
        Session sess = null;
        Transaction trans = null;

        for (int tries = 0; tries < 2 && rval == null; tries++) {
            try {
                sess = getSessionFactory().openSession();
                trans = sess.beginTransaction();

                Criteria crit = sess.createCriteria(Level.class);

                Criterion nameCrit = Restrictions.eq("masterLevel.name", level
                        .getMasterLevel().getName());
                Criterion lvl1Crit = Restrictions.eq("levelonevalue",
                        level.getLevelonevalue());
                Criterion lvl2Crit = Restrictions.eq("leveltwovalue",
                        level.getLeveltwovalue());
                crit.add(Restrictions.and(nameCrit,
                        Restrictions.and(lvl1Crit, lvl2Crit)));
                List<?> vals = crit.list();

                if (vals.size() > 0) {
                    rval = (Level) vals.get(0);
                } else {
                    sess.saveOrUpdate(level);
                    rval = level;
                }
                trans.commit();
            } catch (Exception e) {
                rval = null;
                if (tries > 0) {
                    logger.error("Error occurred looking up level [" + level
                            + "]", e);
                }

                if (trans != null) {
                    try {
                        trans.rollback();
                    } catch (Exception e1) {
                        logger.error("Error occurred rolling back transaction",
                                e);
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

        return rval;
    }

    public Level lookupLevel(long id) {
        Level rval = null;

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(Level.class);
            crit.add(Restrictions.eq("id", id));
            List<?> vals = crit.list();

            if (vals.size() > 0) {
                rval = (Level) vals.get(0);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred looking up level id [" + id + "]", e);

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

    public LevelContainer lookupAllLevels(MasterLevel level) {
        LevelContainer rval = new LevelContainer();

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(Level.class);
            crit.add(Restrictions.eq("masterLevel.name", level.getName()));
            List<?> vals = crit.list();

            for (Object val : vals) {
                rval.add((Level) val);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error(
                    "Error occurred looking up all levels for master level ["
                            + level + "]", e);

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

    public LevelContainer lookupAllLevels() {
        LevelContainer rval = new LevelContainer(450);

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(Level.class);
            List<?> vals = crit.list();

            for (Object val : vals) {
                rval.add((Level) val);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred looking up all levels", e);

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

    public MasterLevelContainer lookupAllMasterLevels() {
        MasterLevelContainer rval = new MasterLevelContainer(1 - 0);

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(MasterLevel.class);
            List<?> vals = crit.list();

            for (Object val : vals) {
                rval.add((MasterLevel) val);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred looking up all levels", e);

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
}
