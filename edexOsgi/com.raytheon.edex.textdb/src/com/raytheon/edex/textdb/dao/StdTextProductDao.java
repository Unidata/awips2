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

package com.raytheon.edex.textdb.dao;

import java.math.BigInteger;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProductId;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfoPK;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;

/**
 * The dao implementation associated with the TextDao classes used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/04/07     400         garmendariz Initial Check in
 * Oct 1, 2008        1538  jkorman     Added additional functionality.
 * Aug 18, 2009 2191        rjpeter     Refactored and added additional functionality.
 * Apr 14, 2010 4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 21May2010    2187        cjeanbap    Added operational/test or practice mode functionality.
 * 28Jul2010    2187        cjeanbap    Fixed class exception in cccnnnxxxReadVersion.
 * 05Oct2010                cjeanbap    Fixed a bug introduced on #2187; return distinct rows.
 * 23May2012    14952       rferrel     Added cccnnnxxxByRefTime.
 * </pre>
 * 
 * @author garmendariz
 * @version 1
 */

public class StdTextProductDao extends CoreDao {
    private static final String PLUGIN_NAME = "text";

    private static final int MAX_FIELD_LENGTH = 3;

    private static final int MILLIS_PER_SECOND = 1000;

    private boolean operationalMode = true;

    private static final String BBB_ID = "bbbid";

    private static final String ProdCCC_ID = "prodId.cccid";

    private static final String CCC_ID = "cccid";

    private static final String REFTIME = "refTime";

    private static final String INSERTTIME = "insertTime";

    private static final String ProdHDRTIME = "prodId.hdrtime";

    private static final String ProdNNN_ID = "prodId.nnnid";

    private static final String NNN_ID = "nnnid";

    private static final String PRODUCT = "product";

    private static final String ProdSITE = "prodId.site";

    private static final String ProdWMO_ID = "prodId.wmoid";

    private static final String ProdXXX_ID = "prodId.xxxid";

    private static final String XXX_ID = "xxxid";

    private static final String OPERATIONAL_TABLE = "stdtextproducts";

    private static final String PRACTICE_TABLE = "practicestdtextproducts";

    private static final String TM_QUERY_FMT = "select refTime from table_name where cccid='%s' and nnnid='%s' and xxxid='%s';";

    private static final String AFOS_QUERY_STMT = "from StdTextProduct prod where "
            + ProdCCC_ID
            + " = :"
            + CCC_ID
            + " and "
            + ProdNNN_ID
            + " = :"
            + NNN_ID
            + " and "
            + ProdXXX_ID
            + " = :"
            + XXX_ID
            + " order by "
            + REFTIME + " desc" + ", " + INSERTTIME + " desc";

    private Log logger = LogFactory.getLog(getClass());

    /**
     * 
     */
    public StdTextProductDao() {
        super(DaoConfig.forClass("fxa", OperationalStdTextProduct.class));
        this.operationalMode = true;
    }

    /**
     * 
     */
    public StdTextProductDao(boolean operationalModeFlag) {
        super(DaoConfig.forClass("fxa",
                (operationalModeFlag ? OperationalStdTextProduct.class
                        : PracticeStdTextProduct.class)));
        this.operationalMode = operationalModeFlag;
    }

    /**
     * Add text Product into the database.
     * 
     * @param textProduct
     * @return true when entered otherwise false
     */
    public boolean write(StdTextProduct textProduct) {
        boolean success = false;
        StdTextProductId prodId = textProduct.getProdId();
        // Queries pad so must pad what is placed in database.
        String ccc = StringUtils.rightPad(prodId.getCccid(), MAX_FIELD_LENGTH);
        String nnn = StringUtils.rightPad(prodId.getNnnid(), MAX_FIELD_LENGTH);
        String xxx = StringUtils.rightPad(prodId.getXxxid(), MAX_FIELD_LENGTH);
        prodId.setCccid(ccc);
        prodId.setNnnid(nnn);
        prodId.setXxxid(xxx);
        try {
            Query query = this.getSession().createQuery(
                    "SELECT refTime from "
                            + textProduct.getClass().getSimpleName()
                            + " where prodId = :prodid");
            query.setParameter("prodid", prodId);
            List<?> results = query.list();

            if (results == null || results.size() < 1) {
                // save
                create(textProduct);
                success = true;
            } else {
                // don't save
                success = false;
            }
        } catch (Exception e) {
            logger.error("Error storing text product", e);
        }

        if (success) {
            try {
                String cccid = prodId.getCccid();
                String nnnid = prodId.getNnnid();
                String xxxid = prodId.getXxxid();
                Query query = this
                        .getSession()
                        .createQuery(
                                "SELECT versionstokeep FROM TextProductInfo WHERE "
                                        + "prodId.cccid = :cccid AND prodId.nnnid = :nnnid AND prodId.xxxid = :xxxid");
                query.setParameter("cccid", cccid);
                query.setParameter("nnnid", nnnid);
                query.setParameter("xxxid", xxxid);
                List<?> results = query.list();
                if (results == null || results.size() < 1) {
                    TextProductInfo tpi = new TextProductInfo(cccid, nnnid,
                            xxxid);
                    create(tpi);
                }
            } catch (Exception e) {
                logger.error("Error verify text product info", e);
            }
        }

        return success;
    }

    /**
     * This function reads a specific version of the specified product.
     * 
     * As of January 2000, a given AFOS ID can be stored in multiple (WMOID +
     * site) combinations. All we can do here is order by time from all WMOID +
     * sites that have this CCCNNNXXX and hope for the best.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param version
     *            Version to fetch: less than 0 retrieves all versions, 0
     *            retrieves the current version, 1 retrieves the previous
     *            version, 2 retrieves two versions ago, etc.
     * @param pastHours
     * @return
     */
    public List<StdTextProduct> cccnnnxxxReadVersion(String ccc, String nnn,
            String xxx, int version) {
        List<StdTextProduct> products = null;
        ccc = StringUtils.rightPad(ccc, MAX_FIELD_LENGTH);
        nnn = StringUtils.rightPad(nnn, MAX_FIELD_LENGTH);
        xxx = StringUtils.rightPad(xxx, MAX_FIELD_LENGTH);
        boolean hasCCC = ((ccc != null) && (ccc.length() > 0) && (!ccc
                .equals("000")));
        boolean hasNNN = ((nnn != null) && (nnn.length() > 0) && (!nnn
                .equals("000")));
        boolean hasXXX = ((xxx != null) && (xxx.length() > 0) && (!xxx
                .equals("000")));
        boolean createInitialFilter = !(hasCCC && hasNNN && hasXXX);

        AFOSProductId[] afosIds = null;
        StatelessSession session = null;
        Transaction tx = null;

        try {
            session = getSessionFactory().openStatelessSession();
            tx = session.beginTransaction();
            StdTextProduct stdTextProduct = getStdTextProductInstance();

            if (createInitialFilter) {
                stdTextProduct.setCccid(ccc);
                stdTextProduct.setNnnid(nnn);
                stdTextProduct.setXxxid(xxx);

                Map<String, String> map = buildCriterions(ProdCCC_ID, ccc,
                        ProdNNN_ID, nnn, ProdXXX_ID, xxx);
                Criteria criteria = session.createCriteria(stdTextProduct
                        .getClass());
                ProjectionList projList = Projections.projectionList();
                projList.add(Projections.property(ProdCCC_ID));
                projList.add(Projections.property(ProdNNN_ID));
                projList.add(Projections.property(ProdXXX_ID));
                criteria.setProjection(Projections.distinct(projList));
                criteria.add(Restrictions.allEq(map));
                criteria.addOrder(Order.asc(ProdCCC_ID));
                criteria.addOrder(Order.asc(ProdNNN_ID));
                criteria.addOrder(Order.asc(ProdXXX_ID));

                List<?> list = criteria.list();
                if (list != null && list.size() > 0) {
                    afosIds = new AFOSProductId[list.size()];
                    int i = 0;
                    for (Object row : list) {
                        Object[] cols = (Object[]) row;
                        afosIds[i++] = new AFOSProductId((String) cols[0],
                                (String) cols[1], (String) cols[2]);
                    }
                } else {
                    afosIds = new AFOSProductId[0];
                }
                tx.commit();
            } else {
                afosIds = new AFOSProductId[1];
                afosIds[0] = new AFOSProductId(ccc, nnn, xxx);
            }

            tx = session.beginTransaction();
            Query query = session.createQuery(AFOS_QUERY_STMT);

            if (version >= 0) {
                query.setMaxResults(version + 1);
            }
            for (AFOSProductId afosId : afosIds) {
                query.setParameter(CCC_ID, afosId.getCcc());
                query.setParameter(NNN_ID, afosId.getNnn());
                query.setParameter(XXX_ID, afosId.getXxx());

                List<?> results = query.list();
                if (results != null && results.size() > 0) {
                    if (version == -1) {
                        // want all versions
                        if (products == null) {
                            products = new ArrayList<StdTextProduct>(
                                    results.size() * afosIds.length);
                        }
                        for (Object row : results) {
                            products.add((StdTextProduct) row);
                        }
                    } else if (results.size() > version) {
                        // want specific version
                        if (products == null) {
                            products = new ArrayList<StdTextProduct>(
                                    afosIds.length);
                        }
                        products.add((StdTextProduct) results.get(version));
                    }
                }
            }
            tx.commit();
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
        } finally {
            closeSession(session);
        }

        if (products == null) {
            products = new ArrayList<StdTextProduct>(0);
        }

        return products;
    }

    /**
     * This function reads a specific version of the specified product.
     * 
     * As of January 2000, a given AFOS ID can be stored in multiple (WMOID +
     * site) combinations. All we can do here is order by time from all WMOID +
     * sites that have this CCCNNNXXX and hope for the best.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param pastHours
     * @return
     */
    public List<StdTextProduct> cccnnnxxxReadPreviousHours(String ccc,
            String nnn, String xxx, int pastHours) {
        ccc = StringUtils.rightPad(ccc, MAX_FIELD_LENGTH);
        nnn = StringUtils.rightPad(nnn, MAX_FIELD_LENGTH);
        xxx = StringUtils.rightPad(xxx, MAX_FIELD_LENGTH);

        Session session = null;

        List<StdTextProduct> products = new ArrayList<StdTextProduct>();
        try {
            session = getSession();

            Map<String, String> tmp = buildCriterions(ProdCCC_ID, ccc,
                    ProdNNN_ID, nnn, ProdXXX_ID, xxx);
            long searchTime = System.currentTimeMillis() - pastHours
                    * TimeTools.MILLIS_HOUR;

            Criteria criteria = session
                    .createCriteria(getStdTextProductInstance().getClass());
            criteria.add(Restrictions.allEq(tmp));
            criteria.add(Restrictions.gt(REFTIME, new Long(searchTime)));
            criteria.addOrder(Order.asc(ProdCCC_ID));
            criteria.addOrder(Order.asc(ProdNNN_ID));
            criteria.addOrder(Order.asc(ProdXXX_ID));
            criteria.addOrder(Order.desc(REFTIME));
            criteria.addOrder(Order.desc(INSERTTIME));
            criteria.addOrder(Order.desc(ProdHDRTIME));

            Iterator<?> iter = criteria.list().iterator();

            while (iter.hasNext()) {
                StdTextProduct prod = (StdTextProduct) iter.next();

                if (prod != null && prod.getProduct() != null
                        && prod.getProduct().length() > 0) {
                    products.add(prod);
                }
            }
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }

        return products;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public List<Long> getLatestTimes(AFOSProductId afosId) {
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll((operationalMode ? OPERATIONAL_TABLE
                : PRACTICE_TABLE));

        List<Long> times = new ArrayList<Long>();

        String ccc = afosId.getCcc();
        String nnn = afosId.getNnn();
        String xxx = afosId.getXxx();

        String query = String.format(tempQuery, ccc, nnn, xxx);

        Object[] values = null;
        try {
            values = executeSQLQuery(query);

            for (Object v : values) {
                if (v instanceof Integer) {
                    times.add(((Integer) v).longValue());
                }
            }
        } catch (Exception e) {

        }

        return times;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public long getLatestTime(AFOSProductId afosId) {
        long latestTime = 0L;

        try {
            Session sess = getSession();

            Map<?, ?> tmp = buildCriterions(ProdCCC_ID, afosId.getCcc(),
                    ProdNNN_ID, afosId.getNnn(), ProdXXX_ID, afosId.getXxx());

            Criteria criteria = sess.createCriteria(getStdTextProductInstance()
                    .getClass());
            criteria.setProjection(Projections.max(REFTIME));
            criteria.add(Restrictions.allEq(tmp));

            List<?> list = criteria.list();
            Iterator<?> iter = list.iterator();
            if (iter.hasNext()) {
                // can be returned a null when no results found
                // BigInteger val = (BigInteger) iter.next();
                Long val = (Long) iter.next();
                if (val != null) {
                    latestTime = val.longValue();
                }
            }
        } catch (Exception e) {
            logger.error("Error occurred getting latest time", e);
        }

        return latestTime;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public List<Long> getAllTimes(String ccc, String nnn, String xxx) {
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll((operationalMode ? OPERATIONAL_TABLE
                : PRACTICE_TABLE));

        List<Long> times = new ArrayList<Long>();

        String query = String.format(tempQuery, ccc, nnn, xxx);

        Object[] values = null;

        values = executeSQLQuery(query);

        if (values != null) {
            for (Object o : values) {
                BigInteger val = (BigInteger) o;
                // Long val = (Long) o;
                times.add(val.longValue());
            }
        }

        return times;
    }

    public List<String> getSameMinuteProducts(String wmoId, String siteId,
            int hdrTime, AFOSProductId afosId) {
        List<String> products = new ArrayList<String>();

        Session session = null;
        try {
            session = getSession();

            Map<String, String> tmp = buildCriterions(ProdCCC_ID,
                    afosId.getCcc(), ProdNNN_ID, afosId.getNnn(), ProdXXX_ID,
                    afosId.getXxx(), ProdSITE, siteId, ProdWMO_ID, wmoId,
                    ProdHDRTIME, new Integer(hdrTime).toString());

            Criteria criteria = session
                    .createCriteria(getStdTextProductInstance().getClass());
            criteria.setProjection(Projections.property("product"));
            criteria.add(Restrictions.allEq(tmp));
            Iterator<?> iter = criteria.list().iterator();

            while (iter.hasNext()) {
                products.add((String) iter.next());
            }
        } catch (HibernateException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving same minute products", e);
            products = null;
        } finally {
            closeSession(session);
        }

        return products;
    }

    private AFOSProductId[] getDistinctAfosIds() throws HibernateException {

        AFOSProductId[] products = null;
        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            Criteria crit = sess
                    .createCriteria((this.operationalMode ? OperationalStdTextProduct.class
                            : PracticeStdTextProduct.class));
            ProjectionList fields = Projections.projectionList();
            fields.add(Projections.property(ProdCCC_ID));
            fields.add(Projections.property(ProdNNN_ID));
            fields.add(Projections.property(ProdXXX_ID));
            crit.setProjection(Projections.distinct(fields));
            crit.addOrder(Order.asc(ProdCCC_ID));
            crit.addOrder(Order.asc(ProdNNN_ID));
            crit.addOrder(Order.asc(ProdXXX_ID));
            List<?> results = crit.list();

            if (results != null && results.size() > 0) {
                products = new AFOSProductId[results.size()];
                String cccid = null;
                String nnnid = null;
                String xxxid = null;
                int i = 0;
                for (Object row : results) {
                    Object[] cols = (Object[]) row;
                    cccid = cols[0].toString();
                    nnnid = cols[1].toString();
                    xxxid = cols[2].toString();
                    products[i++] = new AFOSProductId(cccid, nnnid, xxxid);
                }
            }
            tx.commit();
            tx = null;
        } finally {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    logger.error("Caught Exception rolling back transaction", e);
                }
            }
            closeSession(sess);
        }

        return products;
    }

    /**
     * Simple purge routine. Deletes all data that has a refTime older than that
     * specified.
     */
    public int versionPurge() {
        return versionPurge(null);
    }

    /**
     * Deletes from stdTextProduct based on textProductInfo. If afosId is null
     * will process all entries in textProductInfo, otherwise will process only
     * the passed afosId.
     * 
     * @param afosId
     * @return
     */
    public int versionPurge(String afosId) {
        StatelessSession session = null;
        Transaction tx = null;
        int rval = 0;
        if (PurgeLogger.isDebugEnabled()) {
            if (afosId == null) {
                PurgeLogger.logDebug("Purging starting for StdTextProducts",
                        PLUGIN_NAME);
            } else {
                PurgeLogger.logDebug("Purging StdTextProducts for afosId "
                        + afosId, PLUGIN_NAME);
            }
        }

        try {
            TextProductInfoDao prodInfoDao = new TextProductInfoDao();
            List<TextProductInfo> ids = null;
            if (afosId == null) {
                ids = prodInfoDao.getAllTextProductInfo();
            } else if (afosId.length() > 6) {
                TextProductInfo tpi = prodInfoDao.find(afosId.substring(0, 3),
                        afosId.substring(3, 6), afosId.substring(6));
                if (tpi != null) {
                    ids = new ArrayList<TextProductInfo>(1);
                    ids.add(tpi);
                }
            }

            if (ids != null && ids.size() > 0) {
                String cccid = null;
                String nnnid = null;
                String xxxid = null;

                String refTimeQueryString = null;
                {
                    StringBuilder refTimeQueryBuilder = new StringBuilder(200);
                    refTimeQueryBuilder.append("SELECT refTime FROM ");
                    refTimeQueryBuilder.append(getStdTextProductInstance()
                            .getClass().getSimpleName());
                    refTimeQueryBuilder.append(" WHERE ");
                    refTimeQueryBuilder.append(ProdCCC_ID).append(" = :cccid")
                            .append(" AND ");
                    refTimeQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                            .append(" AND ");
                    refTimeQueryBuilder.append(ProdXXX_ID).append(" = :xxxid");
                    refTimeQueryBuilder.append(" ORDER BY refTime DESC");
                    refTimeQueryBuilder.append(", insertTime DESC");
                    refTimeQueryString = refTimeQueryBuilder.toString();
                }

                String delQueryString = null;
                {
                    StringBuilder delQueryBuilder = new StringBuilder(200);
                    delQueryBuilder.append("DELETE FROM ");
                    delQueryBuilder.append(getStdTextProductInstance()
                            .getClass().getSimpleName());
                    delQueryBuilder.append(" WHERE ");
                    delQueryBuilder.append(ProdCCC_ID).append(" = :cccid")
                            .append(" AND ");
                    delQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                            .append(" AND ");
                    delQueryBuilder.append(ProdXXX_ID).append(" = :xxxid")
                            .append(" AND ");
                    delQueryBuilder.append("refTime < :refTime");
                    delQueryString = delQueryBuilder.toString();
                }

                session = getSessionFactory().openStatelessSession();

                for (TextProductInfo prodInfo : ids) {
                    TextProductInfoPK pk = prodInfo.getProdId();
                    cccid = pk.getCccid();
                    nnnid = pk.getNnnid();
                    xxxid = pk.getXxxid();

                    try {
                        tx = session.beginTransaction();
                        Query refTimeQuery = session
                                .createQuery(refTimeQueryString);
                        refTimeQuery.setString("cccid", cccid);
                        refTimeQuery.setString("nnnid", nnnid);
                        refTimeQuery.setString("xxxid", xxxid);
                        refTimeQuery
                                .setMaxResults(prodInfo.getVersionstokeep());
                        List<?> refTimes = refTimeQuery.list();
                        if (refTimes.size() >= prodInfo.getVersionstokeep()) {
                            long refTime = ((Number) refTimes.get(prodInfo
                                    .getVersionstokeep() - 1)).longValue();
                            Query delQuery = session
                                    .createQuery(delQueryString);
                            delQuery.setString("cccid", cccid);
                            delQuery.setString("nnnid", nnnid);
                            delQuery.setString("xxxid", xxxid);
                            delQuery.setLong("refTime", refTime);

                            if (PurgeLogger.isDebugEnabled()) {
                                PurgeLogger.logDebug("Purging records for ["
                                        + cccid + nnnid + xxxid
                                        + "] before refTime [" + refTime + "]",
                                        PLUGIN_NAME);
                            }

                            int rowsDeleted = delQuery.executeUpdate();

                            // commit every afos id purge
                            tx.commit();
                            tx = null;
                            if (PurgeLogger.isDebugEnabled()) {
                                PurgeLogger.logDebug("Purged [" + rowsDeleted
                                        + "] records for [" + cccid + nnnid
                                        + xxxid + "]", PLUGIN_NAME);
                            }
                            rval += rowsDeleted;
                        } else if (PurgeLogger.isDebugEnabled()) {
                            PurgeLogger.logDebug(
                                    "VersionPurge: Product [" + cccid + nnnid
                                            + xxxid + "] has fewer than ["
                                            + prodInfo.getVersionstokeep()
                                            + "] versions", PLUGIN_NAME);
                        }
                    } catch (Exception e) {
                        PurgeLogger.logError(
                                "Exception occurred purging text products ["
                                        + cccid + nnnid + xxxid + "]",
                                PLUGIN_NAME, e);
                        if (tx != null) {
                            try {
                                tx.rollback();
                            } catch (Exception e1) {
                                PurgeLogger
                                        .logError(
                                                "Error occurred rolling back transaction",
                                                PLUGIN_NAME, e1);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            // don't need to worry about rolling back transaction
            PurgeLogger.logError("Error purging text products", PLUGIN_NAME, e);
        } finally {
            closeSession(session);
        }
        return rval;
    }

    public void checkSiteVersionPurge() {
        ClusterTask oldLock = null;
        do {
            if (oldLock != null && oldLock.isRunning()) {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    // ignore
                }
            }

            oldLock = ClusterLockUtils
                    .lookupLock("TextPurge", "ConfiguredSite");
        } while (oldLock.isRunning()
                && oldLock.getLastExecution() > System.currentTimeMillis() - 6000);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String ccc = lc.getContextName();
        if (oldLock == null || !ccc.equals(oldLock.getExtraInfo())
                || oldLock.isRunning()) {
            ClusterLockUtils.lock("TextPurge", "ConfiguredSite", ccc, 60000,
                    true);
            PurgeLogger.logInfo("Localizing TextProductInfo for site " + ccc,
                    PLUGIN_NAME);
            TextProductInfoDao textInfoDao = new TextProductInfoDao();
            try {
                textInfoDao.purgeTable();

                // grab the distinct afos ids
                AFOSProductId[] ids = getDistinctAfosIds();
                for (AFOSProductId id : ids) {
                    TextProductInfo prodInfo = new TextProductInfo(id.getCcc(),
                            id.getNnn(), id.getXxx());
                    textInfoDao.saveOrUpdate(prodInfo);
                }
                PurgeLogger.logInfo("TextProductInfo localized", PLUGIN_NAME);
            } catch (Exception e) {
                PurgeLogger.logError(
                        "Caught exception localizing TextProductInfo",
                        PLUGIN_NAME, e);
            } finally {
                ClusterLockUtils.unlock("TextPurge", "ConfiguredSite");
            }
        }
    }

    /**
     * This function reads specified products.
     * 
     * @param wmoId
     * @param site
     * @param nnn
     * @param xxx
     * @param hdrTime
     * @param bbbb
     * @param readAllVersions
     * @return
     */
    public List<StdTextProduct> awipsRead(String wmoId, String site,
            String nnn, String xxx, String hdrTime, Long startTimeMillis,
            String bbb, int intlProd, boolean readAllVersions,
            boolean returnAllData) {
        List<StdTextProduct> products = new ArrayList<StdTextProduct>();
        Session session = null;
        Connection conn = null;

        try {
            session = getSession();
            conn = session.connection();

            String ccc = null;

            if (intlProd == 1 || intlProd == 3) {
                ccc = "NOA";
                nnn = "FOS";
                xxx = "PIL";
            }

            List<StdTextProduct> distinctProducts = getDistinctProducts(conn,
                    wmoId, site, ccc, nnn, xxx, bbb, hdrTime, startTimeMillis);

            products = getProductMetaData(conn, distinctProducts, bbb, hdrTime,
                    startTimeMillis, readAllVersions);
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeConnection(conn);
            closeSession(session);
        }

        return products;
    }

    @SuppressWarnings("unchecked")
    private List<StdTextProduct> getDistinctProducts(Connection conn,
            String wmoId, String site, String ccc, String nnn, String xxx,
            String bbb, String hdrTime, Long startTimeMillis)
            throws SQLException {
        List<StdTextProduct> products = new ArrayList<StdTextProduct>();

        Session session = null;
        try {
            session = getSession();

            Map<String, String> map = buildCriterions(ProdWMO_ID, wmoId,
                    ProdSITE, site, ProdCCC_ID, ccc, ProdNNN_ID, nnn,
                    ProdXXX_ID, xxx, BBB_ID, bbb, ProdHDRTIME, hdrTime);

            ProjectionList projectionList = Projections.projectionList();
            projectionList.add(Projections.distinct(Projections
                    .property(ProdWMO_ID)));
            projectionList.add(Projections.max(REFTIME));
            projectionList.add(Projections.groupProperty(ProdWMO_ID));
            projectionList.add(Projections.groupProperty(ProdSITE));
            projectionList.add(Projections.groupProperty(ProdCCC_ID));
            projectionList.add(Projections.groupProperty(ProdNNN_ID));
            projectionList.add(Projections.groupProperty(ProdXXX_ID));

            Criteria criteria = session
                    .createCriteria(getStdTextProductInstance().getClass());
            criteria.add(Restrictions.allEq(map));
            criteria.setProjection(projectionList);

            products = criteria.list();

            // ((Object[])((Object[]) products.toArray())[1])[1]
            List<StdTextProduct> tmpProducts = new ArrayList<StdTextProduct>();
            for (int i = 0; i < products.size(); i++) {
                StdTextProduct stdTextProduct = getStdTextProductInstance();
                stdTextProduct.setWmoid((String) ((Object[]) (products
                        .toArray())[i])[0]);
                stdTextProduct.setRefTime((Long) ((Object[]) (products
                        .toArray())[i])[1]);
                stdTextProduct
                        .setSite((String) ((Object[]) (products.toArray())[i])[3]);
                stdTextProduct.setCccid((String) ((Object[]) (products
                        .toArray())[i])[4]);
                stdTextProduct.setNnnid((String) ((Object[]) (products
                        .toArray())[i])[5]);
                stdTextProduct.setXxxid((String) ((Object[]) (products
                        .toArray())[i])[6]);
                tmpProducts.add(stdTextProduct);
            }

            products.clear();
            products.addAll(tmpProducts);
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }

        return products;
    }

    @SuppressWarnings("unchecked")
    private List<StdTextProduct> getProductMetaData(Connection conn,
            List<StdTextProduct> products, String bbb, String hdrTime,
            Long startTimeMillis, boolean readAllVersions) throws SQLException {

        List<StdTextProduct> rval = new ArrayList<StdTextProduct>();
        Session session = null;

        try {
            session = getSession();

            for (StdTextProduct p : products) {

                Map<String, String> map = buildCriterions(ProdWMO_ID,
                        p.getWmoid(), ProdSITE, p.getSite());
                map.putAll(buildCriterions(ProdCCC_ID, p.getCccid(),
                        ProdNNN_ID, p.getNnnid(), ProdXXX_ID, p.getXxxid()));
                map.putAll(buildCriterions(BBB_ID, bbb, ProdHDRTIME, hdrTime));

                ProjectionList projectionList = Projections.projectionList();
                projectionList.add(Projections.distinct(Projections
                        .property(BBB_ID)));
                projectionList.add(Projections.property(ProdWMO_ID));
                projectionList.add(Projections.property(REFTIME));
                projectionList.add(Projections.property(ProdSITE));
                projectionList.add(Projections.property(ProdCCC_ID));
                projectionList.add(Projections.property(ProdNNN_ID));
                projectionList.add(Projections.property(ProdXXX_ID));
                projectionList.add(Projections.property(ProdHDRTIME));
                projectionList.add(Projections.property(PRODUCT));

                Criteria criteria = session
                        .createCriteria(getStdTextProductInstance().getClass());
                if (readAllVersions && startTimeMillis != null) {
                    criteria.add(Restrictions.and(Restrictions.allEq(map),
                            Restrictions.ge(REFTIME, startTimeMillis)));
                } else {
                    criteria.add(Restrictions.and(Restrictions.allEq(map),
                            Restrictions.eq(REFTIME, p.getRefTime())));
                }

                criteria.setProjection(projectionList);
                criteria.addOrder(Order.desc(ProdHDRTIME));
                criteria.addOrder(Order.asc(BBB_ID));

                List<StdTextProduct> list = criteria.list();

                // ((Object[])((Object[]) products.toArray())[1])[1]
                List<StdTextProduct> tmpProducts = new ArrayList<StdTextProduct>();
                for (int i = 0; i < list.size(); i++) {
                    StdTextProduct stdTextProduct = getStdTextProductInstance();
                    stdTextProduct.setBbbid((String) ((Object[]) (list
                            .toArray())[i])[0]);
                    stdTextProduct.setWmoid((String) ((Object[]) (list
                            .toArray())[i])[1]);
                    stdTextProduct.setRefTime((Long) ((Object[]) (list
                            .toArray())[i])[2]);
                    stdTextProduct
                            .setSite((String) ((Object[]) (list.toArray())[i])[3]);
                    stdTextProduct.setCccid((String) ((Object[]) (list
                            .toArray())[i])[4]);
                    stdTextProduct.setNnnid((String) ((Object[]) (list
                            .toArray())[i])[5]);
                    stdTextProduct.setXxxid((String) ((Object[]) (list
                            .toArray())[i])[6]);
                    stdTextProduct.setHdrtime((String) ((Object[]) (list
                            .toArray())[i])[7]);
                    stdTextProduct.setProduct((String) ((Object[]) (list
                            .toArray())[i])[8]);
                    tmpProducts.add(stdTextProduct);
                }
                list.clear();
                list.addAll(tmpProducts);

                if (list != null && list.size() > 0) {
                    rval.addAll(list);
                }
            }
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }

        return rval;
    }

    /*
     * =====================================================================
     * read_w()
     * 
     * Input: a WMOID (TTAAii)
     * 
     * Output: a list of product headers OR the product
     * 
     * PDL: For each site for this WMOID Read fields for the latest products
     * Format the header lines If only one product found then Call read_product
     * to read the latest product Return End
     * ====================================================================
     */
    public List<String> read_w(String wmoId) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        PreparedStatement ps3 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        ResultSet rs3 = null;
        String cccId = null;
        String nnnId = null;
        String xxxId = null;
        String site = null;
        String hdrTime = null;
        String bbbId = null;
        int version = 0;
        final String query1 = "SELECT DISTINCT site FROM stdTextProducts "
                + "WHERE wmoId = ? ORDER BY site ASC";
        final String query2 = "SELECT MAX(refTime) FROM stdTextProducts "
                + "WHERE site = ? and wmoId = ?";
        final String query3 = "SELECT cccId, nnnId, xxxId, hdrTime, bbbId, version "
                + "FROM stdTextProducts "
                + "WHERE site = ? AND wmoId = ? AND refTime = ? "
                + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, hdrTime DESC, bbbId DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query1);
            ps2 = c.prepareStatement(query2);
            ps3 = c.prepareStatement(query3);
            ps1.setString(1, wmoId);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                site = rs1.getString("site");
                ps2.setString(1, site);
                ps2.setString(2, wmoId);
                rs2 = ps2.executeQuery();

                if (rs2.next()) {
                    int refTime = rs2.getInt("refTime");
                    ps3.setString(1, site);
                    ps3.setString(2, wmoId);
                    ps3.setInt(3, refTime);
                    rs3 = ps3.executeQuery();

                    while (rs3.next()) {
                        cccId = rs3.getString("cccId");
                        nnnId = rs3.getString("nnnId");
                        xxxId = rs3.getString("xxxId");
                        hdrTime = rs3.getString("hdrTime");
                        bbbId = rs3.getString("bbbId");
                        version = rs3.getInt("version");

                        if (cccId == null) {
                            cccId = "";
                        }
                        if (nnnId == null || nnnId.length() > 0) {
                            nnnId = "-";
                        }
                        if (xxxId == null) {
                            xxxId = "";
                        }
                        if (bbbId == null || bbbId.length() > 0) {
                            bbbId = "-";
                        }

                        retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                                cccId, nnnId, xxxId));
                    }

                    closeResultSet(rs3);
                    rs3 = null;
                }

                closeResultSet(rs2);
                rs2 = null;
            }

            if (retVal.size() == 1) {
                // read the product
                String product = read_product(c, wmoId, site, cccId, nnnId,
                        xxxId, version);
                if (product != null && product.length() > 0) {
                    retVal.clear();
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs3);
            closeStatement(ps3);
            closeResultSet(rs2);
            closeStatement(ps2);
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);

        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wh()
     * 
     * Input: a WMO ID and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this WMOID and later than startTime DO For
     * each product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_wh(String site, long startTimeMillis) {
        return read_wh(site,
                (Math.round(startTimeMillis / (float) MILLIS_PER_SECOND)));
    }

    /*
     * =======================================================================
     * read_wh()
     * 
     * Input: a WMO ID and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this WMOID and later than startTime DO For
     * each product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_wh(String wmoId, int startTimeSeconds) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String product = null;
        final String query = "SELECT product "
                + "FROM stdTextProducts WHERE wmoId = ? AND refTime >= ? "
                + "ORDER BY refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, wmoId);
            ps1.setInt(2, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if (product != null && product.length() > 0) {
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_s()
     * 
     * Input: a site (CCCC)
     * 
     * Output: a list of product headers OR the product
     * 
     * PDL: For each WMOID for this site Read fields for the latest products
     * Format the header lines If only one product found then Call read_product
     * to read the latest product Return End
     * 
     * ========================================================================
     */
    public List<String> read_s(String site) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        PreparedStatement ps3 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        ResultSet rs3 = null;
        String cccId = null;
        String nnnId = null;
        String xxxId = null;
        String wmoId = null;
        String hdrTime = null;
        String bbbId = null;
        int version = 0;
        final String query1 = "SELECT DISTINCT wmoId "
                + "FROM stdTextProducts WHERE site = ? " + "ORDER BY wmoId ASC";
        final String query2 = "SELECT MAX(refTime) "
                + "FROM stdTextProducts WHERE site = ? and wmoId = ?";
        final String query3 = "SELECT cccId, nnnId, xxxId, hdrTime, bbbId, version "
                + "FROM stdTextProducts "
                + "WHERE site = ? AND wmoId = ? AND refTime = ? "
                + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, hdrTime DESC, bbbId DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query1);
            ps2 = c.prepareStatement(query2);
            ps3 = c.prepareStatement(query3);
            ps1.setString(1, site);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                wmoId = rs1.getString("wmoId");
                ps2.setString(1, site);
                ps2.setString(2, wmoId);
                rs2 = ps2.executeQuery();

                if (rs2.next()) {
                    int refTime = rs2.getInt("refTime");
                    ps3 = c.prepareStatement(query3);
                    ps3.setString(1, site);
                    ps3.setString(2, wmoId);
                    ps3.setInt(3, refTime);
                    rs3 = ps3.executeQuery();

                    while (rs3.next()) {
                        cccId = rs3.getString("cccId");
                        nnnId = rs3.getString("nnnId");
                        xxxId = rs3.getString("xxxId");
                        hdrTime = rs3.getString("hdrTime");
                        bbbId = rs3.getString("bbbId");
                        version = rs3.getInt("version");

                        if (cccId == null) {
                            cccId = "";
                        }
                        if (nnnId == null || nnnId.length() > 0) {
                            nnnId = "-";
                        }
                        if (xxxId == null) {
                            xxxId = "";
                        }
                        if (bbbId == null || bbbId.length() > 0) {
                            bbbId = "-";
                        }

                        retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                                cccId, nnnId, xxxId));
                    }

                    closeResultSet(rs3);
                    rs3 = null;
                }

                closeResultSet(rs2);
                rs2 = null;
            }

            if (retVal.size() == 1) {
                // read the product
                String product = read_product(c, wmoId, site, cccId, nnnId,
                        xxxId, version);
                if (product != null && product.length() > 0) {
                    retVal.clear();
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs3);
            closeStatement(ps3);
            closeResultSet(rs2);
            closeStatement(ps2);
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_sh()
     * 
     * Input: a site and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this site and later than startTime DO For each
     * product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_sh(String site, long startTimeMillis) {
        return read_sh(site,
                (Math.round(startTimeMillis / (float) MILLIS_PER_SECOND)));
    }

    /*
     * =======================================================================
     * read_sh()
     * 
     * Input: a site and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this site and later than startTime DO For each
     * product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_sh(String site, int startTimeSeconds) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String product = null;
        final String query = "SELECT product "
                + "FROM stdTextProducts WHERE site = ? AND refTime >= ? "
                + "ORDER BY refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, site);
            ps1.setInt(2, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if (product != null && product.length() > 0) {
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_i()
     * 
     * Input: an abbreviated AWIPS ID - NNNXXX Output: a list of product headers
     * OR the product PDL: For each WMOID and site pair for this NNNXXX Read
     * fields for latest products Format the header lines If only one pair then
     * Call read_product to read the latest product Return End
     * 
     * Note: Usually there will be only one match. But there is at least one NNN
     * that can be in 2 WMOIDs. (TAF can be in FT and FC)
     * ========================================================================
     */
    public List<String> read_i(String abbrid) {
        List<String> retVal = new ArrayList<String>();

        if (abbrid.length() == 6) {
            Connection c = null;
            Session session = null;
            PreparedStatement ps1 = null;
            PreparedStatement ps2 = null;
            ResultSet rs1 = null;
            ResultSet rs2 = null;
            String cccId = null;
            String nnnId = abbrid.substring(0, 3); // verify abbrid length
            String xxxId = abbrid.substring(3);
            String wmoId = null;
            String site = null;
            String hdrTime = null;
            String bbbId = null;
            int version = 0;
            final String query1 = "SELECT DISTINCT wmoId, site "
                    + "FROM stdTextProducts "
                    + "WHERE nnnId = ? AND xxxId = ? "
                    + "ORDER BY wmoId ASC, site ASC";
            final String query2 = "SELECT cccId, hdrTime, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE site = ? AND wmoId = ? AND nnnId = ? AND xxxId = ? "
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC";

            try {
                session = getSession();
                c = session.connection();
                ps1 = c.prepareStatement(query1);
                ps2 = c.prepareStatement(query2);
                ps1.setString(1, nnnId);
                ps1.setString(2, xxxId);
                rs1 = ps1.executeQuery();

                while (rs1.next()) {
                    wmoId = rs1.getString("wmoId");
                    site = rs1.getString("site");
                    ps2.setString(1, site);
                    ps2.setString(2, wmoId);
                    ps2.setString(3, nnnId);
                    ps2.setString(4, xxxId);
                    rs2 = ps2.executeQuery();

                    while (rs2.next()) {
                        cccId = rs2.getString("cccId");
                        hdrTime = rs2.getString("hdrTime");
                        bbbId = rs2.getString("bbbId");
                        version = rs2.getInt("version");

                        if (cccId == null || cccId.length() > 0) {
                            cccId = "-";
                        }
                        if (bbbId == null || bbbId.length() > 0) {
                            bbbId = "-";
                        }

                        retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                                cccId, nnnId, xxxId));
                    }

                    closeResultSet(rs2);
                    rs2 = null;
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            } catch (SQLException e) {
                // don't need to worry about rolling back transaction
                logger.error("Error retrieving products", e);
            } finally {
                closeResultSet(rs2);
                closeStatement(ps2);
                closeResultSet(rs1);
                closeStatement(ps1);
                closeConnection(c);
                closeSession(session);

            }
        } else {
            logger.error("Received invalid abbrid-" + abbrid);
        }
        return retVal;
    }

    /*
     * =======================================================================
     * readLatestIntr()
     * 
     * Input: site ID, WMO ID for an international product
     * 
     * Output: the latest version
     * 
     * PDL: Read the version that matches the input Return
     * 
     * Note: The MAX function was used just in case refTimes are same. Since the
     * product refTime is the UNIX time which has the unit of microseconds, it
     * is assumed there should not have two products with the same refTime.
     * ====================================================================
     */
    public int readLatestIntr(String wmoId, String site) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        int version = -1;
        final String query1 = "SELECT MAX(refTime) "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL'";
        final String query2 = "SELECT MAX(version) "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' AND refTime = ?";

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query1);
            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            rs1 = ps1.executeQuery();

            if (rs1.next()) {
                int refTime = rs1.getInt("refTime");
                ps2 = c.prepareStatement(query2);
                ps2.setString(1, site);
                ps2.setString(2, wmoId);
                ps2.setInt(3, refTime);
                rs2 = ps2.executeQuery();

                if (rs2.next()) {
                    version = rs2.getInt("version");
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs2);
            closeStatement(ps2);
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);

        }

        return version;
    }

    /*
     * =======================================================================
     * readLatest()
     * 
     * Input: site ID, WMO ID for a national product
     * 
     * Output: the latest version
     * 
     * PDL: Read the version that matches the input Return
     * 
     * Note: The MAX function was used just in case refTimes are same. Since the
     * product refTime is the UNIX time which has the unit of microseconds, it
     * is assumed there should not have two products with the same createTim
     * ====================================================================
     */
    public int readLatest(String wmoId, String site, String cccId,
            String nnnId, String xxxId, String hdrTime) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        int version = -1;
        final String query1 = "SELECT MAX(refTime) "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ?";
        final String query2 = "SELECT MAX(version) "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ? AND refTime = ?";

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query1);
            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            ps1.setString(3, cccId);
            ps1.setString(4, nnnId);
            ps1.setString(5, xxxId);
            ps1.setString(6, hdrTime);
            rs1 = ps1.executeQuery();

            if (rs1.next()) {
                int refTime = rs1.getInt("refTime");
                ps2 = c.prepareStatement(query2);
                ps2.setString(1, wmoId);
                ps2.setString(2, site);
                ps2.setString(3, cccId);
                ps2.setString(4, nnnId);
                ps2.setString(5, xxxId);
                ps2.setString(6, hdrTime);
                ps2.setInt(7, refTime);
                rs2 = ps2.executeQuery();

                if (rs2.next()) {
                    version = rs2.getInt("version");
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs2);
            closeStatement(ps2);
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);

        }

        return version;
    }

    /*
     * =======================================================================
     * read_ws()
     * 
     * Input: a site ID and a WMO ID
     * 
     * Output: a list of product headers OR the product
     * 
     * PDL: If this is an international product Call readLatestIntr() to read
     * the product latest version Call read_product() to read the latest product
     * Return End For each NNN and XXX pairs for this WMOID and site Read fields
     * for latest products Format the header lines If only one pair then Call
     * read_product to read the latest product Return End
     * 
     * Assumption: A given WMOID (TTAAii) is either international OR national.
     * Internationals do not have NNN,XXX fields.
     * 
     * Note: For now (Jan, 2000) international have AFOS Id of NOAFOSPIL
     * 
     * Note 1: Replace count selection by a cursor for testing the existence of
     * target products to improve performance (Myron)
     * 
     * ========================================================================
     */
    public List<String> read_ws(String wmoId, String site, int intlProd) {
        List<String> retVal = new ArrayList<String>();
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        String cccId = null;
        String nnnId = null;
        String xxxId = null;
        String hdrTime = null;
        String bbbId = null;
        int version = -1;
        final String countQuery = "SELECT COUNT(DISTINCT nnnId) "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId != 'NOA' AND nnnId != 'FOS' AND xxxId != 'PIL'";
        final String abbrIdQuery = "SELECT DISTINCT nnnId, xxxId "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? ORDER BY nnnId ASC, xxxId ASC";
        final String hdrQuery = "SELECT DISTINCT cccId, hdrTime, bbbId, version, refTime "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND nnnId = ? AND xxxId = ? "
                + "ORDER by cccId ASC, refTime DESC, insertTime DESC";

        try {
            session = getSession();
            c = session.connection();
            int count = 0;

            /*
             * get count for national product
             */
            if (intlProd == 0) {
                ps1 = c.prepareStatement(countQuery);
                ps1.setString(1, wmoId);
                ps1.setString(2, site);
                rs1 = ps1.executeQuery();
                if (rs1.next()) {
                    count = rs1.getInt(1);
                }

                // close the cursor
                closeResultSet(rs1);
                closeStatement(ps1);
                rs1 = null;
                ps1 = null;
            }

            if (intlProd != 0 || count == 0) {
                /*
                 * no national product found or just for international products
                 * Check products for international WMOID and site
                 */
                version = readLatestIntr(wmoId, site);

                if (version != -1) {
                    String product = read_product(c, wmoId, site, "NOA", "FOS",
                            "PIL", version);

                    if (product != null && product.length() > 0) {
                        retVal.add(product);
                    }
                }
            } else {
                /* Get national products only - read NNNXXX */

                ps1 = c.prepareStatement(abbrIdQuery);
                ps2 = c.prepareStatement(hdrQuery);
                ps1.setString(1, wmoId);
                ps1.setString(2, site);

                rs1 = ps1.executeQuery();
                while (rs1.next()) {
                    nnnId = rs1.getString("nnnId");
                    xxxId = rs1.getString("xxxId");

                    // separate correlated sub-query into a primary query
                    ps2.setString(1, wmoId);
                    ps2.setString(2, site);
                    ps2.setString(3, nnnId);
                    ps2.setString(4, xxxId);

                    rs2 = ps2.executeQuery();

                    while (rs2.next()) {
                        cccId = rs2.getString("cccId");
                        hdrTime = rs2.getString("hdrTime");
                        bbbId = rs2.getString("bbbId");
                        version = rs2.getInt("version");

                        if (bbbId == null || bbbId.length() > 0) {
                            bbbId = "-";
                        }
                        if (cccId == null) {
                            cccId = "";
                        }

                        retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                                cccId, nnnId, xxxId));
                    }

                    closeResultSet(rs2);
                    rs2 = null;
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs2);
            closeStatement(ps2);
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);

        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wsh()
     * 
     * Input: a WMO ID, a site, and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this WMO ID, site and later than startTime DO
     * For each product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_wsh(String wmoId, String site, long startTimeMillis) {
        return read_wsh(wmoId, site,
                (Math.round(startTimeMillis / (float) MILLIS_PER_SECOND)));
    }

    /*
     * =======================================================================
     * read_wsh()
     * 
     * Input: a WMO ID, a site, and startTime
     * 
     * Output: a list of the products
     * 
     * PDL: Read all products for this WMO ID, site and later than startTime DO
     * For each product Append product into the product list ENDDO Return End
     * 
     * ========================================================================
     */
    public List<String> read_wsh(String wmoId, String site, int startTimeSeconds) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String product = null;
        final String query = "SELECT product FROM stdTextProducts "
                + "WHERE wmoId = ? site = ? AND refTime >= ? "
                + "ORDER BY refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            ps1.setInt(3, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if (product != null && product.length() > 0) {
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_ws_all()
     * 
     * Input: a site and a WMO ID Output: a list of product headers OR the
     * product PDL: Read all rows for this international WMOID and site If only
     * one row Call read_product to read the latest product Return End For each
     * row Format the header lines Return End
     * 
     * Assumption: This is for an inventory of international WMOID and site.
     * 
     * ========================================================================
     */
    public List<String> read_ws_all(String wmoId, String site) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String cccId = null;
        String nnnId = null;
        String xxxId = null;
        String hdrTime = null;
        String bbbId = null;
        int version = 0;
        final String hdrQuery = "SELECT cccId, nnnId, xxxId, hdrTime, bbbId, version "
                + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? "
                + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<String>();

        try {
            session = getSession();
            c = session.connection();
            ps1 = c.prepareStatement(hdrQuery);
            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                cccId = rs1.getString("cccId");
                nnnId = rs1.getString("nnnId");
                xxxId = rs1.getString("xxxId");
                hdrTime = rs1.getString("hdrTime");
                bbbId = rs1.getString("bbbId");
                version = rs1.getInt("version");

                if (bbbId == null || bbbId.length() > 0) {
                    bbbId = "-";
                }
                if (cccId == null) {
                    cccId = "";
                }
                if (nnnId == null || nnnId.length() > 0) {
                    nnnId = "-";
                }
                if (xxxId == null) {
                    xxxId = "";
                }

                retVal.add(generateHeader(wmoId, site, hdrTime, bbbId, cccId,
                        nnnId, xxxId));
            }

            if (retVal.size() == 1) {
                // read the product
                String product = read_product(c, wmoId, site, cccId, nnnId,
                        xxxId, version);
                if (product != null && product.length() > 0) {
                    retVal.clear();
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wsi_all()
     * 
     * Input: a site ID , a WMO ID, and an abbreviated AFOS Id
     * 
     * Output: a list of product headers OR the product
     * 
     * PDL: Read all rows for this national WMOID, site, NNN and XXX. If only
     * one row Call read_product to read the latest product Return End For each
     * row Format the header lines Return End
     * 
     * Assumption: This is for an inventory of national WMOID and sites.
     * 
     * ========================================================================
     */
    public List<String> read_wsi_all(String wmoId, String site, String abbrId) {
        List<String> retVal = new ArrayList<String>();

        if (abbrId.length() == 6) {
            Connection c = null;
            Session session = null;
            PreparedStatement ps1 = null;
            ResultSet rs1 = null;
            String cccId = null;
            String nnnId = abbrId.substring(0, 3);
            String xxxId = abbrId.substring(3);
            String hdrTime = null;
            String bbbId = null;
            int version = 0;
            final String hdrQuery = "SELECT cccId, hdrTime, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE wmoId = ? AND site = ? AND nnnId = ? AND xxxId = ? "
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC";

            try {
                session = getSession();
                c = session.connection();
                ps1 = c.prepareStatement(hdrQuery);
                ps1.setString(1, wmoId);
                ps1.setString(2, site);
                ps1.setString(3, nnnId);
                ps1.setString(4, xxxId);
                rs1 = ps1.executeQuery();

                while (rs1.next()) {
                    cccId = rs1.getString("cccId");
                    hdrTime = rs1.getString("hdrTime");
                    bbbId = rs1.getString("bbbId");
                    version = rs1.getInt("version");

                    if (bbbId == null || bbbId.length() > 0) {
                        bbbId = "-";
                    }
                    if (cccId == null) {
                        cccId = "";
                    }

                    retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                            cccId, nnnId, xxxId));
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            } catch (SQLException e) {
                // don't need to worry about rolling back transaction
                logger.error("Error retrieving products", e);
            } finally {
                closeResultSet(rs1);
                closeStatement(ps1);
                closeConnection(c);
                closeSession(session);
            }
        } else {
            logger.error("Received invalid abbrid-" + abbrId);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wsi()
     * 
     * Input: a site ID , a WMO ID, and an abbreviated AFOS Id
     * 
     * Output: the latest product
     * 
     * PDL: For each product that matches national WMOID, site, NNN [and XXX]
     * Read fields for the latest products Format the header lines If only one
     * product found then Call read_product to read the latest product Return
     * End
     * 
     * Note: a partial AFOS ID consisting of just the NNN is allowed
     * ========================================================================
     */
    public List<String> read_wsi(String wmoId, String site, String abbrId) {
        List<String> retVal = new ArrayList<String>();

        if (abbrId.length() >= 3) {
            Connection c = null;
            Session session = null;
            PreparedStatement ps1 = null;
            ResultSet rs1 = null;
            String cccId = null;
            String nnnId = abbrId.substring(0, 3);
            String xxxId = null;
            String hdrTime = null;
            String bbbId = null;
            int version = 0;
            boolean retrieveXXX = true;
            final String hdrQuery = "SELECT cccId, hdrTime, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE wmoId = ? AND site = ? AND nnnId = ? AND xxxId = ? "
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC";

            final String noXxxQuery = "SELECT cccId, xxxId hdrTime, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE wmoId = ? AND site = ? AND nnnId = ? "
                    + "ORDER BY cccId ASC, xxxId ASC, refTime DESC, insertTime DESC";

            if (abbrId.length() > 3) {
                xxxId = abbrId.substring(3);
            }

            try {
                session = getSession();
                c = session.connection();

                if (xxxId == null || xxxId.length() > 0) {
                    ps1 = c.prepareStatement(noXxxQuery);
                } else {
                    ps1 = c.prepareStatement(hdrQuery);
                    ps1.setString(4, xxxId);
                    retrieveXXX = false;
                }

                ps1.setString(1, wmoId);
                ps1.setString(2, site);
                ps1.setString(3, nnnId);

                rs1 = ps1.executeQuery();

                while (rs1.next()) {
                    cccId = rs1.getString("cccId");
                    hdrTime = rs1.getString("hdrTime");
                    bbbId = rs1.getString("bbbId");
                    version = rs1.getInt("version");

                    if (retrieveXXX) {
                        xxxId = rs1.getString("xxxId");
                    }

                    if (bbbId == null || bbbId.length() > 0) {
                        bbbId = "-";
                    }
                    if (cccId == null) {
                        cccId = "";
                    }

                    retVal.add(generateHeader(wmoId, site, hdrTime, bbbId,
                            cccId, nnnId, xxxId));
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            } catch (SQLException e) {
                // don't need to worry about rolling back transaction
                logger.error("Error retrieving products", e);
            } finally {
                closeResultSet(rs1);
                closeStatement(ps1);
                closeConnection(c);
                closeSession(session);
            }
        } else {
            logger.error("Received invalid abbrid-" + abbrId);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wsit()
     * 
     * Input: a site ID, a WMO ID, an abbreviated AFOS Id and header time
     * Output: the latest product PDL: For each product that matches national
     * WMOID, site, NNN and time Read fields for the latest products Format the
     * header lines If only one product found then Call read_product to read the
     * latest product Return End
     * 
     * Note 1: Since the granularity of header time is minute, multiple products
     * may be inserted into the one minute time frame. This creates problem to
     * retrieve them since they all have the same keys. Unless NWS communication
     * policy governing the WMO head is changed, the temporary solution will
     * just retrieve the latest version in a minute time frame.
     * 
     * ========================================================================
     */
    public List<String> read_wsit(String wmoId, String site, String abbrId,
            String hdrTime) {
        List<String> retVal = new ArrayList<String>();

        if (abbrId.length() >= 6) {
            Connection c = null;
            Session session = null;
            PreparedStatement ps1 = null;
            ResultSet rs1 = null;
            String cccId = null;
            String nnnId = abbrId.substring(0, 3);
            String xxxId = abbrId.substring(3);
            String bbbId = null;
            int version = 0;
            final String hdrQuery = "SELECT cccId, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE wmoId = ? AND site = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ?"
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC, version DESC";

            try {
                session = getSession();
                c = session.connection();

                ps1 = c.prepareStatement(hdrQuery);
                ps1.setString(1, wmoId);
                ps1.setString(2, site);
                ps1.setString(3, nnnId);
                ps1.setString(4, xxxId);
                ps1.setString(5, hdrTime);

                rs1 = ps1.executeQuery();

                while (rs1.next()) {
                    cccId = rs1.getString("cccId");
                    bbbId = rs1.getString("bbbId");
                    version = rs1.getInt("version");

                    if (bbbId == null || bbbId.length() > 0) {
                        bbbId = "-";
                    }
                    if (cccId == null) {
                        cccId = "";
                    }

                    String hdr = generateHeader(wmoId, site, hdrTime, bbbId,
                            cccId, nnnId, xxxId);

                    // order by ensures MAX(refTime) and MAX(version) is
                    // loaded first
                    if (retVal.size() > 0) {
                        if (!retVal.get(retVal.size() - 1).equals(hdr)) {
                            retVal.add(hdr);
                        }
                    } else {
                        retVal.add(hdr);
                    }
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            } catch (SQLException e) {
                // don't need to worry about rolling back transaction
                logger.error("Error retrieving products", e);
            } finally {
                closeResultSet(rs1);
                closeStatement(ps1);
                closeConnection(c);
                closeSession(session);
            }
        } else {
            logger.error("Received invalid abbrid-" + abbrId);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wsitb()
     * 
     * Input: site ID, WMO ID, abbreviated AFOS Id, header time and BBB Output:
     * the latest product PDL: For each product that matches all the input Read
     * fields for the latest products Format the header lines If only one
     * product found then Call read_product to read the latest product Return
     * End
     * 
     * Note: There should always be one and only one product for this request If
     * there are more than one, they are duplicates and should only return one
     * product.
     * ========================================================================
     */
    public List<String> read_wsitb(String wmoId, String site, String abbrId,
            String hdrTime, String bbbId) {
        List<String> retVal = new ArrayList<String>();

        if (abbrId.length() >= 6) {
            Connection c = null;
            Session session = null;
            PreparedStatement ps1 = null;
            ResultSet rs1 = null;
            String cccId = null;
            String nnnId = abbrId.substring(0, 3);
            String xxxId = abbrId.substring(3);
            int version = 0;
            final String hdrQuery = "SELECT cccId, version "
                    + "FROM stdTextProducts "
                    + "WHERE wmoId = ? AND site = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ? AND bbbId = ? "
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC, version DESC";

            try {
                session = getSession();
                c = session.connection();

                ps1 = c.prepareStatement(hdrQuery);
                ps1.setString(1, wmoId);
                ps1.setString(2, site);
                ps1.setString(3, nnnId);
                ps1.setString(4, xxxId);
                ps1.setString(5, hdrTime);
                ps1.setString(6, bbbId);

                rs1 = ps1.executeQuery();

                while (rs1.next()) {
                    cccId = rs1.getString("cccId");
                    version = rs1.getInt("version");

                    if (cccId == null) {
                        cccId = "";
                    }

                    String hdr = generateHeader(wmoId, site, hdrTime, bbbId,
                            cccId, nnnId, xxxId);

                    // order by ensures MAX(refTime) and MAX(version) is
                    // loaded first
                    if (retVal.size() > 0) {
                        if (!retVal.get(retVal.size() - 1).equals(hdr)) {
                            retVal.add(hdr);
                        }
                    } else {
                        retVal.add(hdr);
                    }
                }

                if (retVal.size() == 1) {
                    // read the product
                    String product = read_product(c, wmoId, site, cccId, nnnId,
                            xxxId, version);
                    if (product != null && product.length() > 0) {
                        retVal.clear();
                        retVal.add(product);
                    }
                }
            } catch (SQLException e) {
                // don't need to worry about rolling back transaction
                logger.error("Error retrieving products", e);
            } finally {
                closeResultSet(rs1);
                closeStatement(ps1);
                closeConnection(c);
                closeSession(session);
            }
        } else {
            logger.error("Received invalid abbrid-" + abbrId);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wst()
     * 
     * Input: a site ID, a WMO ID, and header time
     * 
     * Output: the latest product
     * 
     * PDL: For each product that matches national WMOID, site, and time Read
     * fields for the latest products Format the header lines If only one
     * product found then Call read_product to read the latest product Return
     * End
     * 
     * Note 1: intlProd code: 0 - national product but no duplicate checking
     * (DUPCK0) 1 - international product for duplicate checking within DS
     * (textdb) (DUPCK1) 2 - national product for duplicate checking (DUPCK0) 3
     * - international product for duplicate checking outside DS (textdbRemote)
     * (DUPCK2)
     * 
     * Note 2: Since the granularity of header time is minute, multiple products
     * may be inserted into the one minute time frame. This creates problem to
     * retrieve them since they all have the same keys. Unless NWS communication
     * policy governing the WMO head is changed, the temporary solution will
     * just retrieve the latest version in a minute time frame.
     * ========================================================================
     */
    public List<String> read_wst(String wmoId, String site, String hdrTime,
            int intlProd) {
        List<String> retVal = new ArrayList<String>();
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String cccId = "NOA";
        String nnnId = "FOS";
        String xxxId = "PIL";
        String bbbId = "-";
        int version = 0;

        try {
            session = getSession();
            c = session.connection();

            switch (intlProd) {
            case 0: // fall through
            case 2: {
                final String hdrQuery = "SELECT cccId, nnnId, xxxId, bbbId, version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND cccId IS NOT NULL AND nnnId IS NOT NULL AND xxxId IS NOT NULL "
                        + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, bbbId DESC, refTime DESC, insertTime DESC, version DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            case 1: {
                final String hdrQuery = "SELECT version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' "
                        + "ORDER BY refTime DESC, insertTime DESC, version DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            case 3: {
                final String hdrQuery = "SELECT version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' "
                        + "ORDER BY refTime DESC, insertTime DESC, version DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            }

            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            ps1.setString(3, hdrTime);

            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                version = rs1.getInt("version");

                switch (intlProd) {
                // nothing to do for case 1 and case 3
                case 0:
                case 2: {
                    cccId = rs1.getString("cccId");
                    nnnId = rs1.getString("nnnId");
                    xxxId = rs1.getString("xxxId");
                    bbbId = rs1.getString("bbbId");

                    if (bbbId == null || bbbId.length() == 0) {
                        bbbId = "-";
                    }
                    break;
                }
                }

                String hdr = generateHeader(wmoId, site, hdrTime, bbbId, cccId,
                        nnnId, xxxId);

                // order by ensures MAX(refTime) and MAX(version) is
                // loaded first
                if (retVal.size() > 0) {
                    if (!retVal.get(retVal.size() - 1).equals(hdr)) {
                        retVal.add(hdr);
                    }
                } else {
                    retVal.add(hdr);
                }
            }

            if (retVal.size() == 1) {
                // read the product
                String product = read_product(c, wmoId, site, cccId, nnnId,
                        xxxId, version);
                if (product != null && product.length() > 0) {
                    retVal.clear();
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_wstb()
     * 
     * Input: site ID, WMO ID, header time and BBB
     * 
     * Output: the latest product
     * 
     * PDL: For each product that matches all the input Read fields for the
     * latest products Format the header lines If only one product found then
     * Call read_product to read the latest product Return End
     * 
     * Note 1: Since the granularity of header time is minute, multiple products
     * may be inserted into the one minute time frame. This creates problem to
     * retrieve them since they all have the same keys. Unless NWS communication
     * policy governing the WMO head is changed, the temporary solution will
     * just retrieve the latest version in a minute time frame.
     * ========================================================================
     */
    public List<String> read_wstb(String wmoId, String site, String hdrTime,
            String bbbId, int intlProd) {
        List<String> retVal = new ArrayList<String>();
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String cccId = "NOA";
        String nnnId = "FOS";
        String xxxId = "PIL";
        int version = 0;

        try {
            session = getSession();
            c = session.connection();

            switch (intlProd) {
            case 0: // fall through
            case 2: {
                final String hdrQuery = "SELECT cccId, nnnId, xxxId, version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND bbbId = ? AND cccId IS NOT NULL AND nnnId IS NOT NULL AND xxxId IS NOT NULL "
                        + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, refTime DESC, insertTime DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            case 1: {
                final String hdrQuery = "SELECT version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND bbbId = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' "
                        + "ORDER BY refTime DESC, insertTime DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            case 3: {
                final String hdrQuery = "SELECT version "
                        + "FROM stdTextProducts "
                        + "WHERE wmoId = ? AND site = ? AND hdrTime = ? AND bbbId = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' "
                        + "ORDER BY refTime DESC, insertTime DESC";
                ps1 = c.prepareStatement(hdrQuery);
                break;
            }
            }

            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            ps1.setString(3, hdrTime);
            ps1.setString(4, bbbId);

            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                version = rs1.getInt("version");

                switch (intlProd) {
                // nothing to do for case 1 and case 3
                case 0: // fall through
                case 2: {
                    cccId = rs1.getString("cccId");
                    nnnId = rs1.getString("nnnId");
                    xxxId = rs1.getString("xxxId");
                    break;
                }
                }

                String hdr = generateHeader(wmoId, site, hdrTime, bbbId, cccId,
                        nnnId, xxxId);

                // order by ensures MAX(refTime) and MAX(version) is
                // loaded first
                if (retVal.size() > 0) {
                    if (!retVal.get(retVal.size() - 1).equals(hdr)) {
                        retVal.add(hdr);
                    }
                } else {
                    retVal.add(hdr);
                }
            }

            if (retVal.size() == 1) {
                // read the product
                String product = read_product(c, wmoId, site, cccId, nnnId,
                        xxxId, version);
                if (product != null && product.length() > 0) {
                    retVal.clear();
                    retVal.add(product);
                }
            }
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeResultSet(rs1);
            closeStatement(ps1);
            closeConnection(c);
            closeSession(session);
        }

        return retVal;
    }

    /*
     * =======================================================================
     * read_product()
     * 
     * Input: site ID, WMO ID, and version number Output: the product and its
     * length PDL: Read fields for the header line to prepend to product and the
     * product and its length
     * 
     * ====================================================================
     */
    public String read_product(String wmoId, String site, String cccId,
            String nnnId, String xxxId, int version) {
        String ret = null;
        Connection c = null;
        Session session = null;

        try {
            session = getSession();
            c = session.connection();
            ret = read_product(c, wmoId, site, cccId, nnnId, xxxId, version);
        } catch (SQLException e) {
            // don't need to worry about rolling back transaction
            logger.error("Error retrieving products", e);
        } finally {
            closeConnection(c);
            closeSession(session);
        }

        return ret;
    }

    /**
     * Allows for re-use of connection object.
     * 
     * @param c
     * @param wmoId
     * @param site
     * @param cccId
     * @param nnnId
     * @param xxxId
     * @param version
     * @return
     * @throws SQLException
     */
    private String read_product(Connection c, String wmoId, String site,
            String cccId, String nnnId, String xxxId, int version)
            throws SQLException {
        StringBuilder retVal = new StringBuilder();
        PreparedStatement ps = null;
        ResultSet rs = null;
        final String query = "SELECT hdrTime, bbbId, product "
                + "FROM stdTextProducts "
                + "WHERE site = ? AND wmoId = ? AND cccId = ? AND nnnId = ? AND xxxId = ? AND version = ?";

        try {
            ps = c.prepareStatement(query);
            ps.setString(1, site);
            ps.setString(2, wmoId);
            ps.setString(3, cccId);
            ps.setString(4, nnnId);
            ps.setString(5, xxxId);
            ps.setInt(6, version);
            rs = ps.executeQuery();

            if (rs.next()) {
                // should only be one
                String hdrTime = rs.getString("hdrTime");
                String bbbId = rs.getString("bbbId");
                String product = rs.getString("product");

                /* Format the header line to prepend to product */
                if (bbbId == null || bbbId.length() == 0) {
                    bbbId = "-";
                }

                retVal.append(generateHeader(wmoId, site, hdrTime, bbbId,
                        cccId, nnnId, xxxId));
                retVal.append(product);
                if (retVal.charAt(retVal.length() - 1) != '\n') {
                    retVal.append('\n');
                }
            }
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }

        return retVal.toString();
    }

    /**
     * 
     * @param wmoId
     * @param site
     * @param hdrTime
     * @param bbbId
     * @param cccId
     * @param nnnId
     * @param xxxId
     * @return
     */
    private static String generateHeader(String wmoId, String site,
            String hdrTime, String bbbId, String cccId, String nnnId,
            String xxxId) {
        StringBuilder hdr = new StringBuilder(40);
        hdr.append(wmoId);
        hdr.append(' ');
        hdr.append(site);
        hdr.append(' ');
        hdr.append(hdrTime);
        hdr.append(' ');
        hdr.append(bbbId);
        hdr.append(' ');
        hdr.append(nnnId);
        hdr.append(xxxId);
        hdr.append(' ');
        hdr.append(cccId);
        hdr.append(nnnId);
        hdr.append(xxxId);
        hdr.append('\n');
        return hdr.toString();
    }

    private void closeResultSet(ResultSet rs) {
        if (rs != null) {
            try {
                rs.close();
            } catch (SQLException e) {
                logger.error("Error closing ResultSet", e);
            }
        }
    }

    private void closeStatement(Statement s) {
        if (s != null) {
            try {
                s.close();
            } catch (SQLException e) {
                logger.error("Error closing Statement", e);
            }
        }
    }

    private void closeSession(Session s) {
        if (s != null) {
            try {
                s.close();
            } catch (Exception e) {
                logger.error("Error closing Session", e);
            }
        }
    }

    private void closeSession(StatelessSession s) {
        if (s != null) {
            try {
                s.close();
            } catch (Exception e) {
                logger.error("Error closing Session", e);
            }
        }
    }

    private void closeConnection(Connection c) {
        if (c != null) {
            try {
                c.close();
            } catch (SQLException e) {
                logger.error("Error closing Connection", e);
            }
        }
    }

    public static final void main(String[] args) {
        long time = System.currentTimeMillis() / 1000L;

        final String TM_QUERY_FMT = "delete from table_name where refTime < %d;";
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll(TM_QUERY_FMT);

        String query = String.format(tempQuery, time);

        System.out.println(query);
    }

    /**
     * Build a SQL criterion list based on name value pairs; eg ({"cccid",
     * "KWBC"}, {"nnnid", ""}, {"xxxid", ""}) that will be assigned all the same
     * Restriction type (alleq, ge, gt, etc..... ).
     * 
     * @param strings
     *            a comma seperated string containing a field name and its
     *            value; field name, value, field name, value......
     * @return a map of the name value pairs.
     */
    private Map<String, String> buildCriterions(String... strings) {

        Map<String, String> map = new HashMap<String, String>();

        for (int i = 0; i < strings.length; i += 2) {
            if ((strings[i + 1] != null)
                    && (strings[i + 1].length() > 0 && !strings[i + 1]
                            .equals("000"))) {
                map.put(strings[i], strings[i + 1]);
            }
        }

        return map;
    }

    /**
     * Returns the appropriate StdTextProduct Instance based upon the
     * OPERATIONALMODE flag.
     * 
     * @return StdTextProduct, an
     */
    private StdTextProduct getStdTextProductInstance() {
        return (this.operationalMode ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
    }
}
