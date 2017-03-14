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

package com.raytheon.uf.edex.plugin.text.dao;

import java.math.BigInteger;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.hibernate4.SessionFactoryUtils;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.site.SiteUtil;
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
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.plugin.text.IcaoMap;

/**
 * The dao implementation associated with the TextDao classes used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Sep 04, 0007  400      garmendari  Initial Check in
 * Oct 01, 2008  1538     jkorman     Added additional functionality.
 * Aug 18, 2009  2191     rjpeter     Refactored and added additional
 *                                    functionality.
 * Apr 14, 2010  4734     mhuang      Corrected StdTextProduct import dependency
 * May 21, 2010  2187     cjeanbap    Added operational/test or practice mode
 *                                    functionality.
 * Jul 28, 2010  2187     cjeanbap    Fixed class exception in
 *                                    cccnnnxxxReadVersion.
 * Oct 05, 2010           cjeanbap    Fixed a bug introduced on #2187; return
 *                                    distinct rows.
 * May 23, 2012  14952    rferrel     Added cccnnnxxxByRefTime.
 * Oct 03, 2012  15244    mgamazaych  kov  Added the fix to query the
 *                                    appropriate table (operational or
 *                                    practice)
 * May 20, 2014  2536     bclement    moved from edex.textdb to edex.plugin.text
 * Sep 18, 2014  3627     mapeters    Updated deprecated TimeTools usage.
 * Oct 16, 2014  3454     bphillip    Upgrading to Hibernate 4
 * Oct 28, 2014  3454     bphillip    Fix usage of getSession()
 * Jan 27, 2015  4031     rferrel     Resolve AFOS PILs site conflict using
 *                                    preferredAfosFirstLetter.
 * May 05, 2015  4462     rferrel     {@link #write(StdTextProduct)} when
 *                                    missing set the textProduct's site.
 * Jul 06, 2015  4612     rferrel     Get all sites matching the
 *                                    preferredafosFirstLetter.
 * Dec 09, 2015  5166     kbisanz     Update logging to use SLF4J.
 * Feb 15, 2015  4716     rferrel     Added {@link #queryProductList(int, List)}
 *                                    with common transaction code. Use {@link
 *                                    IcaoMap} to determine site.
 * Jun 20, 2016  5679     rjpeter     Fix NPE.
 * Aug 28, 2016  5839     rferrel     Added past version.
 * 
 * </pre>
 * 
 * @author garmendariz
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

    private static final String SITE_ID = "site";

    private static final String SITES = "sites";

    private static final String PRODUCT = "product";

    private static final String ProdSITE = "prodId.site";

    private static final String ProdWMO_ID = "prodId.wmoid";

    private static final String ProdXXX_ID = "prodId.xxxid";

    private static final String XXX_ID = "xxxid";

    private static final String OPERATIONAL_TABLE = "stdtextproducts";

    private static final String PRACTICE_TABLE = "practicestdtextproducts";

    private static final String TM_QUERY_FMT = "select refTime from table_name where cccid='%s' and nnnid='%s' and xxxid='%s';";

    private static final String SITE_QUERY_FMT = "select distinct site from table_name where cccid='%s' and nnnid='%s' and xxxid='%s';";

    private static final String DEFAULT_PREFERRED_AFOS_FIRST_LETTER = "KCPTXM";

    private static final String AFOS_QUERY_STMT = "from StdTextProduct where "
            + ProdCCC_ID + " = :" + CCC_ID

            + " and " + ProdNNN_ID + " = :" + NNN_ID

            + " and " + ProdXXX_ID + " = :" + XXX_ID

            + " and " + ProdSITE + " in (:" + SITES + ")"

            + " order by " + REFTIME + " desc" + ", " + INSERTTIME + " desc";

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final static char[] preferredAfosFirstLetter;

    static {
        String afosLetters = System.getenv("PREFERRED_AFOS_FIRST_LETTER");
        if (afosLetters == null) {
            afosLetters = DEFAULT_PREFERRED_AFOS_FIRST_LETTER;
        }

        // Place site's first letter at the start of the preferred letters.
        String awSite = SiteMap.getInstance()
                .getSite4LetterId(SiteUtil.getSite());
        afosLetters = afosLetters.replaceAll(awSite.substring(0, 1), "");
        preferredAfosFirstLetter = new char[afosLetters.length() + 1];
        afosLetters.getChars(0, afosLetters.length(), preferredAfosFirstLetter,
                1);
        preferredAfosFirstLetter[0] = awSite.charAt(0);
    }

    private String siteQueryFmt;

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
        Session session = this.getSession();
        String site = textProduct.getProdId().getSite();
        if (StringUtils.isBlank(site)) {
            // Determine product site.
            site = IcaoMap.siteToIcaoId(xxx, prodId.getSite());
            if (logger.isInfoEnabled()) {
                logger.info("Write \"" + ccc + nnn + xxx + "\" setting site to "
                        + site);
            }
            textProduct.getProdId().setSite(site);
        }

        try {
            try {
                Query query = session.createQuery("SELECT refTime from "
                        + textProduct.getClass().getSimpleName()
                        + " where prodId = :prodid");
                query.setParameter("prodid", prodId);
                List<?> results = query.list();

                if ((results == null) || (results.size() < 1)) {
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
                    Query query = session.createQuery(
                            "SELECT versionstokeep FROM TextProductInfo WHERE "
                                    + "prodId.cccid = :cccid AND prodId.nnnid = :nnnid AND prodId.xxxid = :xxxid");
                    query.setParameter("cccid", cccid);
                    query.setParameter("nnnid", nnnid);
                    query.setParameter("xxxid", xxxid);
                    List<?> results = query.list();
                    if ((results == null) || (results.size() < 1)) {
                        TextProductInfo tpi = new TextProductInfo(cccid, nnnid,
                                xxxid);
                        create(tpi);
                    }
                } catch (Exception e) {
                    logger.error("Error verify text product info", e);
                }
            }
        } finally {
            if (session != null) {
                session.close();
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
    public List<StdTextProduct> cccnnnxxxReadVersion(final String ccc,
            final String nnn, final String xxx, final int version) {
        List<StdTextProduct> products = null;

        try {
            final List<Pair<String[], AFOSProductId>> siteAfosIdList = querySiteAfosId(
                    ccc, nnn, xxx);

            products = queryProductList(version, siteAfosIdList);

        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        }

        if (products == null) {
            products = new ArrayList<>(0);
        }

        return products;
    }

    /**
     * This method retrieves specific versions of the specified awips product.
     * 
     * @param cccc
     * @param nnn
     * @param xxx
     * @param version
     *            Version to fetch: less than 0 retrieves all versions, 0
     *            retrieves the current version, 1 retrieves the previous
     *            version, 2 retrieves two versions ago, etc.
     * @return products
     */
    public List<StdTextProduct> ccccnnnxxxReadVersion(final String cccc,
            final String nnn, final String xxx, final int version) {
        List<StdTextProduct> products = null;

        try {
            final List<Pair<String[], AFOSProductId>> siteAfosIdList = querySiteAfosId(
                    null, nnn, xxx, cccc);

            products = queryProductList(version, siteAfosIdList);
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        }

        if (products == null) {
            products = new ArrayList<>(0);
        }

        return products;
    }

    /**
     * Common transaction code for the various queries.
     * 
     * @param version
     * @param siteAfosIdList
     * @return products
     */
    private List<StdTextProduct> queryProductList(final int version,
            final List<Pair<String[], AFOSProductId>> siteAfosIdList) {

        return txTemplate
                .execute(new TransactionCallback<List<StdTextProduct>>() {

                    @Override
                    public List<StdTextProduct> doInTransaction(
                            TransactionStatus status) {
                        List<StdTextProduct> products = null;
                        Session session = getCurrentSession();
                        /*
                         * DR15244 - Make sure that the query is performed on
                         * the appropriate table based on what StdTextProduct is
                         * requested (ultimately on CAVE mode)
                         */
                        Matcher m = Pattern.compile("StdTextProduct")
                                .matcher(AFOS_QUERY_STMT);
                        String tableName = getStdTextProductInstance()
                                .getClass().getSimpleName();
                        String tableQuery = m.replaceAll(tableName);
                        Query query = session.createQuery(tableQuery);

                        if (version >= 0) {
                            query.setMaxResults(version + 1);
                        }

                        for (Pair<String[], AFOSProductId> siteAfosId : siteAfosIdList) {
                            String[] sites = siteAfosId.getFirst();
                            AFOSProductId afosId = siteAfosId.getSecond();
                            if (sites != null) {
                                query.setParameter(CCC_ID, afosId.getCcc());
                                query.setParameter(NNN_ID, afosId.getNnn());
                                query.setParameter(XXX_ID, afosId.getXxx());
                                query.setParameterList(SITES, sites);

                                List<?> results = query.list();
                                if ((results != null) && (results.size() > 0)) {
                                    if (version == -1) {
                                        // want all versions
                                        if (products == null) {
                                            products = new ArrayList<>(results
                                                    .size()
                                                    * siteAfosIdList.size());
                                        }
                                        for (Object row : results) {
                                            products.add((StdTextProduct) row);
                                        }
                                    } else if (results.size() > version) {
                                        // want specific version
                                        if (products == null) {
                                            products = new ArrayList<>(
                                                    siteAfosIdList.size());
                                        }
                                        products.add((StdTextProduct) results
                                                .get(version));
                                    }
                                }
                            }
                        }
                        return products;
                    }
                });

    }

    /**
     * Get desired site's afosId the pairs are order by AfosId.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @return siteAfosIds
     */
    private List<Pair<String[], AFOSProductId>> querySiteAfosId(
            final String ccc, final String nnn, final String xxx) {
        return querySiteAfosId(ccc, nnn, xxx, null);
    }

    /**
     * Get desired site's afosId the pairs are order by AfosId.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param site
     * @return siteAfosIds
     */
    private List<Pair<String[], AFOSProductId>> querySiteAfosId(
            final String ccc, final String nnn, final String xxx,
            final String site) {
        boolean hasCCC = ((ccc != null) && (ccc.length() > 0)
                && (!ccc.equals("000")));
        boolean hasNNN = ((nnn != null) && (nnn.length() > 0)
                && (!nnn.equals("000")));
        boolean hasXXX = ((xxx != null) && (xxx.length() > 0)
                && (!xxx.equals("000")));
        final boolean createInitialFilter = !(hasCCC && hasNNN && hasXXX);

        return txTemplate.execute(
                new TransactionCallback<List<Pair<String[], AFOSProductId>>>() {

                    @Override
                    public List<Pair<String[], AFOSProductId>> doInTransaction(
                            TransactionStatus status) {
                        String paddedccc = StringUtils.rightPad(ccc,
                                MAX_FIELD_LENGTH);
                        String paddednnn = StringUtils.rightPad(nnn,
                                MAX_FIELD_LENGTH);
                        String paddedxxx = StringUtils.rightPad(xxx,
                                MAX_FIELD_LENGTH);
                        Session session = getCurrentSession();
                        List<Pair<String[], AFOSProductId>> siteProductPairList = null;
                        StdTextProduct stdTextProduct = getStdTextProductInstance();

                        if (createInitialFilter) {
                            stdTextProduct.setCccid(paddedccc);
                            stdTextProduct.setNnnid(paddednnn);
                            stdTextProduct.setXxxid(paddedxxx);

                            Map<String, String> map = buildCriterions(
                                    ProdCCC_ID, paddedccc, ProdNNN_ID,
                                    paddednnn, ProdXXX_ID, paddedxxx, ProdSITE,
                                    site);
                            Criteria criteria = session
                                    .createCriteria(stdTextProduct.getClass());
                            ProjectionList projList = Projections
                                    .projectionList();
                            projList.add(Projections.property(ProdCCC_ID));
                            projList.add(Projections.property(ProdNNN_ID));
                            projList.add(Projections.property(ProdXXX_ID));
                            projList.add(Projections.property(ProdSITE));
                            criteria.setProjection(
                                    Projections.distinct(projList));
                            criteria.add(Restrictions.allEq(map));
                            criteria.addOrder(Order.asc(ProdCCC_ID));
                            criteria.addOrder(Order.asc(ProdNNN_ID));
                            criteria.addOrder(Order.asc(ProdXXX_ID));

                            List<?> list = criteria.list();
                            if ((list != null) && !list.isEmpty()) {
                                Map<Integer, Set<String>> siteMap = new HashMap<>();
                                List<AFOSProductId> orderedAfosIds = new ArrayList<>(
                                        list.size());
                                for (Object row : list) {
                                    Object[] cols = (Object[]) row;
                                    AFOSProductId afosId = new AFOSProductId(
                                            (String) cols[0], (String) cols[1],
                                            (String) cols[2]);
                                    String site = (String) cols[3];
                                    int index = orderedAfosIds.indexOf(afosId);
                                    if (index < 0) {
                                        index = orderedAfosIds.size();
                                        orderedAfosIds.add(afosId);
                                    }
                                    Set<String> sites = siteMap.get(index);
                                    if (sites == null) {
                                        sites = new HashSet<>();
                                        siteMap.put(index, sites);
                                    }
                                    sites.add(site);
                                }
                                siteProductPairList = new ArrayList<>(
                                        siteMap.size());
                                List<Integer> indices = new ArrayList<>(
                                        siteMap.keySet());
                                Collections.sort(indices);
                                for (int index : indices) {
                                    String[] sites = getSite(
                                            siteMap.get(index).toArray());
                                    AFOSProductId afosId = orderedAfosIds
                                            .get(index);
                                    siteProductPairList
                                            .add(new Pair<>(sites, afosId));
                                }
                            } else {
                                siteProductPairList = new ArrayList<>(0);
                            }
                        } else {
                            AFOSProductId afosId = new AFOSProductId(paddedccc,
                                    paddednnn, paddedxxx);
                            String[] sites = getSite(afosId);
                            if (sites == null) {
                                siteProductPairList = new ArrayList<>(0);
                            } else {
                                siteProductPairList = new ArrayList<>(1);
                                siteProductPairList
                                        .add(new Pair<>(sites, afosId));
                            }
                        }
                        return siteProductPairList;
                    }
                });
    }

    /**
     * Get sites based on the ordering from preferredAfosFirstLetter.
     * 
     * @param afosId
     * @return sites or null when no data.
     */
    private String[] getSite(AFOSProductId afosId) {
        String ccc = afosId.getCcc();
        String nnn = afosId.getNnn();
        String xxx = afosId.getXxx();

        if (siteQueryFmt == null) {
            Matcher m = Pattern.compile("table_name").matcher(SITE_QUERY_FMT);
            siteQueryFmt = m.replaceAll(
                    (operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE));
        }

        String siteQuery = String.format(siteQueryFmt, ccc, nnn, xxx);

        Object[] values = null;

        try {
            values = executeSQLQuery(siteQuery);
        } catch (Exception e) {
            values = null;
        }

        return getSite(values);
    }

    /**
     * From the array of sites determine which ones are preferred sites.
     * 
     * @param values
     *            - Assume sites with common afosId
     * @return sites based on preferredAfosFirstLetter
     */
    private String[] getSite(Object[] values) {
        if (values != null) {
            if (values.length == 1) {
                return new String[] { (String) values[0] };
            } else if (values.length > 1) {
                List<String> results = new ArrayList<>(values.length);
                String[] sites = new String[values.length];
                for (int i = 0; i < values.length; ++i) {
                    sites[i] = (String) values[i];
                }
                for (char c : preferredAfosFirstLetter) {
                    for (String site : sites) {
                        if (site.charAt(0) == c) {
                            results.add(site);
                        }
                    }
                    if (!results.isEmpty()) {
                        return results.toArray(new String[results.size()]);
                    }
                }
                if (logger.isInfoEnabled()) {
                    String message = "None of the sites first character in preferred AFOS first letter list \""
                            + new String(preferredAfosFirstLetter)
                            + "\". Using sites: " + sites;
                    logger.info(message);
                }
                return sites;
            }
        }
        return null;
    }

    /**
     * Use the reference/creation time to restrict the results when obtaining a
     * product.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param refTime
     * @return products
     */
    public List<StdTextProduct> cccnnnxxxByRefTime(String ccc, String nnn,
            String xxx, Long refTime) {
        return cccnnnxxxSiteByRefTime(ccc, nnn, xxx, refTime, null);
    }

    /**
     * Use the reference/creation and site to restrict the results when
     * obtaining a product.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param refTime
     * @param site
     *            - when null obtain matches for all sites
     * @return products
     */
    public List<StdTextProduct> cccnnnxxxSiteByRefTime(String ccc, String nnn,
            String xxx, Long refTime, String site) {
        ccc = StringUtils.rightPad(ccc, MAX_FIELD_LENGTH);
        nnn = StringUtils.rightPad(nnn, MAX_FIELD_LENGTH);
        xxx = StringUtils.rightPad(xxx, MAX_FIELD_LENGTH);
        Session session = null;

        try {
            List<Pair<String[], AFOSProductId>> siteAfosIdList = querySiteAfosId(
                    ccc, nnn, xxx);

            session = getSession();

            Map<String, Object> tmp = new HashMap<>();
            if (ccc != null) {
                tmp.put(ProdCCC_ID, ccc);
            }
            if (nnn != null) {
                tmp.put(ProdNNN_ID, nnn);
            }
            if (xxx != null) {
                tmp.put(ProdXXX_ID, xxx);
            }
            if (refTime != null) {
                tmp.put(REFTIME, refTime);
            }

            if (site != null) {
                tmp.put(SITE_ID, site);
            }

            Criteria criteria = session
                    .createCriteria(getStdTextProductInstance().getClass());
            criteria.add(Restrictions.allEq(tmp));
            criteria.addOrder(Order.asc(ProdCCC_ID));
            criteria.addOrder(Order.asc(ProdNNN_ID));
            criteria.addOrder(Order.asc(ProdXXX_ID));
            criteria.addOrder(Order.desc(REFTIME));
            criteria.addOrder(Order.desc(ProdHDRTIME));

            return listProducts(criteria, siteAfosIdList);

        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }

        return new ArrayList<>(0);

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
     * @return products
     */
    public List<StdTextProduct> cccnnnxxxReadPreviousHours(String ccc,
            String nnn, String xxx, int pastHours) {
        return cccnnnxxxSiteReadPreviousHours(ccc, nnn, xxx, pastHours, null);
    }

    /**
     * This method reads a specific past hour product for the specified site and
     * product.
     * 
     * @param ccc
     * @param nnn
     * @param xxx
     * @param pastHours
     * @param site
     * @return products
     */
    public List<StdTextProduct> cccnnnxxxSiteReadPreviousHours(String ccc,
            String nnn, String xxx, int pastHours, String site) {
        ccc = StringUtils.rightPad(ccc, MAX_FIELD_LENGTH);
        nnn = StringUtils.rightPad(nnn, MAX_FIELD_LENGTH);
        xxx = StringUtils.rightPad(xxx, MAX_FIELD_LENGTH);

        Session session = null;

        try {
            List<Pair<String[], AFOSProductId>> siteAfosIdList = querySiteAfosId(
                    ccc, nnn, xxx, site);

            session = getSession();

            Map<String, String> tmp = buildCriterions(ProdCCC_ID, ccc,
                    ProdNNN_ID, nnn, ProdXXX_ID, xxx, ProdSITE, site);
            long searchTime = System.currentTimeMillis()
                    - (pastHours * TimeUtil.MILLIS_PER_HOUR);

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

            return listProducts(criteria, siteAfosIdList);

        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }

        return new ArrayList<>(0);
    }

    /**
     * Get non-empty products and when sites have the same afoisId only add the
     * products from the site based on preferredAfosFirstLetter. Keep products
     * in the order returned by the criteria list. This assumes the results are
     * ordered so identical afosIds are together.
     * 
     * 
     * @param criteria
     *            - Assume results ordered by afosId
     * @param siteAfosIdList
     *            - Assume ordered by afosId
     * @return products
     * @throws HibernateException
     */
    private List<StdTextProduct> listProducts(Criteria criteria,
            List<Pair<String[], AFOSProductId>> siteAfosIdList)
                    throws HibernateException {

        List<?> prodList = criteria.list();
        List<StdTextProduct> products = null;

        if ((prodList == null) || prodList.isEmpty()) {
            products = new ArrayList<>(0);
        } else {
            products = new ArrayList<>(prodList.size());

            Iterator<?> iter = prodList.iterator();
            Iterator<Pair<String[], AFOSProductId>> siteAfosIdIter = siteAfosIdList
                    .iterator();
            Pair<String[], AFOSProductId> pair = siteAfosIdIter.next();

            while (iter.hasNext()) {
                StdTextProduct prod = (StdTextProduct) iter.next();

                if ((prod != null) && (prod.getProduct() != null)
                        && !prod.getProduct().isEmpty()) {

                    AFOSProductId afosId = new AFOSProductId(prod.getCccid(),
                            prod.getNnnid(), prod.getXxxid());
                    while (!afosId.equals(pair.getSecond())) {
                        pair = siteAfosIdIter.next();
                    }
                    String site = prod.getSite();
                    for (String pSite : pair.getFirst()) {
                        if (site.equals(pSite)) {
                            products.add(prod);
                            break;
                        }
                    }
                }
            }
        }

        return products;
    }

    /**
     * 
     * @param afosId
     * @return times
     */
    public List<Long> getLatestTimes(AFOSProductId afosId) {
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll(
                (operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE));

        List<Long> times = new ArrayList<>();

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
     * @return times
     */
    public long getLatestTime(AFOSProductId afosId) {
        long latestTime = 0L;

        Session sess = null;

        try {
            sess = getSession();

            Map<?, ?> tmp = buildCriterions(ProdCCC_ID, afosId.getCcc(),
                    ProdNNN_ID, afosId.getNnn(), ProdXXX_ID, afosId.getXxx());

            Criteria criteria = sess
                    .createCriteria(getStdTextProductInstance().getClass());
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
        } finally {
            if (sess != null) {
                sess.close();
            }
        }

        return latestTime;
    }

    /**
     * 
     * @param afosId
     * @return times
     */
    public List<Long> getAllTimes(String ccc, String nnn, String xxx) {
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll(
                (operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE));

        List<Long> times = new ArrayList<>();

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

    /**
     * 
     * @param wmoId
     * @param siteId
     * @param hdrTime
     * @param afosId
     * @return
     */
    public List<String> getSameMinuteProducts(String wmoId, String siteId,
            int hdrTime, AFOSProductId afosId) {
        List<String> products = new ArrayList<>();

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
        return txTemplate.execute(new TransactionCallback<AFOSProductId[]>() {
            @Override
            public AFOSProductId[] doInTransaction(TransactionStatus status) {
                AFOSProductId[] products = null;
                Session sess = getCurrentSession();
                Criteria crit = sess.createCriteria(
                        (operationalMode ? OperationalStdTextProduct.class
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

                if ((results != null) && (results.size() > 0)) {
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

                return products;
            }
        });
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
        int rval = 0;
        if (PurgeLogger.isDebugEnabled()) {
            if (afosId == null) {
                PurgeLogger.logDebug("Purging starting for StdTextProducts",
                        PLUGIN_NAME);
            } else {
                PurgeLogger.logDebug(
                        "Purging StdTextProducts for afosId " + afosId,
                        PLUGIN_NAME);
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
                    ids = new ArrayList<>(1);
                    ids.add(tpi);
                }
            }

            if ((ids != null) && (ids.size() > 0)) {

                StringBuilder refTimeQueryBuilder = new StringBuilder(200);
                refTimeQueryBuilder.append("SELECT refTime FROM ");
                refTimeQueryBuilder.append(
                        getStdTextProductInstance().getClass().getSimpleName());
                refTimeQueryBuilder.append(" WHERE ");
                refTimeQueryBuilder.append(ProdCCC_ID).append(" = :cccid")
                        .append(" AND ");
                refTimeQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                        .append(" AND ");
                refTimeQueryBuilder.append(ProdXXX_ID).append(" = :xxxid");
                refTimeQueryBuilder.append(" ORDER BY refTime DESC");
                refTimeQueryBuilder.append(", insertTime DESC");
                final String refTimeQueryString = refTimeQueryBuilder
                        .toString();

                StringBuilder delQueryBuilder = new StringBuilder(200);
                delQueryBuilder.append("DELETE FROM ");
                delQueryBuilder.append(
                        getStdTextProductInstance().getClass().getSimpleName());
                delQueryBuilder.append(" WHERE ");
                delQueryBuilder.append(ProdCCC_ID).append(" = :cccid")
                        .append(" AND ");
                delQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                        .append(" AND ");
                delQueryBuilder.append(ProdXXX_ID).append(" = :xxxid")
                        .append(" AND ");
                delQueryBuilder.append("refTime < :refTime");
                final String delQueryString = delQueryBuilder.toString();

                for (final TextProductInfo prodInfo : ids) {
                    rval += txTemplate
                            .execute(new TransactionCallback<Integer>() {

                                @Override
                                public Integer doInTransaction(
                                        TransactionStatus status) {
                                    Session session = getCurrentSession();
                                    TextProductInfoPK pk = prodInfo.getProdId();
                                    String cccid = pk.getCccid();
                                    String nnnid = pk.getNnnid();
                                    String xxxid = pk.getXxxid();
                                    int rowsDeleted = 0;

                                    try {
                                        Query refTimeQuery = session
                                                .createQuery(
                                                        refTimeQueryString);
                                        refTimeQuery.setString("cccid", cccid);
                                        refTimeQuery.setString("nnnid", nnnid);
                                        refTimeQuery.setString("xxxid", xxxid);
                                        refTimeQuery.setMaxResults(
                                                prodInfo.getVersionstokeep());
                                        List<?> refTimes = refTimeQuery.list();
                                        if (refTimes.size() >= prodInfo
                                                .getVersionstokeep()) {
                                            long refTime = ((Number) refTimes
                                                    .get(prodInfo
                                                            .getVersionstokeep()
                                                            - 1)).longValue();
                                            Query delQuery = session
                                                    .createQuery(
                                                            delQueryString);
                                            delQuery.setString("cccid", cccid);
                                            delQuery.setString("nnnid", nnnid);
                                            delQuery.setString("xxxid", xxxid);
                                            delQuery.setLong("refTime",
                                                    refTime);

                                            if (PurgeLogger.isDebugEnabled()) {
                                                PurgeLogger.logDebug(
                                                        "Purging records for ["
                                                                + cccid + nnnid
                                                                + xxxid
                                                                + "] before refTime ["
                                                                + refTime + "]",
                                                        PLUGIN_NAME);
                                            }

                                            rowsDeleted = delQuery
                                                    .executeUpdate();
                                            if (PurgeLogger.isDebugEnabled()) {
                                                PurgeLogger.logDebug(
                                                        "Purged [" + rowsDeleted
                                                                + "] records for ["
                                                                + cccid + nnnid
                                                                + xxxid + "]",
                                                        PLUGIN_NAME);
                                            }
                                        } else if (PurgeLogger
                                                .isDebugEnabled()) {
                                            PurgeLogger.logDebug(
                                                    "VersionPurge: Product ["
                                                            + cccid + nnnid
                                                            + xxxid
                                                            + "] has fewer than ["
                                                            + prodInfo
                                                                    .getVersionstokeep()
                                                            + "] versions",
                                                    PLUGIN_NAME);
                                        }
                                    } catch (Exception e) {
                                        PurgeLogger
                                                .logError(
                                                        "Exception occurred purging text products ["
                                                                + cccid + nnnid
                                                                + xxxid + "]",
                                                        PLUGIN_NAME, e);
                                    }
                                    return rowsDeleted;
                                }
                            });

                }
            }
        } catch (Exception e) {
            // don't need to worry about rolling back transaction
            PurgeLogger.logError("Error purging text products", PLUGIN_NAME, e);
        }
        return rval;
    }

    public void checkSiteVersionPurge() {
        ClusterTask oldLock = null;
        do {
            if ((oldLock != null) && oldLock.isRunning()) {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    // ignore
                }
            }

            oldLock = ClusterLockUtils.lookupLock("TextPurge",
                    "ConfiguredSite");
        } while (oldLock.isRunning() && (oldLock
                .getLastExecution() > (System.currentTimeMillis() - 6000)));

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String ccc = lc.getContextName();
        if ((oldLock == null) || !ccc.equals(oldLock.getExtraInfo())
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
                if (ids != null) {
                    for (AFOSProductId id : ids) {
                        TextProductInfo prodInfo = new TextProductInfo(
                                id.getCcc(), id.getNnn(), id.getXxx());
                        textInfoDao.saveOrUpdate(prodInfo);
                    }
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
    public List<StdTextProduct> awipsRead(String wmoId, String site, String nnn,
            String xxx, String hdrTime, String pastVersion,
            Long startTimeMillis, String bbb, int intlProd,
            boolean readAllVersions, boolean returnAllData) {
        Session session = null;
        Connection conn = null;

        try {
            session = getSession();
            conn = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();

            String ccc = null;

            if ((intlProd == 1) || (intlProd == 3)) {
                ccc = "NOA";
                nnn = "FOS";
                xxx = "PIL";
            }

            if (readAllVersions) {
                return ccccnnnxxxReadVersion(site, nnn, xxx, -1);
            } else if (pastVersion != null) {
                return ccccnnnxxxReadVersion(site, nnn, xxx,
                        Integer.parseInt(pastVersion));
            } else if (StringUtil.isEmptyString(hdrTime)) {
                return ccccnnnxxxReadVersion(site, nnn, xxx, 0);
            } else {

                List<StdTextProduct> distinctProducts = getDistinctProducts(
                        conn, wmoId, site, ccc, nnn, xxx, bbb, hdrTime,
                        startTimeMillis);

                return getProductMetaData(conn, distinctProducts, bbb, hdrTime,
                        startTimeMillis, readAllVersions);
            }
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeConnection(conn);
            closeSession(session);
        }

        return new ArrayList<>(0);
    }

    @SuppressWarnings("unchecked")
    private List<StdTextProduct> getDistinctProducts(Connection conn,
            String wmoId, String site, String ccc, String nnn, String xxx,
            String bbb, String hdrTime, Long startTimeMillis)
                    throws SQLException {
        List<StdTextProduct> products = new ArrayList<>();

        Session session = null;
        try {
            session = getSession();

            Map<String, String> map = buildCriterions(ProdWMO_ID, wmoId,
                    ProdSITE, site, ProdCCC_ID, ccc, ProdNNN_ID, nnn,
                    ProdXXX_ID, xxx, BBB_ID, bbb, ProdHDRTIME, hdrTime);

            ProjectionList projectionList = Projections.projectionList();
            projectionList.add(
                    Projections.distinct(Projections.property(ProdWMO_ID)));
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
            List<StdTextProduct> tmpProducts = new ArrayList<>();
            for (int i = 0; i < products.size(); i++) {
                StdTextProduct stdTextProduct = getStdTextProductInstance();
                stdTextProduct.setWmoid(
                        (String) ((Object[]) (products.toArray())[i])[0]);
                stdTextProduct.setRefTime(
                        (Long) ((Object[]) (products.toArray())[i])[1]);
                stdTextProduct.setSite(
                        (String) ((Object[]) (products.toArray())[i])[3]);
                stdTextProduct.setCccid(
                        (String) ((Object[]) (products.toArray())[i])[4]);
                stdTextProduct.setNnnid(
                        (String) ((Object[]) (products.toArray())[i])[5]);
                stdTextProduct.setXxxid(
                        (String) ((Object[]) (products.toArray())[i])[6]);
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

        List<StdTextProduct> rval = new ArrayList<>();
        Session session = null;

        try {
            session = getSession();

            for (StdTextProduct p : products) {

                Map<String, String> map = buildCriterions(ProdWMO_ID,
                        p.getWmoid(), ProdSITE, p.getSite());
                map.putAll(buildCriterions(ProdCCC_ID, p.getCccid(), ProdNNN_ID,
                        p.getNnnid(), ProdXXX_ID, p.getXxxid()));
                map.putAll(buildCriterions(BBB_ID, bbb, ProdHDRTIME, hdrTime));

                ProjectionList projectionList = Projections.projectionList();
                projectionList.add(
                        Projections.distinct(Projections.property(BBB_ID)));
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
                if (readAllVersions && (startTimeMillis != null)) {
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
                List<StdTextProduct> tmpProducts = new ArrayList<>();
                for (int i = 0; i < list.size(); i++) {
                    StdTextProduct stdTextProduct = getStdTextProductInstance();
                    stdTextProduct.setBbbid(
                            (String) ((Object[]) (list.toArray())[i])[0]);
                    stdTextProduct.setWmoid(
                            (String) ((Object[]) (list.toArray())[i])[1]);
                    stdTextProduct.setRefTime(
                            (Long) ((Object[]) (list.toArray())[i])[2]);
                    stdTextProduct.setSite(
                            (String) ((Object[]) (list.toArray())[i])[3]);
                    stdTextProduct.setCccid(
                            (String) ((Object[]) (list.toArray())[i])[4]);
                    stdTextProduct.setNnnid(
                            (String) ((Object[]) (list.toArray())[i])[5]);
                    stdTextProduct.setXxxid(
                            (String) ((Object[]) (list.toArray())[i])[6]);
                    stdTextProduct.setHdrtime(
                            (String) ((Object[]) (list.toArray())[i])[7]);
                    stdTextProduct.setProduct(
                            (String) ((Object[]) (list.toArray())[i])[8]);
                    tmpProducts.add(stdTextProduct);
                }
                list.clear();
                list.addAll(tmpProducts);

                if ((list != null) && (list.size() > 0)) {
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
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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
                        if ((nnnId == null) || (nnnId.length() > 0)) {
                            nnnId = "-";
                        }
                        if (xxxId == null) {
                            xxxId = "";
                        }
                        if ((bbbId == null) || (bbbId.length() > 0)) {
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
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, wmoId);
            ps1.setInt(2, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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
                        if ((nnnId == null) || (nnnId.length() > 0)) {
                            nnnId = "-";
                        }
                        if (xxxId == null) {
                            xxxId = "";
                        }
                        if ((bbbId == null) || (bbbId.length() > 0)) {
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
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, site);
            ps1.setInt(2, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

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
                    + "FROM stdTextProducts " + "WHERE nnnId = ? AND xxxId = ? "
                    + "ORDER BY wmoId ASC, site ASC";
            final String query2 = "SELECT cccId, hdrTime, bbbId, version "
                    + "FROM stdTextProducts "
                    + "WHERE site = ? AND wmoId = ? AND nnnId = ? AND xxxId = ? "
                    + "ORDER BY cccId ASC, refTime DESC, insertTime DESC";

            try {
                session = getSession();
                c = SessionFactoryUtils.getDataSource(getSessionFactory())
                        .getConnection();
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

                        if ((cccId == null) || (cccId.length() > 0)) {
                            cccId = "-";
                        }
                        if ((bbbId == null) || (bbbId.length() > 0)) {
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
                    if ((product != null) && (product.length() > 0)) {
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
        final String query1 = "SELECT MAX(refTime) " + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL'";
        final String query2 = "SELECT MAX(version) " + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = 'NOA' AND nnnId = 'FOS' AND xxxId = 'PIL' AND refTime = ?";

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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
    public int readLatest(String wmoId, String site, String cccId, String nnnId,
            String xxxId, String hdrTime) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        ResultSet rs1 = null;
        ResultSet rs2 = null;
        int version = -1;
        final String query1 = "SELECT MAX(refTime) " + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ?";
        final String query2 = "SELECT MAX(version) " + "FROM stdTextProducts "
                + "WHERE wmoId = ? AND site = ? AND cccId = ? AND nnnId = ? AND xxxId = ? AND hdrTime = ? AND refTime = ?";

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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
        List<String> retVal = new ArrayList<>();
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
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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

            if ((intlProd != 0) || (count == 0)) {
                /*
                 * no national product found or just for international products
                 * Check products for international WMOID and site
                 */
                version = readLatestIntr(wmoId, site);

                if (version != -1) {
                    String product = read_product(c, wmoId, site, "NOA", "FOS",
                            "PIL", version);

                    if ((product != null) && (product.length() > 0)) {
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

                        if ((bbbId == null) || (bbbId.length() > 0)) {
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
                    if ((product != null) && (product.length() > 0)) {
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
    public List<String> read_wsh(String wmoId, String site,
            long startTimeMillis) {
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
    public List<String> read_wsh(String wmoId, String site,
            int startTimeSeconds) {
        Connection c = null;
        Session session = null;
        PreparedStatement ps1 = null;
        ResultSet rs1 = null;
        String product = null;
        final String query = "SELECT product FROM stdTextProducts "
                + "WHERE wmoId = ? site = ? AND refTime >= ? "
                + "ORDER BY refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
            ps1 = c.prepareStatement(query);
            ps1.setString(1, wmoId);
            ps1.setString(2, site);
            ps1.setInt(3, startTimeSeconds);
            rs1 = ps1.executeQuery();

            while (rs1.next()) {
                product = rs1.getString("product");
                if ((product != null) && (product.length() > 0)) {
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
                + "FROM stdTextProducts " + "WHERE wmoId = ? AND site = ? "
                + "ORDER BY cccId ASC, nnnId ASC, xxxId ASC, refTime DESC, insertTime DESC";
        List<String> retVal = new ArrayList<>();

        try {
            session = getSession();
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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

                if ((bbbId == null) || (bbbId.length() > 0)) {
                    bbbId = "-";
                }
                if (cccId == null) {
                    cccId = "";
                }
                if ((nnnId == null) || (nnnId.length() > 0)) {
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
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

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
                c = SessionFactoryUtils.getDataSource(getSessionFactory())
                        .getConnection();
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

                    if ((bbbId == null) || (bbbId.length() > 0)) {
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
                    if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

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
                c = SessionFactoryUtils.getDataSource(getSessionFactory())
                        .getConnection();

                if ((xxxId == null) || (xxxId.length() > 0)) {
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

                    if ((bbbId == null) || (bbbId.length() > 0)) {
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
                    if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

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
                c = SessionFactoryUtils.getDataSource(getSessionFactory())
                        .getConnection();

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

                    if ((bbbId == null) || (bbbId.length() > 0)) {
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
                    if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();

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
                c = SessionFactoryUtils.getDataSource(getSessionFactory())
                        .getConnection();

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
                    if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();
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
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();

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

                    if ((bbbId == null) || (bbbId.length() == 0)) {
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
                if ((product != null) && (product.length() > 0)) {
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
        List<String> retVal = new ArrayList<>();
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
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();

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
                if ((product != null) && (product.length() > 0)) {
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
            c = SessionFactoryUtils.getDataSource(getSessionFactory())
                    .getConnection();
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
                if ((bbbId == null) || (bbbId.length() == 0)) {
                    bbbId = "-";
                }

                retVal.append(generateHeader(wmoId, site, hdrTime, bbbId, cccId,
                        nnnId, xxxId));
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

        Map<String, String> map = new HashMap<>();

        for (int i = 0; i < strings.length; i += 2) {
            if ((strings[i + 1] != null) && ((strings[i + 1].length() > 0)
                    && !strings[i + 1].startsWith("000"))) {
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
