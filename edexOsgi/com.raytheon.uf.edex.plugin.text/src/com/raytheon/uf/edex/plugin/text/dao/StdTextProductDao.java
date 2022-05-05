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

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
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
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProductId;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfoPK;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.plugin.text.IcaoMap;
import com.raytheon.uf.edex.plugin.text.impl.TextDBStaticData;

/**
 * The dao implementation associated with the TextDao classes used for all
 * database interaction.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer       Description
 * ------------- -------- -------------- ---------------------------------------
 * Sep 04, 0007  400      garmendari     Initial Check in
 * Oct 01, 2008  1538     jkorman        Added additional functionality.
 * Aug 18, 2009  2191     rjpeter        Refactored and added additional
 *                                       functionality.
 * Apr 14, 2010  4734     mhuang         Corrected StdTextProduct import
 *                                       dependency
 * May 21, 2010  2187     cjeanbap       Added operational/test or practice mode
 *                                       functionality.
 * Jul 28, 2010  2187     cjeanbap       Fixed class exception in
 *                                       cccnnnxxxReadVersion.
 * Oct 05, 2010           cjeanbap       Fixed a bug introduced on #2187; return
 *                                       distinct rows.
 * May 23, 2012  14952    rferrel        Added cccnnnxxxByRefTime.
 * Oct 03, 2012  15244    mgamazaychkov  Added the fix to query the appropriate
 *                                       table (operational or practice)
 * May 20, 2014  2536     bclement       moved from edex.textdb to
 *                                       edex.plugin.text
 * Sep 18, 2014  3627     mapeters       Updated deprecated TimeTools usage.
 * Oct 16, 2014  3454     bphillip       Upgrading to Hibernate 4
 * Oct 28, 2014  3454     bphillip       Fix usage of getSession()
 * Jan 27, 2015  4031     rferrel        Resolve AFOS PILs site conflict using
 *                                       preferredAfosFirstLetter.
 * May 05, 2015  4462     rferrel        {@link #write(StdTextProduct)} when
 *                                       missing set the textProduct's site.
 * Jul 06, 2015  4612     rferrel        Get all sites matching the
 *                                       preferredafosFirstLetter.
 * Dec 09, 2015  5166     kbisanz        Update logging to use SLF4J.
 * Feb 15, 2015  4716     rferrel        Added {@link #queryProductList(int,
 *                                       List)} with common transaction code.
 *                                       Use {@link IcaoMap} to determine site.
 * Jun 20, 2016  5679     rjpeter        Fix NPE.
 * Aug 28, 2016  5839     rferrel        Added past version.
 * Nov 17, 2017  19846    hzhang         Fix SIOOBE.
 * Apr 25, 2018  6966     randerso       Added getAfosIdsToPurge(). Code
 *                                       cleanup.
 * Apr 23, 2019  6140     tgurney        Hibernate 5 fixes
 * Sep 09, 2019  6140     randerso       Fix queries with maxResults.
 * Oct 13, 2020  8238     randerso       Fix merge error in getLatestTime()
 * Jan 13, 2021  7864     randerso       Added getter for
 *                                       preferredAfosFirstLetter for use in
 *                                       StdTextSeparator.
 * Mar 02, 2021  22508    mgamazaychikov Added insertTime check for duplicates
 * Nov 09, 2021  22859    zalberts       Added additional sort by datacrc to
 *                                       AFOS_QUERY_STMT for consistent results
 *                                       when reftime and inserttime are equal.
 *
 * </pre>
 *
 * @author garmendariz
 */

public class StdTextProductDao extends CoreDao {
    private static final String PLUGIN_NAME = "text";

    private static final int MAX_FIELD_LENGTH = 3;

    private boolean operationalMode = true;

    private static final String BBB_ID = "bbbid";

    private static final String CCC_ID = "cccid";

    private static final String DATACRC = "datacrc";

    private static final String REFTIME = "refTime";

    private static final String INSERTTIME = "insertTime";

    private static final String ProdHDRTIME = "prodId.hdrtime";

    private static final String ProdNNN_ID = "prodId.nnnid";

    private static final String NNN_ID = "nnnid";

    private static final String SITE_ID = "site";

    private static final String SITES = "sites";

    private static final String PRODUCT = "product";

    private static final String ProdSITE = "prodId.site";

    private static final String WMO_ID = "wmoid";

    private static final String ProdXXX_ID = "prodId.xxxid";

    private static final String XXX_ID = "xxxid";

    private static final String Prod_ID = "prodId";

    private static final String OPERATIONAL_TABLE = "stdtextproducts";

    private static final String PRACTICE_TABLE = "practicestdtextproducts";

    private static final String TM_QUERY_FMT = "select refTime from table_name where cccid='%s' and nnnid='%s' and xxxid='%s';";

    private static final String SITE_QUERY_FMT = "select distinct site from table_name where cccid='%s' and nnnid='%s' and xxxid='%s';";

    private static final String DEFAULT_PREFERRED_AFOS_FIRST_LETTER = "KCPTXM";

    private static final String AFOS_QUERY_STMT = "from StdTextProduct where "
            + CCC_ID + " = :" + CCC_ID

            + " and " + ProdNNN_ID + " = :" + NNN_ID

            + " and " + ProdXXX_ID + " = :" + XXX_ID

            + " and " + ProdSITE + " in (:" + SITES + ")"

            + " order by " + REFTIME + " desc" + ", " + INSERTTIME + " desc"

            + ", " + DATACRC + " desc";

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final char[] preferredAfosFirstLetter;

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

    /**
     * @return array of first station first letters in order of preference
     */
    public static char[] getPreferredafosfirstletter() {
        return preferredAfosFirstLetter;
    }

    private String siteQueryFmt;

    /**
     * Constructs an instance of StdTextProductDao for the operational table
     */
    public StdTextProductDao() {
        this(true);
    }

    /**
     * Constructs an instance of StdTextProductDao for the desired table
     *
     * @param operationalModeFlag
     *            true for operational table, false for practice table
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
        // Queries pad so must pad what is placed in database.
        String ccc = StringUtils.rightPad(textProduct.getCccid(),
                MAX_FIELD_LENGTH);
        String nnn = StringUtils.rightPad(textProduct.getNnnid(),
                MAX_FIELD_LENGTH);
        String xxx = StringUtils.rightPad(textProduct.getXxxid(),
                MAX_FIELD_LENGTH);
        textProduct.setNnnid(nnn);
        textProduct.setXxxid(xxx);
        Session session = this.getSession();
        String site = textProduct.getSite();
        if (StringUtils.isBlank(site)) {
            // Determine product site.
            site = IcaoMap.siteToIcaoId(xxx, textProduct.getSite());
            if (logger.isInfoEnabled()) {
                logger.info("Write \"" + ccc + nnn + xxx + "\" setting site to "
                        + site);
            }
            textProduct.setSite(site);
        }

        try {
            try {
                // check if a duplicate of the textProduct is already stored
                StdTextProduct dupProduct = getDuplicateProduct(textProduct.getProdId());
                if ( dupProduct == null ) {
                    // there is no duplicate stored - save the product
                    create(textProduct);
                    success = true;
                } else {
                    // duplicate found
                    // before discarding the product check if the difference 
                    // between the incoming and stored products time stamps
                    // exceeds the allowed time window
                    Calendar storedProductInsertTime = dupProduct.getInsertTime();
                    Long timeDiff = Math.abs(textProduct.getInsertTime().getTimeInMillis() 
                            - storedProductInsertTime.getTimeInMillis());
                    if (timeDiff > 1000*TextDBStaticData.getDuplicateTimeWindow()) {
                        logger.info("Allowed duplicate detected: discarding previously stored text product "
                                + textProduct.getProdId().toString()
                                + " as duplicate");
                        // remove the old product and store the new one
                        saveOrUpdate(textProduct);
                        success = true;
                    } else {
                        // don't save
                        logger.info("Discarding text product "
                                + textProduct.getProdId().toString()
                                + " as duplicate");
                        success = false;
                    }
                }
            } catch (Exception e) {
                logger.error("Error storing text product", e);
            }

            if (success) {
                try {
                    String cccid = textProduct.getCccid();
                    String nnnid = textProduct.getNnnid();
                    String xxxid = textProduct.getXxxid();
                    Query query = session.createQuery(
                            "SELECT versionstokeep FROM TextProductInfo WHERE "
                                    + "prodId.cccid = :cccid AND prodId.nnnid = :nnnid AND prodId.xxxid = :xxxid");
                    query.setParameter("cccid", cccid);
                    query.setParameter("nnnid", nnnid);
                    query.setParameter("xxxid", xxxid);
                    List<?> results = query.list();
                    if (results == null || results.isEmpty()) {
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

    @SuppressWarnings("unchecked")
    private StdTextProduct getDuplicateProduct(StdTextProductId stdTextProductId) {
        Session session = null;
        List<StdTextProduct> prodList = null;
        try {
            session = getSession();

            Map<String, Object> tmp = new HashMap<>();
            if (stdTextProductId != null) {
                tmp.put(Prod_ID, stdTextProductId);
            }
            Criteria criteria = session.createCriteria(getStdTextProductInstance().getClass());
            criteria.add(Restrictions.allEq(tmp));
            criteria.addOrder(Order.desc(REFTIME));
            prodList = criteria.list();
        } catch (Exception e) {
            logger.error("Error occurred reading products", e);
        } finally {
            closeSession(session);
        }
        if (prodList == null || prodList.isEmpty()) {
            return null;
        } else {
            return prodList.get(0);
        }
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
     * @return list of matching text products
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
                                if (results != null && !results.isEmpty()) {
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
        boolean hasCCC = ccc != null && ccc.length() > 0 && !"000".equals(ccc);
        boolean hasNNN = nnn != null && nnn.length() > 0 && !"000".equals(nnn);
        boolean hasXXX = xxx != null && xxx.length() > 0 && !"000".equals(xxx);
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

                            Map<String, String> map = buildCriterions(CCC_ID,
                                    paddedccc, ProdNNN_ID, paddednnn,
                                    ProdXXX_ID, paddedxxx, ProdSITE, site);
                            Criteria criteria = session
                                    .createCriteria(stdTextProduct.getClass());
                            ProjectionList projList = Projections
                                    .projectionList();
                            projList.add(Projections.property(CCC_ID));
                            projList.add(Projections.property(ProdNNN_ID));
                            projList.add(Projections.property(ProdXXX_ID));
                            projList.add(Projections.property(ProdSITE));
                            criteria.setProjection(
                                    Projections.distinct(projList));
                            criteria.add(Restrictions.allEq(map));
                            criteria.addOrder(Order.asc(CCC_ID));
                            criteria.addOrder(Order.asc(ProdNNN_ID));
                            criteria.addOrder(Order.asc(ProdXXX_ID));

                            List<?> list = criteria.list();
                            if (list != null && !list.isEmpty()) {
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
                    operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE);
        }

        String siteQuery = String.format(siteQueryFmt, ccc, nnn, xxx);

        Object[] values = null;

        try {
            values = executeSQLQuery(siteQuery);
        } catch (Exception e) {
            values = null;
            logger.error("Error getting sites for " + afosId, e);
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
                        if (site != null && !site.isEmpty()
                                && site.charAt(0) == c) {
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
                tmp.put(CCC_ID, ccc);
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
            criteria.addOrder(Order.asc(CCC_ID));
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

            Map<String, String> tmp = buildCriterions(CCC_ID, ccc, ProdNNN_ID,
                    nnn, ProdXXX_ID, xxx, ProdSITE, site);
            long searchTime = System.currentTimeMillis()
                    - pastHours * TimeUtil.MILLIS_PER_HOUR;

            Criteria criteria = session
                    .createCriteria(getStdTextProductInstance().getClass());
            criteria.add(Restrictions.allEq(tmp));
            criteria.add(Restrictions.gt(REFTIME, new Long(searchTime)));
            criteria.addOrder(Order.asc(CCC_ID));
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

        if (prodList == null || prodList.isEmpty()) {
            products = new ArrayList<>(0);
        } else {
            products = new ArrayList<>(prodList.size());

            Iterator<?> iter = prodList.iterator();
            Iterator<Pair<String[], AFOSProductId>> siteAfosIdIter = siteAfosIdList
                    .iterator();
            Pair<String[], AFOSProductId> pair = siteAfosIdIter.next();

            while (iter.hasNext()) {
                StdTextProduct prod = (StdTextProduct) iter.next();

                if (prod != null && prod.getProduct() != null
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
                operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE);

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
            logger.error("Error getting latest times for " + afosId, e);
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

            Map<String, String> tmp = buildCriterions(CCC_ID, afosId.getCcc(),
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
     * @param ccc
     * @param nnn
     * @param xxx
     * @return the refTimes of all matching products
     */
    public List<Long> getAllTimes(String ccc, String nnn, String xxx) {
        Matcher m = Pattern.compile("table_name").matcher(TM_QUERY_FMT);
        String tempQuery = m.replaceAll(
                operationalMode ? OPERATIONAL_TABLE : PRACTICE_TABLE);

        List<Long> times = new ArrayList<>();

        String query = String.format(tempQuery, ccc, nnn, xxx);

        Object[] values = null;

        values = executeSQLQuery(query);

        if (values != null) {
            for (Object o : values) {
                Number val = (Number) o;
                times.add(val.longValue());
            }
        }

        return times;
    }

    /**
     * Get the list of distinct AFOS IDs that have been inserted since the last
     * incremental purge
     *
     * @param lastPurgeTime
     * @return the AFOS IDs to purge
     */
    public List<AFOSProductId> getAfosIdsToPurge(Long lastPurgeTime) {
        return getDistinctAfosIds(lastPurgeTime);
    }

    /**
     *
     * @param wmoId
     * @param siteId
     * @param hdrTime
     * @param afosId
     * @return list of strings containing the product text
     */
    public List<String> getSameMinuteProducts(String wmoId, String siteId,
            int hdrTime, AFOSProductId afosId) {
        List<String> products = new ArrayList<>();

        Session session = null;
        try {
            session = getSession();

            Map<String, String> tmp = buildCriterions(CCC_ID, afosId.getCcc(),
                    ProdNNN_ID, afosId.getNnn(), ProdXXX_ID, afosId.getXxx(),
                    ProdSITE, siteId, WMO_ID, wmoId, ProdHDRTIME,
                    Integer.toString(hdrTime));

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

    private List<AFOSProductId> getDistinctAfosIds(Long lastPurgeTime)
            throws HibernateException {
        return txTemplate
                .execute(new TransactionCallback<List<AFOSProductId>>() {
                    @Override
                    public List<AFOSProductId> doInTransaction(
                            TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess.createCriteria(operationalMode
                                ? OperationalStdTextProduct.class
                                : PracticeStdTextProduct.class);
                        ProjectionList fields = Projections.projectionList();
                        fields.add(Projections.property(CCC_ID));
                        fields.add(Projections.property(ProdNNN_ID));
                        fields.add(Projections.property(ProdXXX_ID));
                        crit.setProjection(Projections.distinct(fields));
                        crit.addOrder(Order.asc(CCC_ID));
                        crit.addOrder(Order.asc(ProdNNN_ID));
                        crit.addOrder(Order.asc(ProdXXX_ID));
                        if (lastPurgeTime != null) {
                            Calendar cal = TimeUtil
                                    .newGmtCalendar(lastPurgeTime);
                            Criterion criterion = Restrictions.gt(INSERTTIME,
                                    cal);
                            crit.add(criterion);
                        }

                        List<?> results = crit.list();
                        List<AFOSProductId> products = Collections.emptyList();
                        if (results != null && !results.isEmpty()) {
                            products = new ArrayList<>(results.size());
                            String cccid = null;
                            String nnnid = null;
                            String xxxid = null;
                            for (Object row : results) {
                                Object[] cols = (Object[]) row;
                                cccid = cols[0].toString();
                                nnnid = cols[1].toString();
                                xxxid = cols[2].toString();
                                products.add(
                                        new AFOSProductId(cccid, nnnid, xxxid));
                            }
                        }

                        return products;
                    }
                });
    }

    /**
     * Simple purge routine. Deletes all data that has a refTime older than that
     * specified.
     *
     * @return the number of records purged
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
     * @return number of rows deleted
     */
    public int versionPurge(AFOSProductId afosId) {
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
            } else {
                TextProductInfo tpi = prodInfoDao.find(afosId.getCcc(),
                        afosId.getNnn(), afosId.getXxx());
                if (tpi != null) {
                    ids = new ArrayList<>(1);
                    ids.add(tpi);
                }
            }

            if (ids != null && !ids.isEmpty()) {

                StringBuilder refTimeQueryBuilder = new StringBuilder(200);
                refTimeQueryBuilder.append("SELECT refTime FROM ");
                refTimeQueryBuilder.append(
                        getStdTextProductInstance().getClass().getSimpleName());
                refTimeQueryBuilder.append(" WHERE ");
                refTimeQueryBuilder.append(CCC_ID).append(" = :cccid")
                        .append(" AND ");
                refTimeQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                        .append(" AND ");
                refTimeQueryBuilder.append(ProdXXX_ID).append(" = :xxxid");
                refTimeQueryBuilder.append(" ORDER BY refTime DESC");
                final String refTimeQueryString = refTimeQueryBuilder
                        .toString();

                StringBuilder delQueryBuilder = new StringBuilder(200);
                delQueryBuilder.append("DELETE FROM ");
                delQueryBuilder.append(
                        getStdTextProductInstance().getClass().getSimpleName());
                delQueryBuilder.append(" WHERE ");
                delQueryBuilder.append(CCC_ID).append(" = :cccid")
                        .append(" AND ");
                delQueryBuilder.append(ProdNNN_ID).append(" = :nnnid")
                        .append(" AND ");
                delQueryBuilder.append(ProdXXX_ID).append(" = :xxxid")
                        .append(" AND ");
                delQueryBuilder.append("refTime < :refTime");
                final String delQueryString = delQueryBuilder.toString();

                for (final TextProductInfo prodInfo : ids) {
                    // if edex is trying to shut down, exit now
                    if (EDEXUtil.isShuttingDown()) {
                        break;
                    }

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

                                        /*
                                         * if versions to keep is 0 we want an
                                         * unlimited query so we get all the
                                         * reftimes
                                         */
                                        int versionsToKeep = prodInfo
                                                .getVersionstokeep();
                                        refTimeQuery
                                                .setMaxResults(versionsToKeep);
                                        List<?> refTimes = refTimeQuery.list();
                                        if (refTimes.size() >= versionsToKeep) {
                                            long refTime = ((Number) refTimes
                                                    .get(versionsToKeep - 1))
                                                            .longValue();
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
                                                PurgeLogger.logDebug("Purged ["
                                                        + rowsDeleted
                                                        + "] records for ["
                                                        + cccid + nnnid + xxxid
                                                        + "]", PLUGIN_NAME);
                                            }
                                        } else if (PurgeLogger
                                                .isDebugEnabled()) {
                                            PurgeLogger.logDebug(
                                                    "VersionPurge: Product ["
                                                            + cccid + nnnid
                                                            + xxxid
                                                            + "] has fewer than ["
                                                            + versionsToKeep
                                                            + "] versions",
                                                    PLUGIN_NAME);
                                        }
                                    } catch (Exception e) {
                                        PurgeLogger.logError(
                                                "Exception occurred purging text products ["
                                                        + cccid + nnnid + xxxid
                                                        + "]",
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

    /**
     * Initialize the TextProductInfo table if necessary
     */
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

            oldLock = ClusterLockUtils.lookupLock("TextPurge",
                    "ConfiguredSite");
        } while (oldLock.isRunning()
                && oldLock.getLastExecution() > System.currentTimeMillis()
                        - 6 * TimeUtil.MILLIS_PER_SECOND);

        String ccc = EDEXUtil.getEdexSite();
        if (!ccc.equals(oldLock.getExtraInfo()) || oldLock.isRunning()) {
            ClusterLockUtils.lock("TextPurge", "ConfiguredSite", ccc,
                    TimeUtil.MILLIS_PER_MINUTE, true);
            PurgeLogger.logInfo("Localizing TextProductInfo for site " + ccc,
                    PLUGIN_NAME);
            TextProductInfoDao textInfoDao = new TextProductInfoDao();
            try {
                textInfoDao.purgeTable();

                // grab the distinct afos ids
                List<AFOSProductId> ids = getDistinctAfosIds(null);
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
     * @param pastVersion
     * @param startTimeMillis
     * @param bbb
     * @param intlProd
     * @param readAllVersions
     * @param returnAllData
     * @return the desired text products
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

            if (intlProd == 1 || intlProd == 3) {
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

            Map<String, String> map = buildCriterions(WMO_ID, wmoId, ProdSITE,
                    site, CCC_ID, ccc, ProdNNN_ID, nnn, ProdXXX_ID, xxx, BBB_ID,
                    bbb, ProdHDRTIME, hdrTime);

            ProjectionList projectionList = Projections.projectionList();
            projectionList
                    .add(Projections.distinct(Projections.property(WMO_ID)));
            projectionList.add(Projections.max(REFTIME));
            projectionList.add(Projections.groupProperty(WMO_ID));
            projectionList.add(Projections.groupProperty(ProdSITE));
            projectionList.add(Projections.groupProperty(CCC_ID));
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
                        (String) ((Object[]) products.toArray()[i])[0]);
                stdTextProduct.setRefTime(
                        (Long) ((Object[]) products.toArray()[i])[1]);
                stdTextProduct.setSite(
                        (String) ((Object[]) products.toArray()[i])[3]);
                stdTextProduct.setCccid(
                        (String) ((Object[]) products.toArray()[i])[4]);
                stdTextProduct.setNnnid(
                        (String) ((Object[]) products.toArray()[i])[5]);
                stdTextProduct.setXxxid(
                        (String) ((Object[]) products.toArray()[i])[6]);
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

    private List<StdTextProduct> getProductMetaData(Connection conn,
            List<StdTextProduct> products, String bbb, String hdrTime,
            Long startTimeMillis, boolean readAllVersions) throws SQLException {

        List<StdTextProduct> rval = new ArrayList<>();
        Session session = null;

        try {
            session = getSession();

            for (StdTextProduct p : products) {

                Map<String, String> map = buildCriterions(WMO_ID, p.getWmoid(),
                        ProdSITE, p.getSite());
                map.putAll(buildCriterions(CCC_ID, p.getCccid(), ProdNNN_ID,
                        p.getNnnid(), ProdXXX_ID, p.getXxxid()));
                map.putAll(buildCriterions(BBB_ID, bbb, ProdHDRTIME, hdrTime));

                ProjectionList projectionList = Projections.projectionList();
                projectionList.add(
                        Projections.distinct(Projections.property(BBB_ID)));
                projectionList.add(Projections.property(WMO_ID));
                projectionList.add(Projections.property(REFTIME));
                projectionList.add(Projections.property(ProdSITE));
                projectionList.add(Projections.property(CCC_ID));
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
                List<StdTextProduct> tmpProducts = new ArrayList<>();
                for (int i = 0; i < list.size(); i++) {
                    StdTextProduct stdTextProduct = getStdTextProductInstance();
                    stdTextProduct.setBbbid(
                            (String) ((Object[]) list.toArray()[i])[0]);
                    stdTextProduct.setWmoid(
                            (String) ((Object[]) list.toArray()[i])[1]);
                    stdTextProduct.setRefTime(
                            (Long) ((Object[]) list.toArray()[i])[2]);
                    stdTextProduct.setSite(
                            (String) ((Object[]) list.toArray()[i])[3]);
                    stdTextProduct.setCccid(
                            (String) ((Object[]) list.toArray()[i])[4]);
                    stdTextProduct.setNnnid(
                            (String) ((Object[]) list.toArray()[i])[5]);
                    stdTextProduct.setXxxid(
                            (String) ((Object[]) list.toArray()[i])[6]);
                    stdTextProduct.setHdrtime(
                            (String) ((Object[]) list.toArray()[i])[7]);
                    stdTextProduct.setProduct(
                            (String) ((Object[]) list.toArray()[i])[8]);
                    tmpProducts.add(stdTextProduct);
                }
                list.clear();
                list.addAll(tmpProducts);

                if (!list.isEmpty()) {
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

    /**
     * Build a SQL criterion list based on name value pairs; eg ({"cccid",
     * "KWBC"}, {"nnnid", ""}, {"xxxid", ""}) that will be assigned all the same
     * Restriction type (alleq, ge, gt, etc..... ).
     *
     * @param strings
     *            a comma separated string containing a field name and its
     *            value; field name, value, field name, value......
     * @return a map of the name value pairs.
     */
    private Map<String, String> buildCriterions(String... strings) {

        Map<String, String> map = new HashMap<>();

        for (int i = 0; i < strings.length; i += 2) {
            if (strings[i + 1] != null && strings[i + 1].length() > 0
                    && !strings[i + 1].startsWith("000")) {
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
        return this.operationalMode ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct();
    }
}
