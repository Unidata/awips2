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
package com.raytheon.uf.edex.plugin.text.db;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StateMatch;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.util.AFOSParser;
import com.raytheon.uf.common.dataplugin.text.util.AWIPSParser;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.plugin.text.IcaoMap;
import com.raytheon.uf.edex.plugin.text.dao.StateMatchDao;
import com.raytheon.uf.edex.plugin.text.dao.StdTextProductDao;
import com.raytheon.uf.edex.plugin.text.dao.TextProductInfoDao;
import com.raytheon.uf.edex.plugin.text.impl.WMOReportData;

/**
 * Text Database.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Oct 01, 2008  1538     jkorman      Initial creation
 * 20090327      2151     jkorman      Modified writeProduct to account for
 *                                     possible leading carriage control.
 * Jul 10, 2009  2191     rjpeter      Add additional methods.
 * Aug 18, 2009  2191     rjpeter      Switched to version purging.
 * Feb 15, 2010  4426     MW Fegan     Use default CCC ID when not provided in
 *                                     write.
 * Apr 14, 2010  4734     mhuang       Corrected StdTextProduct import
 *                                     dependency
 * Jun 01, 2010           cjeanbap     Added operational mode functionality.
 * Jul 09, 2010  2187     cjeanbap     Added additional operational mode
 *                                     functionality.
 * Aug 02, 2010  2187     cjeanbap     Added parameter constructor.
 * Aug 09, 2010  3944     cjeanbap     Add new method, queryAllWatchWarn.
 * Nov 08, 2010  7294     cjeanbap     Update logic in executeAFOSCommand.
 *                                     Removed commented out code.
 * Apr 18, 2012  479      jkorman      Modified to pad xxxid to 3 characters in
 *                                     queries.
 * May 23, 2012  14952    rferrel      Allow queries with refTime.
 * Feb 18, 2014  2652     skorolev     Corrected writeProduct for WMO header if
 *                                     archive is allowed. Deleted unused code.
 * May 14, 2014  2536     bclement     moved WMO Header to common, removed
 *                                     TimeTools usage
 * May 15, 2014  2536     bclement     moved asciiToHex() hexToAscii() and
 *                                     getProperty() to PropConverter
 * May 20, 2014  2536     bclement     moved from edex.textdb to
 *                                     edex.plugin.text
 * Jul 10, 2014  2914     garmendariz  Remove EnvProperties
 * Dec 09, 2015  5166     kbisanz      Update logging to use SLF4J.
 * Feb 05, 2016  5269     skorolev     Removed WatchWarn methods.
 * Feb 12, 2016  4716     rferrel      Modified readAwips to get all hdrTimes
 *                                     when hdrTimes value is "ALL".
 * Aug 28, 2016  5839     rferrel      Added past version.
 * Apr 25, 2018  6966     randerso     Code cleanup.
 * Aug 15, 2018  7197     randerso     Changed executeAWIPSCommand to leave site
 *                                     null if it comes in that way.
 *
 * </pre>
 *
 * @author jkorman
 */

public class TextDB {

    private Logger logger = LoggerFactory.getLogger(getClass());

    private String siteName = SiteUtil.getSite();

    private boolean operationalMode = true;

    /**
     * Constructor.
     */
    public TextDB() {
    }

    /**
     * Text database.
     *
     * @param operationalMode
     */
    public TextDB(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }

    /**
     * Purge Standard Text Products.
     *
     * @param operationalMode
     *            true for the operational table, false for practice
     *
     * @return total products updated
     */
    public static int purgeStdTextProducts(boolean operationalMode) {
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        int totalProducts = dao.versionPurge();
        return totalProducts;
    }

    /**
     * Returns a List of String objects representing headers or full products
     * that matched the passed parameters.
     *
     * @param wmoId
     * @param site
     * @param requestType
     *            contains Request Type
     * @param abbrId
     * @param lastHrs
     * @param hdrTime
     * @param pastVersion
     * @param bbbId
     * @return standard text products
     */
    public List<StdTextProduct> readAwips(RequestType requestType, String wmoId,
            String site, String abbrId, String lastHrs, String hdrTime,
            String pastVersion, String bbbId) {
        int intlProd = 0;

        switch (requestType) {
        case DUPCK0: {
            intlProd = 0;
            break;
        }
        case DUPCK1: {
            intlProd = 1;
            break;
        }
        case DUPCK2: {
            intlProd = 3;
            break;
        }
        default: {
            intlProd = 2;
            break;
        }
        }

        return readAwips(wmoId, site, intlProd, abbrId, lastHrs, hdrTime,
                pastVersion, bbbId, false, operationalMode);
    }

    /**
     * Returns a List of String objects representing headers or full products
     * that matched the passed parameters.
     *
     * TODO: Verify if optimization is needed to act like previous system and
     * not return full product.
     *
     * @param wmoId
     * @param site
     * @param intlProd
     * @param abbrId
     * @param lastHrs
     * @param hdrTime
     * @param pastVersion
     * @param bbbId
     * @param fullDataRead
     * @param operationalMode
     * @return specified products
     */
    public List<StdTextProduct> readAwips(String wmoId, String site,
            int intlProd, String abbrId, String lastHrs, String hdrTime,
            String pastVersion, String bbbId, boolean fullDataRead,
            boolean operationalMode) {
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        String nnn = null;
        String xxx = null;
        Long startTimeMillis = null;
        boolean readAllVersions = false;

        if (!StringUtils.isEmpty(abbrId)) {
            if (abbrId.length() >= 3) {
                nnn = abbrId.substring(0, 3);
            }
            if (abbrId.length() > 3) {
                xxx = abbrId.substring(3,
                        (abbrId.length() < 6 ? abbrId.length() : 6));
            }
        }

        if (!StringUtils.isEmpty(hdrTime)) {
            if ("000000".equals(hdrTime)) {
                readAllVersions = true;
                hdrTime = null;
            }
        }
        if (!StringUtils.isEmpty(lastHrs)) {
            int hours = 0;

            try {
                hours = Integer.parseInt(lastHrs);
            } catch (NumberFormatException e) {
                // ignore
            }

            if (hours <= 0) {
                hours = 1;
            }

            /*
             * getting as GMT technically not necessary since converting to
             * millis anyway.
             */
            Calendar currentTime = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT"));

            // subtract the hours
            currentTime.add(Calendar.HOUR_OF_DAY, -hours);
            startTimeMillis = currentTime.getTimeInMillis();
        }

        return dao.awipsRead(wmoId, site, nnn, xxx, hdrTime, pastVersion,
                startTimeMillis, bbbId, intlProd, readAllVersions,
                fullDataRead);
    }

    /**
     * Add Versions
     *
     * @param ccc
     * @param nnn
     * @param xxx
     * @param versions
     *            contains number of versions to keep.
     * @return success
     */
    public boolean addVersions(String ccc, String nnn, String xxx,
            int versions) {
        boolean success = false;
        TextProductInfoDao dao = new TextProductInfoDao();
        try {
            TextProductInfo info = dao.find(ccc, nnn, xxx);
            if (info == null) {
                info = new TextProductInfo(ccc, nnn, xxx);
            }
            info.setVersionstokeep(versions);
            dao.persist(info);
            success = true;
        } catch (Exception e) {
            String msg = "Error persisting versions to keep: ccc=" + ccc
                    + ", nnn=" + nnn + ", xxx=" + xxx + ", versions="
                    + versions;
            logger.error(msg, e);
        }
        return success;
    }

    /**
     * Get versions.
     *
     * @param ccc
     * @param nnn
     * @param xxx
     * @return number of versions to keep
     */
    public String getVersions(String ccc, String nnn, String xxx) {
        String rval = null;
        TextProductInfoDao dao = new TextProductInfoDao();
        try {
            TextProductInfo info = dao.find(ccc, nnn, xxx);
            if (info != null) {
                rval = Integer.toString(info.getVersionstokeep());
            }
        } catch (Exception e) {
            String msg = "Error retrieving versions: ccc=" + ccc + ", nnn="
                    + nnn + ", xxx=" + xxx;
            logger.error(msg, e);
        }

        return rval;
    }

    /**
     * Deletes the ccc,nnn,xxx entry from the textProductInfo table.
     *
     * @param ccc
     * @param nnn
     * @param xxx
     * @return success
     */
    public boolean deleteVersions(String ccc, String nnn, String xxx) {
        boolean success = false;

        TextProductInfoDao dao = new TextProductInfoDao();
        try {
            TextProductInfo info = dao.find(ccc, nnn, xxx);
            if (info != null) {
                dao.delete(info);
            }
            success = true;
        } catch (Exception e) {
            String msg = "Error deleting versions: ccc=" + ccc + ", nnn=" + nnn
                    + ", xxx=" + xxx;
            logger.error(msg, e);
        }
        return success;
    }

    /**
     * --- statematch Add a mapping for state -> (ccc and xxx)
     *
     * @param stateInfo
     * @return success
     */
    public boolean addState(StateMatch stateInfo) {
        boolean success = false;

        StateMatchDao dao = new StateMatchDao();
        try {
            success = dao.addState(stateInfo);
        } catch (Exception e) {
            String msg = "Error adding mapping for state: stateInfo="
                    + stateInfo;

            logger.error(msg, e);
        }
        return success;
    }

    /**
     * --- statematch Add a mapping for state -> (ccc and xxx)
     *
     * @param stateId
     * @param cccId
     * @param xxxId
     * @return success
     */
    public boolean addState(String stateId, String cccId, String xxxId) {
        StateMatch stateInfo = new StateMatch(stateId, xxxId, cccId);
        return addState(stateInfo);
    }

    /**
     * --- statematch Remove a mapping for state -> (ccc and xxx)
     *
     * @param stateInfo
     * @return success
     */
    public boolean removeState(StateMatch stateInfo) {
        boolean success = false;
        StateMatchDao dao = new StateMatchDao();
        try {
            success = dao.removeState(stateInfo);
        } catch (Exception e) {
            String msg = "Error removing mapping for state: stateInfo="
                    + stateInfo;
            logger.error(msg, e);
        }
        return success;
    }

    /**
     * --- statematch Remove a mapping for state -> (ccc and xxx)
     *
     * @param stateId
     * @param xxxId
     * @param cccId
     * @return success
     */
    public boolean removeState(String stateId, String xxxId, String cccId) {
        StateMatch stateInfo = new StateMatch(stateId, xxxId, cccId);
        return removeState(stateInfo);
    }

    /**
     * --- statematch
     *
     * @param state
     * @return stateList
     */
    public List<StateMatch> queryState(String state) {
        List<StateMatch> stateList = null;
        StateMatchDao dao = new StateMatchDao();
        try {
            stateList = dao.queryState(state);
        } catch (Exception e) {
            String msg = "Error retrieving mappings for state: state=" + state;
            logger.error(msg, e);
        }

        return stateList;
    }

    /**
     * Get the latest time for each product in the stdtextproducts table.
     *
     * @param afosIds
     *            List of product ids to query for.
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return List of the latest time.
     */
    public List<Long> getLatestTimes(List<AFOSProductId> afosIds,
            boolean operationalMode) {
        List<Long> times = new ArrayList<>();

        for (AFOSProductId id : afosIds) {
            times.add(getLatestTime(id, operationalMode));
        }
        return times;
    }

    /**
     * Get the latest time for each product in the stdtextproducts table.
     *
     * @param afosId
     *            product id to query for.
     * @param operationalMode
     *            true for Operational table, false for Practice
     * @return list of the latest time
     */
    public List<Long> getLatestTimes(AFOSProductId afosId,
            boolean operationalMode) {
        List<Long> times = null;

        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        try {
            times = dao.getLatestTimes(afosId);
        } catch (Exception e) {
            String msg = "Error getting latest times: afosId=" + afosId
                    + ", operationalMode=" + operationalMode;
            logger.error(msg, e);
        }

        return times;
    }

    /**
     * Get the latest time for one or more products in the stdtextproducts
     * table.
     *
     * @param afosId
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return latest time
     */
    public Long getLatestTime(AFOSProductId afosId, boolean operationalMode) {

        Long latestTime = 0L;

        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        try {
            latestTime = dao.getLatestTime(afosId);
        } catch (Exception e) {
            String msg = "Error getting latest time: afosId=" + afosId
                    + ", operationalMode=" + operationalMode;
            logger.error(msg, e);
        }

        return latestTime;
    }

    /**
     * Get the latest time for one or more products in the stdtextproducts
     * table.
     *
     * @param productId
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return latest time
     */
    public Long getLatestTime(String productId, boolean operationalMode) {

        // Need to construct an AFOSProductId from the productId
        String ccc = productId.substring(0, 3);
        String nnn = productId.substring(3, 6);
        String xxx = productId.substring(6);
        if (xxx.length() == 1) {
            xxx = xxx + "  ";
        } else if (xxx.length() == 2) {
            xxx = xxx + " ";
        }

        AFOSProductId afosId = new AFOSProductId(ccc, nnn, xxx);

        return getLatestTime(afosId, operationalMode);
    }

    /**
     * Get all times for one product in the stdtextproducts table.
     *
     * @param afosId
     *            Product id to query for.
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return List of time
     */
    public List<Long> getAllTimes(AFOSProductId afosId,
            boolean operationalMode) {
        return getAllTimes(afosId.getCcc(), afosId.getNnn(), afosId.getXxx(),
                operationalMode);
    }

    /**
     * Get all times for one product in the stdtextproducts table.
     *
     * @param productId
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return List of time
     */
    public List<Long> getAllTimes(String productId, boolean operationalMode) {

        // Need to construct an AFOSProductId from the productId
        String ccc = productId.substring(0, 3);
        String nnn = productId.substring(3, 6);
        String xxx = productId.substring(6);
        if (xxx.length() == 1) {
            xxx = xxx + "  ";
        } else if (xxx.length() == 2) {
            xxx = xxx + " ";
        }

        return getAllTimes(ccc, nnn, xxx, operationalMode);
    }

    /**
     * Get all times for one product in the stdtextproducts table.
     *
     * @param ccc
     * @param nnn
     * @param xxx
     * @param operationalMode
     *            true, read data from operationalstdtextproduct table
     * @return List of time
     */
    public List<Long> getAllTimes(String ccc, String nnn, String xxx,
            boolean operationalMode) {
        List<Long> times = null;

        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        try {
            times = dao.getAllTimes(ccc, nnn, xxx);
        } catch (Exception e) {
            String msg = "Error retrieving all times: ccc=" + ccc + ", nnn="
                    + nnn + ", xxx=" + xxx + ", operationalMode="
                    + operationalMode;
            logger.error(msg, e);
        }

        return times;
    }

    /**
     * Get Same Minute Products
     *
     * @param wmoId
     * @param siteId
     * @param hdrTime
     * @param afosId
     * @param operationalMode
     * @return products
     */
    public List<String> getSameMinuteProducts(String wmoId, String siteId,
            int hdrTime, AFOSProductId afosId, boolean operationalMode) {
        List<String> products = null;
        StdTextProductDao dao = new StdTextProductDao(operationalMode);

        try {
            products = dao.getSameMinuteProducts(wmoId, siteId, hdrTime,
                    afosId);
        } catch (Exception e) {
            String msg = "Error getting products within the same minute: wmoId="
                    + wmoId + ", siteId=" + siteId + ", hdrTime=" + hdrTime
                    + ", afosId=" + afosId + ", operationalMode="
                    + operationalMode;
            logger.error(msg, e);
        }

        return products;
    }

    /**
     *
     * @param cccId
     * @param nnnId
     * @param xxxId
     * @param wmoId
     * @return sites
     */
    @Deprecated
    public List<String> siteRead(String cccId, String nnnId, String xxxId,
            String wmoId) {
        /*
         * This looks like a stub method that was never completed and it appears
         * to only be called by the siteRead method below.
         */
        List<String> sites = null;

        return sites;
    }

    /**
     *
     * Get the local prefix for a WFO localization.
     *
     * @return The local prefix
     */
    public String getLocal() {
        return siteName;
    }

    /**
     * Reads products from list of AFOS commands from a state/nnn query.
     *
     * @param state
     *            Two character state abbrevation.
     * @param nnn
     *            The category to use for the commands.
     * @param operationalMode
     * @return List of Standard text products.
     */
    public List<StdTextProduct> stateNNNRead(String state, String nnn,
            boolean operationalMode) {
        List<StdTextProduct> products = new ArrayList<>();
        StateMatchDao stateDao = new StateMatchDao();

        List<AFOSProductId> cmds = stateDao.makeAFOSCommands(state, nnn);
        StdTextProductDao dao = new StdTextProductDao(operationalMode);

        for (AFOSProductId cmd : cmds) {
            products.addAll(dao.cccnnnxxxReadVersion(cmd.getCcc(), cmd.getNnn(),
                    cmd.getXxx(), 0));
        }

        return products;
    }

    /**
     * Execute AWIPS command.
     *
     * @param awipsCommand
     * @param site
     * @param operationalMode
     * @return products
     */
    public List<StdTextProduct> executeAWIPSCommand(String awipsCommand,
            String site, boolean operationalMode) {
        return executeAWIPSCommand(awipsCommand, site, operationalMode, false,
                null);
    }

    /**
     * Execute AWIPS command
     *
     * @param awipsCommand
     * @param site
     * @param operationalMode
     * @param refTimeMode
     * @param refTime
     * @return products
     */
    public List<StdTextProduct> executeAWIPSCommand(String awipsCommand,
            String site, boolean operationalMode, boolean refTimeMode,
            Long refTime) {
        logger.info(String.format(
                "executeAWIPSCommand(\"%s\", \"%s\", %s, %s, \"%s\")",
                awipsCommand, site, Boolean.toString(operationalMode),
                Boolean.toString(refTimeMode), refTime));
        List<StdTextProduct> products = null;
        AWIPSParser parser = new AWIPSParser(awipsCommand, site);
        if (parser.isValidCommand()) {
            String cccc = parser.getSite();
            String nnn = parser.getNnn();
            String xxx = parser.getXxx();

            // much cleaner version
            if (parser.isStateQuery()) {
                products = stateNNNRead(parser.getState(), parser.getNnn(),
                        operationalMode);
            } else if (refTimeMode) {
                products = (new StdTextProductDao(operationalMode))
                        .cccnnnxxxSiteByRefTime(null, nnn, xxx, refTime, cccc);
            } else {
                // default version number; read the latest version
                int versionNo = 0;

                if (parser.isPastVers() || parser.isAllVersions()) {
                    if (parser.isPastVers()) {
                        versionNo = parser.getPastVersNumber();
                    } else {
                        versionNo = -1;
                    }
                    products = (new StdTextProductDao(operationalMode))
                            .ccccnnnxxxReadVersion(cccc, nnn, xxx, versionNo);
                } else if (parser.isPastHours()) {
                    versionNo = parser.getPastNumberHours();
                    products = (new StdTextProductDao(operationalMode))
                            .cccnnnxxxSiteReadPreviousHours(null, nnn, xxx,
                                    versionNo, cccc);
                } else {
                    // read latest version
                    StdTextProductDao dao = new StdTextProductDao(
                            operationalMode);
                    products = dao.ccccnnnxxxReadVersion(cccc, nnn, xxx,
                            versionNo);
                }
            }
        }
        if (products == null) {
            products = new ArrayList<>();
        }
        return products;
    }

    /**
     * Execute AFOS Command.
     *
     * @param afosCommand
     * @param locale
     *            contains a local prefix for a WFO localization.
     * @param operationalMode
     * @return List of Standard text products.
     */
    public List<StdTextProduct> executeAFOSCommand(String afosCommand,
            String locale, boolean operationalMode) {
        return executeAFOSCommand(afosCommand, locale, operationalMode, false,
                null);
    }

    /**
     * Execute AFOS Command.
     *
     * @param afosCommand
     * @param locale
     *            contains a local prefix for a WFO localization.
     * @param operationalMode
     * @param refTimeMode
     * @param refTime
     * @return List of Standard text products.
     */
    public List<StdTextProduct> executeAFOSCommand(String afosCommand,
            String locale, boolean operationalMode, boolean refTimeMode,
            Long refTime) {
        logger.info("executeAFOSCommand: " + afosCommand);
        List<StdTextProduct> products = null;

        AFOSParser parser = null;
        if (locale == null) {
            parser = new AFOSParser(afosCommand, getLocal());
        } else {
            parser = new AFOSParser(afosCommand, locale);
        }
        if (parser.isValidCommand()) {
            String ccc = parser.getCcc();
            String nnn = parser.getNnn();
            String xxx = parser.getXxx();

            // much cleaner version
            if (parser.isStateQuery()) {
                products = stateNNNRead(parser.getState(), parser.getNnn(),
                        operationalMode);
            } else if (refTimeMode) {
                products = (new StdTextProductDao(operationalMode))
                        .cccnnnxxxByRefTime(ccc, nnn, xxx, refTime);
            } else {
                // default version number; read the latest version
                int versionNo = 0;

                if (parser.isPastVers() || parser.isAllVersions()) {
                    if (parser.isPastVers()) {
                        versionNo = parser.getPastVersNumber();
                    } else {
                        versionNo = -1;
                    }
                    products = (new StdTextProductDao(operationalMode))
                            .cccnnnxxxReadVersion(ccc, nnn, xxx, versionNo);
                } else if (parser.isPastHours()) {
                    versionNo = parser.getPastNumberHours();
                    products = (new StdTextProductDao(operationalMode))
                            .cccnnnxxxReadPreviousHours(ccc, nnn, xxx,
                                    versionNo);
                } else {
                    // read latest version
                    StdTextProductDao dao = new StdTextProductDao(
                            operationalMode);
                    products = dao.cccnnnxxxReadVersion(ccc, nnn, xxx,
                            versionNo);
                }
            }
        }
        if (products == null) {
            products = new ArrayList<>();
        }
        return products;
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. This version takes a StdTextProduct
     * object containing the data.
     *
     * @param textProduct
     *            contains the text product to write to the database
     * @return success
     */
    public boolean writeProduct(StdTextProduct textProduct) {
        boolean operationalMode = (textProduct instanceof OperationalStdTextProduct
                ? true : false);
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        boolean success = false;
        try {
            if (textProduct.getRefTime() == null) {
                textProduct.setRefTime(System.currentTimeMillis());
            }
            textProduct.setInsertTime(
                    Calendar.getInstance(TimeZone.getTimeZone("GMT")));
            success = dao.write(textProduct);
        } catch (Exception e) {
            String msg = "Error writing product: textProduct=" + textProduct;
            logger.error(msg, e);
        }

        if (logger.isDebugEnabled()) {
            if (success) {
                logger.debug("StdTextProduct [" + textProduct.getCccid()
                + textProduct.getNnnid() + textProduct.getXxxid()
                + "] saved");
            } else {
                logger.debug("StdTextProduct [" + textProduct.getCccid()
                + textProduct.getNnnid() + textProduct.getXxxid()
                + "] not saved");
            }
        }

        return success;
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. This version takes a WMOHeader object, an
     * AFOSProductId object, and the text to insert.
     *
     * @param header
     *            contains the WMO header
     * @param prodId
     *            contains the AFOS PIL
     * @param reportData
     *            the body of the product
     * @param operationalMode
     * @return write time
     */
    public long writeProduct(WMOHeader header, AFOSProductId prodId,
            String reportData, boolean operationalMode) {

        String wmoid = null;
        String siteid = null;
        String hdrTime = null;
        String bbbIndicator = null;
        StringBuilder product = new StringBuilder();
        String cccc = null;

        if ((header != null) && header.isValid()) {
            product.append(header.getWmoHeader());
            product.append("\n");
            wmoid = header.getTtaaii();
            hdrTime = header.getYYGGgg();
            bbbIndicator = header.getBBBIndicator();
            cccc = header.getCccc();
        } else {
            wmoid = "";
            hdrTime = "";
        }

        product.append(reportData);

        Long writeTime = new Long(System.currentTimeMillis());
        if (WMOTimeParser.allowArchive() && header.getHeaderDate() != null) {
            Calendar c = header.getHeaderDate();
            writeTime = new Long(c.getTimeInMillis());
        }

        boolean success = false;

        StdTextProduct textProduct = (operationalMode
                ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
        textProduct.setWmoid(wmoid);
        textProduct.setCccid(prodId.getCcc());
        textProduct.setXxxid(prodId.getXxx());
        siteid = IcaoMap.siteToIcaoId(prodId.getXxx(), cccc);
        textProduct.setSite(siteid);
        textProduct.setNnnid(prodId.getNnn());
        textProduct.setHdrtime(hdrTime);
        textProduct.setBbbid(bbbIndicator);
        textProduct.setRefTime(writeTime);
        textProduct.setProduct(product.toString());
        success = writeProduct(textProduct);

        if (success) {
            return writeTime;
        } else {
            return Long.MIN_VALUE;
        }
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. Provides a wrapper for the case when on
     * WMO header is provided.
     *
     * @param prodId
     *            contains the AFOS PIL
     * @param reportData
     *            the body of the product
     * @param operationalMode
     * @return write time
     */
    public long writeProductNoHeader(AFOSProductId prodId, String reportData,
            boolean operationalMode) {
        return writeProduct(null, prodId, reportData, operationalMode);
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. This version accepts a WMOReportData
     * object containing the product to insert.
     *
     * @param data
     *            the data to insert
     * @param operationalMode
     * @param headers
     *            contains header Map
     * @return write time
     */
    public long writeProduct(WMOReportData data, boolean operationalMode,
            Headers headers) {
        return writeProduct(data.getWmoHeader(), data.getAfosProdId(),
                data.getReportData(), operationalMode);
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. This version accepts an AFOSProductId
     * object and the body of the product.
     *
     * @param prodId
     *            contains the AFOS PIL
     * @param reportData
     *            the body of the product
     * @param operationalMode
     * @param headers
     *            contains header Map
     * @return write time
     */
    public long writeProduct(AFOSProductId prodId, String reportData,
            boolean operationalMode, Headers headers) {
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader header = new WMOHeader(reportData.getBytes(), fileName);

        long retValue = -1;

        if (header.isValid()) {
            reportData = reportData.substring(header.getMessageDataStart());
            retValue = writeProduct(header, prodId, reportData,
                    operationalMode);
        } else {
            retValue = writeProductNoHeader(prodId, reportData,
                    operationalMode);
        }
        return retValue;
    }

    /**
     * Writes the text product to the text database standard text product table
     * based upon the operationalMode. This version accepts the AFOS PIL and the
     * body of the product as strings. If the AFOS PIL is under 7 characters in
     * length, it assumes the PIL has format NNNXXX and prepends the default CCC
     * ID.
     *
     * @param productId
     *            the AFOS PIL
     * @param reportData
     *            the body of the product.
     * @param operationalMode
     * @param headers
     *            contains header Map
     * @return write time
     */
    public long writeProduct(String productId, String reportData,
            boolean operationalMode, Headers headers) {
        // Look for a WMO heading on the first line
        String[] pieces = reportData.split("\r*\n", 2);
        if (pieces.length > 1) {
            // WMOHeader expects this
            pieces[0] += "\n";
        }
        byte[] bytes = pieces[0].getBytes();
        WMOHeader header;
        if (headers != null) {
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            header = new WMOHeader(bytes, fileName);
        } else {
            header = new WMOHeader(bytes);
        }

        // Need to construct an AFOSProductId from the productId
        if (productId.length() <= 6) {
            String ccc = SiteMap.getInstance().getCCCFromXXXCode(this.siteName);
            if (ccc == null) {
                ccc = this.siteName;
            }
            productId = ccc + productId;
        }
        String ccc = productId.substring(0, 3);
        String nnn = productId.substring(3, 6);
        String xxx = productId.substring(6);
        logger.debug("ccc=[" + ccc + "] nnn=[" + nnn + "] xxx=[" + xxx + "]");

        AFOSProductId afosId = new AFOSProductId(ccc, nnn, xxx);

        long retValue = -1;
        if (header.isValid()) {
            reportData = pieces.length > 1 ? pieces[1] : "";
            retValue = writeProduct(header, afosId, reportData,
                    operationalMode);
        } else {
            retValue = writeProductNoHeader(afosId, reportData,
                    operationalMode);
        }
        return retValue;
    }

    /**
     * Get Operational Mode
     *
     * @return TRUE if mode is operational
     */
    public boolean getOperationalMode() {

        return this.operationalMode;
    }
}
