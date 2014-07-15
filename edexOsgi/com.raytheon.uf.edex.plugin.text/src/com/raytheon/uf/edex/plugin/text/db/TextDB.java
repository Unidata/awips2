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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StateMatch;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.db.WatchWarn;
import com.raytheon.uf.common.dataplugin.text.util.AFOSParser;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.plugin.text.dao.StateMatchDao;
import com.raytheon.uf.edex.plugin.text.dao.StdTextProductDao;
import com.raytheon.uf.edex.plugin.text.dao.TextProductInfoDao;
import com.raytheon.uf.edex.plugin.text.dao.WatchWarnDao;
import com.raytheon.uf.edex.plugin.text.impl.WMOReportData;

/**
 * Text Database.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2008        1538 jkorman     Initial creation
 * 20090327           2151 jkorman     Modified writeProduct to account for possible leading
 *                                     carriage control.
 * Jul 10, 2009 2191       rjpeter     Add additional methods.
 * Aug 18, 2009 2191       rjpeter     Switched to version purging.
 * Feb 15, 2010 4426       MW Fegan    Use default CCC ID when not provided in write.
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 01Jun2010               cjeanbap    Added operational mode functionality.
 * 09Jul2010    2187       cjeanbap    Added additional operational mode functionality.
 * 02Aug2010    2187       cjeanbap    Added parameter constructor.
 * 09Aug2010    3944       cjeanbap    Add new method, queryAllWatchWarn.
 * 8Nov2010     7294       cjeanbap    Update logic in executeAFOSCommand.
 *                                     Removed committed out code.
 * ------------------------------------
 * 18 Apr 2012         479 jkorman     Modified to pad xxxid to 3 characters in queries.
 * 23 May 2012       14952 rferrel     Allow queries with refTime.
 * Feb 18, 2014       2652  skorolev    Corrected writeProduct for WMO header if archive is allowed. Deleted unused code.
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * May 15, 2014 2536        bclement    moved asciiToHex() hexToAscii() and getProperty() to PropConverter
 * May 20, 2014 2536        bclement    moved from edex.textdb to edex.plugin.text
 * Jul 10, 2014 2914        garmendariz Remove EnvProperties
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextDB {

    private Log logger = LogFactory.getLog(getClass());

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
     * @return total products updated
     */
    public static int purgeStdTextProducts() {
        StdTextProductDao dao = new StdTextProductDao();
        int totalProducts = dao.versionPurge();
        dao = new StdTextProductDao(false);
        totalProducts += dao.versionPurge();
        return totalProducts;
    }

    /**
     * Returns a List of String objects representing headers or full products
     * that matched the passed parameters.
     * 
     * @param wmoId
     * @param site
     * @param intlProd
     *            contains Request Type
     * @param abbrId
     * @param lastHrs
     * @param hdrTime
     * @param bbbId
     * @return standard text products
     */
    public List<StdTextProduct> readAwips(RequestType requestType,
            String wmoId, String site, String abbrId, String lastHrs,
            String hdrTime, String bbbId) {
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
                bbbId, false, operationalMode);
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
     * @param bbbId
     * @param fullDataRead
     * @param operationalMode
     * @return specified products
     */
    public List<StdTextProduct> readAwips(String wmoId, String site,
            int intlProd, String abbrId, String lastHrs, String hdrTime,
            String bbbId, boolean fullDataRead, boolean operationalMode) {
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        String nnn = null;
        String xxx = null;
        Long startTimeMillis = null;
        boolean readAllVersions = false;

        if (abbrId != null) {
            if (abbrId.length() > 0) {
                nnn = abbrId.substring(0,
                        (abbrId.length() >= 3 ? 3 : abbrId.length()));
            }
            if (abbrId.length() > 3) {
                xxx = abbrId.substring(3,
                        (abbrId.length() < 6 ? abbrId.length() : 6));
            }
        }
        if (hdrTime != null && hdrTime.length() > 0) {
            if (hdrTime.equals("000000")) {
                readAllVersions = true;
                hdrTime = null;
            }
        }
        if (lastHrs != null && lastHrs.length() > 0) {
            int hours = 0;

            try {
                hours = Integer.parseInt(lastHrs);
            } catch (NumberFormatException e) {
                // ignore
            }

            if (hours <= 0) {
                hours = 1;
            }

            // getting as GMT technically not necessary since converting to
            // millis anyway
            Calendar currentTime = Calendar.getInstance(TimeZone
                    .getTimeZone("GMT"));
            currentTime.add(Calendar.HOUR_OF_DAY, -hours); // subtract the hours
            startTimeMillis = currentTime.getTimeInMillis();
        }

        return dao.awipsRead(wmoId, site, nnn, xxx, hdrTime, startTimeMillis,
                bbbId, intlProd, readAllVersions, fullDataRead);
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
    public boolean addVersions(String ccc, String nnn, String xxx, int versions) {
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
            logger.error(e);
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
            logger.error(e);
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
            logger.error(e);
        }
        return success;
    }

    /**
     * --- statematch Add a mapping for state -> (CCC and XXX)
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
            logger.error(e);
        }
        return success;
    }

    /**
     * --- statematch Add a mapping for state -> (CCC and XXX)
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
     * --- statematch Remove a mapping for state -> (CCC and XXX)
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
            logger.error(e);
        }
        return success;
    }

    /**
     * --- statematch Remove a mapping for state -> (CCC and XXX)
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
            logger.error(e);
        }

        return stateList;
    }

    /**
     * --- watchwarn
     * 
     * @param watchWarn
     * @return success
     */
    public boolean addWatchWarn(WatchWarn watchWarn) {
        boolean success = false;

        WatchWarnDao dao = new WatchWarnDao();
        try {
            success = dao.addEntry(watchWarn);
        } catch (Exception e) {
            logger.error(e);
        }
        return success;
    }

    /**
     * --- watchwarn
     * 
     * @param productId
     *            A not null reference to the product identifier to store.
     * @param script
     *            A not null reference to the script to store.
     * @return success
     */
    public boolean addWatchWarn(String productId, String script) {
        return addWatchWarn(new WatchWarn(productId, script));
    }

    /**
     * --- watchwarn Get a list of all entries for a specific state.
     * 
     * @param stateInfo
     *            A state to lookup.
     * @return List of StateMatch entries for the specified state. If none were
     *         found, an empty list is returned.
     */
    public List<WatchWarn> queryWatchWarn(String productId) {

        List<WatchWarn> watchList = new ArrayList<WatchWarn>();

        List<String> results = null;

        WatchWarnDao dao = new WatchWarnDao();
        try {
            results = dao.queryWatchWarn(productId);
        } catch (Exception e) {
            logger.error(e);
        }
        if (results != null) {
            for (String s : results) {
                watchList.add(new WatchWarn(productId, s));
            }
        }

        return watchList;
    }

    /**
     * Get a list of all entries in database table.
     * 
     * @return List of WatchWarn entries.
     */
    public List<WatchWarn> queryAllWatchWarn() {
        WatchWarnDao dao = new WatchWarnDao();

        List<WatchWarn> results = null;

        try {
            results = dao.queryAllWatchWarn();
        } catch (Exception e) {
            logger.error(e);
        }
        return results;
    }

    /**
     * --- watchwarn
     * 
     * @param watchWarn
     * @return success
     */
    public boolean deleteWatchWarn(WatchWarn watchWarn) {
        boolean success = false;

        WatchWarnDao dao = new WatchWarnDao();
        try {
            success = dao.deleteEntry(watchWarn);
        } catch (Exception e) {
            logger.error(e);
        }
        return success;
    }

    /**
     * --- watchwarn
     * 
     * @param productId
     *            A not null reference to the product identifier to store.
     * @param script
     *            A not null reference to the script to store.
     * @return success
     */
    public boolean deleteWatchWarn(String productId, String script) {
        return deleteWatchWarn(new WatchWarn(productId, script));
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
        List<Long> times = new ArrayList<Long>();

        for (AFOSProductId id : afosIds) {
            times.add(getLatestTime(id, operationalMode));
        }
        return times;
    }

    /**
     * Get the latest time for each product in the stdtextproducts table.
     * 
     * @param afosIds
     *            List of product ids to query for.
     * @return list of the latest time
     */
    public List<Long> getLatestTimes(AFOSProductId afosId,
            boolean operationalMode) {
        List<Long> times = null;

        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        try {
            times = dao.getLatestTimes(afosId);
        } catch (Exception e) {
            logger.error(e);
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
            logger.error(e);
        }

        return latestTime;
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
    public List<Long> getAllTimes(AFOSProductId afosId, boolean operationalMode) {
        return getAllTimes(afosId.getCcc(), afosId.getNnn(), afosId.getXxx(),
                operationalMode);
    }

    /**
     * Get all times for one product in the stdtextproducts table.
     * 
     * @param afosId
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
     * @param afosId
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
            logger.error(e);
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
            products = dao
                    .getSameMinuteProducts(wmoId, siteId, hdrTime, afosId);
        } catch (Exception e) {
            logger.error(e);
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
    public List<String> siteRead(String cccId, String nnnId, String xxxId,
            String wmoId) {

        List<String> sites = null;

        return sites;
    }

    /**
     * 
     * @param afosId
     * @param wmoId
     * @return
     */
    public List<String> siteRead(AFOSProductId afosId, String wmoId) {
        return siteRead(afosId.getCcc(), afosId.getNnn(), afosId.getXxx(),
                wmoId);
    }

    /**
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
        List<StdTextProduct> products = new ArrayList<StdTextProduct>();
        StateMatchDao stateDao = new StateMatchDao();

        List<AFOSProductId> cmds = stateDao.makeAFOSCommands(state, nnn);
        StdTextProductDao dao = new StdTextProductDao(operationalMode);

        for (AFOSProductId cmd : cmds) {
            products.addAll(dao.cccnnnxxxReadVersion(cmd.getCcc(),
                    cmd.getNnn(), cmd.getXxx(), 0));
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
                int versionNo = 0; // default version number; read the latest
                // version
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
            products = new ArrayList<StdTextProduct>();
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
        boolean operationalMode = (textProduct instanceof OperationalStdTextProduct ? true
                : false);
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        boolean success = false;
        try {
            if (textProduct.getRefTime() == null) {
                textProduct.setRefTime(System.currentTimeMillis());
            }
            textProduct.setInsertTime(Calendar.getInstance(TimeZone
                    .getTimeZone("GMT")));
            success = dao.write(textProduct);
        } catch (Exception e) {
            logger.error(e);
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

        if (header != null && header.isValid()) {
            product.append(header.getWmoHeader());
            product.append("\n");
            wmoid = header.getTtaaii();
            siteid = header.getCccc();
            hdrTime = header.getYYGGgg();
            bbbIndicator = header.getBBBIndicator();
        } else {
            wmoid = "";
            siteid = "";
            hdrTime = "";
        }

        product.append(reportData);

        Long writeTime = new Long(System.currentTimeMillis());
        if (WMOTimeParser.allowArchive() && header.getHeaderDate() != null) {
            Calendar c = header.getHeaderDate();
            writeTime = new Long(c.getTimeInMillis());
        }

        StdTextProduct textProduct = (operationalMode ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
        textProduct.setWmoid(wmoid);
        textProduct.setSite(siteid);
        textProduct.setCccid(prodId.getCcc());
        textProduct.setXxxid(prodId.getXxx());
        textProduct.setNnnid(prodId.getNnn());
        textProduct.setHdrtime(hdrTime);
        textProduct.setBbbid(bbbIndicator);
        textProduct.setRefTime(writeTime);
        textProduct.setProduct(product.toString());
        boolean success = writeProduct(textProduct);
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
     * @return
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
     * @return
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
            retValue = writeProduct(header, prodId, reportData, operationalMode);
        } else {
            retValue = writeProductNoHeader(prodId, reportData, operationalMode);
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
            pieces[0] += "\n"; // WMOHeader expects this
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
            retValue = writeProduct(header, afosId, reportData, operationalMode);
        } else {
            retValue = writeProductNoHeader(afosId, reportData, operationalMode);
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