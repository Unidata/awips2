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
package com.raytheon.edex.textdb.dbapi.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.textdb.dao.StateMatchDao;
import com.raytheon.edex.textdb.dao.StdTextProductDao;
import com.raytheon.edex.textdb.dao.TextProductInfoDao;
import com.raytheon.edex.textdb.dao.WatchWarnDao;
import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StateMatch;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.db.WatchWarn;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * TODO Add Description
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
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextDB {

    private Log logger = LogFactory.getLog(getClass());

    // private HDF5Dao hdf5dao = null;

    private String siteName = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("SITENAME");

    private boolean operationalMode = true;

    public TextDB() {
    }

    public TextDB(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }

    /**
     * 
     * @param time
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
     * @param abbrId
     * @param lastHrs
     * @param hdrTime
     * @param bbbId
     * @return
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
     * @return
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
        /*
         * int action = 0;
         * 
         * if (wmoId != null && wmoId.length() > 0) { action += 32; } if (site
         * != null && site.length() > 0) { action += 16; } if (abbrId != null &&
         * abbrId.length() > 0) { action += 8; } if (lastHrs != null &&
         * lastHrs.length() > 0) { action += 4; } if (hdrTime != null &&
         * hdrTime.length() > 0) { action += 2; } if (bbbId != null &&
         * bbbId.length() > 0) { action += 1; }
         * 
         * switch (action) { case 8: { // abbrId rval = dao.read_i(abbrId);
         * break; } case 16: { // site rval = dao.read_s(site); break; } case
         * 20: { // site + lastHrs rval = dao.read_sh(site, lastHrsMillis);
         * break; } case 32: { // wmoId rval = dao.read_w(wmoId); break; } case
         * 36: { // wmoId + lastHrs rval = dao.read_wh(wmoId, lastHrsMillis);
         * break; } case 48: { // wmoId + site rval = dao.read_ws(wmoId, site,
         * intlProd); break; } case 50: { // wmoId + site + hdrTime if
         * ("000000".equals(hdrTime)) { rval = dao.read_ws_all(wmoId, site); }
         * else { rval = dao.read_wst(wmoId, site, hdrTime, intlProd); } break;
         * } case 51: { // wmoId + site + hdrTime + bbb rval =
         * dao.read_wstb(wmoId, site, hdrTime, bbbId, intlProd); break; } case
         * 52: { // wmoId + site + lastHrs rval = dao.read_wsh(wmoId, site,
         * lastHrsMillis); break; } case 56: { // wmoId + site + abbrId rval =
         * dao.read_wsi(wmoId, site, abbrId); break; } case 58: { // wmoId +
         * site + abbrId + hdrTime if ("000000".equals(hdrTime)) { rval =
         * dao.read_wsi_all(wmoId, site, abbrId); } else { rval =
         * dao.read_wsit(wmoId, site, abbrId, hdrTime); } break; } case 59: { //
         * wmoId + site + abbrId + hdrTime + bbb rval = dao.read_wsitb(wmoId,
         * site, abbrId, hdrTime, bbbId); break; } default: {
         * logger.debug("TextDB:readAwips action not implemented-" + action);
         * break; } }
         * 
         * if (action != 20 && action != 36 && action != 52) { // first line is
         * to be the number of products returned rval.add(0, "" + rval.size());
         * }
         * 
         * return rval;
         */
    }

    /**
     * 
     * @param stateId
     * @param cccId
     * @param xxxId
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
     * 
     * @param cccId
     * @param nnnId
     * @param xxxId
     * @return
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
     * @return
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
     * @param stateId
     * @param cccId
     * @param xxxId
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
     */
    public boolean addState(String stateId, String cccId, String xxxId) {
        StateMatch stateInfo = new StateMatch(stateId, xxxId, cccId);
        return addState(stateInfo);
    }

    /**
     * --- statematch Remove a mapping for state -> (CCC and XXX)
     * 
     * @param stateId
     * @param cccId
     * @param xxxId
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
     * @param cccId
     * @param xxxId
     */
    public boolean removeState(String stateId, String xxxId, String cccId) {
        StateMatch stateInfo = new StateMatch(stateId, xxxId, cccId);
        return removeState(stateInfo);
    }

    /**
     * --- statematch
     * 
     * @param state
     * @return
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
     * @return
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
     * @param script
     * @return
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
     * @return
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
     * @param script
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
     */
    public Long getLatestTime(String productId, boolean operationalMode) {

        // Need to construct an AFOSProductId from the productId
        String ccc = productId.substring(0, 3);
        String nnn = productId.substring(3, 6);
        String xxx = productId.substring(6);
        if(xxx.length() == 1) {
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
     * @return
     */
    public List<Long> getAllTimes(String productId, boolean operationalMode) {

        // Need to construct an AFOSProductId from the productId
        String ccc = productId.substring(0, 3);
        String nnn = productId.substring(3, 6);
        String xxx = productId.substring(6);
        if(xxx.length() == 1) {
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
     * @return
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
     * 
     * @param wmoId
     * @param siteId
     * @param hdrTime
     * @param afosId
     * @return
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
     * @return
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
     * 
     * @param state
     * @param nnn
     * @return
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
     * 
     * @param afosCommand
     * @return
     */
    public List<StdTextProduct> executeAFOSCommand(String afosCommand,
            String locale, boolean operationalMode) {
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
     */
    public boolean writeProduct(StdTextProduct textProduct) {
        boolean operationalMode = (textProduct instanceof OperationalStdTextProduct ? true
                : false);
        StdTextProductDao dao = new StdTextProductDao(operationalMode);
        boolean success = false;
        try {
            if (textProduct.getCreatetime() == null) {
                textProduct.setCreatetime(System.currentTimeMillis());
            }
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
        long writeTime = System.currentTimeMillis();

        StdTextProduct textProduct = (operationalMode ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
        textProduct.setWmoid(wmoid);
        textProduct.setSite(siteid);
        textProduct.setCccid(prodId.getCcc());
        textProduct.setXxxid(prodId.getXxx());
        textProduct.setNnnid(prodId.getNnn());
        textProduct.setHdrtime(hdrTime);
        textProduct.setBbbid(bbbIndicator);
        textProduct.setCreatetime(writeTime);
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
     * @param headers
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
     */
    public long writeProduct(AFOSProductId prodId, String reportData,
            boolean operationalMode, Headers headers) {
        WMOHeader header = new WMOHeader(reportData.getBytes(), headers);

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
     */
    public long writeProduct(String productId, String reportData,
            boolean operationalMode, Headers headers) {
        // Look for a WMO heading on the first line
        String[] pieces = reportData.split("\r*\n", 2);
        if (pieces.length > 1)
            pieces[0] += "\n"; // WMOHeader expects this
        WMOHeader header = new WMOHeader(pieces[0].getBytes(), headers);

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
     * 
     * @param <T>
     * @param values
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getValue(Object[] values) {

        T retValue = null;

        if ((values != null) && (values.length > 0)) {
            Object o = values[0];
            if (o != null) {
                retValue = (T) o;
            }
        }
        return retValue;
    }

    /**
     * 
     * @param <T>
     * @param values
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getValue(Object value) {

        T retValue = null;

        if (value != null) {
            retValue = (T) value;
        }
        return retValue;
    }

    public static String marshalToXml(Object obj) throws JAXBException {
        JAXBContext ctx = SerializationUtil.getJaxbContext();
        Marshaller msh = ctx.createMarshaller();
        msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.FALSE);

        ByteArrayOutputStream strm = new ByteArrayOutputStream();
        msh.marshal(obj, strm);
        return strm.toString();
    }

    /**
     * Instantiates an object from the XML representation in a string. Uses
     * JAXB.
     * 
     * @param xml
     *            The XML representation
     * @return A new instance from the XML representation
     * @throws JAXBException
     */
    public static Object unmarshalFromXml(String xml) throws JAXBException {
        JAXBContext ctx = SerializationUtil.getJaxbContext();
        Unmarshaller msh = ctx.createUnmarshaller();
        ByteArrayInputStream strm = new ByteArrayInputStream(xml.getBytes());
        Object obj = msh.unmarshal(strm);
        return obj;
    }

    /**
     * 
     * @param string
     * @return
     */
    public static String asciiToHex(String string) {
        return new HexBinaryAdapter().marshal(string.getBytes());
    }

    /**
     * 
     * @param hexString
     * @return
     */
    public static String hexToAscii(String hexString) {

        byte[] b = new HexBinaryAdapter().unmarshal(hexString);

        return new String(b);
    }

    /**
     * 
     * @param header
     * @param propName
     * @return
     */
    public static String getProperty(Header header, String propName) {
        String result = null;

        String value = header.getProperty(propName);

        if (value != null) {
            result = hexToAscii(value);
        }
        return result;
    }

    public boolean getOperationalMode() {

        return this.operationalMode;
    }
}