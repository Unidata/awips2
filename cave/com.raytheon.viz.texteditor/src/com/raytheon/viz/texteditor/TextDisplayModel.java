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

package com.raytheon.viz.texteditor;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.msgs.IAviationObserver;
import com.raytheon.viz.texteditor.msgs.IRadarObserver;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;
import com.raytheon.viz.texteditor.msgs.ITextWorkstationCallback;

/**
 * Singleton class that contains information related to querying for a text
 * product or reflecting a dipped text product's contents.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 9/19/2007    368         grichard    Initial creation.
 * 10/4/2007    459         grichard    Revised for AFOS PIL fields.
 * 10/9/2007    459         grichard    Revised for StdTextProduct.
 * 10/11/2007   482         grichard    Reformatted file.
 * 10/16/2007   482         grichard    Implemented build 9 features.
 * 11/28/2007   520         grichard    Implemented build 11 features.
 * 12/7/2007    582         grichard    Implemented build 12 features.
 * 1/3/2008     637         grichard    Implemented build 13 features.
 * 1/10/2008    722         grichard    Implemented localization.
 * 5/16/2008    1119        grichard    Added support for IAviationObserver.
 * 5/8/2009     2104        grichard    Added support for IRadarObserver.
 * 5/8/2009     2104        grichard    Added support for IScriptRunnerObserver.
 * 7/30/2009    2718        rjpeter     Added logic for creation of StdTextProduct.
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 05/28/2010   2187        cjeanbap    Added StdTextProductFactory functionality.
 * 03/18/2014   DR 17174    D. Friedman Return correct 3-letter site IDs in getNnnXxx.
 * </pre>
 * 
 * @author grichard
 */
public final class TextDisplayModel {

    /**
     * Matches VTEC Phenomenas to NNN product IDs
     */
    private static final String[][] VTEC_PP_TO_NNN = { { "SV.W", "SVR" },
            { "TO.W", "TOR" }, { "FF.W", "FFW" }, { "FA.W", "FLW" },
            { "FA.Y", "FLS" }, { "EW.W", "EWW" } };

    private static final Map<String, String> vtecPpToNnn = new HashMap<String, String>();

    private static final Pattern warningPattern = Pattern
            .compile("/[A-Z]\\.([A-Z]{3})\\.(\\p{Alnum}{4})\\.([A-Z]{2}\\.[A-Z]{1})");

    private static final Pattern nnnxxxPattern = Pattern
            .compile("[\\r\\n]+([A-Z]{3})([A-Z]{3})(| WRKWG[0-9])[\\r\\n]+");

    /**
     * The static singleton instance.
     */
    private static TextDisplayModel instance;

    /**
     * 
     */
    private final String siteNode;

    /**
     * Standard Text Product from Text Database.
     */
    private Map<String, StdTextProduct> stdTxtProd = new HashMap<String, StdTextProduct>();

    /**
     * Afos Browser Control for show/hide capability in Text Workstation Control
     * Dialog.
     */
    private Map<String, ITextWorkstationCallback> controlAfosBrowser = new HashMap<String, ITextWorkstationCallback>();

    /**
     * Text Aviation Utility for use by Aviation for saving/sending TAFs.
     */
    private IAviationObserver textAviationUtility;

    /**
     * Text Radar Utility for use by Radar for saving/sending radar text
     * products.
     */
    private IRadarObserver textRadarUtility;

    /**
     * Text Script Runner Utility for use by ScriptRunner for issuing status
     * messages in the status message bar of the text window.
     */
    private IScriptRunnerObserver textScriptRunnerUtility;

    /**
     * The afos command map.
     */
    private Map<String, String> afosCommand = new HashMap<String, String>();

    /**
     * 
     */
    static {
        for (String[] str : VTEC_PP_TO_NNN) {
            vtecPpToNnn.put(str[0], str[1]);
        }
    }

    /**
     * Singleton constructor.
     * 
     * @return the text display model.
     */
    public static synchronized TextDisplayModel getInstance() {
        if (instance == null) {
            instance = new TextDisplayModel();
        }

        return instance;
    }

    /**
     * Private constructor: Use getInstance().
     */
    private TextDisplayModel() {
        // Set the node based on localization.
        String siteNode = LocalizationManager.getInstance().getCurrentSite();

        this.siteNode = siteNode;
    }

    /**
     * @return the afosCommand
     */
    public String getAfosCommand(String token) {
        return afosCommand.get(token);
    }

    /**
     * @param afosCommand
     *            the afosCommand to set
     */
    public void setAfosCommand(String token, String afosCommand) {
        this.afosCommand.put(token, afosCommand);
    }

    /**
     * Getter/Accessor of WMO ID.
     */
    public String getWmoId(String token) {
        return getStdTextProduct(token, true).getWmoid();
    }

    /**
     * Setter/Mutator of WMO ID.
     */
    public void setWmoId(String token, String wmoid) {
        getStdTextProduct(token, true).setWmoid(wmoid);
    }

    /**
     * Getter/Accessor of Site ID.
     */
    public String getSiteId(String token) {
        return getStdTextProduct(token, true).getSite();
    }

    /**
     * Setter/Mutator of Site ID.
     */
    public void setSiteId(String token, String site) {
        getStdTextProduct(token, true).setSite(site);
    }

    /**
     * Getter/Accessor of product node ("CCC") of AFOS PIL.
     */
    public String getProductNode(String token) {
        return getStdTextProduct(token, true).getCccid();
    }

    /**
     * Setter/Mutator of product node ("CCC") of AFOS PIL.
     */
    public void setProductNode(String token, String ccc) {
        getStdTextProduct(token, true).setCccid(ccc);
    }

    /**
     * Getter/Accessor of product category ("NNN") of AFOS PIL.
     */
    public String getProductCategory(String token) {
        return getStdTextProduct(token, true).getNnnid();
    }

    /**
     * Setter/Mutator of product category ("NNN") of AFOS PIL.
     */
    public void setProductCategory(String token, String nnn) {
        getStdTextProduct(token, true).setNnnid(nnn);
    }

    /**
     * Getter/Accessor of product designator ("X-X-X") of AFOS PIL.
     */
    public String getProductDesignator(String token) {
        return getStdTextProduct(token, true).getXxxid();
    }

    /**
     * Setter/Mutator of product designator ("X-X-X") of AFOS PIL.
     */
    public void setProductDesignator(String token, String xxx) {
        getStdTextProduct(token, true).setXxxid(xxx);
    }

    /**
     * Getter/Accessor of product.
     */
    public String getProduct(String token) {
        return getStdTextProduct(token, true).getProduct();
    }

    /**
     * Setter/Mutator of product.
     */
    public void setProduct(String token, String product) {
        getStdTextProduct(token, true).setProduct(product);
    }

    /**
     * Getter/Accessor of AFOS PIL.
     */
    public String getAfosPil(String token) {
        StdTextProduct prod = getStdTextProduct(token, true);
        return prod.getCccid() + prod.getNnnid() + prod.getXxxid();
    }

    /**
     * Setter/Mutator of AFOS PIL.
     */
    public void setAfosPil(String token, String node) {
        AFOSProductId afosId = new AFOSProductId(node);
        StdTextProduct prod = getStdTextProduct(token, true);
        prod.setCccid(afosId.getCcc());
        prod.setNnnid(afosId.getNnn());
        prod.setXxxid(afosId.getXxx());
    }

    /**
     * Getter/Accessor of Header Time.
     */
    public String getHdrTime(String token) {
        return getStdTextProduct(token, true).getHdrtime();
    }

    /**
     * Setter/Mutator of Header Time.
     */
    public void setHdrTime(String token, String ht) {
        getStdTextProduct(token, true).setHdrtime(ht);
    }

    /**
     * Getter/Accessor of BBB ID.
     */
    public String getBbbId(String token) {
        return getStdTextProduct(token, true).getBbbid();
    }

    /**
     * Setter/Mutator of BBB ID.
     */
    public void setBbbId(String token, String bbb) {
        getStdTextProduct(token, true).setBbbid(bbb);
    }

    /**
     * Getter/Accessor of Create Time.
     */
    public long getCreateTime(String token) {
        return getStdTextProduct(token, true).getRefTime();
    }

    /**
     * Setter/Mutator of Create Time.
     */
    public void setCreateTime(String token, long ct) {
        getStdTextProduct(token, true).setRefTime(ct);
    }

    /**
     * Getter/Accessor of Afos Browser Control for show/hide in Control Dialog.
     */
    public ITextWorkstationCallback getITextWorkstationCallback(String token) {
        return controlAfosBrowser.get(token);
    }

    /**
     * Setter/Mutator of Afos Browser Control for show/hide in Control Dialog.
     */
    public void setITextWorkstationCallback(String token,
            ITextWorkstationCallback cb) {
        controlAfosBrowser.put(token, cb);
    }

    /**
     * Getter/Accessor of Text Aviation Utility for use by Aviation.
     */
    public IAviationObserver getTextAviation()
            throws TextWorkstationNotStartedException {
        if (textAviationUtility != null) {
            return textAviationUtility;
        } else {
            throw new TextWorkstationNotStartedException(
                    "Text Workstation has not been started!");
        }
    }

    /**
     * Setter/Mutator of Text Aviation Utility for use by Aviation.
     */
    public void setTextAviation(IAviationObserver cb) {
        textAviationUtility = cb;
    }

    /**
     * Getter/Accessor of Text Radar Utility for use by Radar.
     */
    public IRadarObserver getTextRadar()
            throws TextWorkstationNotStartedException {
        if (textRadarUtility != null) {
            return textRadarUtility;
        } else {
            throw new TextWorkstationNotStartedException(
                    "Text Workstation has not been started!");
        }
    }

    /**
     * Setter/Mutator of Text Radar Utility for use by Radar.
     */
    public void setTextRadar(IRadarObserver cb) {
        textRadarUtility = cb;
    }

    /**
     * Getter/Accessor of Text ScriptRunner Utility for use by ScriptRunner.
     */
    public IScriptRunnerObserver getTextScriptRunner()
            throws TextWorkstationNotStartedException {
        if (textScriptRunnerUtility != null) {
            return textScriptRunnerUtility;
        } else {
            throw new TextWorkstationNotStartedException(
                    "Text Workstation has not been started!");
        }
    }

    /**
     * Setter/Mutator of Text ScriptRunner Utility for use by ScriptRunner.
     */
    public void setTextScriptRunner(IScriptRunnerObserver cb) {
        textScriptRunnerUtility = cb;
    }

    /**
     * Getter/Accessor of standard text product.
     */
    public StdTextProduct getStdTextProduct(String token) {
        return getStdTextProduct(token, false);
    }

    /**
     * Setter/Mutator of standard text product.
     */
    public boolean hasStdTextProduct(String token) {
        return stdTxtProd.containsKey(token);
    }

    /**
     * Overloaded Getter/Accessor of standard text product.
     * 
     * @param token
     * @param createFlag
     *            True if the product should be created if it does not exist,
     *            false if the product should be returned as is, allowing for
     *            null returns.
     */
    public StdTextProduct getStdTextProduct(String token, boolean createFlag) {
        StdTextProduct prod = stdTxtProd.get(token);

        if (prod == null && createFlag) {
            prod = createBlankProduct();
            stdTxtProd.put(token, prod);
        }

        return prod;
    }

    /**
     * 
     * @param token
     * @param rawProduct
     * @param siteNode
     */
    public void createStdTextProduct(String token, String rawProduct,
            String siteNode) {
        StdTextProduct prod = createBlankProduct();

        if (rawProduct != null && rawProduct.length() > 0) {
            // parse header
            WMOHeader header = new WMOHeader(rawProduct.getBytes());
            String[] nnnxxx = getNnnXxx(rawProduct);
            prod.setWmoid(header.getTtaaii());
            prod.setSite(header.getCccc());
            prod.setCccid(siteNode);
            prod.setNnnid(nnnxxx[0]);
            prod.setXxxid(nnnxxx[1]);
            prod.setHdrtime(header.getYYGGgg());

            String bbb = header.getBBBIndicator();
            if (bbb != null && bbb.length() > 0) {
                prod.setBbbid(bbb);
            }
        }

        stdTxtProd.put(token, prod);
    }

    /**
     * Setter/Mutator of standard text product.
     */
    public void setStdTextProduct(String token, String wmoid, String site,
            String cccid, String nnnid, String xxxid, String hdrtime,
            String bbbid, long createtime, String product) {
        StdTextProduct tmpProduct = StdTextProductFactory.getInstance(CAVEMode
                .getMode());
        tmpProduct.setWmoid(wmoid);
        tmpProduct.setSite(site);
        tmpProduct.setCccid(cccid);
        tmpProduct.setNnnid(nnnid);
        tmpProduct.setXxxid(xxxid);
        tmpProduct.setHdrtime(hdrtime);
        tmpProduct.setBbbid(bbbid);
        tmpProduct.setRefTime(createtime);
        tmpProduct.setProduct(product);
        stdTxtProd.put(token, tmpProduct);
    }

    /**
     * Setter/Mutator of standard text product.
     */
    public void setStdTextProduct(String token, StdTextProduct prod) {
        stdTxtProd.put(token, prod);
    }

    /**
     * Clear the standard text product.
     */
    public void clearStdTextProduct(String token) {
        stdTxtProd.remove(token);
    }

    private StdTextProduct createBlankProduct() {
        StdTextProduct prod = StdTextProductFactory.getInstance(CAVEMode
                .getMode());

        // Set the Site ID based on localization.
        String fullSiteName = SiteMap.getInstance().getSite4LetterId(siteNode);
        prod.setSite(fullSiteName);
        prod.setWmoid("");
        prod.setCccid(siteNode);
        prod.setNnnid("");
        prod.setXxxid("");

        return prod;
    }

    /**
     * Method to capture the product category and product designator from a
     * warning message
     * 
     * @param warning
     * @return the product category and product designator strings
     */
    public static String[] getNnnXxx(String warning) {
        if (warning != null) {
            Matcher m = warningPattern.matcher(warning);
            if (m.find() && m.group(1).equals("NEW")) {
                String nnn = vtecPpToNnn.get(m.group(3));
                Set<String> siteSet = SiteMap.getInstance().getSite3LetterIds(m.group(2));
                if (nnn != null && siteSet.size() == 1) {
                    return new String[] { nnn, siteSet.iterator().next() };
                }
            }
            m = nnnxxxPattern.matcher(warning);
            if (m.find()) {
                return new String[] { m.group(1), m.group(2) };
            }
        }
        return new String[] { "nnn", "xxx" };
    }
}
