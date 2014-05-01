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
package com.raytheon.uf.edex.dat.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuItem;
import com.raytheon.uf.common.menus.xml.CommonMenuContributionFile;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DATA_TYPE;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2010            mnash     Initial creation
 * Aug 20, 2010            dhladky   Added more for FFMP, moved to DatUtils
 * June 6, 2011 #9827      bkowal    Skip scan menu creation if the Run
 *                                   Configuration Manager has not read a
 *                                   FFMPRunConfig.xml file.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class DatMenuUtil extends AbstractMenuUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatMenuUtil.class);

    private boolean override = false;

    private String datSite = null;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#createMenus()
     */
    @Override
    public void createMenus() {
        statusHandler.info("Creating DAT menus...");
        List<CommonAbstractMenuContribution> listContributions = new ArrayList<CommonAbstractMenuContribution>();
        MenuTemplateFile menuTemplate = new MenuTemplateFile();

        if (!override) {
            if (getSite() != null
                    && !getSite().equals(
                            System.getProperty("aw.site.identifier"))) {
                statusHandler
                        .info(getSite()
                                + " is not a configured site for DAT.  No menus will be created.");
                return;
            } else if (getSite() == null) {
                statusHandler
                        .info("No site is set for menu creation.  No menus will be created.");
                return;
            }
        } else {
            if (getDatSite() != null) {
                setSite(getDatSite());
            } else {
                statusHandler
                        .info("No site is set for menu creation.  No menus will be created.");
                return;
            }
        }

        FFMPRunConfigurationManager ffmpManager = FFMPRunConfigurationManager
                .getInstance();
        SCANRunSiteConfigurationManager scanManager = SCANRunSiteConfigurationManager
                .getInstance();

        if (!ffmpManager.isPopulated() && scanManager.isPopulated()) {
            statusHandler.info("Skipping scan menu creation.");
            return;
        }
        List<FFMPRunXML> ffmp = ffmpManager.getFFMPRunners();
        List<String> radars = new ArrayList<String>();
        List<String> ffmpProductKeys = new ArrayList<String>();

        for (int i = 0; i < ffmp.size(); i++) {
            FFMPRunXML runner = ffmp.get(i);
            if (runner.getProducts() != null) {
                for (ProductRunXML product : runner.getProducts()) {
                    if (!ffmpProductKeys.contains(product.getProductKey())) {
                        ffmpProductKeys.add(product.getProductKey());
                    }
                }
            }
        }

        for (int i = 0; i < ffmp.size(); i++) {
            radars.addAll(scanManager.getLocalSiteNames());
        }

        CommonTitleContribution titleItem = new CommonTitleContribution();
        titleItem.titleText = "------ FFMP ------";
        titleItem.id = "FFMPSCAN";
        listContributions.add(titleItem);

        for (String productKey : ffmpProductKeys) {
            for (FFMPRunXML runner : ffmp) {

                int i = 0;
                ProductRunXML product = runner.getProduct(productKey);
                CommonSubmenuContribution mainSiteMenu = new CommonSubmenuContribution();

                SourceXML primarySource = FFMPSourceConfigurationManager
                        .getInstance().getSource(product.getProductName());
                if (primarySource.getDataType().equals(
                        DATA_TYPE.XMRG.getDataType())) {
                    mainSiteMenu.id = product.getProductKey().toUpperCase();
                    mainSiteMenu.menuText = product.getProductKey()
                            .toUpperCase();
                } else {
                    mainSiteMenu.id = product.getProductKey();
                    mainSiteMenu.menuText = product.getProductKey();
                }

                // see how many types we have (DPR, DHR, etc) within each
                // siteKey

                ArrayList<ProductRunXML> products = runner.getProducts();

                // table entries + QPF + Guidance --start QPE source drop down
                int tableEntrySize = runner
                        .getProducts(product.getProductKey()).size();
                mainSiteMenu.contributions = new CommonAbstractMenuContribution[products
                        .size() + tableEntrySize];

                for (ProductRunXML innerproduct : runner.getProducts(product
                        .getProductKey())) {

                    SourceXML qpesource = FFMPSourceConfigurationManager
                            .getInstance().getSource(
                                    innerproduct.getProductName());

                    String qpepath = "menus" + File.separator + "ffmp"
                            + File.separator + "";
                    CommonIncludeMenuContribution qpeIncludeMenuCont = new CommonIncludeMenuContribution();
                    qpeIncludeMenuCont.fileName = new File(qpepath
                            + "ffmpQPE.xml");
                    qpeIncludeMenuCont.id = "QPE"
                            + innerproduct.getProductName();

                    // do substitutions
                    String dataKey = innerproduct.getProductKey();

                    qpeIncludeMenuCont = (CommonIncludeMenuContribution) processFFMPContribution(
                            innerproduct.getProductKey(),
                            qpesource.getSourceName(), dataKey,
                            qpesource.getDisplayName(),
                            qpeIncludeMenuCont.fileName.getPath(),
                            qpeIncludeMenuCont);

                    mainSiteMenu.contributions[i] = qpeIncludeMenuCont;
                    i++;
                }

                // process QPF

                CommonSubmenuContribution qpfSiteMenu = new CommonSubmenuContribution();

                ProductXML qpfProduct = FFMPSourceConfigurationManager
                        .getInstance()
                        .getProduct(primarySource.getSourceName());

                ArrayList<String> qpfTypes = product.getQpfTypes(qpfProduct);

                qpfSiteMenu.contributions = new CommonAbstractMenuContribution[qpfTypes
                        .size()];
                qpfSiteMenu.id = product.getProductKey() + "QPF";
                qpfSiteMenu.menuText = "QPF";
                int qpfTypeCounter = 0;

                // figure what types of qpf we have...
                for (String qpfType : qpfTypes) {

                    ArrayList<SourceXML> qpfSources = product.getQpfSources(
                            qpfProduct, qpfType);

                    if (qpfSources.size() > 0) {
                        CommonSubmenuContribution qpfTypeMenu = new CommonSubmenuContribution();

                        qpfTypeMenu.contributions = new CommonAbstractMenuContribution[qpfSources
                                .size()];
                        qpfTypeMenu.id = product.getProductKey()
                                + qpfSources.get(0).getDisplayName();
                        qpfTypeMenu.menuText = qpfSources.get(0)
                                .getDisplayName();

                        int qpfSourceCounter = 0;

                        for (SourceXML qpfSource : qpfSources) {

                            CommonIncludeMenuContribution qpfIncludeMenuCont = new CommonIncludeMenuContribution();
                            String qpfpath = "menus" + File.separator + "ffmp"
                                    + File.separator + "";
                            qpfIncludeMenuCont.fileName = new File(qpfpath
                                    + "ffmpQPF.xml");
                            qpfIncludeMenuCont.id = "QPF"
                                    + qpfSource.getSourceName();

                            // do substitutions
                            String dataKey = product.getProductKey();

                            qpfIncludeMenuCont = (CommonIncludeMenuContribution) processFFMPContribution(
                                    product.getProductKey(),
                                    qpfSource.getSourceName(), dataKey,
                                    qpfSource.getDisplayName(),
                                    qpfIncludeMenuCont.fileName.getPath(),
                                    qpfIncludeMenuCont);

                            qpfTypeMenu.contributions[qpfSourceCounter] = qpfIncludeMenuCont;
                            qpfSourceCounter++;
                        }

                        if (qpfSourceCounter > 0) {
                            qpfSiteMenu.contributions[qpfTypeCounter] = qpfTypeMenu;
                            qpfTypeCounter++;
                        }
                    }
                }

                if (qpfTypeCounter > 0) {
                    mainSiteMenu.contributions[i] = qpfSiteMenu;
                    i++;
                }

                // process guidance

                CommonSubmenuContribution guidSiteMenu = new CommonSubmenuContribution();
                guidSiteMenu.id = "Guidance";
                guidSiteMenu.menuText = "Guidance";

                ProductXML guidProduct = FFMPSourceConfigurationManager
                        .getInstance()
                        .getProduct(primarySource.getSourceName());

                ArrayList<String> guidTypes = product
                        .getGuidanceTypes(guidProduct);

                guidSiteMenu.contributions = new CommonAbstractMenuContribution[guidTypes
                        .size()];

                int guidTypeCounter = 0;

                // figure what type of guidance we have...
                for (String guidType : guidTypes) {

                    CommonSubmenuContribution guidTypeMenu = new CommonSubmenuContribution();
                    ArrayList<SourceXML> guidSources = product
                            .getGuidanceSources(guidProduct, guidType);

                    guidTypeMenu.id = product.getProductKey()
                            + guidSources.get(0).getDisplayName();
                    guidTypeMenu.menuText = guidSources.get(0).getDisplayName();

                    guidTypeMenu.contributions = new CommonAbstractMenuContribution[guidSources
                            .size()];

                    int guidCounter = 0;
                    for (SourceXML guidSource : guidSources) {

                        CommonIncludeMenuContribution guidIncludeMenuCont = new CommonIncludeMenuContribution();
                        String guidpath = "menus" + File.separator + "ffmp"
                                + File.separator + "";
                        guidIncludeMenuCont.fileName = new File(guidpath
                                + "ffmpGuidance.xml");
                        guidIncludeMenuCont.id = "Guidance"
                                + guidSource.getSourceName();

                        String guiddataKey = DATA_TYPE.XMRG.getDataType();

                        // construct the string with the duration hour for FFG
                        guidIncludeMenuCont = (CommonIncludeMenuContribution) processFFMPContribution(
                                product.getProductKey(),
                                guidSource.getSourceName(),
                                guiddataKey,
                                guidSource.getDisplayName() + " "
                                        + guidSource.getDurationHour() + " HR",
                                guidIncludeMenuCont.fileName.getPath(),
                                guidIncludeMenuCont);

                        guidTypeMenu.contributions[guidCounter] = guidIncludeMenuCont;
                        guidCounter++;

                    }

                    if (guidCounter > 0) {
                        guidSiteMenu.contributions[guidTypeCounter] = guidTypeMenu;
                        guidTypeCounter++;
                    }
                }

                if (guidTypeCounter > 0) {
                    mainSiteMenu.contributions[i] = guidSiteMenu;
                    listContributions.add(mainSiteMenu);
                }
            }
        }

        CommonIncludeMenuContribution ffmpAuxMenuCont = new CommonIncludeMenuContribution();
        String path = "menus" + File.separator + "ffmp" + File.separator + "";
        ffmpAuxMenuCont.fileName = new File(path + "auxFFMPMenu.xml");
        listContributions.add(ffmpAuxMenuCont);

        CommonSeparatorMenuContribution dedSeparator = new CommonSeparatorMenuContribution();
        dedSeparator.id = "dedSeparator";
        listContributions.add(dedSeparator);

        titleItem = new CommonTitleContribution();
        titleItem.titleText = "------ Dedicated Radars ------";
        titleItem.id = "DedicatedRadarsSCAN";
        listContributions.add(titleItem);
        listContributions = processRadarContributions(radars,
                "menus" + File.separator + "scan" + File.separator
                        + "baseScanMenu.xml", listContributions);

        CommonSeparatorMenuContribution dialSBNSeparator = new CommonSeparatorMenuContribution();
        dialSBNSeparator.id = "dialSBNSeparator";
        listContributions.add(dialSBNSeparator);
        titleItem = new CommonTitleContribution();
        titleItem.titleText = "------ Dial/SBN Radars ------";
        titleItem.id = "DialSBNRadarsSCAN";
        listContributions.add(titleItem);
        radars = scanManager.getDialSiteNames();
        CommonSubmenuContribution submenuContribution;

        // takes care of the radar sites
        for (int ii = 0; ii < radars.size(); ii++) {
            submenuContribution = new CommonSubmenuContribution();
            submenuContribution.contributions = new CommonIncludeMenuContribution[1];
            submenuContribution.menuText = radars.get(ii);
            submenuContribution.id = radars.get(ii) + "SCAN";
            submenuContribution.contributions[0] = processContribution(
                    radars.get(ii), "menus" + File.separator + "scan"
                            + File.separator + "baseScanMenu.xml");
            listContributions.add(submenuContribution);
        }
        menuTemplate.contributions = listContributions
                .toArray(new CommonAbstractMenuContribution[listContributions
                        .size()]);
        toXml(menuTemplate, "menus" + File.separator + "scan" + File.separator
                + "scan.xml");

        /*************************************** OTHER ********************************/
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String cwa = commonStaticSite.getContextName();

        CommonMenuContributionFile fogMenuContributionFile = new CommonMenuContributionFile();
        fogMenuContributionFile.contribution = new CommonIncludeMenuItem[1];

        fogMenuContributionFile.contribution[0] = new CommonIncludeMenuItem();

        fogMenuContributionFile.contribution[0] = processOtherContribution(cwa,
                "menus/fog/baseFog.xml", "menu:obs?after=FOGPLACEHOLDER",
                "Fog Monitor", fogMenuContributionFile.contribution[0]);

        toXml(fogMenuContributionFile, "menus" + File.separator + "fog"
                + File.separator + "index.xml");

        CommonMenuContributionFile safeMenuContributionFile = new CommonMenuContributionFile();
        safeMenuContributionFile.contribution = new CommonIncludeMenuItem[1];

        safeMenuContributionFile.contribution[0] = new CommonIncludeMenuItem();

        safeMenuContributionFile.contribution[0] = processOtherContribution(
                cwa, "menus/safeseas/baseSafeSeas.xml",
                "menu:obs?before=EndOfMaritime", "SAFESEAS",
                safeMenuContributionFile.contribution[0]);

        toXml(safeMenuContributionFile, "menus" + File.separator + "safeseas"
                + File.separator + "index.xml");

        CommonMenuContributionFile snowMenuContributionFile = new CommonMenuContributionFile();
        snowMenuContributionFile.contribution = new CommonIncludeMenuItem[1];

        snowMenuContributionFile.contribution[0] = new CommonIncludeMenuItem();

        snowMenuContributionFile.contribution[0] = processOtherContribution(
                cwa, "menus/snow/baseSnow.xml",
                "menu:obs?after=SNOWPLACEHOLDER", "SNOW",
                safeMenuContributionFile.contribution[0]);

        toXml(snowMenuContributionFile, "menus" + File.separator + "snow"
                + File.separator + "index.xml");

        statusHandler.info("Finished processing DAT menus.");
        override = false;
        datSite = null;
    }

    private CommonAbstractMenuContribution processContribution(String radar,
            String filePath) {
        CommonIncludeMenuContribution subIncludeContribution = new CommonIncludeMenuContribution();
        subIncludeContribution.substitutions = new VariableSubstitution[1];
        subIncludeContribution.substitutions[0] = new VariableSubstitution();
        subIncludeContribution.substitutions[0].key = "icao";
        subIncludeContribution.substitutions[0].value = radar;
        subIncludeContribution.fileName = new File(filePath);
        return subIncludeContribution;
    }

    /**
     * Process multiple contributions at the same time
     * 
     * @param radars
     * @param filePath
     * @param list
     * @return
     */
    private List<CommonAbstractMenuContribution> processRadarContributions(
            List<String> radars, String filePath,
            List<CommonAbstractMenuContribution> list) {
        for (int i = 0; i < radars.size(); i++) {
            CommonAbstractMenuContribution cont = processContribution(
                    radars.get(i), filePath);
            list.add(cont);
        }

        return list;
    }

    private CommonAbstractMenuContribution processFFMPContribution(String site,
            String source, String dataKey, String displayName, String filePath,
            CommonIncludeMenuContribution subIncludeContribution) {
        subIncludeContribution.substitutions = new VariableSubstitution[4];

        subIncludeContribution.substitutions[0] = new VariableSubstitution();
        subIncludeContribution.substitutions[0].key = "site";
        subIncludeContribution.substitutions[0].value = site;

        subIncludeContribution.substitutions[1] = new VariableSubstitution();
        subIncludeContribution.substitutions[1].key = "source";
        subIncludeContribution.substitutions[1].value = source;

        subIncludeContribution.substitutions[2] = new VariableSubstitution();
        subIncludeContribution.substitutions[2].key = "dataKey";
        subIncludeContribution.substitutions[2].value = dataKey;

        subIncludeContribution.substitutions[3] = new VariableSubstitution();
        subIncludeContribution.substitutions[3].key = "displayName";
        subIncludeContribution.substitutions[3].value = displayName;
        subIncludeContribution.fileName = new File(filePath);
        return subIncludeContribution;
    }

    private CommonIncludeMenuItem processOtherContribution(String cwa,
            String filePath, String installLoc, String subMenuName,
            CommonIncludeMenuItem commonIncludeMenu) {

        commonIncludeMenu.substitutions = new VariableSubstitution[1];
        commonIncludeMenu.substitutions[0] = new VariableSubstitution();
        commonIncludeMenu.substitutions[0].key = "cwa";
        commonIncludeMenu.substitutions[0].value = cwa;
        commonIncludeMenu.fileName = new File(filePath);
        commonIncludeMenu.installationLocation = installLoc;
        commonIncludeMenu.subMenuName = subMenuName;

        return commonIncludeMenu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#checkCreated()
     */
    @Override
    public boolean checkCreated() {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext ct = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile lfile = pm.getLocalizationFile(ct, "ffmp"
                + File.separator + "FFMPRunConfig.xml");
        long runConfigModified = 0;
        if (lfile != null && lfile.exists()) {
            runConfigModified = lfile.getFile().lastModified();
        }

        lfile = pm.getLocalizationFile(ct, "ffmp" + File.separator
                + "FFMPSourceConfig.xml");
        long sourceConfigModified = 0;
        if (lfile != null && lfile.exists()) {
            sourceConfigModified = lfile.getFile().lastModified();
        }

        lfile = pm.getLocalizationFile(caveConfigured, "menus" + File.separator
                + "scan" + File.separator + "scan.xml");
        long menuModified = 0;
        if (lfile != null && lfile.exists()) {
            menuModified = lfile.getFile().lastModified();
        }

        long scanRunConfigModified = 0;
        lfile = pm.getLocalizationFile(ct, "scan" + File.separator
                + "ScanRunConfig.xml");
        if (lfile != null && lfile.exists()) {
            scanRunConfigModified = lfile.getFile().lastModified();
        }

        if (runConfigModified > menuModified
                || sourceConfigModified > menuModified
                || scanRunConfigModified > menuModified) {
            return false;
        } else {
            statusHandler.info("Menus already created for site " + getSite()
                    + " for decision assist tools");
            return true;
        }
    }

    public void setOverride(boolean override) {
        this.override = override;
    }

    public boolean isOverride() {
        return override;
    }

    public void setDatSite(String datSite) {
        this.datSite = datSite;
    }

    public String getDatSite() {
        return datSite;
    }
}
