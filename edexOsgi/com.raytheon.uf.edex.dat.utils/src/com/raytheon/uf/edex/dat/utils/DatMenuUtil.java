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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;

/**
 * Utility for DAT menu creation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 20, 2010           mnash     Initial creation
 * Aug 20, 2010           dhladky   Added more for FFMP, moved to DatUtils
 * Jun 06, 2011  9827     bkowal    Skip scan menu creation if the Run
 *                                  Configuration Manager has not read a
 *                                  FFMPRunConfig.xml file.
 * Sep 04, 2014  3220     skorolev  Updated menu creation for Fog, Safeseas and
 *                                  Snow monitors.
 * Sep 18, 2015  3873     skorolev  Corrected Fog, Safeseas and Snow monitor's
 *                                  names.
 * Oct 15, 2015  4897     bkowal    Update the base fog, snow, and safeseas menu
 *                                  xml.
 * Jan 04, 2016  5115     skorolev  Replaced Mon.Name with App.Name for Fog,
 *                                  Safeseas and Snow monitors
 * Aug 09, 2016  5819     mapeters  Get lowest localized level of config files
 *                                  in checkCreated(), use correct scan config
 *                                  file name
 * Jan 11, 2017  5934     njensen   Refactored to make separate menu files for
 *                                  FFMP and SCAN
 * Aug 21, 2017  6372     mapeters  Use common_static instead of cave_static
 * Jul 10, 2018  6695     njensen   Update for single FFMPRunXML
 * Jul 30, 2018  6720     njensen   Update for changed method names
 *
 * </pre>
 *
 * @author mnash
 */

public class DatMenuUtil extends AbstractMenuUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatMenuUtil.class);

    private boolean override = false;

    private String datSite = null;

    protected LocalizationContext commonStaticBase = pm
            .getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

    private static final String MENUS_PATH = "menus";

    private static final String FOG_PATH = "fog";

    private static final String SAFESEAS_PATH = "safeseas";

    private static final String SNOW_PATH = "snow";

    private static final String BASE_FOG_XML = "baseFog.xml";

    private static final String BASE_SAFESEAS_XML = "baseSafeSeas.xml";

    private static final String BASE_SNOW_XML = "baseSnow.xml";

    @Override
    public void createMenus() {
        statusHandler.info("Creating DAT menus...");
        if (!override) {
            if (getSite() != null && !getSite()
                    .equals(System.getProperty("aw.site.identifier"))) {
                statusHandler.info(getSite()
                        + " is not a configured site for DAT.  No menus will be created.");
                return;
            } else if (getSite() == null) {
                statusHandler.info(
                        "No site is set for menu creation.  No menus will be created.");
                return;
            }
        } else {
            if (getDatSite() != null) {
                setSite(getDatSite());
            } else {
                statusHandler.info(
                        "No site is set for menu creation.  No menus will be created.");
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

        List<String> radars = new ArrayList<>();
        List<String> ffmpProductKeys = new ArrayList<>();
        FFMPRunXML runner = ffmpManager.getFFMPRunner();
        if (runner.getProducts() != null) {
            for (ProductRunXML product : runner.getProducts()) {
                if (!ffmpProductKeys.contains(product.getProductKey())) {
                    ffmpProductKeys.add(product.getProductKey());
                }
            }
        }

        radars.addAll(scanManager.getLocalSiteNames());

        createFFMPMenu(runner, ffmpProductKeys);
        createSCANMenu(scanManager, radars);

        /***************************************
         * OTHER
         ********************************/
        createFogMenu();
        createSafeseasMenu();
        createSnowMenu();

        statusHandler.info("Finished processing DAT menus.");
        override = false;
        datSite = null;
    }

    /**
     * Process Contribution.
     *
     * @param radar
     * @param filePath
     * @return
     */
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

    /**
     * Process FFMP Contribution.
     *
     * @param site
     * @param source
     * @param dataKey
     * @param displayName
     * @param filePath
     * @param subIncludeContribution
     * @return
     */
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

    /**
     * Creates FFMP Menus.
     */
    private void createFFMPMenu(FFMPRunXML runner,
            List<String> ffmpProductKeys) {
        List<CommonAbstractMenuContribution> listContributions = new ArrayList<>();
        CommonTitleContribution titleItem = new CommonTitleContribution();
        titleItem.titleText = "------ FFMP ------";
        titleItem.id = "FFMPSCAN";
        listContributions.add(titleItem);

        for (String productKey : ffmpProductKeys) {
            int i = 0;
            ProductRunXML product = runner.getProduct(productKey);
            CommonSubmenuContribution mainSiteMenu = new CommonSubmenuContribution();

            SourceXML primarySource = FFMPSourceConfigurationManager
                    .getInstance().getSource(product.getProductName());
            if (primarySource.getDataType() == DataType.XMRG) {
                mainSiteMenu.id = product.getProductKey().toUpperCase();
                mainSiteMenu.menuText = product.getProductKey().toUpperCase();
            } else {
                mainSiteMenu.id = product.getProductKey();
                mainSiteMenu.menuText = product.getProductKey();
            }

            // see how many types we have (DPR, DHR, etc) within each
            // siteKey

            List<ProductRunXML> products = runner.getProducts();

            // table entries + QPF + Guidance --start QPE source drop down
            int tableEntrySize = runner.getProducts(product.getProductKey())
                    .size();
            mainSiteMenu.contributions = new CommonAbstractMenuContribution[products
                    .size() + tableEntrySize];

            for (ProductRunXML innerproduct : runner
                    .getProducts(product.getProductKey())) {

                SourceXML qpesource = FFMPSourceConfigurationManager
                        .getInstance().getSource(innerproduct.getProductName());

                String qpepath = "menus" + IPathManager.SEPARATOR + "ffmp"
                        + IPathManager.SEPARATOR + "";
                CommonIncludeMenuContribution qpeIncludeMenuCont = new CommonIncludeMenuContribution();
                qpeIncludeMenuCont.fileName = new File(qpepath + "ffmpQPE.xml");
                qpeIncludeMenuCont.id = "QPE" + innerproduct.getProductName();

                // do substitutions
                String dataKey = innerproduct.getProductKey();

                qpeIncludeMenuCont = (CommonIncludeMenuContribution) processFFMPContribution(
                        innerproduct.getProductKey(), qpesource.getSourceName(),
                        dataKey, qpesource.getDisplayName(),
                        qpeIncludeMenuCont.fileName.getPath(),
                        qpeIncludeMenuCont);

                mainSiteMenu.contributions[i] = qpeIncludeMenuCont;
                i++;
            }

            // process QPF

            CommonSubmenuContribution qpfSiteMenu = new CommonSubmenuContribution();

            ProductXML qpfProduct = FFMPSourceConfigurationManager.getInstance()
                    .getProductByPrimarySourceName(
                            primarySource.getSourceName());

            List<String> qpfDispNames = product.getQpfDisplayNames(qpfProduct);

            qpfSiteMenu.contributions = new CommonAbstractMenuContribution[qpfDispNames
                    .size()];
            qpfSiteMenu.id = product.getProductKey() + "QPF";
            qpfSiteMenu.menuText = "QPF";
            int qpfTypeCounter = 0;

            // figure what types of qpf we have...
            for (String qpfDispName : qpfDispNames) {
                List<SourceXML> qpfSources = product
                        .getQpfSourcesByDisplayName(qpfProduct, qpfDispName);

                if (!qpfSources.isEmpty()) {
                    CommonSubmenuContribution qpfTypeMenu = new CommonSubmenuContribution();

                    qpfTypeMenu.contributions = new CommonAbstractMenuContribution[qpfSources
                            .size()];
                    qpfTypeMenu.id = product.getProductKey() + qpfSources.get(0)
                            .getDisplayName();
                    qpfTypeMenu.menuText = qpfSources.get(0).getDisplayName();

                    int qpfSourceCounter = 0;

                    for (SourceXML qpfSource : qpfSources) {
                        CommonIncludeMenuContribution qpfIncludeMenuCont = new CommonIncludeMenuContribution();
                        String qpfpath = "menus" + IPathManager.SEPARATOR
                                + "ffmp" + IPathManager.SEPARATOR + "";
                        qpfIncludeMenuCont.fileName = new File(
                                qpfpath + "ffmpQPF.xml");
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
                    .getInstance().getProductByPrimarySourceName(
                            primarySource.getSourceName());

            List<String> guidDispNames = product
                    .getGuidanceDisplayNames(guidProduct);

            guidSiteMenu.contributions = new CommonAbstractMenuContribution[guidDispNames
                    .size()];

            int guidTypeCounter = 0;

            // figure what type of guidance we have...
            for (String guidDisplayName : guidDispNames) {

                CommonSubmenuContribution guidTypeMenu = new CommonSubmenuContribution();
                List<SourceXML> guidSources = product
                        .getGuidanceSourcesByDisplayName(guidProduct,
                                guidDisplayName);

                guidTypeMenu.id = product.getProductKey() + guidSources.get(0)
                        .getDisplayName();
                guidTypeMenu.menuText = guidSources.get(0).getDisplayName();

                guidTypeMenu.contributions = new CommonAbstractMenuContribution[guidSources
                        .size()];

                int guidCounter = 0;
                for (SourceXML guidSource : guidSources) {
                    CommonIncludeMenuContribution guidIncludeMenuCont = new CommonIncludeMenuContribution();
                    String guidpath = "menus" + IPathManager.SEPARATOR + "ffmp"
                            + IPathManager.SEPARATOR + "";
                    guidIncludeMenuCont.fileName = new File(
                            guidpath + "ffmpGuidance.xml");
                    guidIncludeMenuCont.id = "Guidance"
                            + guidSource.getSourceName();

                    String guiddataKey = DataType.XMRG.name();

                    // construct the string with the duration hour for FFG
                    guidIncludeMenuCont = (CommonIncludeMenuContribution) processFFMPContribution(
                            product.getProductKey(), guidSource.getSourceName(),
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

        CommonIncludeMenuContribution ffmpAuxMenuCont = new CommonIncludeMenuContribution();
        String path = "menus" + IPathManager.SEPARATOR + "ffmp"
                + IPathManager.SEPARATOR + "";
        ffmpAuxMenuCont.fileName = new File(path + "auxFFMPMenu.xml");
        listContributions.add(ffmpAuxMenuCont);
        MenuTemplateFile menuTemplate = new MenuTemplateFile();
        menuTemplate.contributions = listContributions.toArray(
                new CommonAbstractMenuContribution[listContributions.size()]);
        toXml(menuTemplate, "menus" + IPathManager.SEPARATOR + "ffmp"
                + IPathManager.SEPARATOR + "ffmp.xml");
    }

    /**
     * Creates SCAN Menus.
     */
    private void createSCANMenu(SCANRunSiteConfigurationManager scanManager,
            List<String> radars) {
        List<CommonAbstractMenuContribution> listContributions = new ArrayList<>();

        CommonTitleContribution titleItem = new CommonTitleContribution();
        titleItem.titleText = "------ Dedicated Radars ------";
        titleItem.id = "DedicatedRadarsSCAN";
        listContributions.add(titleItem);
        listContributions = processRadarContributions(radars,
                "menus" + IPathManager.SEPARATOR + "scan"
                        + IPathManager.SEPARATOR + "baseScanMenu.xml",
                listContributions);

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
                    radars.get(ii), "menus" + IPathManager.SEPARATOR + "scan"
                            + IPathManager.SEPARATOR + "baseScanMenu.xml");
            listContributions.add(submenuContribution);
        }

        MenuTemplateFile menuTemplate = new MenuTemplateFile();
        menuTemplate.contributions = listContributions.toArray(
                new CommonAbstractMenuContribution[listContributions.size()]);
        toXml(menuTemplate, "menus" + IPathManager.SEPARATOR + "scan"
                + IPathManager.SEPARATOR + "scan.xml");
    }

    /**
     * Creates Safeseas Menu.
     */
    private void createSafeseasMenu() {
        FSSObsMonitorConfigurationManager ssConfig = FSSObsMonitorConfigurationManager
                .getInstance(AppName.SAFESEAS);
        Set<String> ssStns = ssConfig.getStationIDs();
        StringBuilder stations = new StringBuilder();
        for (String s : ssStns) {
            if (stations.length() > 0) {
                stations.append(",");
            }
            stations.append(s);
        }
        String ssStations = stations.toString();
        Path safeseasPath = Paths.get(MENUS_PATH, SAFESEAS_PATH,
                BASE_SAFESEAS_XML);
        MenuTemplateFile menuTemplate = (MenuTemplateFile) this
                .fromXml(safeseasPath.toString(), this.commonStaticBase);
        this.updateStations(menuTemplate, "SAFESEASMONITOR", ssStations);
        this.toXml(menuTemplate, safeseasPath.toString());
        ssConfig = null;
    }

    private void updateStations(MenuTemplateFile menuTemplate,
            final String bundleId, final String stationString) {
        for (CommonAbstractMenuContribution contribution : menuTemplate.contributions) {
            if (contribution instanceof CommonBundleMenuContribution
                    && ((CommonBundleMenuContribution) contribution).id
                            .equals(bundleId)) {
                CommonBundleMenuContribution bundleContribution = (CommonBundleMenuContribution) contribution;
                if (bundleId.equals(bundleContribution.id)) {
                    for (VariableSubstitution substitution : bundleContribution.substitutions) {
                        if ("stations".equals(substitution.key)) {
                            substitution.value = stationString;
                        }
                    }
                    break;
                }
            }
        }
    }

    /**
     * Creates Fog Menu.
     */
    private void createFogMenu() {
        FSSObsMonitorConfigurationManager fogConfig = FSSObsMonitorConfigurationManager
                .getInstance(AppName.FOG);
        Set<String> fogStns = fogConfig.getStationIDs();
        StringBuilder stations = new StringBuilder();
        for (String s : fogStns) {
            if (stations.length() > 0) {
                stations.append(",");
            }
            stations.append(s);
        }
        String fogStations = stations.toString();
        Path fogPath = Paths.get(MENUS_PATH, FOG_PATH, BASE_FOG_XML);
        MenuTemplateFile menuTemplate = (MenuTemplateFile) this
                .fromXml(fogPath.toString(), this.commonStaticBase);
        this.updateStations(menuTemplate, "FOGMONITOR", fogStations);
        this.toXml(menuTemplate, fogPath.toString());
        fogConfig = null;
    }

    /**
     * Creates Snow Menu.
     */
    private void createSnowMenu() {
        FSSObsMonitorConfigurationManager snowConfig = FSSObsMonitorConfigurationManager
                .getInstance(AppName.SNOW);
        Set<String> snowStns = snowConfig.getStationIDs();
        StringBuilder stations = new StringBuilder();
        for (String s : snowStns) {
            if (stations.length() > 0) {
                stations.append(",");
            }
            stations.append(s);
        }
        String snowStations = stations.toString();
        Path snowPath = Paths.get(MENUS_PATH, SNOW_PATH, BASE_SNOW_XML);
        MenuTemplateFile menuTemplate = (MenuTemplateFile) this
                .fromXml(snowPath.toString(), this.commonStaticBase);
        this.updateStations(menuTemplate, "SNOWMONITOR", snowStations);
        this.toXml(menuTemplate, snowPath.toString());
        snowConfig = null;
    }

    @Override
    public boolean checkCreated() {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        ILocalizationFile lfile = pm.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC,
                "ffmp" + IPathManager.SEPARATOR + "FFMPRunConfig.xml");
        Date runConfigModified = new Date(0);
        if (lfile != null && lfile.exists()) {
            runConfigModified = lfile.getTimeStamp();
        }

        lfile = pm.getLocalizationFile(commonConfigured,
                "menus" + IPathManager.SEPARATOR + "ffmp"
                        + IPathManager.SEPARATOR + "ffmp.xml");
        Date ffmpMenuModified = new Date(0);
        if (lfile != null && lfile.exists()) {
            ffmpMenuModified = lfile.getTimeStamp();
        }

        lfile = pm.getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                "ffmp" + IPathManager.SEPARATOR + "FFMPSourceConfig.xml");
        Date sourceConfigModified = new Date(0);
        if (lfile != null && lfile.exists()) {
            sourceConfigModified = lfile.getTimeStamp();
        }

        lfile = pm.getLocalizationFile(commonConfigured,
                "menus" + IPathManager.SEPARATOR + "scan"
                        + IPathManager.SEPARATOR + "scan.xml");
        Date scanMenuModified = new Date(0);
        if (lfile != null && lfile.exists()) {
            scanMenuModified = lfile.getTimeStamp();
        }

        Date scanRunConfigModified = new Date(0);
        lfile = pm.getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                "scan" + IPathManager.SEPARATOR + "SCANRunSiteConfig.xml");
        if (lfile != null && lfile.exists()) {
            scanRunConfigModified = lfile.getTimeStamp();
        }

        if (runConfigModified.after(scanMenuModified)
                || sourceConfigModified.after(scanMenuModified)
                || scanRunConfigModified.after(scanMenuModified)
                || runConfigModified.after(ffmpMenuModified)
                || sourceConfigModified.after(ffmpMenuModified)
                || scanRunConfigModified.after(ffmpMenuModified)) {
            return false;
        }

        statusHandler.info("Menus already created for site " + getSite()
                + " for decision assist tools");
        return true;
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
