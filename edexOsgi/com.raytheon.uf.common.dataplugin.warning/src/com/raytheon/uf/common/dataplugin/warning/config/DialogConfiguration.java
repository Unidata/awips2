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
package com.raytheon.uf.common.dataplugin.warning.config;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Configuration for warngen dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * 10/22/2013   2361       njensen     Use JAXBManager for XML
 * Apr 28, 2014 3033       jsanchez    Refactored file retrieval.
 * Jul 02, 2014 DR 17450   D. Friedman Support using list of templates from backup site.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "configuration")
public class DialogConfiguration {

    private static final SingleTypeJAXBManager<DialogConfiguration> jaxb = SingleTypeJAXBManager
            .createWithoutException(DialogConfiguration.class);
    private static final String CONFIG_FILE = "config.xml";


    @XmlElement
    private String warngenOfficeShort;

    @XmlElement
    private String warngenOfficeLoc;

    @XmlElement
    private String backupCWAs;

    @XmlElement
    private String siteNode;

    @XmlElement
    private String defaultTemplate;

    @XmlElement
    private String mainWarngenProducts;

    @XmlElement
    private String otherWarngenProducts;

    @XmlElement
    private long followupListRefeshDelay;

    @XmlElement
    private GridSpacing gridSpacing;

    public static DialogConfiguration loadDialogConfig(String localSite)
            throws FileNotFoundException, IOException, JAXBException {
        String xml = WarnFileUtil.convertFileContentsToString(CONFIG_FILE,
                localSite, null);
        return (DialogConfiguration) jaxb.unmarshalFromXml(xml);
    }

    public static DialogConfiguration loadDialogConfigNoUser(String site)
            throws FileNotFoundException, IOException, JAXBException {
        String xml = WarnFileUtil.convertFileContentsToStringNoUser(
                CONFIG_FILE, site);
        return (DialogConfiguration) jaxb.unmarshalFromXml(xml);
    }

    public static boolean isSiteDialogConfigExtant(String backupSite) {
        return WarnFileUtil.isLocalizationFileExtantAtSiteLevel(CONFIG_FILE,
                backupSite);
    }

    public String getWarngenOfficeShort() {
        return warngenOfficeShort;
    }

    public void setWarngenOfficeShort(String warngenOfficeShort) {
        this.warngenOfficeShort = warngenOfficeShort;
    }

    public String getWarngenOfficeLoc() {
        return warngenOfficeLoc;
    }

    public void setWarngenOfficeLoc(String warngenOfficeLoc) {
        this.warngenOfficeLoc = warngenOfficeLoc;
    }

    public String getBackupCWAs() {
        return backupCWAs;
    }

    public void setBackupCWAs(String backupCWAs) {
        this.backupCWAs = backupCWAs;
    }

    public String getSiteNode() {
        return siteNode;
    }

    public void setSiteNode(String siteNode) {
        this.siteNode = siteNode;
    }

    public String getDefaultTemplate() {
        return defaultTemplate;
    }

    public void setDefaultTemplate(String defaultTemplate) {
        this.defaultTemplate = defaultTemplate;
    }

    public String getMainWarngenProducts() {
        return mainWarngenProducts;
    }

    public void setMainWarngenProducts(String mainWarngenProducts) {
        this.mainWarngenProducts = mainWarngenProducts;
    }

    public String getOtherWarngenProducts() {
        return otherWarngenProducts;
    }

    public void setOtherWarngenProducts(String otherWarngenProducts) {
        this.otherWarngenProducts = otherWarngenProducts;
    }

    public long getFollowupListRefeshDelay() {
        return followupListRefeshDelay;
    }

    public void setFollowupListRefeshDelay(long followupListRefeshDelay) {
        this.followupListRefeshDelay = followupListRefeshDelay;
    }

    public GridSpacing getGridSpacing() {
        return gridSpacing;
    }

    public void setGridSpacing(GridSpacing gridSpacing) {
        this.gridSpacing = gridSpacing;
    }

}
