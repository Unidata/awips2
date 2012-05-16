package com.raytheon.uf.common.dataplugin.warning.config;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "configuration")
public class DialogConfiguration implements ISerializableObject {
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
        String xml = FileUtil.open("config.xml", localSite);
        return (DialogConfiguration) SerializationUtil.unmarshalFromXml(xml);
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
