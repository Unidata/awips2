package com.raytheon.uf.featureexplorer.jaxb;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class Plugin {

    @XmlAttribute
    private String id;

    @XmlAttribute(name = "download-size")
    private double downloadSize;

    @XmlAttribute
    private String version;

    @XmlAttribute(name = "install-size")
    private double installSize;

    @XmlAttribute
    private boolean unpack;

    public Plugin() {
    }

    public Plugin(String anId, double aDownloadSize, double anInstallSize,
            String aVersion, boolean doUnpack) {
        this.id = anId;
        this.downloadSize = aDownloadSize;
        this.version = aVersion;
        this.installSize = anInstallSize;
        this.unpack = doUnpack;
    }

    public String getId() {
        return this.id;
    }

    public double getDownloadSize() {
        return this.downloadSize;
    }

    public String getVersion() {
        return this.version;
    }

    public double getInstallSize() {
        return this.installSize;
    }

    public boolean getUnpack() {
        return this.unpack;
    }

    public void setId(String anId) {
        this.id = anId;
    }

    public void setDownloadSize(double aDownloadSize) {
        this.downloadSize = aDownloadSize;
    }

    public void setVersion(String aVersion) {
        this.version = aVersion;
    }

    public void setInstallSize(double anInstallSize) {
        this.installSize = anInstallSize;
    }

    public void setUnpack(boolean doUnpack) {
        this.unpack = doUnpack;
    }
}
