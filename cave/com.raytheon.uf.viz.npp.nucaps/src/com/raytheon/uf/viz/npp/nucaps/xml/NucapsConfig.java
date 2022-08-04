package com.raytheon.uf.viz.npp.nucaps.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * NUCAPS Configuration: QC color model: 0: pass, 1,17: partially passed, 9,25:
 * totally failed, others.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2017 19924       wkwock     Initial creation
 *
 * </pre>
 *
 * @author wkwock
 */
@XmlRootElement(name = "nucapsConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class NucapsConfig {
    @XmlElement(name = "passed")
    private String passedColorName;

    @XmlElement(name = "partiallyPassed")
    private String partiallyPassedColorName;

    @XmlElement(name = "totallyFailed")
    private String totallyFailedColorName;

    @XmlElement(name = "default")
    private String defaultColorName;

    public String getPassedColorName() {
        return passedColorName;
    }

    public void setPassedColorName(String passedColorName) {
        this.passedColorName = passedColorName;
    }

    public String getPartiallyPassedColorName() {
        return partiallyPassedColorName;
    }

    public void setPartiallyPassedColorName(String partiallyPassedColorName) {
        this.partiallyPassedColorName = partiallyPassedColorName;
    }

    public String getTotallyFailedColorName() {
        return totallyFailedColorName;
    }

    public void setTotallyFailedColorName(String totallyFailedColorName) {
        this.totallyFailedColorName = totallyFailedColorName;
    }

    public String getDefaultColorName() {
        return defaultColorName;
    }

    public void setDefaultColorName(String defaultColorName) {
        this.defaultColorName = defaultColorName;
    }
}
