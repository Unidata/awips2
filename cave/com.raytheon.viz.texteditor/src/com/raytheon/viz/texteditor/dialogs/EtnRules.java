package com.raytheon.viz.texteditor.dialogs;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Describe rules for modifying VTEC ETNs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 07, 2015 ASM #18132 D. Friedman Initial creation
 *
 * </pre>
 *
 */
@XmlRootElement(name = "etnRules")
@XmlAccessorType(XmlAccessType.NONE)
public class EtnRules {
    private List<String> excludePhenSigs = new ArrayList<String>();

    @XmlElement
    @XmlList
    public List<String> getExcludePhenSigs() {
        return excludePhenSigs;
    }

    public void setExcludePhenSigs(List<String> excludePhenSigs) {
        this.excludePhenSigs = excludePhenSigs;
    }
}
