package com.raytheon.viz.texteditor.dialogs;

import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlList;
import jakarta.xml.bind.annotation.XmlRootElement;

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
