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
package com.raytheon.uf.edex.core.modes;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Represents the xml file for dynamic configuration of spring files to include
 * at EDEX startup
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2010            njensen     Initial creation
 * Sep 19, 2012 1195       djohnson    Allow 0..n other modes to be included.
 * Dec 05, 2013 2566       bgonzale    Migrated to edex.core.modes package.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement(name = "mode")
@XmlAccessorType(XmlAccessType.NONE)
public class EdexMode extends DefaultEdexMode {

    @XmlAttribute(name = "name")
    @XmlID
    private String name;

    @XmlElements( { @XmlElement(name = "include", type = String.class) })
    private final List<String> includeList;

    private final List<Pattern> compiledIncludes;

    @XmlElements( { @XmlElement(name = "exclude", type = String.class) })
    private final List<String> excludeList;

    private final List<Pattern> compiledExcludes;

    @XmlElements({ @XmlElement(name = "includeMode") })
    @XmlIDREF
    private final List<EdexMode> includedModes;

    private boolean inited;

    @XmlAttribute
    private boolean template;

    public EdexMode() {
        this(new ArrayList<String>(), new ArrayList<String>(),
                new ArrayList<EdexMode>());
    }

    // @VisibleForTesting
    EdexMode(List<String> includeList, List<String> excludeList,
            List<EdexMode> includedModes) {
        this.includeList = includeList;
        compiledIncludes = new ArrayList<Pattern>(includeList.size());
        this.excludeList = excludeList;
        compiledExcludes = new ArrayList<Pattern>(excludeList.size());
        this.includedModes = includedModes;
    }

    /**
     * Compiles the patterns
     */
    public void init() {

        for (EdexMode includedMode : includedModes) {
            if (!includedMode.isInited()) {
                includedMode.init();
            }
        }

        for (String s : includeList) {
            compiledIncludes.add(Pattern.compile(s));
        }

        for (String s : excludeList) {
            compiledExcludes.add(Pattern.compile(s));
        }

        for (EdexMode includedMode : includedModes) {
            compiledIncludes.addAll(includedMode.compiledIncludes);
            compiledExcludes.addAll(includedMode.compiledExcludes);
        }
        inited = true;
    }

    /**
     * Checks if the filename matches against the include and exclude regexes
     * 
     * @param filename
     *            the filename to check
     * @return true if the file should be included, otherwise false
     */
    public boolean includeFile(String filename) {
        if (filename.contains(File.separator)) {
            filename = filename
                    .substring(filename.lastIndexOf(File.separator) + 1);
        }

        // If we explicitly exclude the pattern, just return false
        for (Pattern p : compiledExcludes) {
            if (p.matcher(filename).find()) {
                return false;
            }
        }

        boolean matches = false;

        // default to include * if no include regexes are present
        if (compiledIncludes.isEmpty()) {
            matches = true;
        } else {
            for (Pattern p : compiledIncludes) {
                if (p.matcher(filename).find()) {
                    matches = true;
                    break;
                }
            }
        }

        return matches;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean accept(File dir, String name) {
        boolean result = super.accept(dir, name);
        if (result) {
            result = includeFile(name);
        }
        return result;
    }

    public boolean isInited() {
        return inited;
    }

    public void setInited(boolean inited) {
        this.inited = inited;
    }

    /**
     * Return whether or not the mode is a template mode. Template modes cannot
     * be booted.
     * 
     * @return the template
     */
    public boolean isTemplate() {
        return template;
    }

    /**
     * Set whether the mode is a template or not.
     * 
     * @param template
     *            true to denote a template mode
     */
    public void setTemplate(boolean template) {
        this.template = template;
    }

}
