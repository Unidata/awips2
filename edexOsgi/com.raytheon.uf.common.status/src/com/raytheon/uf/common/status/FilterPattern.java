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
package com.raytheon.uf.common.status;

import java.util.ArrayList;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Filter strings by defined include and exclude patterns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2010            njensen     Initial creation
 * Apr 12, 2011            bgonzale    Refactored from EdexMode
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement(name = "filterPattern")
@XmlAccessorType(XmlAccessType.NONE)
public class FilterPattern {

    @XmlAttribute(name = "name")
    private String name;

    @XmlElements({ @XmlElement(name = "include", type = String.class) })
    private ArrayList<String> includeList;

    private ArrayList<Pattern> compiledIncludes;

    @XmlElements({ @XmlElement(name = "exclude", type = String.class) })
    private ArrayList<String> excludeList;

    private ArrayList<Pattern> compiledExcludes;

    public FilterPattern() {
        includeList = new ArrayList<String>();
        compiledIncludes = new ArrayList<Pattern>();
        excludeList = new ArrayList<String>();
        compiledExcludes = new ArrayList<Pattern>();
    }

    public FilterPattern(String name) {
        this();
        this.name = name;
    }

    public void addInclude(String include) {
        includeList.add(include);
    }

    public void addExclude(String exclude) {
        excludeList.add(exclude);
    }

    /**
     * Checks if the string matches against the include and exclude regexes
     * 
     * @param str
     *            the string to check
     * @return true if the string should be included, otherwise false
     */
    public boolean accept(String str) {
        boolean matches = false;

        // default to include * if no include regexes are present
        if (compiledIncludes.size() == 0) {
            matches = false;
        } else {
            for (Pattern p : compiledIncludes) {
                if (!p.matcher("").find() && p.matcher(str).find()) {
                    matches = true;
                    break;
                }
            }
        }

        if (matches) {
            for (Pattern p : compiledExcludes) {
                if (p.matcher(str).find()) {
                    matches = false;
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

    /**
     * Compiles the patterns
     */
    public void compile() {
        compiledIncludes.clear();
        for (String s : includeList) {
            compiledIncludes.add(Pattern.compile(s));
        }
        compiledExcludes.clear();
        for (String s : excludeList) {
            compiledExcludes.add(Pattern.compile(s));
        }
    }

}
