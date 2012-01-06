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
package com.raytheon.edex.plugin.grib;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Object containing the Grib Patterns which receive single threaded decode
 * handling. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/15/10     6644        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 * @see com.raytheon.edex.plugin.grib.GribLargeFileChecker
 * @see com.raytheon.edex.plugin.grib.GribLockRelease
 */
@XmlRootElement(name = "largeGribPatterns")
@XmlAccessorType(XmlAccessType.NONE)
public class LargeGribPatterns implements ISerializableObject {
    /**
     * List of patterns
     */
    @XmlElements({ @XmlElement(name = "regex", type = String.class) })
    private List<String> patterns;

    /** List of compiled patterns */
    private List<Pattern> compiledPatterns;

    /**
     * Creates a new instance of the container.
     */
    public LargeGribPatterns() {
        this.patterns = new ArrayList<String>();
        this.compiledPatterns = new ArrayList<Pattern>();
    }

    /**
     * Returns a list of the stored patterns as a series of strings.
     * 
     * @return a list of regex pattern strings
     */
    public List<String> getPatterns() {
        return patterns;
    }

    /**
     * Sets the list of regex strings for this container.
     * 
     * @param patterns
     *            an arraylist of regex strings
     */
    public void setPatterns(ArrayList<String> patterns) {
        this.patterns = patterns;
    }

    /**
     * Inserts a single string into the list.
     * 
     * @param pattern
     *            The regex string to insert
     */
    public void setPattern(String pattern) {
        this.patterns.add(pattern);
    }

    /**
     * Will compile the strings into Pattern objects.
     * 
     */
    public void compilePatterns() {
        for (String pattern : patterns) {
            compiledPatterns.add(Pattern.compile(pattern));
        }
    }

    /**
     * Takes a string and compares against the patterns in this container. The
     * first one that matches breaks the search and returns true.
     * 
     * @param header
     *            The string to search for
     * @return a boolean indicating success
     */
    public boolean isDesiredHeader(String header) {
        boolean isFound = false;
        for (Pattern headerPattern : compiledPatterns) {
            if (headerPattern.matcher(header).find()) {
                isFound = true;
                break;
            }
        }
        return isFound;
    }
}
