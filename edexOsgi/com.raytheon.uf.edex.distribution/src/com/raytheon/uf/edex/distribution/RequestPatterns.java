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
package com.raytheon.uf.edex.distribution;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A container of regular expressions, both original strings and the compiled
 * patterns. Used by the DistributionSrv bean to store regex patterns for
 * plugins. It is important to note that no validation is done on duplicate
 * regex patterns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2009            brockwoo     Initial creation
 * May 16, 2011 7317       cjeanbap     Added try-catch statement
 *                                      for PatternSyntaxException.
 * Mar 19, 2013 1794       djohnson     Add toString() for debugging.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

@XmlRootElement(name = "requestPatterns")
@XmlAccessorType(XmlAccessType.NONE)
public class RequestPatterns implements ISerializableObject{
    
    /**
     * List of patterns requested by a plugin.
     */
    @XmlElements( { @XmlElement(name = "regex", type = String.class) })
    private List<String> patterns = new ArrayList<String>();
    
    private final List<Pattern> compiledPatterns = new ArrayList<Pattern>();
    
    protected transient Log patternFailedLogger = LogFactory.getLog("PatternFailedLog");
    
    /**
     * Creates a new instance of the container.
     */
    public RequestPatterns(){
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
     * @param patterns an arraylist of regex strings
     */
    public void setPatterns(List<String> patterns) {
        this.patterns = patterns;
    }
    
    /**
     * Inserts a single string into the list.
     * 
     * @param pattern The regex string to insert
     */
    public void setPattern(String pattern) {
        this.patterns.add(pattern);
    }
    
    /**
     * Will compile the strings into Pattern objects.
     * 
     */
    public void compilePatterns(){
        for(String pattern : patterns) {
            try {
                compiledPatterns.add(Pattern.compile(pattern));
            } catch (PatternSyntaxException e) {
                StringBuilder sb = new StringBuilder();
                sb.append("Failed to compile pattern: ");
                sb.append(pattern).append(".");
                patternFailedLogger.error(sb.toString(), e);
            }
        }
    }
    
    /**
     * Takes a string and compares against the patterns in this
     * container.  The first one that matches breaks the search and 
     * returns true.
     * 
     * @param header  The string to search for
     * @return a boolean indicating success
     */
    public boolean isDesiredHeader(String header) {
        boolean isFound = false;
        for(Pattern headerPattern : compiledPatterns) {
            if(headerPattern.matcher(header).find()) {
                isFound = true;
                break;
            }
        }
        return isFound;
    }

    @Override
    public String toString() {
        return patterns.toString();
    }
}
