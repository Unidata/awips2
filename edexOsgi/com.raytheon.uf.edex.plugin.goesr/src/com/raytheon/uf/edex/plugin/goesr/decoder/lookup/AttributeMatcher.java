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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrDecoderException;

/**
 * 
 * Object which can be used to test a {@link NetcdfFile} to see if it contains
 * global attributes that match a specified pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AttributeMatcher {

    @XmlAttribute
    private String attribute;

    @XmlAttribute
    private String pattern;

    private transient Pattern compiled;

    public String getAttribute() {
        return attribute;
    }

    public void setAttribute(String attribute) {
        this.attribute = attribute;
    }

    public String getPattern() {
        return pattern;
    }

    public void setPattern(String pattern) {
        this.pattern = pattern;
    }

    public boolean matches(NetcdfFile cdfFile) throws GoesrDecoderException {
        Attribute attr = cdfFile.findGlobalAttribute(attribute);
        if (attr == null) {
            return false;
        }
        if (compiled == null) {
            try {
                compiled = Pattern.compile(pattern);
            } catch (PatternSyntaxException e) {
                throw new GoesrDecoderException("Cannot parse pattern ["
                        + pattern + "] for attribute + [" + attribute + "]", e);
            }
        }
        return compiled.matcher(attr.getStringValue()).matches();

    }

}
