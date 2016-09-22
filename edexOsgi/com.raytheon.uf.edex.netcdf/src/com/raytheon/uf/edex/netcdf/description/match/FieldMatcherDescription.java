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
package com.raytheon.uf.edex.netcdf.description.match;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.DelegateDescription;

/**
 *
 * Object which can be used to test a {@link NetcdfFile} to see if it contains a
 * field that matches a specified pattern.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * Apr 14, 2016  5450     nabowle     Adapted from goes-r to common plugin.
 * May 19, 2016  5584     nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FieldMatcherDescription extends DelegateDescription implements
        IMatcherDescription {

    @XmlAttribute
    private String pattern;

    private transient Pattern compiled;

    public String getPattern() {
        return pattern;
    }

    public void setPattern(String pattern) {
        this.pattern = pattern;
    }

    public boolean matches(NetcdfFile cdfFile)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            return false;
        }
        String fieldVal = this.getDelegate().getString(cdfFile);
        if (fieldVal == null) {
            return false;
        }
        if (compiled == null) {
            compilePattern();
        }
        return compiled.matcher(fieldVal).matches();
    }

    /**
     * @throws InvalidDescriptionException
     */
    private void compilePattern() throws InvalidDescriptionException {
        try {
            compiled = Pattern.compile(pattern);
        } catch (PatternSyntaxException e) {
            throw new InvalidDescriptionException("Cannot parse pattern ["
                    + pattern + "]", e);
        }
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.pattern == null) {
            throw new InvalidDescriptionException("pattern is not configured");
        }
        compilePattern();
        super.validate();
    }

}
