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
package com.raytheon.uf.edex.netcdf.description.field.indirect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Contains the information necessary to extract a pattern string.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2016  5446       skorolev     Initial creation
 * May 19, 2016 5584       nabowle      Updates for consolidation.
 *
 * </pre>
 *
 * @author skorolev
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class PatternDescription extends DelegateFieldDescription {

    /**
     * Default output format if not configured. For patterns with multiple
     * groups, this pattern is repeated with a single space in between.
     */
    private static final String DEFAULT_FORMAT = "%s";

    /**
     * The regex to be applied.
     */
    @XmlAttribute(required = true)
    private String inputPattern;

    /**
     * A format string to be applied to the groups returned from the
     * inputPattern
     */
    @XmlAttribute
    private String outputFormat;


    public String getInputPattern() {
        return inputPattern;
    }

    public void setInputPattern(String inputPattern) {
        this.inputPattern = inputPattern;
    }

    public String getOutputFormat() {
        return outputFormat;
    }

    public void setOutputFormat(String outputFormat) {
        this.outputFormat = outputFormat;
    }

    /**
     * Outputs string matching input pattern.
     *
     * @param rawFieldValue
     * @return
     */
    protected String outputString(String rawFieldValue) {
        Pattern pattern = Pattern.compile(this.inputPattern);
        if (rawFieldValue != null) {
            Matcher m = pattern.matcher(rawFieldValue);
            String[] groups = new String[m.groupCount()];
            if (m.find()) {
                for (int i = 1; i <= m.groupCount(); i++) {
                    groups[i - 1] = m.group(i);
                }
                if (this.outputFormat != null) {
                    return String.format(this.outputFormat, (Object[]) groups);
                } else if (groups.length == 1) {
                    // simplest, and likely most common case
                    return String.format(DEFAULT_FORMAT, (Object[]) groups);
                } else {
                    StringBuilder sb = new StringBuilder(groups.length * 3 - 1);
                    for (int i = 0; i < groups.length; i++) {
                        if (i > 0) {
                            sb.append(" ");
                        }
                        sb.append(DEFAULT_FORMAT);
                    }
                    return String.format(sb.toString(), (Object[]) groups);
                }
            }
        }
        return null;
    }

    @Override
    public String getString(NetcdfFile file)
            throws InvalidDescriptionException {
        return outputString(super.getString(file));
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        return outputString(super.getString(file, index));
    }

    @Override
    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        String value = this.getString(file);
        if (value != null) {
            try {
                return Double.parseDouble(value);
            } catch (NumberFormatException e) {
                throw new InvalidDescriptionException(value
                        + " cannot be parsed as a Number.", e);
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        String value = this.getString(file, index);
        if (value != null) {
            try {
                return Double.parseDouble(value);
            } catch (NumberFormatException e) {
                throw new InvalidDescriptionException(value
                        + " cannot be parsed as a Number.", e);
            }
        }
        return null;
    }


    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            throw new InvalidDescriptionException("delegate is not configured");
        }
        try {
            getNumber(file);
            return true;
        } catch (InvalidDescriptionException e) {
            return false;
        }
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        return getString(file) != null;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (this.inputPattern == null) {
            throw new InvalidDescriptionException(
                    "inputPattern is not configured.");
        }

        try {
            Pattern.compile(inputPattern);
        } catch (PatternSyntaxException e) {
            new InvalidDescriptionException("inputPattern is invalid.");
        }
    }

}
