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
package com.raytheon.uf.edex.netcdf.description.field.date;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 *
 * Contains the information necessary to extract a {@link Date} from a
 * {@link NetcdfFile} in a specific format.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Aug 25, 2015  4699     nabowle   Extracted from Pointset netcdf plugin,
 *                                  renamed, and refactored.
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * Apr 19, 2016  5450     nabowle   Add multi-date retrieval.
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FormattedDateValue extends AbstractDateValue {

    @XmlAttribute(required = true)
    private String dateFormat;

    private transient SimpleDateFormat sdf;

    public String getDateFormat() {
        return dateFormat;
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    private SimpleDateFormat getSimpleDateFormat()
            throws InvalidDescriptionException {
        if (sdf == null) {
            try {
                sdf = new SimpleDateFormat(dateFormat);
            } catch (IllegalArgumentException e) {
                throw new InvalidDescriptionException(
                        "Unable to parse date format", e);
            }
        }
        return sdf;
    }

    @Override
    public synchronized Date getDate(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        String dateString = this.getDelegate().getString(file, index);
        if (dateString == null) {
            return null;
        }
        try {
            return getSimpleDateFormat().parse(dateString);
        } catch (ParseException e) {
            throw new InvalidDescriptionException(e);
        }
    }

    @Override
    public synchronized void validate() throws InvalidDescriptionException {
        super.validate();
        if (dateFormat == null) {
            throw new InvalidDescriptionException(
                    "The data format attribute is not configured.");
        }
        getSimpleDateFormat();
    }

}
