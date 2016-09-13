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
package com.raytheon.uf.edex.netcdf.description.date;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * 
 * Base class to extract a {@link Date} from a {@link NetcdfFile}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 25, 2015  4699     nabowle   Initial creation
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlSeeAlso({ FormattedDateValue.class, EpochOffsetDateValue.class })
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractDateValue {

    @XmlElements({ @XmlElement(name = "value", type = ValueDescription.class),
            @XmlElement(name = "variable", type = VariableDescription.class),
            @XmlElement(name = "attribute", type = AttributeDescription.class) })
    private AbstractFieldDescription field;

    public AbstractFieldDescription getField() {
        return field;
    }

    public void setField(AbstractFieldDescription field) {
        this.field = field;
    }

    public abstract Date getDate(NetcdfFile file)
            throws InvalidDescriptionException;

    public void validate() throws InvalidDescriptionException {
        if (field == null) {
            throw new InvalidDescriptionException(
                    "A field element is not present.");
        }
        try {
            field.validate();
        } catch (InvalidDescriptionException e) {
            throw new InvalidDescriptionException("Invalid field: "
                    + e.getMessage(), e);
        }
    }
}
