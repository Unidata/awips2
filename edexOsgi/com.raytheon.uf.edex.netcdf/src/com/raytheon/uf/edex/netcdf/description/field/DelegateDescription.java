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
package com.raytheon.uf.edex.netcdf.description.field;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.FilenameDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.ValuesDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableAttributeDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.FormattedFieldsDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.PatternDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.ReferencedFieldDescription;

/**
 * Base class for a description that uses a delegate.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2016  5584      nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlSeeAlso({ AbstractFieldDescription.class, ValueDescription.class,
        ValuesDescription.class, AttributeDescription.class,
        VariableDescription.class, VariableAttributeDescription.class,
        ReferencedFieldDescription.class, FilenameDescription.class,
        PatternDescription.class, FormattedFieldsDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public class DelegateDescription {

    /*
     * When updating this list. Update the @XmlSeeAlso above, and make the same
     * change to DelegatesDescription.
     */
    @XmlElements({
            @XmlElement(name = "value", type = ValueDescription.class),
            @XmlElement(name = "values", type = ValuesDescription.class),
            @XmlElement(name = "attribute", type = AttributeDescription.class),
            @XmlElement(name = "variable", type = VariableDescription.class),
            @XmlElement(name = "variableAttribute", type = VariableAttributeDescription.class),
            @XmlElement(name = "reference", type = ReferencedFieldDescription.class),
            @XmlElement(name = "filename", type = FilenameDescription.class),
            @XmlElement(name = "pattern", type = PatternDescription.class),
            @XmlElement(name = "formatted", type = FormattedFieldsDescription.class) })
    private IFieldDescription delegate;

    /**
     * @return the delegate
     */
    public IFieldDescription getDelegate() {
        return delegate;
    }

    /**
     * @param delegate
     *            the delegate to set
     */
    public void setDelegate(IFieldDescription delegate) {
        this.delegate = delegate;
    }

    public void validate() throws InvalidDescriptionException {
        if (this.delegate == null) {
            throw new InvalidDescriptionException(
                    "the delegate description is not configured");
        }
        this.delegate.validate();
    }

    public boolean isPresent(NetcdfFile netcdfFile)
            throws InvalidDescriptionException {
        return this.delegate.isPresent(netcdfFile);
    }
}
