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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.DelegateDescription;
import com.raytheon.uf.edex.netcdf.description.field.IFieldDescription;

/**
 * A generic field descriptor that enables simpler xml by requiring a delegate
 * AbstractFieldDescription with a simple, standard naming scheme:
 *
 * <pre>
 *  &lt;field name="datasetId"&gt;
 *    &lt;variable name="dataset" /&gt;
 *  &lt;/field&gt;
 *  or
 *  &lt;field name="datasetId"&gt;
 *    &lt;attribute name="dataset" /&gt;
 *  &lt;/field&gt;
 *  and so on for each AbstractFieldDescription subclass
 *
 *  instead of
 *
 *  &lt;datasetIdVariable name="dataset" /&gt;
 *  or
 *  &lt;datasetIdAttribute name="dataset" /&gt;
 *  and so on for each AbstractFieldDescription subclass since this has become
 *  unwieldy as new subclasses were added.
 * </pre>
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 09, 2016 5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DelegateFieldDescription extends DelegateDescription implements
        IFieldDescription {
    /**
     * Constructor
     */
    public DelegateFieldDescription() {
        super();
    }

    public String getString(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            return null;
        }

        return this.getDelegate().getString(file);
    }

    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (index < 0) {
            throw new InvalidDescriptionException(
                    new IndexOutOfBoundsException("Invalid index " + index));
        }

        if (this.getDelegate() == null) {
            return null;
        }
        return this.getDelegate().getString(file, index);
    }

    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            return null;
        }
        return this.getDelegate().getNumber(file);
    }

    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (index < 0) {
            throw new InvalidDescriptionException(
                    new IndexOutOfBoundsException("Invalid index " + index));
        }

        if (this.getDelegate() == null) {
            return null;
        }
        return this.getDelegate().getNumber(file, index);
    }

    public long getLength(NetcdfFile file) throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            return 0;
        }
        return this.getDelegate().getLength(file);
    }

    public boolean isNumeric(NetcdfFile netcdfFile)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            throw new InvalidDescriptionException(
                    "delegate field description is not configured");
        }
        return this.getDelegate().isNumeric(netcdfFile);
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.getDelegate() == null) {
            return false;
        }
        return this.getDelegate().isPresent(file);
    }

    /**
     * Get the name of this field if configured, otherwise get the delegate
     * field's name if possible.
     */
    public String getName() {
        if (this.getDelegate() == null) {
            return null;
        }
        return this.getDelegate().getName();
    }
}
