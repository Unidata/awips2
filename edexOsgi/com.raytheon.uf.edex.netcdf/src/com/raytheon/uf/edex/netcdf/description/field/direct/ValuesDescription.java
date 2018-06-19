package com.raytheon.uf.edex.netcdf.description.field.direct;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;

/**
 * Specifies fixed list of values in the XML.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2016 5452       skorolev    Initial creation
 * May 19, 2016 5584       nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author skorolev
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ValuesDescription extends AbstractFieldDescription {

    public ValuesDescription() {
        super();
    }

    @XmlElement(name = "value")
    private List<String> values = new ArrayList<>();

    public List<String> getValues() {
        return values;
    }

    public void setValues(List<String> values) {
        this.values = values;
    }

    @Override
    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        return getString(file, 0);
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {

        if (values == null && values.isEmpty()) {
            return null;
        }

        // should have used a ValueDescription.
        if (values.size() == 1) {
            return values.get(0);
        }

        if (index >= values.size()) {
            return null;
        }

        return values.get(index);
    }

    @Override
    public Number getNumber(NetcdfFile file) throws InvalidDescriptionException {
        return getNumber(file, 0);
    }

    @Override
    public Number getNumber(NetcdfFile file, int n)
            throws InvalidDescriptionException {
        String value = getString(file, n);
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
    public long getLength(NetcdfFile file) {
        if (values != null) {
            return values.size();
        }
        return 0;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.values == null) {
            throw new InvalidDescriptionException("values is not configured.");
        }
        try {
            getNumber(file, 0);
            return true;
        } catch (InvalidDescriptionException e) {
            return false;
        }
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        return this.values != null;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.values == null) {
            throw new InvalidDescriptionException("values is not configured.");
        }

        if (this.values.isEmpty()) {
            throw new InvalidDescriptionException("values is empty.");
        }
    }

}
