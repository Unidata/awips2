package com.raytheon.uf.edex.netcdf.description.field.direct;

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;

/**
 * Contains the information necessary to extract a filename.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2016 5446       skorolev     Initial creation
 * May 19, 2016 5584       nabowle      Updates for consolidation.
 *
 * </pre>
 *
 * @author skorolev
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FilenameDescription extends AbstractFieldDescription {

    @Override
    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        // remove path string
        String[] st = file.getLocation().split(File.separator);
        return st[st.length - 1];
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        return getString(file);
    }

    @Override
    public Number getNumber(NetcdfFile file) throws InvalidDescriptionException {
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file, int n)
            throws InvalidDescriptionException {
        return null;
    }

    @Override
    public long getLength(NetcdfFile file) {
        return 1;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        return false;
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        return true;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        // do nothing
    }
}
