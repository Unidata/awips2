/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.object.fits;

import java.util.*;
import ncsa.hdf.object.*;

/**
 * An H5Group represents HDF5 group, inheriting from Group.
 * Every HDF5 object has at least one name. An HDF5 group is used to store
 * a set of the names together in one place, i.e. a group. The general
 * structure of a group is similar to that of the UNIX file system in
 * that the group may contain references to other groups or data objects
 * just as the UNIX directory may contain subdirectories or files.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class FitsGroup extends Group
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object. Members of the list are
     * instance of Attribute.
     */
    private List attributeList;

    /** The default object ID for HDF5 objects */
    private final static long[] DEFAULT_OID = {0};

    /**
     * Constructs an HDF5 group with specific name, path, and parent.
     * <p>
     * @param fileFormat the file which containing the group.
     * @param name the name of this group.
     * @param path the full path of this group.
     * @param parent the parent of this group.
     * @param oid the unique identifier of this data object.
     */
    public FitsGroup(
        FileFormat fileFormat,
        String name,
        String path,
        Group parent,
        long[] theID) {
        super (fileFormat, name, path, parent, ((theID == null) ? DEFAULT_OID : theID));
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () { return false; }

    // Implementing DataFormat
    public List getMetadata() throws Exception {
        if (!isRoot()) {
            return null; // there is only one group in the file: the root
        }

        if (attributeList != null) {
            return attributeList;
        }

        return attributeList;
    }

    /**
     * Creates a new attribute and attached to this dataset if attribute does
     * not exist. Otherwise, just update the value of the attribute.
     *
     * <p>
     * @param info the atribute to attach
     */
    public void writeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /**
     * Deletes an attribute from this dataset.
     * <p>
     * @param info the attribute to delete.
     */
    public void removeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // Implementing DataFormat
    public int open() {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /** close group access */
    public void close(int gid) {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /**
     * Creates a new group.
     * @param file the file which the group is added to.
     * @param name the name of the group to create.
     * @param pgroup the parent group of the new group.
     * @return the new group if successful. Otherwise returns null.
     */
    public static FitsGroup create(String name, Group pgroup)
        throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

}
