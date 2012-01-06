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

package ncsa.hdf.object;

import java.util.List;

/**
 * An interface that provides general I/O operations for read/write object data.
 * For example, reading data content or data attribute from file into memory
 * or writing data content or data attribute from memory into file.
 * <p>
 * @see ncsa.hdf.object.HObject
 *
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public interface DataFormat
{
    /**
     * Returns the full path of the file that contains this data object.
     * <p>
     * The file name is necessary because data objects are uniquely identified
     * by object reference and file name when mutilple files are opened at
     * the same time.
     */
    public abstract String getFile();

    /**
     * Retrieves the metadata such as attributes from file.
     * <p>
     * Metadata such as attributes are stored in a List.
     *
     * @return the list of metadata objects.
     */
    public abstract List getMetadata() throws Exception;

    /**
     * Writes a specific metadata (such as attribute) into file. 
     * 
     * <p>
     * If metadata exists, the method updates its value. If the metadata 
     * does not exists in file, it creates the metadata in file and attaches 
     * it to the object.
     *
     * @param info the metadata to write.
     */
    public abstract void writeMetadata(Object info) throws Exception;

    /**
     * Deletes an existing metadata from this data object.
     *
     * @param info the metadata to delete.
     */
    public abstract void removeMetadata(Object info) throws Exception;
    
    /**
     * Check if the object has any attributes attached.
     *
     * @return true if it has any attribute(s), false otherwise.
     */
    public abstract boolean hasAttribute ();

}
