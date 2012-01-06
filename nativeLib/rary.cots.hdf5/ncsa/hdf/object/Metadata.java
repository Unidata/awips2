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

/**
 * Metadata is a general interface for metadata attached to data objects.
 * Metadata contains supporting information attacehd to a primary data or
 * component. Particular implementations of Metadata often provide additional
 * context-specific objects as well.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public interface Metadata extends java.io.Serializable
{
    /**
     * Returns the value of this Metadata.
     */
    public abstract Object getValue();

    /**
     * Sets the value of this Metadata.
     */
    public abstract void setValue(Object value);
}
