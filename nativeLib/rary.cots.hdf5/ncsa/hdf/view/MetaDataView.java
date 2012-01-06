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

package ncsa.hdf.view;

import ncsa.hdf.object.*;

/**
 *
 *The metadata view interface for displaying metadata information
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public abstract interface MetaDataView extends DataView
{
    /** add an attribute to a data object.*/
    public abstract Attribute addAttribute(HObject obj);

    /** delete an attribribute from a data object.*/
    public abstract Attribute deleteAttribute(HObject obj);

}
