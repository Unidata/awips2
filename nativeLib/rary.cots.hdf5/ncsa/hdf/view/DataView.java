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
 * The data view interface for displaying data object
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public abstract interface DataView {
    /** The unknown view type */
    public final static int DATAVIEW_UNKNOWN = -1;

    /** The table view type */
    public final static int DATAVIEW_TABLE = 1;

    /** The image view type */
    public final static int DATAVIEW_IMAGE = 2;

    /** The text view type */
    public final static int DATAVIEW_TEXT = 3;

    /** returns the data object displayed in this data viewer */
    public abstract HObject getDataObject();

    /** Disposes this datao viewer */
    public abstract void dispose();

}
