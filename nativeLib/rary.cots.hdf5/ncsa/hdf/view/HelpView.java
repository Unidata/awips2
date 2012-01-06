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

/**
 *
 * The helpview interface for displaying user help information
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public abstract interface HelpView {
    /** display help informaion */
    public abstract void show();

    /** Returns the HelpView's label, which is used to displayed in the HDFVIew help menu. */
    public abstract String getLabel();

    /** Returns the action command for this HelpView. */
    public abstract String getActionCommand();
}
