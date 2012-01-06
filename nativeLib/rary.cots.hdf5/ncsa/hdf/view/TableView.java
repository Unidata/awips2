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

import javax.swing.JTable;

/**
 *
 * The table view interface for displaying data in table form
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public abstract interface TableView extends DataView
{
    /** returns the table */
    public abstract JTable getTable();

    /** returns array of selected data */
    public abstract Object getSelectedData();

    /** Write the change of a dataset into file. */
    public abstract void updateValueInFile();


}
