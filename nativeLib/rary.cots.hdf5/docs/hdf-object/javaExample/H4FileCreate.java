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

package javaExample;

import ncsa.hdf.object.*;     // the common object package
import ncsa.hdf.object.h4.*;  // the HDF4 implementation

/**
 * <p>Title: HDF Object Package (Java) Example</p>
 * <p>Description: this example shows how to create an empty HDF4 file using the
 * "HDF Object Package (Java)".</p>
 *
 * @author Peter X. Cao
 * @version 2.4
 */
public class H4FileCreate
{
    private static String fname = "H4FileCreate.hdf";

    public static void main( String args[] ) throws Exception
    {
        // retrieve an instance of H4File
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4);

        if (fileFormat == null)
        {
            System.err.println("Cannot find HDF4 FileFormat.");
            return;
        }

        // create a new file with a given file name.
        H4File testFile = (H4File)fileFormat.create(fname);

        if (testFile == null)
        {
            System.err.println("Failed to create file:"+fname);
            return;
        }
    }
}
