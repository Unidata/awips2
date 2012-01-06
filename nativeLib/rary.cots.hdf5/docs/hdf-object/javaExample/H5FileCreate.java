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

import ncsa.hdf.object.*;     // the common object package
import ncsa.hdf.object.h5.*;  // the HDF5 implementation

/**
 * <p>Title: HDF Object Package (Java) Example</p>
 * <p>Description: This example shows how to create an empty HDF5 file using 
 * the "HDF Object Package (Java)".  If the file (H5FileCreate.h5) already 
 * exists, it will be truncated to zero length.
 * </p>
 *
 * @author Peter X. Cao
 * @version 2.4
 */
public class H5FileCreate
{
    // The name of the file we'll create.
    private static String fname = "H5FileCreate.h5";

    public static void main( String args[] ) throws Exception
    {
        // Retrieve an instance of the implementing class for the HDF5 format
        FileFormat fileFormat = 
                   FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

        // If the implementing class wasn't found, it's an error.
        if (fileFormat == null)
        {
            System.err.println("Cannot find HDF5 FileFormat.");
            return;
        }

        // If the implementing class was found, use it to create a new HDF5 file
        // with a specific file name.  
        //
        // If the specified file already exists, it is truncated.
        // The default HDF5 file creation and access properties are used.
        // 
        H5File testFile = (H5File)fileFormat.createFile(fname, 
					         FileFormat.FILE_CREATE_DELETE);

        // Check for error condition and report.
        if (testFile == null)
        {
            System.err.println("Failed to create file: "+fname);
            return;
        }

        // End of example that creates an empty HDF5 file named H5FileCreate.h5.
    }
}
