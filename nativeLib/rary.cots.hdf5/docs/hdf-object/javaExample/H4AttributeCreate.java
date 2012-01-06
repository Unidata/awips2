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
import java.util.List;

/**
 * <p>Title: HDF Object Package (Java) Example</p>
 * <p>Description: this example shows how to create/read/write HDF attribute using the
 * "HDF Object Package (Java)". The example creates an attribute and,
 * read and write the attribute value:
 * <pre>
 *     "/" (root)
 *             2D 32-bit integer 20x10
 *             (attribute: name="data range", value=[0, 10000])
 * </pre>
 * </p>
 *
 * @author Peter X. Cao
 * @version 2.4
 */
public class H4AttributeCreate
{
    private static String fname = "H4AttributeCreate.hdf";
    private static long[] dims2D = {20, 10};

    public static void main( String args[] ) throws Exception
    {
        // create the file and add groups ans dataset into the file
        createFile();

        // retrieve an instance of H4File
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4);

        if (fileFormat == null)
        {
            System.err.println("Cannot find HDF4 FileFormat.");
            return;
        }

        // open the file with read and write  access
        FileFormat testFile = fileFormat.open(fname, FileFormat.WRITE);

        if (testFile == null)
        {
            System.err.println("Failed to open file: "+fname);
            return;
        }

        // open the file and retrieve the file structure
        testFile.open();
        Group root = (Group)((javax.swing.tree.DefaultMutableTreeNode)testFile.getRootNode()).getUserObject();

        // retrieve athe dataset "2D 32-bit integer 20x10"
        Dataset dataset = (Dataset)root.getMemberList().get(0);

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        Datatype dtype = testFile.createDatatype(
            Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE);
        long[] attrDims = {2}; // 1D of size two
        int[] attrValue = {0, 10000}; // attribute value

        // create a attribute of 1D integer of size two
        Attribute attr = new Attribute("data range", dtype, attrDims);
        attr.setValue(attrValue); // set the attribute value

        // attach the attribute to the dataset
        dataset.writeMetadata(attr);

        // read the attribute into memory
        List attrList = dataset.getMetadata();
        attr = (Attribute)attrList.get(0);

        // print out attribute value
        System.out.println( attr.toString() );
        System.out.println( attr.toString("  ") );

        // close file resource
        testFile.close();
    }

    /**
     * create the file and add groups ans dataset into the file,
     * which is the same as javaExample.H4DatasetCreate
     * @see javaExample.H4DatasetCreate
     * @throws Exception
     */
    private static void createFile() throws Exception
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

        // open the file and retrieve the root group
        testFile.open();
        Group root = (Group)((javax.swing.tree.DefaultMutableTreeNode)testFile.getRootNode()).getUserObject();

        // set the data values
        int[] dataIn = new int[20*10];
        for (int i=0; i<20; i++)
        {
            for (int j=0; j<10; j++)
            {
                dataIn[i*10+j] = i*100+j;
            }
        }

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        Datatype dtype = testFile.createDatatype(
            Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE);
        Dataset dataset = testFile.createScalarDS
            ("2D 32-bit integer 20x10", root, dtype, dims2D, null, null, 0, dataIn);

        // close file resource
        testFile.close();
    }

}
