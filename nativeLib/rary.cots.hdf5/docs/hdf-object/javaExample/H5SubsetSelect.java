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
import ncsa.hdf.object.h5.*;  // the HDF5 implementation

/**
 * <p>Title: HDF Object Package (Java) Example</p>
 * <p>Description: this example shows how to select a subset using the
 * "HDF Object Package (Java)". The example creates an integer dataset,
 * and read subset of the dataset:
 * <pre>
 *     "/" (root)
 *             2D 32-bit integer 20x10
 * </pre>
 *
 *The whole 20x10 data set is
 * <pre>
1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009
1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1109
1200, 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 1209
1300, 1301, 1302, 1303, 1304, 1305, 1306, 1307, 1308, 1309
1400, 1401, 1402, 1403, 1404, 1405, 1406, 1407, 1408, 1409
1500, 1501, 1502, 1503, 1504, 1505, 1506, 1507, 1508, 1509
1600, 1601, 1602, 1603, 1604, 1605, 1606, 1607, 1608, 1609
1700, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709
1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809
1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909
2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109
2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207, 2208, 2209
2300, 2301, 2302, 2303, 2304, 2305, 2306, 2307, 2308, 2309
2400, 2401, 2402, 2403, 2404, 2405, 2406, 2407, 2408, 2409
2500, 2501, 2502, 2503, 2504, 2505, 2506, 2507, 2508, 2509
2600, 2601, 2602, 2603, 2604, 2605, 2606, 2607, 2608, 2609
2700, 2701, 2702, 2703, 2704, 2705, 2706, 2707, 2708, 2709
2800, 2801, 2802, 2803, 2804, 2805, 2806, 2807, 2808, 2809
2900, 2901, 2902, 2903, 2904, 2905, 2906, 2907, 2908, 2909
</pre>
 *
 * Subset: start=(4, 2), size=(5, 3) and stride=(3, 2).
 * The subset values are:
 * <pre>
1402,1404,1406
1702,1704,1706
2002,2004,2006
2302,2304,2306
2602,2604,2606
 * </pre>
 * </p>
 *
 * @author Peter X. Cao
 * @version 2.4
 */
public class H5SubsetSelect
{
    private static String fname = "H5SubsetSelect.h5";
    private static long[] dims2D = {20, 10};

    public static void main( String args[] ) throws Exception
    {
        // create the file and add groups ans dataset into the file
        createFile();

        // retrieve an instance of H5File
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

        if (fileFormat == null)
        {
            System.err.println("Cannot find HDF5 FileFormat.");
            return;
        }

        // open the file with read and write access
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

        // initialize the dataset: load dataset information such as datatype
        // and dataspace into memory.
        dataset.init();

        // start, stride and sizes will determined the selected subset
        long[] start = dataset.getStartDims();
        long[] stride = dataset.getStride();
        long[] sizes = dataset.getSelectedDims();

        // select the subset: starting at (4, 2)
        start[0] = 4;
        start[1] = 2;

        // select the subset: subset size (5, 3)
        sizes[0] = 5;
        sizes[1] = 3;

        // select the subset: set stride to (3, 2)
        stride[0] = 3;
        stride[1] = 2;

        // read the data of the subset
        int[] dataRead = (int[])dataset.read();

        // print out the data values
        System.out.println("\n\nSubset Data Values");
        for (int i=0; i<5; i++)
        {
            System.out.print("\n"+dataRead[i*3]);
            for (int j=1; j<3; j++)
            {
                System.out.print(","+dataRead[i*3+j]);
            }
        }

        // close file resource
        testFile.close();
    }

    /**
     * create the file and add groups ans dataset into the file,
     * which is the same as javaExample.H5DatasetCreate
     * @see javaExample.H5DatasetCreate
     * @throws Exception
     */
    private static void createFile() throws Exception
    {
        // retrieve an instance of H5File
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

        if (fileFormat == null)
        {
            System.err.println("Cannot find HDF5 FileFormat.");
            return;
        }

        // create a new file with a given file name.
        H5File testFile = (H5File)fileFormat.create(fname);

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
                dataIn[i*10+j] = 1000+i*100+j;
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
