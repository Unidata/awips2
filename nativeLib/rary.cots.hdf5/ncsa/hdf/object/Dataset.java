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

import java.lang.reflect.Array;
import java.util.Vector;

/**
 * The abstract class provides general APIs to create and manipulate dataset objects, 
 * and retrieve dataset properties datatype and dimension sizes. 
 * <p>
 * This class provides two convenient functions, read()/write(), to read/write data values.
 * Reading/writing data may take many library calls if we use the library APIs directly.
 * The read() and write functions hide all the details of these calls from users.
 * 
 * @see ncsa.hdf.object.ScalarDS
 * @see ncsa.hdf.object.CompoundDS
 *
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public abstract class Dataset extends HObject
{
    /**
     * The memory buffer that holds the raw data of the dataset.
     */
    protected Object data;

    /**
     * The number of dimensions of the dataset.
     */
    protected int rank;

    /**
     * The current dimension sizes of the dataset
     */
    protected long[] dims;
    
    /**
     * The max dimension sizes of the dataset
     */
    protected long[] maxDims;    

    /**
     * Array that contains the number of data points selected (for read/write) in each dimension.
     * <p>
     * The select size must be less than or equal to the current dimension size.
     * A subset of a rectangle selection is defined by the starting position 
     * and selected sizes.
     * <p>
     * For example, a 4 X 5 dataset
     * <pre>
     *     0,  1,  2,  3,  4
     *    10, 11, 12, 13, 14
     *    20, 21, 22, 23, 24
     *    30, 31, 32, 33, 34
     * long[] dims = {4, 5};
     * long[] startDims = {1, 2};
     * long[] selectedDims = {3, 3};
     *
     * then the following subset is selected by the startDims and selectedDims above
     *     12, 13, 14
     *     22, 23, 24
     *     32, 33, 34
     */
    protected long[] selectedDims;

    /**
     * The starting position of each dimension of a selected subset.
     * With both the starting position and selected sizes, the subset of a
     * rectangle selection is fully defined.
     */
    protected long[] startDims;

    /**
     * Array that contains the indices of the dimensions selected for display.
     * <p>
     * <B>selectedIndex[] is provied for two purpose:</B>
     * <OL>
     * <LI>
     * selectedIndex[] is used to indicate the order of dimensions for display, i.e.
     * selectedIndex[0] = row, selectedIndex[1] = column and selectedIndex[2] = depth.
     * For example, for a four dimesion dataset, if selectedIndex[] is {1, 2, 3},
     * then dim[1] is selected as row index, dim[2] is selected as column index
     * and dim[3] is selected as depth index.
     * <LI>
     * selectedIndex[] is also used to select dimensions for display for
     * datasets with three or more dimensions. We assume that applications such
     * as HDFView can only display data up to three dimensions (a 2D spreadsheet/image
     * with a third dimension that the 2D spreadsheet/image is cut from). For
     * dataset with more than three dimensions, we need selectedIndex[] to store
     * which three dimensions are chosen for display.
     * For example, for a four dimesion dataset, if selectedIndex[] = {1, 2, 3},
     * then dim[1] is selected as row index, dim[2] is selected as column index
     * and dim[3] is selected as depth index. dim[0] is not selected. Its location
     * is fixed at 0 by default.
     * </OL>
     */
    protected final int[] selectedIndex;

    /**
     * The number of elements to move from the start location in each dimension.
     *  For example, if selectedStride[0] = 2, every other data point is selected
     *  along dim[0].
     */
    protected long[] selectedStride;

    /**
     * The array of dimension sizes for a chunk.
     */
    protected long[] chunkSize;

    /**
     * The GZIP compression level.
     */
    protected String compression;

    /** The datatype object of the dataset. */
    protected Datatype datatype;

    /** 
     * Array of strings that represent the dimension names.
     * It is null if dimension names do not exist.
     */
    protected String[] dimNames;

    /** Flag to indicate if the byte[] array is converted to strings */
    protected boolean convertByteToString = true;

    /** Flag to indicate if data values are loaded into memory. */
    protected boolean isDataLoaded = false;

    /** The number of data points in the memory buffer. */
    protected long nPoints = 1;

    /** 
     * The data buffer that contains the raw data directly reading from 
     * file (before any data conversion). 
     */
    protected Object originalBuf = null;

    /** 
     * The array that holds the converted data of unsigned C-type integers.
     * <p>
     * For example, Suppose that the original data is an array of unsigned 16-bit 
     * short integers. Since Java does not support unsigned integer, the data is
     * converted to an array of 32-bit singed integer. In that case, the converted
     * buffer is the array of 32-bit singed integer.
     */
    protected Object convertedBuf = null;
    
    /**
     * Flag to indicate if the enum data is converted to strings.
     */
    protected boolean enumConverted = false;


    /**
     * Constructs a Dataset object with a given file, name and path.
     * <p>
     * @param theFile the file that contains the dataset.
     * @param name the name of the Dataset, e.g. "dset1".
     * @param path the full group path of this Dataset, e.g. "/arrays/".
     */
    public Dataset(FileFormat theFile, String name, String path)
    {
        this(theFile, name, path, null);
    }

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #Dataset(FileFormat, String, String)}
     */
    public Dataset(
        FileFormat theFile,
        String name,
        String path,
        long[] oid)
    {
        super (theFile, name, path, oid);

        rank = 0;
        data = null;
        dims = null;
        maxDims = null;
        selectedDims = null;
        startDims = null;
        selectedStride = null;
        chunkSize = null;
        compression = "NONE";
        dimNames = null;

        selectedIndex = new int[3];
        selectedIndex[0] = 0;
        selectedIndex[1] = 1;
        selectedIndex[2] = 2;
    }

    /**
     * Clears memory held by the dataset, such as data buffer.
     */
    public void clear() {
     	if (data != null) {
     		if (data instanceof Vector) {
                ((Vector) data).setSize(0);
            }
     		data = null;
            originalBuf = null;
            convertedBuf = null;
     	}
     	isDataLoaded = false;
    }
    
    /**
     * Retrieves datatype and dataspace information from file and sets the dataset in memory.
     * <p>
     * The init() is designed to support lazy operation in dataset object. 
     * When a data object is retrieved from file, the datatype, dataspace 
     * and raw data are not loaded into memory. When it is asked 
     * to read the raw data from file, init() is first called to get the datatype 
     * and dataspace information, then load the raw data from file.
     * <p>
     * init() is also used to reset selection of a dataset (start, stride and count)
     * to the default, which is the entire dataset for 1D or 2D datasets. In the
     * following example, init() at step 1) retrieve datatype and dataspace information
     * from file. getData() at step 3) read only one data point. init() at step 4) reset
     * the selection to the whole dataset. getData() at step 4) reads the values of
     * whole dataset into memory.
     * 
     * <pre>
            dset = (Dataset)file.get(NAME_DATASET);
            
            // 1) get datatype and dataspace information from file
            dset.init();
            rank = dset.getRank(); // rank = 2, a 2D dataset
            count = dset.getSelectedDims();
            start = dset.getStartDims();
            dims = dset.getDims();

            // 2) select only one data point
            for (int i=0; i<rank; i++) {
                start[0] = 0;
                count[i] = 1;
            }
            
            // 3)  read one data point
            data = dset.getData();
           
            // 4)  reset to select the whole dataset
            dset.init();
            
            // 5) clean the memory data buffer
            dset.clearData();
     
            // 6) Read the whole dataset
            data = dset.getData();
        </pre>
     */
    public abstract void init();

    /**
     * Returns the rank (number of dimensions) of the dataset.
     * 
     * @return the number of dimensions of the dataset.
     */
    public final int getRank()
    {
        return rank;
    }

    /**
     * Returns the array that contins the dimension sizes of the dataset.
     * 
     * @return the dimension sizes of the dataset.
     */
    public final long[] getDims()
    {
        return dims;
    }

    /**
     * Returns the array that contins the max dimension sizes of the dataset.
     * 
     * @return the max dimension sizes of the dataset.
     */
    public final long[] getMaxDims()
    {
        if (maxDims == null)
            return dims;
        
        return maxDims;
    }
    
    /**
     * Returns the dimension sizes of the selected subset.
     * <p>
     * The SelectedDims is the number of data points of the selected subset. Applications
     * can use this array to change the size of selected subset.
     *  
     * The select size must be less than or equal to the current dimension size.
     * Combined with the starting position, selected sizes and stride, the subset of a
     * rectangle selection is fully defined.
     * <p>
     * For example, a 4 X 5 dataset
     * <pre>
     *     0,  1,  2,  3,  4
     *    10, 11, 12, 13, 14
     *    20, 21, 22, 23, 24
     *    30, 31, 32, 33, 34
     * long[] dims = {4, 5};
     * long[] startDims = {1, 2};
     * long[] selectedDims = {3, 3};
     * long[] selectedStride = {1, 1};
     *
     * then the following subset is selected by the startDims and selectedDims
     *     12, 13, 14
     *     22, 23, 24
     *     32, 33, 34
     * </pre>
     * 
     * @return the dimension sizes of the selected subset.
     */
    public final long[] getSelectedDims()
    {
        return selectedDims;
    }

    /**
     * Returns the starting position of a selected subset.
     * <p>
     * Applications can use this array to change the starting position of a selection. 
     * Combined with the selected dimensions, selected sizes and stride, the subset of a
     * rectangle selection is fully defined.
     * <p>
     * For example, a 4 X 5 dataset
     * <pre>
     *     0,  1,  2,  3,  4
     *    10, 11, 12, 13, 14
     *    20, 21, 22, 23, 24
     *    30, 31, 32, 33, 34
     * long[] dims = {4, 5};
     * long[] startDims = {1, 2};
     * long[] selectedDims = {3, 3};
     * long[] selectedStride = {1, 1};
     *
     * then the following subset is selected by the startDims and selectedDims
     *     12, 13, 14
     *     22, 23, 24
     *     32, 33, 34
     * </pre>
     * 
     * @return the starting position of a selected subset.
     */
    public final long[] getStartDims()
    {
        return startDims;
    }

    /**
     * Returns the selectedStride of the selected dataset.
     * <p>
     * Applications can use this array to change how many elements to move in each dimension.
     * 
     * Combined with the starting position and selected sizes, the subset of a
     * rectangle selection is defined.
     * <p>
     * For example, a 4 X 5 dataset
     * <pre>
     *     0,  1,  2,  3,  4
     *    10, 11, 12, 13, 14
     *    20, 21, 22, 23, 24
     *    30, 31, 32, 33, 34
     * long[] dims = {4, 5};
     * long[] startDims = {0, 0};
     * long[] selectedDims = {2, 2};
     * long[] selectedStride = {2, 3};
     *
     * then the following subset is selected by the startDims and selectedDims
     *     0,   3
     *     20, 23
     * </pre>
     */
    public final long[] getStride()
    {
        if (rank <=0) {
            return null;
        }

        if (selectedStride == null)
        {
            selectedStride = new long[rank];
            for (int i=0; i<rank; i++) {
                selectedStride[i] = 1;
            }
        }

        return selectedStride;
    }

    /**
     * Sets the flag that indicates if a byte array is converted to a string array.
     * <p>
     * In a string dataset, the raw data from file is stored in a byte array.
     * By default, this byte array is converted to an array of strings.
     * For a large dataset (e.g. more than one million strings), the converson takes 
     * a long time and requires a lot of memory space to store the strings. At some 
     * applications, such a conversion can be delayed. For example, A GUI application
     * may convert only part of the strings that are visible to the users, not the 
     * entire data array.
     * <p>
     * setConvertByteToString(boolean b) allows users to set the flag so that 
     * applications can choose to perform the byte-to-string conversion or not. 
     * If the flag is set to false, the getData() returns a array of byte instead
     * of an array of strings. 
     *  
     * @param b convert bytes to strings if b is true; otherwise,
     *          if false, do not convert bytes to strings.
     */
    public final void setConvertByteToString(boolean b)
    {
        convertByteToString = b;
    }

    /**
     * Returns the flag that indicates if a byte array is converted to a string array..
     * 
     * @return true if byte array is converted to string; otherwise, returns false if there is no conversion.
     */
    public final boolean getConvertByteToString()
    {
        return convertByteToString;
    }

    /**
     * Reads the data from file.
     * <p>
     * read() reads the data from file to a memory buffer and returns the memory buffer.
     * The dataset object does not hold the memobry buffer. To store the memory buffer
     * in the dataset object, one must call getData(). 
     * <p>
     * By default, the whole dataset is read into memory. Users can also 
     * select subset to read. Subsetting is done in an implicit way.
     * <p>
     * <b>How to Select a Subset</b>
     * <p>
     * A selection is specified by three arrays: start, stride and count.
     * <ol>
     *   <li>start:  offset of a selection
     *   <li>stride: determining how many elements to move in each dimension
     *   <li>count:  number of elements to select in each dimension
     * </ol>
     * getStartDims(), getStartDims() and getSelectedDims() returns the start, 
     * stride and count arrays respectively. Applications can make a selection 
     * by changing the values of the arrays.
     * <p>
     * The following example shows how to make a subset. In the example, the 
     * dataset is a 4-dimensional array of [200][100][50][10], i.e.
     * dims[0]=200; dims[1]=100; dims[2]=50; dims[3]=10; <br>
     * We want to select every other data point in dims[1] and dims[2]
     * <pre>
         int rank = dataset.getRank();   // number of dimension of the dataset
         long[] dims = dataset.getDims(); // the dimension sizes of the dataset
         long[] selected = dataset.getSelectedDims(); // the selected size of the dataet
         long[] start = dataset.getStartDims(); // the off set of the selection
         long[] stride = dataset.getStride(); // the stride of the dataset
         int[]  selectedIndex = dataset.getSelectedIndex(); // the selected dimensions for display
    
         // select dim1 and dim2 as 2D data for display,and slice through dim0
         selectedIndex[0] = 1;
         selectedIndex[1] = 2;
         selectedIndex[1] = 0;
    
         // reset the selection arrays
         for (int i=0; i&lt;rank; i++) {
             start[i] = 0;
             selected[i] = 1;
             stride[i] = 1;
        }
    
        // set stride to 2 on dim1 and dim2 so that every other data points are selected.
        stride[1] = 2;
        stride[2] = 2;
    
        // set the selection size of dim1 and dim2
        selected[1] = dims[1]/stride[1];
        selected[2] = dims[1]/stride[2];
    
        // when dataset.getData() is called, the slection above will be used since
        // the dimension arrays are passed by reference. Changes of these arrays
        // outside the dataset object directly change the values of these array
        // in the dataset object.
     * </pre>
     * <p>
     * For ScalarDS, the memory data buffer is an one-dimensional array of byte,
     * short, int, float, double or String type based on the datatype of the 
     * dataset. 
     * <p>
     * For CompoundDS, the meory data object is an java.util.List object. Each
     * element of the list is a data array that corresponds to a compound field. 
     * <p>
     * For example, if compound dataset "comp" has the following nested structure,
     * and memeber datatypes
     * <pre>
     * comp --> m01 (int)
     * comp --> m02 (float)
     * comp --> nest1 --> m11 (char)
     * comp --> nest1 --> m12 (String)
     * comp --> nest1 --> nest2 --> m21 (long)
     * comp --> nest1 --> nest2 --> m22 (double)
     * </pre>
     * getData() returns a list of six arrays: {int[], float[], char[], Stirng[], long[] and double[]}.
     *  
     *  @return the data read from file.
     *  
     * @see {@link #getData()}
     */
    public abstract Object read() throws Exception, OutOfMemoryError;

    /** 
     * Reads the raw data of the dataset from file to a byte array.
     * <p>
     * readBytes() reads raw data to an array of bytes instead of array of its datatype.
     * For example, for an one-dimension 32-bit integer dataset of size 5,
     * the readBytes() returns of a byte array of size 20 instead of an int array
     * of 5.
     * <p>
     * readBytes() can be used to copy data from one dataset to another efficiently 
     * because the raw data is not converted to its native type, it saves memory 
     * space and CPU time.
     * 
     * @return the byte array of the raw data.
     */
    public abstract byte[] readBytes() throws Exception;

    /**
     * Writes a memory buffer to the dataset in file. 
     * 
     * @param buf the data to write
     */
    public abstract void write(Object buf) throws Exception;

    /** 
     * Writes the memory buffer of this dataset to file. 
     */
    public final void write() throws Exception
    {
        if (data != null) {
            write(data);
        }
    }

    /**
     * Creates a new dataset and writes the data buffer to the new dataset.
     * <p>
     * This function allows applications to create a new dataset for a given
     * data buffer. For example, users can select a specific interesting part 
     * from a large image and create a new image with the selection.
     * <p>
     * The new dataset retains the datatype and dataset creation properties of
     * this dataset.
     *  
     * @param pgroup the group which the dataset is copied to.
     * @param name the name of the new dataset.
     * @param dims the dimension sizes of the the new dataset.
     * @param data the data values of the subset to be copied.
     * 
     * @return the new dataset.
     */
    public abstract Dataset copy(Group pgroup, String name, long[] dims, Object data) 
    throws Exception;

    /** 
     * Returns the datatype object of the dataset. 
     * 
     * @return the datatype object of the dataset.
     */
    public abstract Datatype getDatatype();

    /** 
     * Returns the data buffer of the dataset in memory.
     * <p>
     * If data is already loaded into memory, returns the data; otherwise, 
     *  calls read() to read data from file into a memory buffer and returns
     *  the memory buffer.
     *  <p>
     * <p>
     * By default, the whole dataset is read into memory. Users can also 
     * select subset to read. Subsetting is done in an implicit way.
     * <p>
     * <b>How to Select a Subset</b>
     * <p>
     * A selection is specified by three arrays: start, stride and count.
     * <ol>
     *   <li>start:  offset of a selection
     *   <li>stride: determining how many elements to move in each dimension
     *   <li>count:  number of elements to select in each dimension
     * </ol>
     * getStartDims(), getStartDims() and getSelectedDims() returns the start, 
     * stride and count arrays respectively. Applications can make a selection 
     * by changing the values of the arrays.
     * <p>
     * The following example shows how to make a subset. In the example, the 
     * dataset is a 4-dimensional array of [200][100][50][10], i.e.
     * dims[0]=200; dims[1]=100; dims[2]=50; dims[3]=10; <br>
     * We want to select every other data point in dims[1] and dims[2]
     * <pre>
         int rank = dataset.getRank();   // number of dimension of the dataset
         long[] dims = dataset.getDims(); // the dimension sizes of the dataset
         long[] selected = dataset.getSelectedDims(); // the selected size of the dataet
         long[] start = dataset.getStartDims(); // the off set of the selection
         long[] stride = dataset.getStride(); // the stride of the dataset
         int[]  selectedIndex = dataset.getSelectedIndex(); // the selected dimensions for display
    
         // select dim1 and dim2 as 2D data for display,and slice through dim0
         selectedIndex[0] = 1;
         selectedIndex[1] = 2;
         selectedIndex[1] = 0;
    
         // reset the selection arrays
         for (int i=0; i&lt;rank; i++) {
             start[i] = 0;
             selected[i] = 1;
             stride[i] = 1;
        }
    
        // set stride to 2 on dim1 and dim2 so that every other data points are selected.
        stride[1] = 2;
        stride[2] = 2;
    
        // set the selection size of dim1 and dim2
        selected[1] = dims[1]/stride[1];
        selected[2] = dims[1]/stride[2];
    
        // when dataset.getData() is called, the slection above will be used since
        // the dimension arrays are passed by reference. Changes of these arrays
        // outside the dataset object directly change the values of these array
        // in the dataset object.
     * </pre>
     * <p>
     * For ScalarDS, the memory data buffer is an one-dimensional array of byte,
     * short, int, float, double or String type based on the datatype of the 
     * dataset. 
     * <p>
     * For CompoundDS, the meory data object is an java.util.List object. Each
     * element of the list is a data array that corresponds to a compound field. 
     * <p>
     * For example, if compound dataset "comp" has the following nested structure,
     * and memeber datatypes
     * <pre>
     * comp --> m01 (int)
     * comp --> m02 (float)
     * comp --> nest1 --> m11 (char)
     * comp --> nest1 --> m12 (String)
     * comp --> nest1 --> nest2 --> m21 (long)
     * comp --> nest1 --> nest2 --> m22 (double)
     * </pre>
     * getData() returns a list of six arrays: {int[], float[], char[], Stirng[], long[] and double[]}.
     *  
     *  @return the memory buffer of the dataset.
     */
    public final Object getData() throws Exception, OutOfMemoryError
    {
        if (!isDataLoaded) {
            data = read(); // load the data;
            originalBuf = data;
            isDataLoaded = true;
            nPoints = 1;
            for (int j=0; j<selectedDims.length; j++) {
                nPoints *= selectedDims[j];
            }
        }

        return data;
    }
    
    /**
     * @deprecated  Not for public use in the future.
     * <p>
     * setData() is not safe to use because it changes memory buffer
     * of the dataset object. Dataset operation such as write/read 
     * will fail if the buffer type or size is changed.
     */
    public final void setData(Object d)  { data =d; }

    /**
     * Clears the data buffer in memory and to force the next read() to load data from file.
     * <p>
     * The function read() loads data from file into memory only if 
     * the data is not read. If data is already in memory, read() just
     * returns the memory buffer. Sometimes we want to force read()
     * to re-read data from file. For example, when the selection is 
     * changed, we need to re-read the data. 
     *  
     * clearData() clears the current memory buffer and force the read()
     * to load the data from file.
     * 
     * @see #getData()
     * @see #read()
     */
    public void clearData()
    {
        isDataLoaded = false;
    }

    /**
     * Returns the dimension size of the vertical axis.
     * 
     * <p> 
     *  This function is used by GUI applications such as HDFView. GUI applications
     *  display a dataset in a 2D table or 2D image. The display order is specified 
     *  by the index array of selectedIndex as follow:
     *  <dl>
     *  <dt> selectedIndex[0] -- height</dt>
     *    <dd> The vertical axis </dd>
     *  <dt> selectedIndex[1] -- width </dt>
     *    <dd> The horizontal axis </dd>
     *  <dt> selectedIndex[2] -- depth </dt>
     *    <dd> The depth axis is used for 3 or more dimensional datasets. </dd>
     *  </dl>
     *  Applications can use getSelectedIndex() to access and change the display
     *  order. For example, in a 2D dataset of 200x50 (dim0=200, dim1=50), the following
     *  code will set the height=200 and width=50.
     *  <pre>
     *      long[] selectedIndex = dataset.getSelectedIndex();
     *      selectedIndex[0] = 0;
     *      selectedIndex[1] = 1;
     *  </pre>
     * 
     * @see #getSelectedIndex()
     * @see #getWidth()
     * 
     * @return the size of dimension of the vertical axis.
     */
    public final int getHeight()
    {
        if ((selectedDims == null) ||
            (selectedIndex == null) ) {
            return 0;
        }

        return (int)selectedDims[selectedIndex[0]];
    }

    /**
     * Returns the size of dimension of the horizontal axis.
     * 
     * <p> 
     *  This function is used by GUI applications such as HDFView. GUI applications
     *  display dataset a 2D Table or 2D Image. The display order is specified 
     *  by the index array of selectedIndex as follow:
     *  <dl>
     *  <dt> selectedIndex[0] -- height</dt>
     *    <dd> The vertical axis </dd>
     *  <dt> selectedIndex[1] -- width </dt>
     *    <dd> The horizontal axis </dd>
     *  <dt> selectedIndex[2] -- depth </dt>
     *    <dd> The depth axis, which is used for 3 or more dimension datasets. </dd>
     *  </dl>
     *  Applications can use getSelectedIndex() to access and change the display
     *  order. For example, in a 2D dataset of 200x50 (dim0=200, dim1=50), the following
     *  code will set the height=200 and width=100.
     *  <pre>
     *      long[] selectedIndex = dataset.getSelectedIndex();
     *      selectedIndex[0] = 0;
     *      selectedIndex[1] = 1;
     *  </pre>
     * 
     * @see #getSelectedIndex()
     * @see #getHeight()
     * 
     * @return the size of dimension of the horizontal axis.
     */
    public final int getWidth()
    {
        if ((selectedDims == null) ||
            (selectedIndex == null) ) {
            return 0;
        }

        if ((selectedDims.length < 2) ||
            (selectedIndex.length < 2)) {
            return 1;
        }

        return (int)selectedDims[selectedIndex[1]];
    }

    /**
     * Returns the indices of display order.
     * <p>
     * 
     * selectedIndex[] is provided for two purpose:
     * <OL>
     * <LI>
     * selectedIndex[] is used to indicate the order of dimensions for display.
     * selectedIndex[0] is for the row, selectedIndex[1] is for the column and 
     * selectedIndex[2] for the depth.
     * <p>
     * For example, for a four dimesion dataset, if selectedIndex[] = {1, 2, 3},
     * then dim[1] is selected as row index, dim[2] is selected as column index
     * and dim[3] is selected as depth index.
     * <LI>
     * selectedIndex[] is also used to select dimensions for display for
     * datasets with three or more dimensions. We assume that applications such
     * as HDFView can only display data values up to three dimension (2D spreadsheet/image
     * with a third dimension which the 2D spreadsheet/image is selected from). For
     * dataset with more than three dimensions, we need selectedIndex[] to tell
     * applications which three dimensions are chosen for display.
     * <br>
     * For example, for a four dimesion dataset, if selectedIndex[] = {1, 2, 3},
     * then dim[1] is selected as row index, dim[2] is selected as column index
     * and dim[3] is selected as depth index. dim[0] is not selected. Its location
     * is fixed at 0 by default.
     * </OL>
     * 
     * @return the array of the indices of display order.
     */
    public final int[] getSelectedIndex()
    {
        return selectedIndex;
    }

    /**
     * Returns the string representation of compression information.
     * <p>
     * For example, "SZIP: Pixels per block = 8: H5Z_FILTER_CONFIG_DECODE_ENABLED".
     * 
     * @return the string representation of compression information.
     */
    public final String getCompression()
    {
        return compression;
    }

    /**
     *  Returns the array that contains the dimension sizes of the chunk of the dataset.
     *  Returns null if the dataset is not chunked.
     *  
     *  @return the array of chunk sizes or returns null if the dataset is not chunked.
     */
    public final long[] getChunkSize()
    {
        return chunkSize;
    }

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #convertFromUnsignedC(Object, Object)}
     */
    public static Object convertFromUnsignedC(Object data_in)
    {
        return Dataset.convertFromUnsignedC(data_in, null);
    }

    /**
     * Converts one-dimension array of unsigned C-type integers to a new array of appropriate Java integer in memory.
     * <p>
     * Since Java does not support unsigned integer, values of unsigned C-type integers must
     * be converted into its appropriate Java integer. Otherwise, the data value
     * will not displayed correctly. For example, if an unsigned C byte, x = 200,
     * is stored into an Java byte y, y will be -56 instead of the correct value of 200.
     * <p>
     * Unsigned C integers are upgrade to Java integers according to the following table:
     * <TABLE CELLSPACING=0 BORDER=1 CELLPADDING=5 WIDTH=400>
     *     <caption><b>Mapping Unsigned C Integers to Java Integers</b></caption>
     *     <TR> <TD><B>Unsigned C Integer</B></TD> <TD><B>JAVA Intege</B>r</TD> </TR>
     *     <TR> <TD>unsigned byte</TD> <TD>signed short</TD> </TR>
     *     <TR> <TD>unsigned short</TD> <TD>signed int</TD> </TR>
     *     <TR> <TD>unsigned int</TD> <TD>signed long</TD> </TR>
     *     <TR> <TD>unsigned long</TD> <TD>signed long</TD> </TR>
     * </TABLE>
     * <strong>NOTE: this conversion cannot deal with unsigned 64-bit integers. Therefore,
     *       the values of unsigned 64-bit dataset may be wrong in Java application</strong>.
     * <p>
     * If memory data of unsigned integers is converted by convertFromUnsignedC(),
     * convertToUnsignedC() must be called to convert the data back to unsigned C before
     * data is written into file.
     *  
         * @see #convertToUnsignedC(Object, Object)
     * 
     * @param data_in the input 1D array of the unsigned C-type integers.
     * @param data_out the output converted (or upgraded) 1D array of Java integers.
     * 
     * @return the upgraded 1D array of Java integers.
     */
    public static Object convertFromUnsignedC(Object data_in, Object data_out)
    {
        if (data_in == null) {
            return null;
        }

        Class data_class = data_in.getClass();
        if (!data_class.isArray()) {
            return null;
        }

        if (data_out != null) {
            Class data_class_out = data_out.getClass();
            if (!data_class_out.isArray() || (Array.getLength(data_in) != Array.getLength(data_out))) {
                data_out = null;
            }
        }

        String cname = data_class.getName();
        char dname = cname.charAt(cname.lastIndexOf("[")+1);
        int size = Array.getLength(data_in);
        
		if (dname == 'B') {
            short[] sdata = null;
            if (data_out == null) {
                sdata = new short[size];
            } else {
                sdata = (short[])data_out;
            }
            
            byte[] bdata = (byte[])data_in;
            for (int i=0; i<size; i++) 
            { 
                sdata[i] = (short) ((bdata[i] + 256) & 0xFF); 
            } 

            data_out = sdata;
        }
        else if (dname == 'S') {
            int[] idata = null;
            if (data_out == null) {
                idata = new int[size];
            } else {
                idata = (int[]) data_out;
            }
            
            short[] sdata = (short[])data_in;
            for (int i=0; i<size; i++) 
            { 
                idata[i] = (sdata[i] + 65536) & 0xFFFF; 
            } 

            data_out = idata;
        }
        else if (dname == 'I') {
            long[] ldata = null;
            if (data_out == null) {
                ldata = new long[size];
            } else {
                ldata = (long[])data_out;
            }

            int[] idata = (int[])data_in;
            for (int i=0; i<size; i++) 
            { 
                ldata[i] = (idata[i] + 4294967296L) & 0xFFFFFFFFL; 
            } 
            
            data_out = ldata;
        } else {
            data_out = data_in;
            // Java does not support unsigned long
        }

        return data_out;
    }

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #convertToUnsignedC(Object, Object)}
     */
    public static Object convertToUnsignedC(Object data_in)
    {
        return Dataset.convertToUnsignedC(data_in, null);
    }

    /**
     * Converts the array of converted unsigned integer back to unsigned C-type integer data in memory.
     * <p>
     * If memory data of unsigned integers is converted by convertFromUnsignedC(),
     * convertToUnsignedC() must be called to convert the data back to unsigned C before
     * data is written into file.
     * 
     * @see #convertFromUnsignedC(Object, Object)
     * 
     * @param data_in the input array of the Java integer.
     * @param data_out the output array of the unsigned C-type integer.
     * 
     * @return the converted data of unsigned C-type integer array.
     */
    public static Object convertToUnsignedC(Object data_in, Object data_out)
    {
        if (data_in == null) {
            return null;
        }

        Class data_class = data_in.getClass();
        if (!data_class.isArray()) {
            return null;
        }

        if (data_out != null) {
            Class data_class_out = data_out.getClass();
            if (!data_class_out.isArray() || (Array.getLength(data_in) != Array.getLength(data_out))) {
                data_out = null;
            }
        }

        String cname = data_class.getName();
        char dname = cname.charAt(cname.lastIndexOf("[")+1);
        int size = Array.getLength(data_in);

        if (dname == 'S') {
            byte[] bdata = null;
            if (data_out == null) {
                bdata = new byte[size];
            } else {
                bdata = (byte[])data_out;
            }
            short[] sdata = (short[])data_in;
            for (int i=0; i<size; i++) {
                bdata[i] = (byte)sdata[i];
            }
            data_out = bdata;
        }
        else if (dname == 'I') {
            short[] sdata = null;
            if (data_out == null) {
                sdata = new short[size];
            } else {
                sdata = (short[])data_out;
            }
            int[] idata = (int[])data_in;
            for (int i=0; i<size; i++) {
                sdata[i] = (short)idata[i];
            }
            data_out = sdata;
        }
        else if (dname == 'J') {
            int[] idata = null;
            if (data_out == null) {
                idata = new int[size];
            } else {
                idata = (int[])data_out;
            }
            long[] ldata = (long[])data_in;
            for (int i=0; i<size; i++) {
                idata[i] = (int)ldata[i];
            }
            data_out = idata;
        } else {
            data_out = data_in;
            // Java does not support unsigned long
        }

        return data_out;
    }

    /**
     * Converts an array of bytes into an array of Strings for a fixed string dataset.
     * <p>
     * A C-string is an array of chars while an Java String is an object.
     * When a string dataset is read into Java application, the data is
     * stored in an array of Java bytes. byteToString() is used to convert
     * the array of bytes into array of Java strings so that applications
     * can display and modify the data content.
     * <p>
     * For example, the content of a two element C string dataset is
     * {"ABC", "abc"}. Java applications will read the data into an
     * byte array of {65, 66, 67, 97, 98, 99). byteToString(bytes, 3)
     * returns an array of Java String of strs[0]="ABC", and strs[1]="abc".
     * <p>
     * If memory data of strings is converted to Java Strings, stringToByte()
     * must be called to convert the memory data back to byte array before data
     * is written to file.
     * 
     * @see #stringToByte(String[], int)
     *  
     * @param bytes the array of bytes to convert.
     * @param length the length of string.
     * 
     * @return the array of Java String.
     */
    public static final String[] byteToString(byte[] bytes, int length)
    {
        if (bytes == null) {
            return null;
        }

        int n = bytes.length/length;
        //String bigstr = new String(bytes);
        String[] strArray = new String[n];
        String str = null;
        int idx = 0;
        for (int i=0; i<n; i++) {
            str = new String(bytes, i*length, length);
            // bigstr.substring uses less memory space
            // NOTE: bigstr does not work on linus if bytes.length is very large
            //       see bug 1091
            //offset = i*length;
            //str = bigstr.substring(offset, offset+length);
            
            idx = str.indexOf('\0');
            if (idx > 0) {
                str = str.substring(0, idx);
            }
            
            // trim only the end 
            int end = str.length();
            while (end > 0 && str.charAt(end-1) <= '\u0020')
                end--;
            
            strArray[i] = (end <= 0) ? "" : str.substring(0, end);
            
            // trim both start and end
            //strArray[i] = str.trim();
        }

        return strArray;
    }

    /**
     * Converts a string array into an array of bytes for a fixed string dataset.
     * <p>
     * If memory data of strings is converted to Java Strings, stringToByte()
     * must be called to convert the memory data back to byte array before data
     * is written to file.
     *
     * @see #byteToString(byte[] bytes, int length)
     * 
     * @param strings the array of string.
     * @param length the length of string.
     * 
     * @return the array of bytes.
     */
    public static final byte[] stringToByte(String[] strings, int length)
    {
        if (strings == null) {
            return null;
        }

        int size = strings.length;
        byte[] bytes = new byte[size*length];

        StringBuffer strBuff = new StringBuffer(length);
        for (int i=0; i<size; i++)
        {            
            // initialize the string with spaces
            strBuff.replace(0, length, " ");

            if (strings[i] != null) {
                if (strings[i].length() > length) {
                    strings[i] = strings[i].substring(0, length);
                }
                strBuff.replace(0, length, strings[i]);                    
            }

            strBuff.setLength(length);
            System.arraycopy(strBuff.toString().getBytes(), 0, bytes, length*i, length);
        }

        return bytes;
    }

    /** 
     * Returns the array of strings that represent the dimension names.
     * Returns null if there is no dimension name.
     * <p>
     * Some datasets have pre-defined names for each dimension such as "Latitude"
     * and "Longitude". getDimNames() returns these pre-defined names.
     * 
     * @return the names of dimensions, or null if there is no dimension name.
     */
    public final String[] getDimNames() { return dimNames; }

    /**
     * Checks if a given datatype is a string. Sub-classes must replace this
     * default implementation.
     *
     * @param tid The data type identifier.
     * 
     * @return true if the datatype is a string; otherwise returns false.
     */
    public boolean isString(int tid) { return false; }

    /**
     * Returns the size in bytes of a given datatype. 
     * Sub-classes must replace this default implementation.
     *
     * @param tid The data type identifier.
     * 
     * @return The size of the datatype
     */
    public int getSize(int tid) { return -1; }
    
    /**
     * Get flag that indicate if enum data is converted to strings.
     * 
     * @return the enumConverted
     */
    public boolean isEnumConverted() {
        return enumConverted;
    }

    /**
     * Set flag that indicate if enum data is converted to strings.
     * 
     * @param b the enumConverted to set
     */
    public void setEnumConverted(boolean b) {
        if (enumConverted != b) {
            originalBuf = convertedBuf = null;
            this.clearData();
        }
        
        enumConverted = b;
    }
}
