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
 * A scalar dataset is a multiple dimension array of scalar points. The
 * Datatype of a scalar dataset must be an atomic datatype. Common datatypes
 * of scalar datasets include char, byte, short, int, long, float, double and string.
 * <p>
 * A ScalarDS can be an image or spreadsheet data. ScalarDS defines few methods
 * to deal with both images and spreadsheets.
 * <p>
 * ScalarDS is an abstract class. Current implementing classes are the H4SDS,
 * H5GRImage and H5ScalarDS.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public abstract class ScalarDS extends Dataset
{
    // The following constant strings are copied from
    // http://hdf.ncsa.uiuc.edu/HDF5/doc/ADGuide/ImageSpec.html
    // to make the defination consistent with the image specs.

    /** 
     * Indicates that the pixel RGB values are contiguous. 
     */
    public final static int INTERLACE_PIXEL = 0;

    /** Indicates that each pixel component of RGB is stored as a scan line. */
    public static final int INTERLACE_LINE = 1;

    /** Indicates that each pixel component of RGB is stored as a plane. */
    public final static int INTERLACE_PLANE = 2;

    /**
     * The interlace mode of the stored raster image data.
     * Valid values are INTERLACE_PIXEL, INTERLACE_LINE and INTERLACE_PLANE.
     */
    protected int interlace;

    /**
     * The min-max range of image data values.
     * For example, [0, 255] indicates the min is 0, and the max is 255.
     */
    protected double[] imageDataRange;
    
    /**
     * The indexed RGB color model with 256 colors.
     * <p>
     * The palette values are stored in a two-dimensional byte array and arrange
     * by color components of red, green and blue. palette[][] = byte[3][256],
     * where, palette[0][], palette[1][] and palette[2][] are the red, green and
     * blue components respectively.
     */
    protected byte[][] palette;

    /**
     * True if this dataset is an image.
     */
    protected boolean isImage;

    /**
     * True if this dataset is a true color image.
     */
    protected boolean isTrueColor;

    /**
     * True if this dataset is ASCII text.
     */
    protected boolean isText;

    /**
     * Flag to indicate if the original C data is unsigned integer.
     */
    protected boolean isUnsigned;

    /**
     * Flag to indicate is the original unsigned C data is converted.
     */
    protected boolean unsignedConverted;

    /** The fill value of the dataset. */
    protected Object fillValue = null;
    
    /** Flag to indicate if the dataset is displayed as an image */
    protected boolean isImageDisplay;
    
    /** Flag to indicate if the dataset is displayed as an image with default order of dimensions */
    protected boolean isDefaultImageOrder;
    
    /**
     * Constructs an instance of a ScalarDS with specific name and path.
     * An HDF data object must have a name. The path is the group path starting
     * from the root.
     * <p>
     * For example, in H5ScalarDS(h5file, "dset", "/arrays/"), "dset" is the
     * name of the dataset, "/arrays" is the group path of the dataset.
     *
     * @param theFile the file that contains the data object.
     * @param theName the name of the data object, e.g. "dset".
     * @param thePath the full path of the data object, e.g. "/arrays/".
     */
    public ScalarDS(FileFormat theFile, String theName, String thePath)
    {
        this(theFile, theName, thePath, null);
    }

    /**
     * @deprecated  Not for public use in the future.<br>
     * Using {@link #ScalarDS(FileFormat, String, String)}
     */
    public ScalarDS (
        FileFormat theFile,
        String theName,
        String thePath,
        long[] oid)
    {
        super (theFile, theName, thePath, oid);

        palette = null;
        isImage = false;
        isTrueColor = false;
        isText = false;
        isUnsigned = false;
        interlace = -1;
        datatype = null;
        imageDataRange = null;
        isImageDisplay = false;
        isDefaultImageOrder = true;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#clearData()
     */
    public void clearData()
    {
        super.clearData();
        unsignedConverted = false;
    }

    /**
     * Converts the data values of this dataset to appropriate Java integer if they are unsigned integers.
     * 
     * @see ncsa.hdf.object.Dataset#convertToUnsignedC(Object)
     * @see ncsa.hdf.object.Dataset#convertFromUnsignedC(Object, Object)
     * 
     * @return the converted data buffer.
      */
    public Object convertFromUnsignedC()
    {
        // keep a copy of original buffer and the converted buffer
        // so that they can be reused later to save memory
        if ((data != null) && isUnsigned && !unsignedConverted)
        {
            originalBuf = data;
            convertedBuf = convertFromUnsignedC(originalBuf, convertedBuf);
            data = convertedBuf;
            unsignedConverted = true;
        }
        
        return data;
    }

    /**
     * Converts Java integer data of this dataset back to unsigned C-type integer data
     * if they are unsigned integers. 
     * 
     * @see ncsa.hdf.object.Dataset#convertToUnsignedC(Object)
     * @see ncsa.hdf.object.Dataset#convertToUnsignedC(Object, Object)
     * @see #convertFromUnsignedC(Object data_in)
     * 
     * @return the converted data buffer.
     */
    public Object convertToUnsignedC()
    {
        // keep a copy of original buffer and the converted buffer
        // so that they can be reused later to save memory
        if ((data != null) && isUnsigned)
        {
            convertedBuf = data;
            originalBuf = convertToUnsignedC(convertedBuf, originalBuf);
            data = originalBuf;
        }
        
        return data;
    }

    /**
     * Returns the palette of this scalar dataset or null if palette does not exist.
     * <p>
     * Scalar dataset can be displayed as spreadsheet data or image. When a scalar
     * dataset is chosen to display as an image, the palette or color table may
     * be needed to translate a pixel value to color components (for example,
     * red, green, and blue). Some scalar datasets have no palette and some datasets
     * have one or more than one palettes. If an associated palette exists but not
     * loaded, this interface retrieves the palette from the file and returns
     * the palette. If the palette is loaded, it returnd the palette. It returns
     * null if there is no palette assciated with the dataset.
     * <p>
     * Current implementation only supports palette model of indexed RGB with
     * 256 colors. Other models such as YUV", "CMY", "CMYK", "YCbCr", "HSV will
     * be supported in the future.
     * <p>
     * The palette values are stored in a two-dimensional byte array and arrange
     * by color components of red, green and blue. palette[][] = byte[3][256],
     * where, palette[0][], palette[1][] and palette[2][] are the red, green and
     * blue components respectively.
     * <p>
     * Sub-classes have to implement this interface. HDF4 and HDF5 images use
     * different libraries to retrieve the associated palette.
     * 
     * @return the 2D palette byte array.
     */
    public abstract byte[][] getPalette();

    /**
     * Sets the palette for this dataset.
     * 
     * @param pal the 2D palette byte array.
     */
    public final void setPalette(byte[][] pal)
    {
        palette = pal;
    }

    /** Reads a specific image palette from file.
     * <p>
     * A scalar dataset may have multiple palettes attached to it.
     * readPalette(int idx) returns a specific palette identified by its index.
     * 
     *  @param idx the index of the palette to read.
     */
    public abstract byte[][] readPalette(int idx);

    /** 
     * Returns the byte array of palette refs.
     * <p>
     * A palette reference is an object reference that points to the palette 
     * dataset. 
     * <p>
     * For example, Dataset "Iceberg" has an attribute of object
     * reference "Palette". The arrtibute "Palette" has value "2538" that is
     * the object reference of the palette data set "Iceberg Palette". 
     * 
     * @return null if there is no palette attribute attached to this dataset.
     */
    public abstract byte[] getPaletteRefs();

    /**
     * Returns true if this dataset is an image.
     * <p>
     * For all Images, they must have an attribute called "CLASS".
     * The value of this attribute is "IMAGE". For more details, read   
     * <a href="http://hdfgroup.org/HDF5/doc/ADGuide/ImageSpec.html">
     * HDF5 Image and Palette Specification </a>
     * 
     * @return true if the dataset is an image; otherwise, returns false.
     */
    public final boolean isImage()
    {
        return isImage;
    }

    /**
     * Returns true if this dataset is displayed as an image.
     * <p>
     * A ScalarDS can be displayed as an image or table. 
     *  
     * @return true if this dataset is displayed as an image; otherwise, returns false.
     */
    public final boolean isImageDisplay()
    {
        return isImageDisplay;
    }

    /**
     * Returns true if this dataset is displayed as an image with default image order.
     * <p>
     * A ScalarDS can be displayed as an image with different orders of dimensions. 
     *  
     * @return true if this dataset is displayed as an image with default image order; otherwise, returns false.
     */
    public final boolean isDefaultImageOrder()
    {
        return isDefaultImageOrder;
    }

    /**
     * Sets the flag to display the dataset as an image.
     * 
     * @param b if b is true, display the dataset as an image
     */
    public final void setIsImageDisplay(boolean b)
    {
        isImageDisplay = b;
        
        if (isImageDisplay) {
            enumConverted = false;
        }
    } 
    
    /**
     * Sets the flag to indicate this dataset is an image.
     * 
     * @param b if b is true, the dataset is an image.
     */
    public final void setIsImage(boolean b)
    {
        isImage = b;
        
        if (isImage) {
            enumConverted = false;
        }
    }    
    
    /**
     * Returns true if this dataset is a true color image.
     */
    
    public final boolean isTrueColor()
    {
        return isTrueColor;
    }

    /**
     * Returns true if this dataset is ASCII text.
     * 
     * @return true if this dataset is ASCII text.
     */
    public final boolean isText()
    {
        return isText;
    }

    /**
     * Returns the interlace mode of a true color image (RGB).
     * 
     * Valid values:
     * <pre>
     *     INTERLACE_PIXEL -- RGB components are contiguous, i.e. rgb, rgb, rgb, ...
     *     INTERLACE_LINE -- each RGB component is stored as a scan line
     *     INTERLACE_PLANE -- each RGB component is stored as a plane
     * </pre>
     * 
     * @return the interlace mode of a true color image (RGB).
     */
    public final int getInterlace()
    {
        return interlace;
    }

    /**
     * Returns true if the original C data are unsigned integers.
     * 
     * @return true if the original C data are unsigned integers.
     */
    public final boolean isUnsigned()
    {
        return isUnsigned;
    }

    /** Returns the (min, max) pair of image data range.
     * 
     * @return the (min, max) pair of image data range.
     */
    public double[] getImageDataRange()
    {
        return imageDataRange;
    }

    /** 
     * Returns the fill values for the dataset.
     * 
     * @return the fill values for the dataset.
     */
    public final Object getFillValue()
    {
        return fillValue;
    }

}
