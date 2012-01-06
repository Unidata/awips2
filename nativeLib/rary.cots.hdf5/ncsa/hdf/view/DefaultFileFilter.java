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

import java.io.File;
import java.io.RandomAccessFile;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Enumeration;
import javax.swing.filechooser.*;

/**
 * A convenience implementation of FileFilter that filters out
 * all files except for those type extensions that it knows about.
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class DefaultFileFilter extends FileFilter
{
    private static FileFilter FILE_FILTER_HDF = null;
    private static FileFilter FILE_FILTER_HDF4 = null;
    private static FileFilter FILE_FILTER_HDF5 = null;
    private static FileFilter FILE_FILTER_JPEG = null;
    private static FileFilter FILE_FILTER_TIFF = null;
    private static FileFilter FILE_FILTER_PNG = null;
    private static FileFilter FILE_FILTER_GIF = null;
    private static FileFilter FILE_FILTER_BMP = null;
    private static FileFilter FILE_FILTER_IMG = null;
    private static FileFilter FILE_FILTER_TEXT = null;

    private static String fileExtension = ViewProperties.getFileExtension();

    private Hashtable filters = null;
    private String description = null;
    private String fullDescription = null;
    private boolean useExtensionsInDescription = true;

    /**
     * Creates a file filter. If no filters are added, then all
     * files are accepted.
     *
     * @see #addExtension
     */
    public DefaultFileFilter() {
        this.filters = new Hashtable();
    }

    /**
     * Creates a file filter that accepts files with the given extension.
     * Example: new DefaultFileFilter("jpg");
     *
     * @see #addExtension
     */
    public DefaultFileFilter(String extension) {
        this(extension,null);
    }

    /**
     * Creates a file filter that accepts the given file type.
     * Example: new DefaultFileFilter("jpg", "JPEG Image Images");
     *
     * Note that the "." before the extension is not needed. If
     * provided, it will be ignored.
     *
     * @see #addExtension
     */
    public DefaultFileFilter(String extension, String description) {
        this();
        if(extension!=null) {
            addExtension(extension);
        }
        if(description!=null) {
            setDescription(description);
        }
    }

    /**
     * Creates a file filter from the given string array.
     * Example: new DefaultFileFilter(String {"gif", "jpg"});
     *
     * Note that the "." before the extension is not needed adn
     * will be ignored.
     *
     * @see #addExtension
     */
    public DefaultFileFilter(String[] filters) {
        this(filters, null);
    }

    /**
     * Creates a file filter from the given string array and description.
     * Example: new DefaultFileFilter(String {"gif", "jpg"}, "Gif and JPG Images");
     *
     * Note that the "." before the extension is not needed and will be ignored.
     *
     * @see #addExtension
     */
    public DefaultFileFilter(String[] filters, String description) {
        this();
        for (int i = 0; i < filters.length; i++) {
            // add filters one by one
            addExtension(filters[i]);
        }
        if(description!=null) {
            setDescription(description);
        }
    }

    /**
     * Return true if this file should be shown in the directory pane,
     * false if it shouldn't.
     *
     * Files that begin with "." are ignored.
     *
     * @see #getExtension
     */
    public boolean accept(File f) {
        if(f != null) {
            if(f.isDirectory()) {
                return true;
            }
            String extension = getExtension(f);
            if((extension != null) && (filters.get(getExtension(f)) != null)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Return the extension portion of the file's name .
     *
     * @see #getExtension
     * @see FileFilter#accept
     */
     public String getExtension(File f) {
    if(f != null) {
        String filename = f.getName();
        int i = filename.lastIndexOf('.');
        if((i>0) && (i<filename.length()-1)) {
        return filename.substring(i+1).toLowerCase();
        };
    }
    return null;
    }

    /**
     * Adds a filetype "dot" extension to filter against.
     *
     * For example: the following code will create a filter that filters
     * out all files except those that end in ".jpg" and ".tif":
     *
     *   DefaultFileFilter filter = new DefaultFileFilter();
     *   filter.addExtension("jpg");
     *   filter.addExtension("tif");
     *   or
     *   filter.addExtension("jpg, tif");
     *
     * Note that the "." before the extension is not needed and will be ignored.
     */
    public void addExtension(String extension)
    {
        if(filters == null) {
            filters = new Hashtable(5);
        }

        String ext = null;
        StringTokenizer st = new StringTokenizer(extension, ",");
        while (st.hasMoreElements())
        {
            ext = st.nextToken().trim();
            filters.put(ext.toLowerCase(), this);
        }
        fullDescription = null;
    }

    /**
     * Returns the human readable description of this filter. For
     * example: "JPEG and GIF Image Files (*.jpg, *.gif)"
     */
    public String getDescription() {
        if(fullDescription == null) {
            if((description == null) || isExtensionListInDescription()) {
                fullDescription = description==null ? "(" : description + " (";
                // build the description from the extension list
                Enumeration extensions = filters.keys();
                if(extensions != null) {

                    if (!extensions.hasMoreElements()) {
                        fullDescription = null;
                        return null;
                    }

                    fullDescription += "." + (String) extensions.nextElement();
                    while (extensions.hasMoreElements()) {
                        fullDescription += ", " + (String) extensions.nextElement();
                    }
                }
                fullDescription += ")";
            } else {
                fullDescription = description;
            }
        }
        return fullDescription;
    }

    /**
     * Sets the human readable description of this filter. For
     * example: filter.setDescription("Gif and JPG Images");
     */
    public void setDescription(String description) {
        this.description = description;
        fullDescription = null;
    }

    /**
     * Determines whether the extension list (.jpg, .gif, etc) should
     * show up in the human readable description.
     *
     * Only relevent if a description was provided in the constructor
     * or using setDescription();
     *
     */
    public void setExtensionListInDescription(boolean b) {
        useExtensionsInDescription = b;
        fullDescription = null;
    }

    /**
     * Returns whether the extension list (.jpg, .gif, etc) should
     * show up in the human readable description.
     *
     * Only relevent if a description was provided in the constructor
     * or using setDescription();
     */
    public boolean isExtensionListInDescription() {
        return useExtensionsInDescription;
    }

    /** Return a file filter for HDF4/5 file. */
    public static FileFilter getFileFilter() {
        boolean extensionNotChanged = ( fileExtension.equalsIgnoreCase( ViewProperties.getFileExtension()));

        if ((FILE_FILTER_HDF != null) && extensionNotChanged) {
            return FILE_FILTER_HDF;
        }

        // update extensions
        fileExtension = ViewProperties.getFileExtension();

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.setDescription("HDF & more");

        filter.addExtension(fileExtension);

        return (FILE_FILTER_HDF = filter);
    }

    /** Return a file filter for HDF4 file. */
    public static FileFilter getFileFilterHDF4()
    {
        if (FILE_FILTER_HDF4 != null) {
            return FILE_FILTER_HDF4;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("hdf");
        filter.addExtension("h4");
        filter.addExtension("hdf4");
        filter.setDescription("HDF4 files");
        FILE_FILTER_HDF4 = filter;

        return FILE_FILTER_HDF4;
    }

    /** Return a file filter for HDF5 file. */
    public static FileFilter getFileFilterHDF5()
    {
        if (FILE_FILTER_HDF5 != null) {
            return FILE_FILTER_HDF5;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("h5");
        filter.addExtension("hdf5");
        filter.setDescription("HDF5 files");
        FILE_FILTER_HDF5 = filter;

        return FILE_FILTER_HDF5;
    }

    /** Return a file filter for JPEG image files. */
    public static FileFilter getFileFilterJPEG()
    {
        if (FILE_FILTER_JPEG != null) {
            return FILE_FILTER_JPEG;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("jpg");
        filter.addExtension("jpeg");
        filter.addExtension("jpe");
        filter.addExtension("jif");
        filter.addExtension("jfif");
        filter.addExtension("jfi");
        filter.setDescription("JPEG images");
        FILE_FILTER_JPEG = filter;

        return FILE_FILTER_JPEG;
    }
    
    /** Return a file filter for TIFF image files. */
    public static FileFilter getFileFilterTIFF()
    {
        if (FILE_FILTER_TIFF != null) {
            return FILE_FILTER_TIFF;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("tif");
        filter.addExtension("tiff");
        filter.setDescription("TIFF images");
        FILE_FILTER_TIFF = filter;

        return FILE_FILTER_TIFF;
    }

    /** Return a file filter for PNG image files. */
    public static FileFilter getFileFilterPNG()
    {
        if (FILE_FILTER_PNG != null) {
            return FILE_FILTER_PNG;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("png");
        filter.setDescription("PNG images");
        FILE_FILTER_PNG = filter;

        return FILE_FILTER_PNG;
    }
    
    /** Return a file filter for BMP image files. */
    public static FileFilter getFileFilterBMP()
    {
        if (FILE_FILTER_BMP != null) {
            return FILE_FILTER_BMP;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("bmp");
        filter.addExtension("dib");
        filter.setDescription("BMP images");
        FILE_FILTER_BMP = filter;

        return FILE_FILTER_BMP;
    }

    /** Return a file filter for GIF image files. */
    public static FileFilter getFileFilterGIF()
    {
        if (FILE_FILTER_GIF != null) {
            return FILE_FILTER_GIF;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("gif");
        filter.setDescription("GIF images");
        FILE_FILTER_GIF = filter;

        return FILE_FILTER_GIF;
    }
    
    /** Return a file filter for GIF, JPEG, BMP, or PNG image files. */
    public static FileFilter getImageFileFilter()
    {
        if (FILE_FILTER_IMG != null) {
            return FILE_FILTER_IMG;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("jpg");
        filter.addExtension("jpeg");
        filter.addExtension("jpe");
        filter.addExtension("jif");
        filter.addExtension("jfif");
        filter.addExtension("jfi");
        filter.addExtension("png");
        filter.addExtension("gif");
        filter.addExtension("bmp");
        filter.addExtension("dib");
        filter.setDescription("GIF, JPEG, BMP, or PNG images");
        FILE_FILTER_IMG = filter;

        return FILE_FILTER_IMG;
    }

    /** Return a file filter for text file. */
    public static FileFilter getFileFilterText()
    {
        if (FILE_FILTER_TEXT != null) {
            return FILE_FILTER_TEXT;
        }

        DefaultFileFilter filter = new DefaultFileFilter();
        filter.addExtension("txt");
        filter.addExtension("text");
        filter.setDescription("Text");
        FILE_FILTER_TEXT = filter;

        return FILE_FILTER_TEXT;
    }

    /** look at the first 4 bytes of the file to see if it is an HDF4 file.
     *  byte[0]=14, byte[1]=3, byte[2]=19, byte[3]=1 or
     *  if it is a netCDF file
     *  byte[0]=67, byte[1]=68, byte[2]=70, byte[3]=1 or
     */
    public static boolean isHDF4(String filename)
    {
        boolean ish4 = false;
        RandomAccessFile raf = null;

        try { raf = new RandomAccessFile(filename, "r"); }
        catch (Exception ex) { raf = null; }

        if (raf == null) {
            return false;
        }

        byte[] header = new byte[4];
        try { raf.read(header); }
        catch (Exception ex) { header = null; }

        if (header != null)
        {
            if (
               // HDF4
               ((header[0]==14) &&
                (header[1]==3) &&
                (header[2]==19) &&
                (header[3]==1))
/*
                // netCDF
                ||
               (header[0]==67 &&
                header[1]==68 &&
                header[2]==70 &&
                header[3]==1)
*/
                ) {
                ish4 = true;
            } else {
                ish4 = false;
            }
        }

        try { raf.close();} catch (Exception ex) {}

        return ish4;
    }

    /** look at the first 8 bytes of the file to see if it is an HDF5 file.
     *  byte[0]=-199 which is 137 in unsigned byte, byte[1]=72, byte[2]=68,
     *   byte[3]=70, byte[4]=13, byte[5]=10, byte[6]=26, byte[7]=10
     */
    public static boolean isHDF5(String filename)
    {
        boolean ish5 = false;
        RandomAccessFile raf = null;

        try { raf = new RandomAccessFile(filename, "r"); }
        catch (Exception ex) { raf = null; }

        if (raf == null) {
            return false;
        }

        byte[] header = new byte[8];
        long fileSize = 0;
        try { fileSize = raf.length(); } catch (Exception ex) {}

        // The super block is located by searching for the HDF5 file signature
        // at byte offset 0, byte offset 512 and at successive locations in the
        // file, each a multiple of two of the previous location, i.e. 0, 512,
        // 1024, 2048, etc
        long offset = 0;
        while (!ish5 && (offset<fileSize))
        {
            try {
                raf.seek(offset);
                raf.read(header);
            } catch (Exception ex) { header = null; }

            if ( (header[0]==-119) &&
                (header[1]==72) &&
                (header[2]==68) &&
                (header[3]==70) &&
                (header[4]==13) &&
                (header[5]==10) &&
                (header[6]==26) &&
                (header[7]==10)) {
                ish5 = true;
            } else
            {
                ish5 = false;
                if (offset == 0) {
                    offset = 512;
                } else {
                    offset *= 2;
                }
            }
        }

        try { raf.close();} catch (Exception ex) {}

        return ish5;
    }

    /** look at the first 4 bytes of the file to see  if it is a netCDF file
     *  byte[0]=67, byte[1]=68, byte[2]=70, byte[3]=1 or
     */
    public static boolean isNetcdf(String filename)
    {
        boolean isnc = false;
        RandomAccessFile raf = null;

        try { raf = new RandomAccessFile(filename, "r"); }
        catch (Exception ex) { raf = null; }

        if (raf == null) {
            return false;
        }

        byte[] header = new byte[4];
        try { raf.read(header); }
        catch (Exception ex) { header = null; }

        if (header != null)
        {
            if (
                // netCDF
                (header[0]==67) &&
                (header[1]==68) &&
                (header[2]==70) &&
                (header[3]==1)) {
                isnc = true;
            } else {
                isnc = false;
            }
        }

        try { raf.close();} catch (Exception ex) {}

        return isnc;
    }

    /** Read HDF5 user block data into byte array.
     *  @return a byte array of user block, or null if there is user data.
     */
    public static byte[] getHDF5UserBlock(String filename)
    {
        byte[] userBlock = null;
        RandomAccessFile raf = null;

        try { raf = new RandomAccessFile(filename, "r"); }
        catch (Exception ex)
        {
            try { raf.close();} catch (Throwable err) {;}
            raf = null;
        }

        if (raf == null) {
            return null;
        }

        byte[] header = new byte[8];
        long fileSize = 0;
        try { fileSize = raf.length(); }
        catch (Exception ex) {fileSize = 0;}
        if (fileSize<=0)
        {
            try { raf.close();} catch (Throwable err) {;}
            return null;
        }

        // The super block is located by searching for the HDF5 file signature
        // at byte offset 0, byte offset 512 and at successive locations in the
        // file, each a multiple of two of the previous location, i.e. 0, 512,
        // 1024, 2048, etc
        long offset = 0;
        boolean ish5 = false;
        while (offset<fileSize)
        {
            try {
                raf.seek(offset);
                raf.read(header);
            } catch (Exception ex) { header = null; }

            if ( (header[0]==-119) &&
                (header[1]==72) &&
                (header[2]==68) &&
                (header[3]==70) &&
                (header[4]==13) &&
                (header[5]==10) &&
                (header[6]==26) &&
                (header[7]==10))
            {
                ish5 = true;
                break; // find the end of user block
            }
            else
            {
                ish5 = false;
                if (offset == 0) {
                    offset = 512;
                } else {
                    offset *= 2;
                }
            }
        }

        if (!ish5 || (offset==0))
        {
            try { raf.close();} catch (Throwable err) {;}
            return null;
        }

        int blockSize = (int)offset;
        userBlock = new byte[blockSize];
        try {
            raf.seek(0);
            raf.read(userBlock, 0, blockSize);
        } catch (Exception ex) { userBlock = null; }

        try { raf.close();} catch (Exception ex) {}

        return userBlock;
    }

    /** Write HDF5 user block data into byte array.
     *  @return a byte array of user block, or null if there is user data.
     */
    public static boolean setHDF5UserBlock(String fin, String fout, byte[] buf)
    {
        boolean ish5 = false;

        if ((buf == null) || (buf.length<=0)) {
            return false;
        }

        File tmpFile = new File(fin);
        if (!tmpFile.exists()) {
            return false;
        }

        // find the end of uerser block for the input file;
        RandomAccessFile raf = null;
        try { raf = new RandomAccessFile(fin, "r"); }
        catch (Exception ex) { raf = null; }

        if (raf == null)
        {
            try { raf.close();} catch (Throwable err) {;}
            raf = null;
            return false;
        }

        byte[] header = new byte[8];
        long fileSize = 0;
        try { fileSize = raf.length(); }
        catch (Exception ex) {fileSize = 0;}
        try { fileSize = raf.length(); }
        catch (Exception ex) {fileSize = 0;}
        if (fileSize<=0)
        {
            try { raf.close();} catch (Throwable err) {;}
            return false;
        }

        // The super block is located by searching for the HDF5 file signature
        // at byte offset 0, byte offset 512 and at successive locations in the
        // file, each a multiple of two of the previous location, i.e. 0, 512,
        // 1024, 2048, etc
        long offset = 0;
        while (offset<fileSize)
        {
            try {
                raf.seek(offset);
                raf.read(header);
            } catch (Exception ex) { header = null; }

            if ( (header[0]==-119) &&
                (header[1]==72) &&
                (header[2]==68) &&
                (header[3]==70) &&
                (header[4]==13) &&
                (header[5]==10) &&
                (header[6]==26) &&
                (header[7]==10))
            {
                ish5 = true;
                break;
            }
            else
            {
                ish5 = false;
                if (offset == 0) {
                    offset = 512;
                } else {
                    offset *= 2;
                }
            }
        }
        try { raf.close();} catch (Throwable err) {;}

        if (!ish5) {
            return false;
        }

        int length = 0;
        int bsize = 1024;
        byte[] buffer;
        BufferedInputStream bi = null;
        BufferedOutputStream bo = null;

        try {
            bi = new BufferedInputStream(new FileInputStream(fin));
        }
        catch (Exception ex )
        {
            try { bi.close(); } catch (Exception ex2 ) {}
            return false;
        }

        try {
            bo = new BufferedOutputStream( new FileOutputStream (fout));
        }
        catch (Exception ex )
        {
            try { bo.close(); } catch (Exception ex2 ) {}
            try { bi.close(); } catch (Exception ex2 ) {}
            return false;
        }

        // skip the header of original file
        try { bi.skip(offset); } catch (Exception ex) {}

        // write the header into the new file
        try {bo.write(buf, 0, buf.length); } catch (Exception ex) {}

        // The super block space is allocated by offset 0, 512, 1024, 2048, etc
        offset = 512;
        while (offset < buf.length) {
            offset *= 2;
        }
        int padSize = (int)(offset-buf.length);
        if (padSize>0)
        {
            byte[] padBuf = new byte[padSize];
            try {bo.write(padBuf, 0, padSize); } catch (Exception ex) {}
        }

        // copy the hdf5 file content from input file to the output file
        buffer = new byte[bsize];
        try { length = bi.read(buffer,0,bsize); }
        catch (Exception ex ) { length = 0; }
        while ( length > 0 )
        {
            try {
                bo.write(buffer, 0, length);
                length = bi.read(buffer,0,bsize);
            }
            catch (Exception ex ) { length = 0; }
        }

        try { bo.flush(); } catch (Exception ex ) {}
        try { bi.close(); } catch (Exception ex ) {}
        try { bo.close(); } catch (Exception ex ) {}
        return true;
    }
}
