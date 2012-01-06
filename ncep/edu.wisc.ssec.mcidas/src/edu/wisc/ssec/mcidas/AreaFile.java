//
// AreaFile.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas;


import java.applet.Applet;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.Raster;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import edu.wisc.ssec.mcidas.adde.GetAreaGUI;


/**
 * AreaFile interface with McIDAS 'area' file format image data.
 *
 * <p>This will allow 'area' format data to be read from disk; the
 * navigation block is made available (see GVARnav for example).</p>
 *
 * <p>Calibration is handled via classes that implement the {@link Calibrator}
 * interface.</p>
 *
 * <p>This implementation does not check the 'valcode' on each line.</p>
 *
 * @author Tom Whittaker, SSEC
 * @author Tommy Jasmin, SSEC
 *
 */

public class AreaFile implements java.io.Serializable {

  /**  */
  static final long serialVersionUID = 3145724093430859967L;

  // indeces used by this and client classes

  /** AD_STATUS - old status field, now used as position num in ADDE */
  public static final int AD_STATUS = 0;

  /** AD_VERSION - McIDAS area file format version number */
  public static final int AD_VERSION = 1;

  /** AD_SENSORID - McIDAS sensor identifier */
  public static final int AD_SENSORID = 2;

  /** AD_IMGDATE - nominal year and day of the image, YYYDDD format */
  public static final int AD_IMGDATE = 3;

  /** AD_IMGTIME - nominal time of the image, HHMMSS format */
  public static final int AD_IMGTIME = 4;

  /** AD_STLINE - upper left image line coordinate */
  public static final int AD_STLINE = 5;

  /** AD_STELEM - upper left image element coordinate */
  public static final int AD_STELEM = 6;

  /** AD_NUMLINES - number of lines in the image */
  public static final int AD_NUMLINES = 8;

  /** AD_NUMELEMS - number of data points per line */
  public static final int AD_NUMELEMS = 9;

  /** AD_DATAWIDTH - number of bytes per data point */
  public static final int AD_DATAWIDTH = 10;

  /** AD_LINERES - data resolution in line direction */
  public static final int AD_LINERES = 11;

  /** AD_ELEMRES - data resolution in element direction */
  public static final int AD_ELEMRES = 12;

  /** AD_NUMBANDS - number of spectral bands, or channels, in image */
  public static final int AD_NUMBANDS = 13;

  /** AD_PFXSIZE - length in bytes of line prefix section */
  public static final int AD_PFXSIZE = 14;

  /** AD_PROJNUM - SSEC project number used in creating image */
  public static final int AD_PROJNUM = 15;

  /** AD_CRDATE - year and day image was created, CCYYDDD format */
  public static final int AD_CRDATE = 16;

  /** AD_CRTIME - time image was created, HHMMSS format */
  public static final int AD_CRTIME = 17;

  /** AD_BANDMAP - spectral band map, bit set for each of 32 bands present */
  public static final int AD_BANDMAP = 18;

  /** AD_DATAOFFSET - byte offset to start of data block */
  public static final int AD_DATAOFFSET = 33;

  /** AD_NAVOFFSET - byte offset to start of navigation block */
  public static final int AD_NAVOFFSET = 34;

  /** AD_VALCODE - validity code */
  public static final int AD_VALCODE = 35;

  /** AD_STARTDATE - actual image start year and Julian day, yyyddd format */
  public static final int AD_STARTDATE = 45;

  /**
   * AD_STARTTIME - actual image start time, hhmmss;
   *  in milliseconds for POES data
   */
  public static final int AD_STARTTIME = 46;

  /** AD_STARTSCAN - starting scan number (sensor based) of image */
  public static final int AD_STARTSCAN = 47;

  /** AD_DOCLENGTH - length in bytes of line prefix documentation */
  public static final int AD_DOCLENGTH = 48;

  /** AD_CALLENGTH - length in bytes of line prefix calibration information */
  public static final int AD_CALLENGTH = 49;

  /** AD_LEVLENGTH - length in bytes of line prefix level section */
  public static final int AD_LEVLENGTH = 50;

  /** AD_SRCTYPE - McIDAS source type (ascii, satellite specific) */
  public static final int AD_SRCTYPE = 51;

  /** AD_CALTYPE - McIDAS calibration type (ascii, satellite specific) */
  public static final int AD_CALTYPE = 52;

  /** AD_AVGSMPFLAG - data is averaged (1), or sampled (0) */
  public static final int AD_AVGSMPFLAG = 53;

  /** AD_SRCTYPEORIG - original source type (ascii, satellite specific) */
  public static final int AD_SRCTYPEORIG = 56;

  /** AD_CALTYPEUNIT - calibration unit */
  public static final int AD_CALTYPEUNIT = 57;

  /** AD_CALTYPESCALE - calibration scaling factor */
  public static final int AD_CALTYPESCALE = 58;

  /** AD_AUXOFFSET - byte offset to start of auxilliary data section */
  public static final int AD_AUXOFFSET = 59;

  /** AD_CALOFFSET - byte offset to start of calibration section */
  public static final int AD_CALOFFSET = 62;

  /** AD_NUMCOMMENTS - number of 80 character comment cards */
  public static final int AD_NUMCOMMENTS = 63;

  /** AD_DIRSIZE - size in 4 byte words of an image directory block */
  public static final int AD_DIRSIZE = 64;

  /** VERSION_NUMBER - version number for a valid AREA file (since 1985) */
  public static final int VERSION_NUMBER = 4;

  /** flag for whether a handler was loaded */
  private static boolean handlerLoaded = false;

  // load protocol handler for ADDE URLs
  // See java.net.URL for explanation of URL handling
  static {
    try {
      String handlers = System.getProperty("java.protocol.handler.pkgs");
      String newProperty = null;
      if (handlers == null) newProperty = "edu.wisc.ssec.mcidas";
      else if (handlers.indexOf("edu.wisc.ssec.mcidas") < 0)
             newProperty = "edu.wisc.ssec.mcidas | " + handlers;
      if (newProperty != null) { // was set above
        //Properties sysP = System.getProperties();
        //sysP.put("java.protocol.handler.pkgs", newProperty);
        //System.setProperties(sysP);
        System.setProperty("java.protocol.handler.pkgs", newProperty);
      }
      handlerLoaded = true;
    }
    catch (Exception e) {
      System.out.println(
        "Unable to set System Property: java.protocol.handler.pkgs");
    }

    handlerLoaded = true;
  }

  /**
   *
   *
   * @return
   */
  public static boolean isURLHandlerLoaded() {
    return handlerLoaded;
  }

  /** flag for whether words should be flipped */
  private boolean flipwords;

  /** file ok flag */
  private boolean fileok;

  /** flag for whether or not the readData() has been called */
  private boolean hasReadData;

  /** the DataInputStream */
  transient private DataInputStream af;

  /** status flag */
//  private int status = 0;

  /** location of nav, cal, aux and data blocks */
  private int navLoc, calLoc, auxLoc, datLoc;

  /** the bytes for navigation, calibration and aux data */
  private int navbytes, calbytes, auxbytes;

  /** original line data length, line length, num lines/eles bands */
  private int lineDataLen, lineLength, origNumLines, origNumElements,
              origNumBands;

  /** line prefix length */
  private int linePrefixLength;

  /** position indicator */
  private long position;

  /** bytes to skip */
  private int skipByteCount;

  /** new position */
  private long newPosition;

  /** the directory block */
  int[] dir;

  /** the nav block */
  int[] nav = null;

  /** the calibration block */
  int[] cal = null;

  /** the aux block */
  int[] aux = null;

  /** the data */
  int[][][] data;

  /** the AreaDirectory representing this image */
  private AreaDirectory areaDirectory;

  /** the image source */
  private String imageSource = null;

  /** the AREAnav for this image */
  private AREAnav areaNav;

  /** the calibration type */
  private int calType = Calibrator.CAL_NONE;

  /** flag for remote data */
  private boolean isRemote = false;

  // master of all subsetting paramters
  private class Subset {

    /**  */
    int lineNumber, numLines, lineMag, eleNumber, numEles, eleMag, bandNumber;

    /**
     *
     *
     * @return
     */
    public String toString() {
      return "Start_line:" + lineNumber + " Num_lines:" + numLines +
             " Line_mag:" + lineMag + " Start_ele:" + eleNumber +
             " Num_ele:" + numEles + " Ele_mag:" + eleMag + " Band:" +
             bandNumber;
    }
  }

  /** subset info holder - if subset is not null, we have been subsetted */
  private Subset subset = null;

  /**
   * Creates an AreaFile object that allows reading
   * of McIDAS 'area' file format image data.  allows reading
   * either from a disk file, or a server via ADDE.
   *
   * @param source the file name, ADDE URL, or local file URL to read from
   *
   * @exception AreaFileException if file cannot be opened
   */

  public AreaFile(String source) throws AreaFileException {

    imageSource = source;
    if (imageSource.startsWith("adde://") &&
        (imageSource.endsWith("image?") ||
         imageSource.endsWith("imagedata?"))) {

      GetAreaGUI gag = new GetAreaGUI((Frame)null, true, "Get data", false,
                                      true);
      gag.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          imageSource = e.getActionCommand();
        }
      });
      gag.show();
    }

    // try as a disk file first
    try {
      af = new DataInputStream(new BufferedInputStream(new FileInputStream(imageSource),
              2048));
    }
    catch (IOException eIO) {
      // if opening as a file failed, try as a URL
      URL url;
      try {
        url = new URL(imageSource);
        // System.out.println(url);
        URLConnection urlc = url.openConnection();
        InputStream is = urlc.getInputStream();
        af = new DataInputStream(new BufferedInputStream(is));
      }
      catch (IOException e) {
        fileok = false;
        throw new AreaFileException("Error opening AreaFile: " + e);
      }
      isRemote = url.getProtocol().equalsIgnoreCase("adde");
    }
    fileok = true;
    position = 0;
    readMetaData();
  }

  /**
   * creates an AreaFile object that allows reading
   * of McIDAS 'area' file format image data from an applet
   *
   * @param filename the disk filename (incl path) to read from
   * @param parent the parent applet
   *
   * @exception AreaFileException if file cannot be opened
   *
   */

  public AreaFile(String filename, Applet parent) throws AreaFileException {

    URL url;
    imageSource = filename;
    try {
      url = new URL(parent.getDocumentBase(), filename);
    }
    catch (MalformedURLException e) {
      fileok = false;
      throw new AreaFileException("Error opening URL for AreaFile:" + e);
    }

    try {
      af = new DataInputStream(new BufferedInputStream(url.openStream()));
    }
    catch (IOException e) {
      fileok = false;
      throw new AreaFileException("Error opening AreaFile:" + e);
    }

    isRemote = url.getProtocol().equalsIgnoreCase("adde");

    fileok = true;
    position = 0;
    readMetaData();

  }

  /**
   * create an <code>AreaFile</code> that allows reading of McIDAS 'area'
   * file format image data from a <code>URL</code> with a protocol of either
   * <code>file</code> or <code>ADDE</code>.  See
   * {@link edu.wisc.ssec.mcidas.adde.AddeURLConnection} for more information on
   * constructing ADDE urls.
   *
   *
   * @param url
   * @exception AreaFileException if file cannot be opened
   */
  public AreaFile(URL url) throws AreaFileException {

    imageSource = url.toString();
    try {
      af = new DataInputStream(new BufferedInputStream(url.openStream()));
    }
    catch (IOException e) {
      fileok = false;
      throw new AreaFileException("Error opening URL for AreaFile:" + e);
    }

    isRemote = url.getProtocol().equalsIgnoreCase("adde");

    fileok = true;
    position = 0;
    readMetaData();
  }

  /**
   * Create a subsetted instance. When subsetted, the data not included in the
   * subset is no longer available to this instance due to the directory block
   * being updated with the subsetting parameters.
   *
   *
   * @param source  the path to the file
   * @param startLine the starting image line
   * @param numLines the total number of lines to return
   * @param lineMag the line magnification. Valid values are &lt;= -1.
   *                -1, 0, and 1 are all taken to be full line resolution,
   *                -2 is every other line, -3 every third, etc...
   * @param startElem the starting image element
   * @param numEles the total number of elements to return
   * @param eleMag the element magnification. Valid values are &lt;= -1.
   *               -1, 0, and 1 are all taken to be full element resolution,
   *               -2 is every other element, -3 every third, etc...
   * @param band the 1-based band number for the subset, which must be present
   *             in the directory blocks band map or -1 for the first band
   *
   * @throws AreaFileException if file cannot be opened
   * @throws IllegalArgumentException If the magnification is greater than 1,
   * the band number is not in the band map, or either of the following are
   * true:
   * <pre>
   *  startLine + (numLines * abs(lineMag)) &gt; total number of lines
   *  startElem + (numEles * abs(eleMag)) &gt; total number of elements
   * </pre>
   */
  public AreaFile(String source, int startLine, int numLines, int lineMag,
                  int startElem, int numEles, int eleMag, int band)
          throws AreaFileException {

    this(source);

    // must have subsetted in the url
    if (isSubsetted()) return;

    // 0, 1, -1 are all full res
    if (eleMag == 0) eleMag = 1;
    if (lineMag == 0) lineMag = 1;

    if (lineMag > 1 || eleMag > 1) {
      throw new IllegalArgumentException("Magnifications greater that 1 are not currently supported");

    }
    else if (startLine + numLines * Math.abs(lineMag) > origNumLines ||
             startElem + numEles * Math.abs(eleMag) > origNumElements) {
      throw new IllegalArgumentException("Arguments outside of file line/element counts");
    }

    int bandIdx = -1;
    int[] bands = getAreaDirectory().getBands();
    if (band == -1) {
      bandIdx = 0;
    }
    else {
      for (int i = 0; i < bands.length; i++) {
        if (bands[i] == band) bandIdx = i;
      }
    }

    if (bandIdx == -1) {
      throw new IllegalArgumentException("Band not found in band map");
    }

    // save subset data
    subset = new Subset();
    subset.lineNumber = startLine;
    subset.numLines = numLines;
    subset.lineMag = lineMag;
    subset.eleNumber = startElem;
    subset.numEles = numEles;
    subset.eleMag = eleMag;
    subset.bandNumber = band;

    int newDatOffset = startLine * lineLength;
    newDatOffset += linePrefixLength;
    newDatOffset += startElem * (origNumBands * dir[AD_DATAWIDTH]);
    newDatOffset += (band - 1) * dir[AD_DATAWIDTH];

    // reflect subset in directory
    dir[AD_DATAOFFSET] = newDatOffset;
    dir[AD_NUMLINES] = numLines;
    dir[AD_NUMELEMS] = numEles;
    dir[AD_NUMBANDS] = 1;

    dir[AD_STLINE] = dir[AD_STLINE] + (startLine * dir[AD_LINERES]);
    dir[AD_STELEM] = dir[AD_STELEM] + (startElem * dir[AD_ELEMRES]);

    // if mag is positive divide to get resolution, otherwise mult.
    // NOTE: resolution in area file is not necessarily full resolution, full
    // resolution is based on the instrument, not the file.
    if (lineMag < 0) {
      dir[AD_LINERES] = dir[AD_LINERES] * Math.abs(lineMag);
    }
    else {
      dir[AD_LINERES] = dir[AD_LINERES] / lineMag;;
    }
    if (eleMag < 0) {
      dir[AD_ELEMRES] = dir[AD_ELEMRES] * Math.abs(eleMag);
    }
    else {
      dir[AD_ELEMRES] = dir[AD_ELEMRES] / eleMag;
    }

    // set the band in the band map. Bandmap is in words 18 and 19
    if (band <= 32) {
      dir[AD_BANDMAP] = 1 << (band - 1);
    }
    else {
      dir[AD_BANDMAP + 1] = 1 << (band - 32);
    }

    // FIXME update word 14?

    // create new directory with subsetted parameters
    areaDirectory = new AreaDirectory(dir);
  }

  /**
   * Is this <code>AreaFile</code> instance subseted.
   *
   * @return True if this instance represents a subset of the total data
   * available.
   */
  public boolean isSubsetted() {
    return !(subset == null);
  }

  /**
   * Was this instance create with a remote data source.
   * @return True if created with an ADDE url
   */
  public boolean isRemote() {
    return isRemote;
  }

  /**
   * Read the metadata for an area file (directory, nav, and cal).
   *
   * @exception AreaFileException
   *              if there is a problem reading any portion of the metadata.
   *
   */
  private void readMetaData() throws AreaFileException {

    int i;
    hasReadData = false;

    if (!fileok) {
      throw new AreaFileException("Error reading AreaFile directory");
    }

    dir = new int[AD_DIRSIZE];

    for (i = 0; i < AD_DIRSIZE; i++) {
      try {
        dir[i] = af.readInt();
      }
      catch (IOException e) {
        throw new AreaFileException("Error reading AreaFile directory:" + e);
      }
    }
    position += AD_DIRSIZE * 4;

    // see if the directory needs to be byte-flipped

    if (dir[AD_VERSION] != VERSION_NUMBER) {
      McIDASUtil.flip(dir, 0, 19);
      // check again
      if (dir[AD_VERSION] != VERSION_NUMBER)
        throw new AreaFileException("Invalid version number - probably not an AREA file");
      // word 20 may contain characters -- if small integer, flip it...
      if ((dir[20] & 0xffff) == 0) McIDASUtil.flip(dir, 20, 20);
      McIDASUtil.flip(dir, 21, 23);
      // words 24-31 contain memo field
      McIDASUtil.flip(dir, 32, 50);
      // words 51-2 contain cal info
      McIDASUtil.flip(dir, 53, 55);
      // word 56 contains original source type (ascii)
      McIDASUtil.flip(dir, 57, 63);
      flipwords = true;
    }

    areaDirectory = new AreaDirectory(dir);

    // pull together some values needed by other methods
    navLoc = dir[AD_NAVOFFSET];
    calLoc = dir[AD_CALOFFSET];
    auxLoc = dir[AD_AUXOFFSET];
    datLoc = dir[AD_DATAOFFSET];
    origNumBands = dir[AD_NUMBANDS];
    linePrefixLength = dir[AD_DOCLENGTH] + dir[AD_CALLENGTH] +
                       dir[AD_LEVLENGTH];
    if (dir[AD_VALCODE] != 0) linePrefixLength = linePrefixLength + 4;
    if (linePrefixLength != dir[AD_PFXSIZE])
      throw new AreaFileException("Invalid line prefix length in AREA file.");
    lineDataLen = origNumBands * dir[AD_NUMELEMS] * dir[AD_DATAWIDTH];
    lineLength = linePrefixLength + lineDataLen;
    origNumLines = dir[AD_NUMLINES];
    origNumElements = dir[AD_NUMELEMS];

    if (datLoc > 0 && datLoc != McIDASUtil.MCMISSING) {
      navbytes = datLoc - navLoc;
      calbytes = datLoc - calLoc;
      auxbytes = datLoc - auxLoc;
    }
    if (auxLoc > 0 && auxLoc != McIDASUtil.MCMISSING) {
      navbytes = auxLoc - navLoc;
      calbytes = auxLoc - calLoc;
    }

    if (calLoc > 0 && calLoc != McIDASUtil.MCMISSING) {
      navbytes = calLoc - navLoc;
    }


    // Read in nav block

    if (navLoc > 0 && navbytes > 0) {

      nav = new int[navbytes / 4];

      newPosition = (long)navLoc;
      skipByteCount = (int)(newPosition - position);
      try {
        af.skipBytes(skipByteCount);
      }
      catch (IOException e) {
        throw new AreaFileException("Error skipping AreaFile bytes: " + e);
      }

      for (i = 0; i < navbytes / 4; i++) {
        try {
          nav[i] = af.readInt();
        }
        catch (IOException e) {
          throw new AreaFileException("Error reading AreaFile navigation:" +
                                      e);
        }
      }
      if (flipwords) flipnav(nav);
      position = navLoc + navbytes;
    }


    // Read in cal block

    if (calLoc > 0 && calbytes > 0) {

      cal = new int[calbytes / 4];

      newPosition = (long)calLoc;
      skipByteCount = (int)(newPosition - position);
      try {
        af.skipBytes(skipByteCount);
      }
      catch (IOException e) {
        throw new AreaFileException("Error skipping AreaFile bytes: " + e);
      }

      for (i = 0; i < calbytes / 4; i++) {
        try {
          cal[i] = af.readInt();
        }
        catch (IOException e) {
          throw new AreaFileException("Error reading AreaFile calibration:" +
                                      e);
        }
      }
      // if (flipwords) flipcal(cal);
      position = calLoc + calbytes;
    }

    // Read in aux block

    if (auxLoc > 0 && auxbytes > 0) {
      aux = new int[auxbytes / 4];
      newPosition = (long)auxLoc;
      skipByteCount = (int)(newPosition - position);
      try {
        af.skipBytes(skipByteCount);
      }
      catch (IOException e) {
        throw new AreaFileException("Error skipping AreaFile bytes: " + e);
      }
      for (i = 0; i < auxbytes / 4; i++) {
        try {
          aux[i] = af.readInt();
        }
        catch (IOException e) {
          throw new AreaFileException("Error reading AreaFile aux block:" +
                                      e);
        }
      }
      position = auxLoc + auxbytes;
    }


    // now return the Dir, as requested...
    return;
  }

  /**
   * returns the string of the image source location
   *
   * @return name of image source
   *
   */
  public String getImageSource() {
    return imageSource;
  }

  /**
   * Returns the directory block
   *
   * @return an integer array containing the area directory
   *
   *
   */
  public int[] getDir() {
    return dir;
  }


  /**
   * Returns the AreaDirectory object for this AreaFile
   *
   * @return AreaDirectory
   *
   *
   */
  public AreaDirectory getAreaDirectory() {
    return areaDirectory;
  }


  /**
   * Returns the navigation block
   *
   * @return an integer array containing the nav block data
   *
   *
   */
  public int[] getNav() {

    if (navLoc <= 0 || navLoc == McIDASUtil.MCMISSING) {
      nav = null;
    }

    return nav;

  }

  /**
   * Get the navigation, and pre-set it
   * for the ImageStart and Res (from Directory block), and
   * the file start (0,0), and Mag (1,1).
   *
   * @return  AREAnav for this image  (may be null)
   *
   * @throws AreaFileException
   */
  public AREAnav getNavigation() throws AreaFileException {
    if (areaNav == null) {
      // make the nav module
      try {
        areaNav = AREAnav.makeAreaNav(getNav(), getAux());
        areaNav.setImageStart(dir[AD_STLINE], dir[AD_STELEM]);
        areaNav.setRes(dir[AD_LINERES], dir[AD_ELEMRES]);
        areaNav.setStart(0, 0);
        areaNav.setMag(1, 1);

      }
      catch (McIDASException excp) {
        areaNav = null;
      }
    }
    return areaNav;
  }

  /**
   * Returns calibration block
   *
   * @return an integer array containing the nav block data
   *
   *
   */
  public int[] getCal() {


    if (calLoc <= 0 || calLoc == McIDASUtil.MCMISSING) {
      cal = null;
    }

    return cal;

  }


  /**
   * Returns AUX block
   *
   * @return an integer array containing the aux block data
   *
   *
   */
  public int[] getAux() {

    if (auxLoc <= 0 || auxLoc == McIDASUtil.MCMISSING) {
      aux = null;
    }

    return aux;

  }

  /**
   * Read the AREA data.
   *
   * @return int array[band][lines][element] - If the <code>AreaFile</code>
   * was created as a subset only the band and subset indicated are returned,
   * otherwise all bands are returned.
   *
   * @exception AreaFileException if there is a problem
   *
   */
  public int[][][] getData() throws AreaFileException {
    data = new int[origNumBands][dir[AD_NUMLINES]][dir[AD_NUMELEMS]];
    return getData(data);
  }
  
  /**
   * Read AREA file data by reference. After reading the internal
   *  data array is will be a reference to the target array, so any changes made
   *  to the target array will be reflected in the internal data array.
   * 
   * @param target Array to use as the destination of the data read. This array
   *  must be appropriately dimensioned as [#bands][#lines][#elems].
   * @return int array[band][lines][element] - If the <code>AreaFile</code>
   * was created as a subset only the band and subset indicated are returned,
   * otherwise all bands are returned.
   * @throws IllegalArgumentException If the target array is not dimensioned 
   *  according to the subset, if subsetted, or otherwise has dimensions other than
   *  [bands][lines][elements].
   * @throws AreaFileException If an error occurs while reading data. 
   */
  public int[][][] getData(int[][][] target) throws AreaFileException {
    if (target == null ||
        (isSubsetted() && target.length != 1 && target[0].length != subset.numLines
            && target[0][0].length != subset.numEles)
      || (target.length != origNumBands && target[0].length != dir[AD_NUMLINES]
            && target[0][0].length != dir[AD_NUMELEMS])) {
      throw new IllegalArgumentException("target array is not dimensioned correctly");
    }
    
    if (!hasReadData) {
      if (subset == null) {
        readData(target);
      }
      else {
        readData(target, 
          subset.lineNumber, subset.numLines, subset.lineMag,
          subset.eleNumber, subset.numEles, subset.eleMag, subset.bandNumber);
      }
    }
    hasReadData = true;
    data = target;
    return data;
  }

  /**
   * Set the calibration type that will be used on data returned from
   * <code>getCalibratedData()</code>. This must be called before
   * <code>getCalibratedData()</code> to get calibrated data, otherwise it will
   * just return the data in the format specified in the directory.
   * @param cal calibration type from {@link Calibrator}.
   */
  public void setCalType(int cal) {
    calType = cal;
  }

  /**
   * Get the calibration type that will be used on data returned form
   * <code>getCalibratedData()</code>.
   * @return calibration type from {@link Calibrator}.
   */
  public int getCalType() {
    return calType;
  }


  /**
   * Read the AREA file and return the contents as floats. If the calibration
   * type has been set via <code>setCalType()</code>, <code>isRemote()</code>
   * is false, and a <code>Calibrator</code> is available data is calibrated
   * before returning, otherwise the calibration type is ignored.
   * The original data is alway preserved.
   *
   * @return data[band][lines][elements] as described above
   * @throws AreaFileException on error reading data.
   * @see Calibrator
   */
  public float[][][] getFloatData() throws AreaFileException {

    int[][][] inData = getData();
    float[][][] outData =
      new float[dir[AD_NUMBANDS]][dir[AD_NUMLINES]][dir[AD_NUMELEMS]];

    // create the appropriate calibrator
    Calibrator calibrator = null;

    int origType =
      AreaFileFactory.calStrToInt(areaDirectory.getCalibrationType());

    if (!isRemote() && getCalType() != Calibrator.CAL_NONE &&
        origType != getCalType()) {

      try {
        calibrator =
          CalibratorFactory.getCalibrator(areaDirectory.getSensorID(), cal);
      }
      catch (CalibratorException e) {
        // can't calibrate
      }
    }

    // get all bands
    if (subset == null) {
      for (int band_idx = 0; band_idx < inData.length; band_idx++) {
        for (int line = 0; line < inData[0].length; line++) {
          for (int elem = 0; elem < inData[0][0].length; elem++) {
            if (calibrator != null) {
              outData[band_idx][line][elem] =
                calibrator.calibrate((float)inData[band_idx][line][elem],
                                     band_idx + 1, calType);
            }
            else {
              outData[band_idx][line][elem] = inData[band_idx][line][elem];
            }
          }
        }
      }

      // just subsetted band
    }
    else {
      for (int line = 0; line < inData[0].length; line++) {
        for (int elem = 0; elem < inData[0][0].length; elem++) {
          if (!isRemote && calType != Calibrator.CAL_NONE &&
              calibrator != null) {
            outData[0][line][elem] =
              calibrator.calibrate((float)inData[0][line][elem],
                                   subset.bandNumber, calType);
          }
          else {
            outData[0][line][elem] = inData[0][line][elem];
          }
        }
      }
    }

    return outData;
  }

  /**
   * Read the specified 2-dimensional array of
   * data values from the AREA file.  Values will always be returned
   * as int regardless of whether they are 1, 2, or 4 byte values.
   *
   * @param lineNumber the file-relative image line number that will
   *                   be put in array[0][j]
   * @param eleNumber  the file-relative image element number that will
   *                   be put into array[i][0]
   * @param numLines   the number of lines to return
   * @param numEles    the number of elements to return for each line
   *
   * @return int array[lines][elements] with data values.
   * @deprecated Use one of the factory methods from {@link AreaFileFactory}
   * with the appropriate subsetting parameters.
   * @exception AreaFileException if the is a problem reading the file
   */
  public int[][] getData(int lineNumber, int eleNumber, int numLines,
                         int numEles)
          throws AreaFileException {
    return getData(lineNumber, eleNumber, numLines, numEles, 1);
  }


  /**
   * Read the specified 2-dimensional array of
   * data values from the AREA file.  Values will always be returned
   * as int regardless of whether they are 1, 2, or 4 byte values.
   *
   * @param lineNumber the file-relative image line number that will
   *                   be put in array[0][j]
   * @param eleNumber  the file-relative image element number that will
   *                   be put into array[i][0]
   * @param numLines   the number of lines to return
   * @param numEles    the number of elements to return for each line
   * @param bandNumber the spectral band to return
   *
   * @return int array[lines][elements] with data values.
   * @deprecated Use one of the factory methods from {@link AreaFileFactory}
   * with the appropriate subsetting parameters.
   * @exception AreaFileException if the is a problem reading the file
   */
  public int[][] getData(int lineNumber, int eleNumber, int numLines,
                         int numEles, int bandNumber)
          throws AreaFileException {

    //data = new int[1][numLines][numEles];
    if (!hasReadData) {
      data = new int[origNumBands][dir[AD_NUMLINES]][dir[AD_NUMELEMS]];
      readData(data);
    }
    int[][] subset = new int[numLines][numEles];
    for (int i = 0; i < numLines; i++) {
      int ii = i + lineNumber;
      for (int j = 0; j < numEles; j++) {
        int jj = j + eleNumber;
        if (ii < 0 || ii > (dir[AD_NUMLINES] - 1) || jj < 0 ||
            jj > (dir[AD_NUMELEMS] - 1)) {
          subset[i][j] = 0;
        }
        else {
          subset[i][j] = data[bandNumber - 1][ii][jj];
        }
      }
    }
    return subset;
  }

  /**
   *
   *
   * @param s
   *
   * @return
   */
  private int flipShort(short s) {
    return (int)(((s >> 8) & 0xff) | ((s << 8) & 0xff00));
  }

  /**
   *
   *
   * @param i
   *
   * @return
   */
  private int flipInt(int i) {
    return ((i >>> 24) & 0xff) | ((i >>> 8) & 0xff00) | ((i & 0xff) << 24)
           | ((i & 0xff00) << 8);
  }

  /**
   * Read a single band of subsetted data.
   *
   * @param lineNumber
   * @param numLines
   * @param lineMag
   * @param eleNumber
   * @param numEles
   * @param eleMag
   * @param bandNumber
   *
   * @throws AreaFileException
   */
  private void readData(int[][][] target, 
                        int lineNumber, int numLines, int lineMag,
                        int eleNumber, int numEles, int eleMag,
                        int bandNumber)
          throws AreaFileException {

    if (!fileok) {
      throw new AreaFileException("Error reading AreaFile data");
    }

    // multipliers for line/element skips
    int lineMagMult = (lineMag >= 1)
                      ? 0
                      : Math.abs(lineMag) - 1;
    int eleMagMult = (eleMag >= 1)
                     ? 0
                     : Math.abs(eleMag) - 1;

    int startLoc = dir[AD_DATAOFFSET];
    int elementSize = origNumBands * dir[AD_DATAWIDTH];
    int readElements = eleMagMult == 0
                       ? numEles
                       : numEles * Math.abs(eleMag);

    // num of lines to skip due to line resolution
    int lineSkip = lineMagMult * lineLength;
    // skip to read position on next line
    int readSkip = (origNumElements - readElements) * elementSize +
                   linePrefixLength;

    // number of element to skip due to resolution
    int elementSkip = eleMagMult * elementSize;
    // skip for bands we are not interrested in
    int bandSkip = elementSize - dir[AD_DATAWIDTH];

    int nextReadSkip = readSkip + lineSkip;
    int nextElementSkip = bandSkip + elementSkip;

    short shdata;
    int intdata;

    try {
      DataInputStream df = getInputStreamForData();
      if (df != af) {
        af = df;
      }
    }
    catch (IOException ioe) {
      throw new AreaFileException("Error getting input stream for data");

    }

    try {
      af.skipBytes(startLoc);
    }
    catch (IOException e) {
      throw new AreaFileException("Error skipping to start of data");
    }

    for (int i = 0; i < numLines; i++) {
      for (int j = 0; j < numEles; j++) {

        try {
          // all 1- and 2-byte data are un-signed!
          if (dir[AD_DATAWIDTH] == 1) {
            target[0][i][j] = ((int)af.readByte()) & 0xff;

          }
          else if (dir[AD_DATAWIDTH] == 2) {
            shdata = af.readShort();
            if (flipwords) {
              target[0][i][j] = flipShort(shdata) & 0xffff;
            }
            else {
              target[0][i][j] = ((int)shdata) & 0xffff;
            }

          }
          else if (dir[AD_DATAWIDTH] == 4) {
            intdata = af.readInt();
            if (flipwords) {
              target[0][i][j] = flipInt(intdata);
            }
            else {
              target[0][i][j] = intdata;
            }
          }

          af.skipBytes(nextElementSkip);

        }
        catch (IOException e) {
          throw new AreaFileException("Error reading element " + i +
                                      " in line " + j);
        }
      }

      // done with line, skip to relavent element in next relavent line
      try {
        af.skipBytes(nextReadSkip);
      }
      catch (IOException e) {
        throw new AreaFileException("Error skipping to next line");
      }

    }
  }

  /**
   * Read all data including all bands.
   *
   * @throws AreaFileException
   */
  private void readData(int[][][] target) throws AreaFileException {

    int i, j, k;
    int numLines = dir[AD_NUMLINES], numEles = dir[AD_NUMELEMS];

    if (!fileok) {
      throw new AreaFileException("Error reading AreaFile data");
    }

    try {
      DataInputStream df = getInputStreamForData();
      if (df != af) {
        datLoc = 0;
        position = 0;
        af = df;
      }
    }
    catch (IOException ioe) {
      throw new AreaFileException("Error getting input stream for data");

    }
    
    short shdata;
    int intdata;

    for (i = 0; i < numLines; i++) {

      try {
        newPosition = (long)(datLoc + linePrefixLength + i * lineLength);
        skipByteCount = (int)(newPosition - position);
        af.skipBytes(skipByteCount);
        position = newPosition;

      }
      catch (IOException e) {
        for (j = 0; j < numEles; j++) {
          for (k = 0; k < origNumBands; k++) {
            target[k][i][j] = 0;
          }
        }
        break;
      }

      for (j = 0; j < numEles; j++) {

        for (k = 0; k < origNumBands; k++) {

          if (j > lineDataLen) {
            target[k][i][j] = 0;
          }
          else {

            try {
              // all 1- and 2-byte data are un-signed!

              if (dir[AD_DATAWIDTH] == 1) {
                target[k][i][j] = ((int)af.readByte()) & 0xff;
                position = position + 1;
              }
              else if (dir[AD_DATAWIDTH] == 2) {
                shdata = af.readShort();
                if (flipwords) {
                  target[k][i][j] = flipShort(shdata) & 0xffff;
                }
                else {
                  target[k][i][j] = ((int)shdata) & 0xffff;
                }
                position = position + 2;
              }
              else if (dir[AD_DATAWIDTH] == 4) {
                intdata = af.readInt();
                if (flipwords) {
                  target[k][i][j] = flipInt(intdata);
                }
                else {
                  target[k][i][j] = intdata;
                }
                position = position + 4;
              }

            }
            catch (IOException e) {
              target[k][i][j] = 0;
            }
          }
        }
      }

    }

    hasReadData = true;

    try {
      af.close();
    }
    catch (IOException excp) {
      System.out.println("Couldn't close input stream for " + imageSource);
    }

    return;

  } // end of areaReadData method

  /**
   * Selectively flip the bytes of words in nav block
   *
   * @param nav array of nav parameters
   */
  private void flipnav(int[] nav) {

    // first word is always the satellite id in ASCII
    // check on which type:

    if (nav[0] == AREAnav.GVAR) {

      McIDASUtil.flip(nav, 2, 126);
      McIDASUtil.flip(nav, 129, 254);
      McIDASUtil.flip(nav, 257, 382);
      McIDASUtil.flip(nav, 385, 510);
      McIDASUtil.flip(nav, 513, 638);
    }

    else if (nav[0] == AREAnav.DMSP) {
      McIDASUtil.flip(nav, 1, 43);
      McIDASUtil.flip(nav, 45, 51);
    }

    else if (nav[0] == AREAnav.POES) {
      McIDASUtil.flip(nav, 1, 119);
    }

    else if (nav[0] == AREAnav.GMSX) {}
    else {
      McIDASUtil.flip(nav, 1, nav.length - 1);
    }

    return;
  }

  /**
   * Get a String representation of this image
   *
   * @return
   */
  public String toString() {
    AreaDirectory dir = getAreaDirectory();
    String EOL = "\n";
    StringBuffer buff = new StringBuffer();
    buff.append("Directory values =========" + EOL);
    buff.append("Num Lines: " + dir.getLines() + EOL);
    buff.append("Num Elements: " + dir.getElements() + EOL);
    buff.append("Start Line: " + dir.getDirectoryBlock()[AD_STLINE] + EOL);
    buff.append("Start Element: " + dir.getDirectoryBlock()[AD_STELEM] + EOL);
    buff.append("Line Res: " + dir.getDirectoryBlock()[AD_LINERES] + EOL);
    buff.append("Elem Res: " + dir.getDirectoryBlock()[AD_ELEMRES] + EOL);
    buff.append("Bands:");
    for (int i = 0; i < dir.getBands().length; i++)
      buff.append(" " + dir.getBands()[i]);
    buff.append(EOL);
    buff.append("Source Type: " + dir.getSourceType() + EOL);
    buff.append("Sensor Type: " + dir.getSensorType() + EOL);
    buff.append("Sensor ID: " + dir.getSensorID() + EOL);
    buff.append("Cal Type: " + dir.getCalibrationType() + EOL);
    buff.append(
      "Nominal Time: " + dir.getDirectoryBlock()[AD_IMGDATE] + " " +
      dir.getDirectoryBlock()[AD_IMGTIME] + EOL);
    buff.append("==========================" + EOL);
    try {
      buff.append("Nav: " + getNavigation() + EOL);
    }
    catch (AreaFileException e) {
    }
    buff.append(
      "User Cal Type: " +
      AreaFileFactory.calIntToStr(getCalType()).toUpperCase());
    return buff.toString();
  }

  /**
   * Test Method.
   * <pre>
   * USAGE: AreaFile &lt;source&gt; [(raw|temp|brit|rad|refl)]
   * </pre>
   * <p>If source is a file path or url without subsetting information directory
   * information is printed. If source is a local file url with subsetting
   * information data is printed according to the parameters.</p>
   *
   * <p>This has not been tested with an ADDE url, but it should work ...
   * maybe.</p>
   *
   * @param args
   * @throws Exception
   */
  public static void main(String[] args) throws Exception {
    if (args == null || args.length == 0) {
      System.out.println();
      System.out.println("USAGE: AreaFile <URL or filepath> <show_vals>");
      System.out.println();
      System.exit(1);
    }
    AreaFile af = AreaFileFactory.getAreaFileInstance(args[0]);

    System.out.println(af);

    System.out.println();
    System.out.println(af.subset);

    System.out.println();
    long time = System.currentTimeMillis();
    System.out.print("Getting data ... ");
    float data[][][] = af.getFloatData();

    System.out.println(
      "" + (System.currentTimeMillis() - time) + "ms to retrieve " +
      AreaFileFactory.calIntToStr(af.getCalType()).toUpperCase() + " data");
    System.out.println();

    System.out.println(
      "DATA [" + data.length + "][" + data[0].length + "][" +
      data[0][0].length + "]");

    if (args.length > 1 && !af.isSubsetted()) {
      System.err.println("Sorry, I won't print an unsubsetted file");
      System.exit(0);
    }

    if (args.length > 1) {
      // write data to std err so it may be piped to file w/o all the
      // other garbage
      for (int i = 0; i < data[0].length; i++) {
        for (int j = 0; j < data[0][0].length; j++) {
          System.err.print("" + data[0][i][j] + " ");
        }
        System.err.println();
      }
    }
  }

  /**
   * Get the input stream for the image.  Handles Unidata PNG
   * compressed images.
   *
   * @return  the input stream for the reading
   *
   * @throws IOException
   */
  private DataInputStream getInputStreamForData() throws IOException {

    if (af.markSupported()) {
      //System.out.println("mark is supported");
      // calculate offset to potentially compressed data block
      int numComments = dir[AD_NUMCOMMENTS];
      // System.out.println("number  of comment cards = " + numComments);
      int compressedDataStart = numComments * 80 + datLoc;
      af.mark((int)(compressedDataStart - position + 10));
      af.skip(compressedDataStart - position);
      byte[] test = new byte[8];
      af.read(test);
      af.reset();
      if (isPNG(test)) {
        //System.out.println("isPNG");
        if (numComments > 0) {
          byte[] comments = new byte[numComments * 80];
          af.read(comments);
          /*
          for (int i = 0; i < numComments; i++) {
              byte[] comment = new byte[80];
              System.arraycopy(comments, i*80, comment, 0, 80);
              System.out.println("card["+i+"] = " + new String(comment));
          }
          */
        }
        int available = af.available();

        byte[] data = new byte[available];
        af.readFully(data);
        af.close();
        ByteArrayInputStream ios = new ByteArrayInputStream(data);

        BufferedImage image = javax.imageio.ImageIO.read(ios);
        Raster raster = image.getData();
        DataBuffer db = raster.getDataBuffer();

        if (db instanceof DataBufferByte) {
          DataBufferByte dbb = (DataBufferByte)db;
          byte[] udata = dbb.getData();
          ByteArrayInputStream newios = new ByteArrayInputStream(udata);
          return new DataInputStream(newios);

        }
      }
      else {
        return af;
      }
    }

    return af;
  }

  /**
   * Check if this is a PNG compressed image
   *
   * @param bytes  bytes to check
   *
   * @return  true if it fits the profile
   */
  private boolean isPNG(byte[] bytes) {

    if (bytes.length != 8) return false;
    return bytes[0] == -119 && bytes[1] == 80 && // P
           bytes[2] == 78 && // N
           bytes[3] == 71 && // G
           bytes[4] == 13 && bytes[5] == 10 && bytes[6] == 26 &&
           bytes[7] == 10;
  }

  /**
   * Close this instance.
   */
  public void close() {
    if (af == null) return;
    try {
      af.close();
    }
    catch (IOException ioe) {
    }
  }

  /**
   * Save this AreaFile to the output location
   * @param outputFile  path to the output file
   *
   * @throws AreaFileException  problem saving to the file
   */
  public void save(String outputFile) throws AreaFileException {
    save(outputFile, false);
  }

  /**
   * Save this AreaFile to the output location
   * @param outputFile  path to the output file
   * @param verbose   true to print out status messages
   * @throws AreaFileException on any error writing the file
   */
  public void save(String outputFile, boolean verbose)
          throws AreaFileException {

    int[] dir = getDir();
    if (dir == null) {
      System.out.println("No AREA file directory!");
      return;
    }
    if (verbose) {
      System.out.println("Length of directory = " + dir.length);

      for (int i = 0; i < dir.length; i++) {
        System.out.println(" index " + i + " = " + dir[i]);
      }
    }

    int[] nav = getNav();
    if (nav == null) {
      if (verbose) System.out.println("No navigation block!");
    }
    else {
      if (verbose) System.out.println("Length of nav block = " + nav.length);
    }

    int[] cal = getCal();
    if (cal == null) {
      if (verbose) System.out.println("No calibration block!");
    }
    else {
      if (verbose) System.out.println("Length of cal block = " + cal.length);
    }

    int[] aux = getAux();
    if (aux == null) {
      if (verbose) System.out.println("No aux block");
    }
    else {
      if (verbose) System.out.println("Length of aux block = " + aux.length);
    }

    int NL = dir[8];
    int NE = dir[9];

    if (verbose)
      System.out.println("Start reading data, num points=" + (NL * NE));

    int[][] data;

    data = getData(0, 0, NL, NE);

    if (verbose) System.out.println("Finished reading data");

    try {
      RandomAccessFile raf = new RandomAccessFile(outputFile, "rw");

      if (verbose) System.out.println("Dir to word 0");
      raf.seek(0);
      dir[0] = 0; // make sure this is zero!!
      for (int i = 0; i < dir.length; i++)
        raf.writeInt(dir[i]);

      if (verbose) System.out.println("Nav to word " + dir[AD_NAVOFFSET]);
      if (nav != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_NAVOFFSET]);
        for (int i = 0; i < nav.length; i++)
          raf.writeInt(nav[i]);
      }

      if (verbose) System.out.println("Cal to word " + dir[AD_CALOFFSET]);
      if (cal != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_CALOFFSET]);
        for (int i = 0; i < cal.length; i++)
          raf.writeInt(cal[i]);
      }

      if (verbose) System.out.println("Aux to word " + dir[AD_AUXOFFSET]);
      if (aux != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_AUXOFFSET]);
        for (int i = 0; i < aux.length; i++)
          raf.writeInt(aux[i]);
      }

      if (verbose) System.out.println("Data to word " + dir[AD_DATAOFFSET]);
      if (dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_DATAOFFSET]);
        for (int i = 0; i < data.length; i++) {
          for (int j = 0; j < data[i].length; j++) {
            if (dir[AD_DATAWIDTH] == 1) {
              raf.writeByte(data[i][j]);
            }
            else if (dir[AD_DATAWIDTH] == 2) {
              raf.writeShort(data[i][j]);
            }
            else if (dir[AD_DATAWIDTH] == 4) {
              raf.writeInt(data[i][j]);
            }
          }
        }
      }

      raf.close();
    }
    catch (Exception we) {
      throw new AreaFileException("Unable to save file " + we.getMessage());
    }
    if (verbose)
      System.out.println("Completed. Data saved to: " + outputFile);
  }
}

