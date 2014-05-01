//
// AncillaryData.java
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

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.String;

/**
 * AncillaryData creates an object providing access to image
 * parameters needed to do conversion from McIDAS area format
 * to some other image format.
 *
 * @version 1.6 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */
 
public class AncillaryData {

  private static final int DIR_SIZE = 64;
  private int numLines = 0;
  private int numElems = 0;
  private int firstImageLine = 0;
  private int firstImageElem = 0;
  private int lineResolution = 0;
  private int elemResolution = 0;
  private int imageDate = 0;
  private int imageTime = 0;
  private int creationDate = 0;
  private int creationTime = 0;
  private int bandCount = 0;
  private int sensorId = 0;
  private int status = 0;
  private int version = 0;
  private int dataWidth = 0;
  private int numBands = 0;
  private int prefixSize = 0;
  private int projectNum = 0;
  private int bandMap = 0;
  private int calType = 0;
  private int navOffset = 0;
  private int calOffset = 0;
  private int datOffset = 0;
  private boolean swapWords = false;

  /**
   *
   * constructor
   *
   * @param dis		data input stream 
   *
   */

  public AncillaryData (
    DataInputStream dis
  ) 
    throws IOException

  {

    int [] directory;
    int i;

    // read in what corresponds to the McIDAS area directory
    directory = new int[DIR_SIZE];
    for (i = 0; i < DIR_SIZE; i++) {
      directory[i] = dis.readInt();
    }

    // byte swap if necessary
    if (directory[1] > 255) {
      ConversionUtility.swap(directory,0,19);
      // word 20 may contain characters -- if small integer, swap it...
      if ((directory[20] & 0xffff) == 0) {
        ConversionUtility.swap(directory,20,20);
      }
      ConversionUtility.swap(directory,21,23);
      // words 24-31 contain memo field
      ConversionUtility.swap(directory,32, 50);
      // words 51-2 contain cal info
      ConversionUtility.swap(directory,53,55);
      // word 56 contains original source type (ascii)
      ConversionUtility.swap(directory,57,63);
      swapWords = true;
    }

    // now load the class fields with the appropriate data
    numLines = directory[AreaFile.AD_NUMLINES];
    numElems = directory[AreaFile.AD_NUMELEMS];
    firstImageLine = directory[AreaFile.AD_STLINE];
    firstImageElem = directory[AreaFile.AD_STELEM];
    lineResolution = directory[AreaFile.AD_LINERES];
    elemResolution = directory[AreaFile.AD_ELEMRES];
    imageDate = directory[AreaFile.AD_IMGDATE];
    imageTime = directory[AreaFile.AD_IMGTIME];
    bandCount = directory[AreaFile.AD_NUMBANDS];
    sensorId = directory[AreaFile.AD_SENSORID];
    creationDate = directory[AreaFile.AD_CRDATE];
    creationTime = directory[AreaFile.AD_CRTIME];
    status = directory[AreaFile.AD_STATUS];
    version = directory[AreaFile.AD_VERSION];
    dataWidth = directory[AreaFile.AD_DATAWIDTH];
    numBands = directory[AreaFile.AD_NUMBANDS];
    prefixSize = directory[AreaFile.AD_PFXSIZE];
    projectNum = directory[AreaFile.AD_PROJNUM];
    bandMap = directory[AreaFile.AD_BANDMAP];
    calType = directory[AreaFile.AD_CALTYPE];
    navOffset = directory[AreaFile.AD_NAVOFFSET];
    calOffset = directory[AreaFile.AD_CALOFFSET];
    datOffset = directory[AreaFile.AD_DATAOFFSET];

  }

  public int getCalType() {
    char [] calBuf = new char[4];
    calBuf[0] = (char) ((calType >> 24) & 0xFF);
    calBuf[1] = (char) ((calType >> 16) & 0xFF);
    calBuf[2] = (char) ((calType >> 8) & 0xFF);
    calBuf[3] = (char) ((calType) & 0xFF);
    if (String.valueOf(calBuf).equals("RAW ")) {
      System.out.println("determined input cal type is RAW");
      return Calibrator.CAL_RAW;
    } else if (String.valueOf(calBuf).equals("BRIT")) {
      System.out.println("determined input cal type is BRIT");
      return Calibrator.CAL_BRIT;
    } else if (String.valueOf(calBuf).equals("TEMP")) {
      System.out.println("determined input cal type is TEMP");
      return Calibrator.CAL_TEMP;
    } else if (String.valueOf(calBuf).equals("RAD ")) {
      System.out.println("determined input cal type is RAD");
      return Calibrator.CAL_RAD;
    } else if (String.valueOf(calBuf).equals("ALB")) {
      System.out.println("determined input cal type is ALB");
      return Calibrator.CAL_ALB;
    }
    return Calibrator.CAL_NONE;

  }

  /**
   *
   * return sensor id
   *
   */

  public int getSensorId() {
    return sensorId;
  }

  /**
   *
   * return number of elements
   *
   */

  public int getNumElements() {
    return numElems;
  }

  /**
   *
   * return number of lines
   *
   */

  public int getNumLines() {
    return numLines;
  }

  /**
   *
   * return starting image line
   *
   */

  public int getStartLine() {
    return firstImageLine;
  }

  /**
   *
   * return starting image element
   *
   */

  public int getStartElem() {
    return firstImageElem;
  }

  /**
   *
   * return line resolution
   *
   */

  public int getLineRes() {
    return lineResolution;
  }

  /**
   *
   * return element resolution
   *
   */

  public int getElemRes() {
    return elemResolution;
  }

  /**
   *
   * return image date
   *
   */

  public int getImageDate() {
    return imageDate;
  }

  /**
   *
   * return image time
   *
   */

  public int getImageTime() {
    return imageTime;
  }

  /**
   *
   * return image creation date
   *
   */

  public int getCreationDate() {
    return creationDate;
  }

  /**
   *
   * return image creation time
   *
   */

  public int getCreationTime() {
    return creationTime;
  }

  /**
   *
   * return image status field
   *
   */

  public int getStatus() {
    return status;
  }

  /**
   *
   * return file format version number
   *
   */

  public int getVersion() {
    return version;
  }

  /**
   *
   * return number of bytes per pixel
   *
   */

  public int getDataWidth() {
    return dataWidth;
  }

  /**
   *
   * return number of bands/channels in image
   *
   */

  public int getNumBands() {
    return numBands;
  }

  /**
   *
   * return size in bytes of line prefix
   *
   */

  public int getPrefixSize() {
    return prefixSize;
  }

  /**
   *
   * return project number associated with image
   *
   */

  public int getProjectNum() {
    return projectNum;
  }

  /**
   *
   * return 32 bit band map
   *
   */

  public int getBandMap() {
    return bandMap;
  }

  /**
   *
   * return offset in bytes to navigation data
   *
   */

  public int getNavOffset() {
    return navOffset;
  }

  /**
   *
   * return offset in bytes to calibration data
   *
   */

  public int getCalOffset() {
    return calOffset;
  }

  /**
   *
   * return offset in bytes to image data
   *
   */

  public int getDataOffset() {
    return datOffset;
  }

  /**
   *
   * return flag indicating certain fields were byte flipped
   *
   */

  public boolean isSwapped() {
    return swapWords;
  }

}
