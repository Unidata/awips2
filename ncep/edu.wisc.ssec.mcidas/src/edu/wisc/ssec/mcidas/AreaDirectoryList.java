//
// AreaDirectoryList.java
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

import edu.wisc.ssec.mcidas.adde.AddeURLConnection;
import java.applet.Applet;
import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Vector;


/** 
 * AreaDirectoryList interface for McIDAS 'area' file format image data.
 * Provides access to a list of one or more AreaDirectoy objects.
 *
 * @author Don Murray
 * 
 */
public class AreaDirectoryList
{
  private static boolean debug = false;

  // load protocol for ADDE URLs
  // See java.net.URL for explanation of URL handling
  static 
  {
    try 
    {
      String handlers = System.getProperty("java.protocol.handler.pkgs");
      String newProperty = null;
      if (handlers == null)
        newProperty = "edu.wisc.ssec.mcidas";
      else if (handlers.indexOf("edu.wisc.ssec.mcidas") < 0)
        newProperty = "edu.wisc.ssec.mcidas | " + handlers;
      if (newProperty != null)  // was set above
        System.setProperty("java.protocol.handler.pkgs", newProperty);
    }
    catch (Exception e)
    {
      System.out.println(
        "Unable to set System Property: java.protocol.handler.pkgs"); 
    }
  }

  private boolean flipwords = false;
  private DataInputStream inputStream;  // input stream
  private int status=0;         // read status
  private URLConnection urlc;       // URL connection
  private boolean isADDE = false;     // true if ADDE request
  private int[] dir;          // single directory
  private Date[] nominalTimes;      // array of dates
  private int[] bands;          // array of bands
  private int[] lines;          // array of lines
  private int[] elements;         // array of elements
  private ArrayList dirs;         // list of directories
  private int numDirs = 0;        // number of directories
  
  /**
   * creates an AreaDirectory object that allows reading
   * of McIDAS 'area' file format image data.  allows reading
   * either from a disk file, or a server via ADDE.  
   *
   * @param imageSource the file name or ADDE URL to read from
   *
   * @exception AreaFileException if file cannot be opened
   *
   */
  public AreaDirectoryList(String imageSource) 
    throws AreaFileException 
  {
   
    // try as a disk file first
    try 
    {
      inputStream = 
        new DataInputStream (
          new BufferedInputStream(
            new FileInputStream(imageSource), 2048));
    } 
    catch (IOException eIO) 
    {
      // if opening as a file failed, try as a URL
      URL url;
      try 
      {
        url = new URL(imageSource);
        urlc = url.openConnection();
        InputStream is = urlc.getInputStream();
        inputStream = 
          new DataInputStream(
            new BufferedInputStream(is));
      }
      catch (Exception e) 
      {
        throw new AreaFileException("Error opening AreaFile: " + e);
      }
      if (url.getProtocol().equalsIgnoreCase("adde")) isADDE = true;
    }
    readDirectory();
  }
 

  /**
   * creates an AreaDirectory object that allows reading
   * of the directory of McIDAS 'area' files from a URL
   *
   * @param URL - the URL to go after
   *
   * @exception AreaFileException if file cannot be opened
   *
   */
  public AreaDirectoryList(URL url) 
    throws AreaFileException 
  {
    try 
    { 
      inputStream = 
        new DataInputStream(
          new BufferedInputStream(url.openStream()));
    } 
    catch (IOException e) 
    {
      throw new AreaFileException("Error opening URL for AreaFile:"+e);
    }
    readDirectory();
  }
  
  /** 
   * Read the directory information for an area file or area directory
   * record.
   *
   * @exception   AreaFileException  if there is a problem reading 
   *                   any portion of the metadata.
   *
   */
  private void readDirectory() throws AreaFileException {

    double resolutionLat = Double.NaN;
    double resolutionLon = Double.NaN;
    double centerLat = Double.NaN;
    double centerLon = Double.NaN;
    int band = 0;
    String calname = " ", caldesc = " ";

    dirs = new ArrayList();
    int numBytes = 
      (isADDE) 
        ? ((AddeURLConnection) urlc).getInitialRecordSize() 
        : AreaFile.AD_DIRSIZE;
    while (numBytes > 0) {
      try {
        dir = new int[AreaFile.AD_DIRSIZE];
  
        // skip first int which is dataset area number if ADDE request
        if (isADDE) {
          int areaNumber = inputStream.readInt();
          if (debug) System.out.println("Area number = " + areaNumber);
        }
    
        for (int i=0; i < AreaFile.AD_DIRSIZE; i++) {
          dir[i] = inputStream.readInt();
        }

        if (!isADDE) dir[0] = 0;
    
        // see if the directory needs to be byte-flipped
        if (dir[AreaFile.AD_VERSION] > 255 || flipwords) {
          flipwords = true;
          McIDASUtil.flip(dir,0,19);
          // word 20 may contain characters -- if small int, flip it
          if ( (dir[20] & 0xffff) == 0) McIDASUtil.flip(dir,20,20);
          McIDASUtil.flip(dir,21,23);
          // words 24-31 contain memo field
          McIDASUtil.flip(dir,32,50);
          // words 51-2 contain cal info
          McIDASUtil.flip(dir,53,55);
          // word 56 contains original source type (ascii)
          McIDASUtil.flip(dir,57,63);
        }
  

        if (debug) {
        for (int i = 0; i < AreaFile.AD_DIRSIZE; i++)
          {
            System.out.println("dir[" + i +"] = " + dir[i]);
          }
        }
    
        AreaDirectory ad = new AreaDirectory(dir);
        int[] bands = ad.getBands();
        int numBands = ad.getNumberOfBands();
        // make a Vector to hold the band calibration info (if available)
        Vector [] calInfo = new Vector[numBands];
        for (int k=0; k<numBands; k++) {
          calInfo[k] = new Vector();
        }
  
        if (!isADDE) {
          numBytes = 0;
        } else {
          // last word in trailer is the number of bytes for the
          // next record so we need to read that
          /*
          int skipBytesCount = numBytes - AreaFile.AD_DIRSIZE*4 - 4;
          inputStream.skipBytes(skipBytesCount);
          */
          /* */
          int numCards = dir[AreaFile.AD_DIRSIZE -1];
          if (debug) System.out.println("Number of comment cards = " + numCards);
          for (int i = 0; i < numCards; i++)
          {
            byte[] card = new byte[80];
            int count = 0;
            boolean prevBlank = true;
            for (int j = 0; j < 80; j++) {
              byte b = inputStream.readByte();
              if (b == ' ') {
                if (!prevBlank) {
                card[count++] = b;
                prevBlank = false;
                }
                prevBlank = true;
              }  else {
                card[count++] = b;
                prevBlank = false;
              }
            }
            String cd = new String(card,0,count).trim();

            if (debug) System.out.println("card["+i+"] = " + cd);

            if (cd.indexOf("Center latitude") > -1) {
              int m = cd.indexOf("=");
              if (m > 0) {
                centerLat = Double.valueOf(
                       cd.substring(m+1).trim()).doubleValue();
              } else {
                centerLat = Double.NaN;
              }
            } else if (cd.indexOf("Center longitude") > -1) {
              int m = cd.indexOf("=");
              if (m > 0) {
                centerLon = - Double.valueOf(
                       cd.substring(m+1).trim()).doubleValue();
              } else {
                centerLon = Double.NaN;
              }
            } else if (cd.indexOf("Computed Latitude") > -1) {
              int m = cd.indexOf("=");
              if (m > 0) {
                resolutionLat = Double.valueOf(
                       cd.substring(m+1).trim()).doubleValue();
              } else {
                resolutionLat = Double.NaN;

              }
            } else if (cd.indexOf("Computed Longitude") > -1) {
              int m = cd.indexOf("=");
              if (m > 0) {
                resolutionLon = Double.valueOf(
                       cd.substring(m+1).trim()).doubleValue();
              } else {
                resolutionLon = Double.NaN;
              }
            } else if (cd.indexOf("Valid calibration unit") > -1) {
              int m = cd.indexOf("=");
              if (m > 0) {
                String cdd = cd.replace('"',' ');
                StringTokenizer st = new StringTokenizer(cdd," ");
                int n = st.countTokens();
                band = 0;
                calname = " ";
                caldesc = " ";
                boolean gotit = false;
                for (int k=0; k<n; k++) {
                  if (st.nextToken().trim().equals("band")) {
                  gotit = true;
                  break;
                  }
                }

                if (gotit) {
                  band = Integer.parseInt(st.nextToken().trim());
                  st.nextToken();  // skip = sign
                  calname = st.nextToken();
                  caldesc = calname;
                  if (st.hasMoreTokens()) {
                    StringBuffer buf = new StringBuffer();
                    while (st.hasMoreTokens()) {
                       buf.append(st.nextToken());
                       buf.append(" ");
                    }
                    caldesc = buf.toString().trim();
                  }
                  for (int k=0; k<numBands; k++) {
                    if (band == bands[k]) {
                      calInfo[k].addElement(calname);
                      calInfo[k].addElement(caldesc);
                    }
                  }
                }
              }
              
            } 
          }
          ad.setCenterLatitude(centerLat);
          ad.setCenterLongitude(centerLon);
          ad.setCenterLatitudeResolution(resolutionLat);
          ad.setCenterLongitudeResolution(resolutionLon);
          ad.setCalInfo(calInfo);

          /* */
          numBytes = inputStream.readInt();
          if (debug) System.out.println("Bytes in next record = " + numBytes);
        }
        dirs.add(ad);
      }
      catch (IOException e) {
        status = -1;
        throw new AreaFileException(
          "Error reading Area directory:" + e);
      }
      status = 1;
      numDirs++;
    } 
  }
  
  /** returns the directory blocks for the requested images,
   *  sorted into an array of AreaDirectories by time and
   *  then bands as the second dimenison, if multiple directories
   *  with the same ADDE position number are returned.
   */

   public AreaDirectory[][] getSortedDirs() throws AreaFileException {
    if (status <= 0 || dirs.size() <= 0) {
      throw new AreaFileException( "Error reading directory information");
    }

    // first gather up the positions and nominal times
    Date[] dtg = new Date[numDirs];
    int[] pos = new int[numDirs];
    int[] insitu = new int[numDirs];
    ArrayList al = new ArrayList();
    ArrayList alt = null;

    for (int i=0; i<numDirs; i++) {
      AreaDirectory ad = (AreaDirectory) dirs.get(i);
      dtg[i] = ad.getNominalTime();
      pos[i] = ad.getValue(0);
      insitu[i] = i;
    }

    Date ddd = null;
    // now sort the values by date
    for (int i=0; i<numDirs; i++) {
      int swap = i;
      for (int k=i+1; k<numDirs; k++) {
        if (dtg[swap].compareTo(dtg[k]) > 0) swap = k;
      }
      if (swap != i) {
        Date dtgt = dtg[i];
        dtg[i] = dtg[swap];
        dtg[swap] = dtgt;
        int intt = pos[i];
        pos[i] = pos[swap];
        pos[swap] = intt;
        intt = insitu[i];
        insitu[i] = insitu[swap];
        insitu[swap] = intt;
      }

      // look to see if date-time has changed
      if ( (ddd == null) || (ddd.compareTo(dtg[i]) != 0)) {

         if (ddd != null) al.add(alt);  // add the previous one if there

         ddd = dtg[i];
         alt = new ArrayList();
      }

      alt.add(dirs.get(insitu[i]) );  // copy the AreaDirectory entry over

    }

    // catch the very last one...
    if (alt != null && alt.size() > 0) al.add(alt);

    int num = al.size();

    if (num == 0) return null;

    // now make an array of AreaDirectory objects for return...
    
    AreaDirectory[][] ada = new AreaDirectory[num][];

    for (int i=0; i<num; i++) {
       alt = (ArrayList) al.get(i);
       int knum = alt.size();
       ada[i] = new AreaDirectory[knum];
       for (int k=0; k<knum; k++) {
         ada[i][k] = (AreaDirectory)alt.get(k);
       }
    }

    return ada;
   }



  /** 
   * returns the directory blocks for the requested images.  
   * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
   *    McIDAS Programmer's Manual</A> for information on the parameters
   *    for each value.
   *
   * @return a ArrayList of AreaDirectorys 
   *
   * @exception AreaFileException if there was a problem
   *      reading the directory
   *
   */
  public ArrayList getDirs() 
    throws AreaFileException 
  {
    if (status <= 0 || dirs.size() <= 0) 
    {
      throw new AreaFileException(
        "Error reading directory information");
    }
    return dirs;
  }

  /**
   * Prints out a formatted listing of the directory info
   */
  public String toString()
  {
    if (status <=0 || numDirs <= 0)
    {
      return new String("No directory information available");
    }
    StringBuffer sb = new StringBuffer();
    sb.append("    Date         Time    Lin  Ele  Bands \n");
    sb.append("  -------       -------  ---  ---  --------\n");
    for (int i = 0; i < dirs.size(); i++)
    {
      sb.append( ((AreaDirectory) dirs.get(i)).toString());
      sb.append("\n");
    }
    return sb.toString();
  }

  public static void main(String[] args)
    throws Exception
  {
    if (args.length == 0)
    {
      System.out.println("Must supply a path or ADDE request to images");
      System.exit(1);
    }
    AreaDirectoryList adl = new AreaDirectoryList(args[0]);
    System.out.println(adl.toString());

    // print out test of getSortedDirs()
    AreaDirectory[][] ada = adl.getSortedDirs();
    for (int i=0; i<ada.length; i++) {
      Date dd = ada[i][0].getNominalTime();
      System.out.print(dd+" ");
      for (int k=0; k<ada[i].length; k++) {
        int[] bands = ada[i][k].getBands();

        for (int b=0; b<bands.length; b++) {
          System.out.print(" "+bands[b]);
        }
      }
      System.out.println(" ");
    }
  }
}
