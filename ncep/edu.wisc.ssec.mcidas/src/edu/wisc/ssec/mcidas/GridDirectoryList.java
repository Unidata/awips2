//
// GridDirectoryList.java
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
import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;


/** 
 * GridDirectoryList interface for McIDAS 'grids'.
 * Provides access to a list of one or more GridDirectoy objects.
 *
 * @author Don Murray
 * 
 */
public class GridDirectoryList
{

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
  private int status=-1;                 // read status
  private AddeURLConnection urlc;           // URL connection
  private ArrayList dirs;         // list of directories
  private ArrayList fileHeaders;  // list of file headers
  private int numDirs = 0;        // number of directories
  private final int HEARTBEAT = 11223344;
  
  /**
   * Creates an GridDirectory object that allows reading
   * of McIDAS 'grids' from an ADDE server 
   *
   * @param gridSource    ADDE URL to read from as a String
   *
   * @exception McIDASException if file cannot be opened
   *
   */
  public GridDirectoryList(String gridSource) 
    throws McIDASException 
  {
    try 
    { 
      URL url = new URL(gridSource);
      urlc = (AddeURLConnection) url.openConnection();
      inputStream = 
        new DataInputStream(
          new BufferedInputStream(urlc.getInputStream()));
    } 
    catch (IOException e) 
    {
      throw new McIDASException("Error opening URL for grids:"+e);
    }
    readDirectory();
  }
   
  /**
   * creates an GridDirectory object that allows reading
   * of the directory of McIDAS 'grid' files from a URL
   *
   * @param URL - the URL to go after
   *
   * @exception McIDASException if file cannot be opened
   *
   */
  public GridDirectoryList(URL url) 
    throws McIDASException 
  {
    try 
    { 
      urlc = (AddeURLConnection) url.openConnection();
      inputStream = 
        new DataInputStream(
          new BufferedInputStream(urlc.getInputStream()));
    } 
    catch (IOException e) 
    {
      throw new McIDASException("Error opening URL for grids:"+e);
    }
    readDirectory();
  }
  
  /** 
   * Read the directory information for a grid.
   *
   * @exception   McIDASException  if there is a problem reading 
   *                   any portion of the metadata.
   *
   */
  private void readDirectory() throws McIDASException 
  {

    if (urlc.getRequestType() != AddeURLConnection.GDIR)
      throw new McIDASException("Request must be of GDIR type");
    dirs = new ArrayList();
    fileHeaders = new ArrayList();
    /* See how many bytes there are to read.  
       From the McIDAS Programmers Manual a request transmits back:
        - 4-byte value containing the total number of bytes to transmit 
        - 4-byte value of zero indicating a new file header is being sent; 
          this information is repeated for each new file header to transfer 
          and is sent after all grids from the previous file are transmitted 
        - 256-byte grid file header; this information is repeated for each 
          new grid file header to transmit and is sent after the grids from 
          the previous file are transmitted 
        - 4-byte value of zero indicating a new grid directory is being sent; 
          this information is repeated before each grid directory in the file 
          is sent 
        - 256-byte grid header; this information is repeated for each grid 
          in the file being sent 
        - 4-byte value of 1 indicating no more grid directories in the file 
     */
    int numBytes = urlc.getInitialRecordSize();
    if (numBytes == 0) {
      throw new McIDASException("No datasets found");
    }
    int check;
    byte[] header = new byte[256];
    //  Check for heartbeat
    try
    {
      while (numBytes == 4) 
      {
        check = inputStream.readInt();
        if (check != HEARTBEAT) 
          System.out.println("problem...not heartbeat = "+check);
          numBytes = inputStream.readInt();
      }
      // now we get the file header
      while ( (check = inputStream.readInt()) == 0)  // new header
      {
        inputStream.readFully(header,0,256);
        String head = new String(header,0,32);
        fileHeaders.add(head);

        int check2;
  
        // now we get the grid directory
        while( (check2 = inputStream.readInt()) == 0) 
        {
           int[] dir = new int[GridDirectory.DIRSIZE];
           for (int i=0; i < AreaFile.AD_DIRSIZE; i++) 
           {
             dir[i] = inputStream.readInt();
           }
     
     /*   Debug
           for (int i = 0; i < GridDirectory.DIRSIZE; i++)
             System.out.println("dir[" + i +"] = " + dir[i]);
     */
           GridDirectory gridDir = new GridDirectory(dir);
     
           System.out.println(gridDir);
           dirs.add(gridDir);
        }
      }
    }
    catch (IOException e) 
    {
      status = -1;
      throw new McIDASException("Error reading grid directory:" + e);
    }
    status = 1;
    numDirs++;
  }
  
  /** 
   * returns the directory blocks for the requested grids.  
   * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
   *    McIDAS Programmer's Manual</A> for information on the parameters
   *    for each value.
   *
   * @return a ArrayList of GridDirectorys 
   *
   * @exception AreaFileException if there was a problem
   *      reading the directory
   *
   */
  public ArrayList getDirs() 
    throws McIDASException 
  {
    if (status <= 0 || dirs.size() <= 0) 
    {
      throw new McIDASException(
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
    for (int i = 0; i < dirs.size(); i++)
    {
      sb.append( ((GridDirectory) dirs.get(i)).toString());
      sb.append("\n");
    }
    return sb.toString();
  }

  public static void main(String[] args)
    throws Exception
  {
    if (args.length == 0)
    {
      System.out.println("Must supply a ADDE request to grids");
      System.exit(1);
    }
    GridDirectoryList gdl = new GridDirectoryList(args[0]);
    //System.out.println(gdl.toString());
  }
}
