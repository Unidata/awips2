//
// ReadWxText.java
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

package edu.wisc.ssec.mcidas.adde;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.lang.*;
import java.util.*;
import edu.wisc.ssec.mcidas.McIDASUtil;

/** 
 * Read text from an ADDE server interface for McIDAS ADDE data sets.  
 * This class handles file, weather text and obs text requests.
 * <P>
 * <pre>
 * For File Reading:
 *   URLs must all have the following format   
 *     adde://host/text?file=filename.ext
 *
 *   there can be any valid combination of the following supported keywords:
 *
 *     file - name of text file on ADDE server
 *
 *   the following keywords are required:
 *
 *     file
 *
 *   an example URL might look like:
 *     adde://viper/text?file=filename.ext
 *
 * For Weather Text Reading:
 *   URLs must all have the following format   
 *     adde://host/wxtext?group=group&amp;key1=value1&amp;key2=val2....&amp;keyn=valn
 *
 *   there can be any valid combination of the following supported keywords:
 *
 *     group=&lt;group&gt;         weather text group (default= RTWXTEXT)
 *     prod=&lt;product&gt;        predefind product name
 *     apro=&lt;val1 .. valn&gt;   AFOS/AWIPS product headers to match (don't 
 *                           use with wmo keyword
 *     astn=&lt;val1 .. valn&gt;   AFOS/AWIPS stations to match
 *     wmo= &lt;val1 .. valn&gt;   WMO product headers to match (don't 
 *                           use with apro keyword
 *     wstn=&lt;val1 .. valn&gt;   WMO stations to match
 *     day=&lt;start end&gt;       range of days to search
 *     dtime=&lt;numhours&gt;      maximum number of hours to search back (def=96)
 *     match=&lt;match strings&gt; list of character match strings to find from text
 *     num=&lt;num&gt;             number of matches to find (def=1)
 *
 *   the following keywords are required:
 *
 *     day  (bug causes it not to default to current day)
 *     one of the selection criteria
 *
 *   an example URL might look like:
 *     adde://adde.ucar.edu/wxtext?group=rtwxtext&amp;prod=zone_fcst&amp;astn=bou
 *
 * For Observational Text Reading:
 *   URLs must all have the following format   
 *     adde://host/obtext?group=group&amp;descr=descr&amp;key1=value1....&amp;keyn=valn
 *
 *   there can be any valid combination of the following supported keywords:
 *
 *     group=&lt;group&gt;         weather text group (default= RTWXTEXT)
 *     descr=&lt;descriptor&gt;    weather text subgroup (default=SFCHOURLY)
 *     id=&lt;id1 id2 ... idn&gt;  list of station ids
 *     co=&lt;co1 co2 ... con&gt;  list of countries
 *     reg=&lt;reg1 reg2..regn&gt; list of regions
 *     newest=&lt;day hour&gt;     most recent time to allow in request 
 *                           (def=current time)
 *     oldest=&lt;day hour&gt;     oldest observation time to allow in request
 *     type=&lt;type&gt;           numeric value for the type of ob
 *     nhours=&lt;numhours&gt;     maximum number of hours to search
 *     num=&lt;num&gt;             number of matches to find (def=1)
 *
 *   the following keywords are required:
 *
 *     group
 *     descr
 *     id, co, or reg
 *
 *   an example URL might look like:
 *     adde://adde.ucar.edu/obtext?group=rtwxtext&amp;descr=sfchourly&amp;id=kden&amp;num=2
 *
 * </pre>
 *
 * @author Tom Whittaker/Don Murray
 * 
 */
public class AddeTextReader {

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

  private int status=0;                 // read status
  private String statusString = "OK";
  private boolean debug = false;        // debug
  private Vector linesOfText = null;
  private URLConnection urlc;           
  private DataInputStream dis;
  private final int HEARTBEAT = 11223344;
  private List<WxTextProduct> wxTextProds = new ArrayList<WxTextProduct>();
    
  /**
   * Creates an AddeTextReader object that allows reading an ADDE
   * text file or weather text
   *
   * @param request ADDE URL to read from.  See class javadoc.
   */
  public AddeTextReader(String request) {
   
    try {
      URL url = new URL(request);
      debug = request.indexOf("debug=true") > 0;
      if (debug) System.out.println("Request: "+request);
      urlc = url.openConnection();
      InputStream is = urlc.getInputStream();
      dis = new DataInputStream(is);
    }
    catch (AddeURLException ae) 
    {
      status = -1;
      statusString = "No data found";
      String aes = ae.toString();
      if (aes.indexOf(" Accounting ") != -1) {
        statusString = "No accounting data";
        status = -3;
      }
      if (debug) System.out.println("AddeTextReader Exception:"+aes);
    }
    catch (Exception e) 
    {
      status = -2;
      if (debug) System.out.println("AddeTextReader Exception:"+e);
      statusString = "Error opening connection: "+e;
    }
    linesOfText = new Vector();
    if (status == 0) readText(((AddeURLConnection) urlc).getRequestType());
    if (linesOfText.size() < 1) statusString = "No data read";
    status = linesOfText.size();
  }

  private void readText(int reqType) {

    switch(reqType) {
      case AddeURLConnection.TXTG:
        readTextFile();
        break;
      case AddeURLConnection.WTXG:
        readWxText();
        break;
      case AddeURLConnection.OBTG:
        readObText();
        break;
    }

  }

  private void readTextFile() {
    int numBytes;

    try {
      numBytes = ((AddeURLConnection) urlc).getInitialRecordSize();
      if (debug) 
       System.out.println("ReadTextFile: initial numBytes = " + numBytes);

      numBytes = dis.readInt();
      while ((numBytes = dis.readInt()) != 0) {

        if (debug) 
           System.out.println("ReadTextFile: numBytes = " + numBytes);

        byte[] data = new byte[numBytes];
        dis.readFully(data,0,numBytes);
        String s = new String(data);
        if (debug) System.out.println(s);
        linesOfText.addElement(s);
      } 

    } catch (Exception iox) {
      statusString = " "+iox;
    }
  }
    
  private void readWxText() {
    int numBytes;

    /*
      From the McIDAS programmer's ref:

       The server sends the client the following information:

          o  4-byte value containing the length of the client request 
             string; this lets users know how their request was expanded 
             when the PROD keyword is specified the expanded client 
             request string 
          o  heartbeat value if needed 
          o  4-byte value containing the total number of bytes of 
             data for this text block, including the 64-byte header and
             the text 64-byte text header 
          o  n bytes of 80-character text, blank padded 

       The last three pieces of information are repeated until no more 
       data is found.

   */

   try {
      numBytes = ((AddeURLConnection) urlc).getInitialRecordSize();
      if (debug) 
          System.out.println("ReadWxText: initial numBytes = " + numBytes);

      // Read in the expanded client request string
      byte[] expandedRequest = new byte[numBytes];
      dis.readFully(expandedRequest,0,numBytes);
      String s = new String(expandedRequest);
      if (debug) 
          System.out.println("Server interpreted request as:\n " + s);

      // look for heartbeat
      while ((numBytes = dis.readInt()) == 4) {
        int check = dis.readInt();
        if (check != HEARTBEAT) {  // must be number of bytes to read
            numBytes = check;
            break;
        }
      }

      // Now we go read the data
      if (debug) System.out.println("numBytes for text = "+numBytes);

      wxTextProds = new ArrayList<WxTextProduct>();

      while (numBytes != 0) {
        // read in the header
        byte[] header = new byte[64];
        dis.readFully(header,0,64);
        WxTextProduct wtp = new WxTextProduct(header);
        String head = new String(header);
        // note this is not true text so prints as garbage
        //if (debug) System.out.println(decodeWxTextHeader(header));
        if (debug) System.out.println(wtp);


        // read in the text in 80 byte chunks
        byte[] text = new byte[numBytes-64];
        dis.readFully(text,0,numBytes-64);
        int nLines = text.length/80;
        if (debug) System.out.println("nLines = " + nLines);
        StringBuilder wxText = new StringBuilder();
        for (int i = 0; i < nLines; i++)
        {
          String line = new String(text, i*80, 80);
          linesOfText.add(line);
          wxText.append(line);
          wxText.append("\n");
        }
        wtp.setText(wxText.toString());
        wxTextProds.add(wtp);
        // read in next length, but check for heartbeat
        while ((numBytes = dis.readInt()) == 4) {
            int check = dis.readInt();
            if (check != HEARTBEAT) {  // must be number of bytes to read
               numBytes = check;
               break;
           }
        }

      }

    } catch (Exception iox) {
      statusString = " "+iox;
    }
  }

  private void readObText() {
    int numBytes;
    /*
      From the McIDAS programmer's ref:

      Once the data is formatted, the text data can be sent 
      to the client. The server sends the client the following 
      information:

        o   4-byte value containing the length of the text header, 
            which is currently 96; when all the data is sent, this 
            value is reset to zero 
        o   96-byte text header 
        o   4-byte value containing the number of bytes of text to be sent 
        o   n bytes of text; 80 characters per line, blank padded 

      This information should be repeated until no more data is found.
    */

    try {
      numBytes = ((AddeURLConnection) urlc).getInitialRecordSize();
      if (debug) 
        System.out.println("ReadObText: initial numBytes = " + numBytes);

      while( numBytes != 0) {
        // Read in the header
        byte[] header = new byte[numBytes];
        dis.readFully(header,0,numBytes);
        String s = new String(header);
        if (debug) System.out.println(decodeObsHeader(header));

        numBytes = dis.readInt();
        // Now we go read the data
        if (debug) System.out.println("numBytes for text = "+numBytes);

        // read in the data
        byte[] text = new byte[numBytes];
        dis.readFully(text,0,numBytes);
        int nLines = text.length/80;
        if (debug) System.out.println("nLines = " + nLines);
        for (int i = 0; i < nLines; i++)
        {
          String line = new String(text, i*80, 80);
          linesOfText.add(line);
        }
        numBytes = dis.readInt();
      }

    } catch (Exception iox) {
      statusString = " "+iox;
    }

  }

  /**
   * Get a string representation of the status code
   * @return human readable status
   */
  public String getStatus() {
    return statusString;
  }

  /**
   * Return the status code of the read
   * @return status code (>0 == read okay);
   */
  public int getStatusCode() {
      return status;
  }

  /**
   * Return the number of lines of text that were read.
   * @return number of lines.
   */
  public int getNumLines() {
    return linesOfText.size();
  }

  /**
   * Return the text read from the server.  If there was a problem
   * the error message is returned.
   * @return text from server or error message
   */
  public String getText() {
    StringBuffer buf = new StringBuffer();
    if (getStatusCode() <= 0) {
        buf.append(getStatus());
    } else {
        for (Iterator iter = linesOfText.iterator(); iter.hasNext();) {
           buf.append((String) iter.next());
           buf.append("\n");
        }
    }
    return buf.toString();
  }

  public Vector getLinesOfText() {
    Vector v = new Vector();
    if (getStatusCode() <= 0) {
        v.add(getStatus());
    } else {
        v.addAll(linesOfText);
    }
    return v;
  }

  public List<WxTextProduct> getWxTextProducts() {
    List<WxTextProduct> retList = new ArrayList<WxTextProduct>();
    retList.addAll(wxTextProds);
    return retList;
  }
      

  /** test by running 'java edu.wisc.ssec.mcidas.adde.AddeTextReader' */
  public static void main (String[] args)
      throws Exception
  {
      String request = (args.length == 0)
        ? "adde://adde.ucar.edu/text?file=PUBLIC.SRV"
        : args[0];

      AddeTextReader atr = new AddeTextReader(request);
      String status = atr.getStatus();
      System.out.println("\n" + atr.getText());
  }

  private String decodeObsHeader(byte[] header) {
      StringBuffer buf = new StringBuffer();
      int[] values = McIDASUtil.bytesToIntegerArray(header,0,13);
      buf.append("Ver = ");
      buf.append(values[0]);
      buf.append(" ObType = ");
      buf.append(values[1]);
      buf.append(" ActFlag = ");
      buf.append(values[2]);
      buf.append(" IDType = ");
      buf.append(values[9]);
      buf.append("\nStarting at ");
      buf.append(values[3]);
      buf.append(" ");
      buf.append(values[4]);
      buf.append(" ");
      buf.append(values[5]);
      buf.append("\nEnding   at ");
      buf.append(values[6]);
      buf.append(" ");
      buf.append(values[7]);
      buf.append(" ");
      buf.append(values[8]);
      return buf.toString();
  }

  private String decodeWxTextHeader(byte[] header) {
      StringBuffer buf = new StringBuffer();
      int[] values = McIDASUtil.bytesToIntegerArray(header,0,13);
      buf.append("SOU    nb  location   day    time  WMO     WSTN APRO ASTN\n");
      buf.append("---  ----  -------- ------- ------ ------- ---- ---- ----\n"); 
      buf.append(McIDASUtil.intBitsToString(values[0]));
      buf.append(values[1]);
      buf.append(" ");
      buf.append(values[2]);
      buf.append(" ");
      buf.append(values[10]);
      buf.append(" ");
      buf.append(values[3]);
      buf.append(" ");
      buf.append(McIDASUtil.intBitsToString(values[4]));
      buf.append(values[5]);
      buf.append("  ");
      buf.append(McIDASUtil.intBitsToString(values[6]));
      buf.append(" ");
      buf.append(McIDASUtil.intBitsToString(values[7]));
      buf.append(" ");
      buf.append(McIDASUtil.intBitsToString(values[8]));
      return buf.toString();
  }

}
