//
// DataSetInfo.java
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Hashtable;
import java.util.Arrays;
import java.util.Enumeration;

/** 
 * DataSetInfo interface for McIDAS ADDE data sets.   Simulates a
 * McIDAS DSINFO request using an ADDE URL.
 *
 * <pre>
 * URLs must all have the following format   
 *   adde://host/datasetinfo?keyword_1=value_1&keyword_2=value_2
 *
 * there can be any valid combination of the following supported keywords:
 *
 *   group - ADDE group name   
 *   type  - ADDE data type.  Must be one of the following:
 *               image, point, grid, text, nav
 *           the default is the image type.
 *
 * the following keywords are required:
 *
 *   group
 *
 * an example URL might look like:
 *   adde://viper/datasetinfo?group=gvar&type=image
 * </pre>
 *
 * @author Don Murray
 * 
 */
public class DataSetInfo 
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

    private int status=0;                 // read status
    private char[] data;                  // data returned from server
    private Hashtable descriptorTable;
    private boolean debug = false;        // debug
    
    /**
     * creates a DataSetInfo object that allows reading
     *
     * @param request ADDE URL to read from.  See class javadoc.
     *
     * <pre>
     * an example URL might look like:
     *   adde://viper/datasetinfo?group=gvar&type=image
     * </pre>
     *
     * @exception AddeURLException if there are no datasets of the particular
     *            type or there is an error reading data
     *
     */
    public DataSetInfo(String request)
        throws AddeURLException
    {
   
        URLConnection urlc;           
        BufferedReader reader;
        debug = debug || request.indexOf("debug=true") >= 0;
        try 
        {
            URL url = new URL(request);
            urlc = url.openConnection();
            reader = 
                new BufferedReader(
                    new InputStreamReader(
                        urlc.getInputStream()));
        }
        catch (AddeURLException ae) 
        {
            status = -1;
            throw new AddeURLException("No datasets found");
        }
        catch (Exception e) 
        {
            status = -1;
            throw new AddeURLException("Error opening connection: " + e);
        }
        int numBytes = ((AddeURLConnection) urlc).getInitialRecordSize();
        if (debug) System.out.println("DataSetInfo: numBytes = " + numBytes);
        if (numBytes == 0)
        {
            status = -1;
            throw new AddeURLException("No datasets found");
        }
        else
        {
            data = new char[numBytes];
            try
            {
                int start = 0;
                while (start < numBytes)
                {
                    int numRead = 
                        reader.read(data, start, (numBytes - start));
                    if (debug) System.out.println("bytes read = " + numRead);
                    start += numRead;
                }
            }
            catch (IOException e) 
            {
                status = -1;
                throw new AddeURLException("Error reading dataset info:" + e);
            }
            int numNames = data.length/80;
            descriptorTable = new Hashtable(numNames);
            if (debug) 
                System.out.println("Number of descriptors = " + numNames);
            for (int i = 0; i < numNames; i++)
            {
                String temp = new String(data, i*80, 80);
                if (debug) System.out.println("Parsing: >"+temp+"<");
                if (temp.trim().equals("")) continue;
                String descriptor = temp.substring(0,12).trim();
                if (debug) System.out.println("Descriptor = " + descriptor);
                String comment = descriptor;
                int pos = temp.indexOf('"');
                if (debug) System.out.println("Found quote at " + pos);
                if (pos >= 23)   
                {
                    comment = temp.substring(pos + 1).trim();
                    if (comment.equals("")) comment = descriptor;
                }
                if (debug) System.out.println("Comment = " + comment);
                descriptorTable.put(comment, descriptor);
            }
        } 
    }

    /**
     * Return the data sent by the server
     *
     * @return  array of the data.  Data is in the format of a McIDAS DSINFO
     *          (LWPR) request.
     *
     * @exception AddeURLException if there was an error reading data
     */
    public char[] getData()
        throws AddeURLException
    {
        if (status < 0)
            throw new AddeURLException("No data available");

        return data;
    }

    /**
     * Return a hashtable of descriptive names and ADDE dataset descriptors
     * Descriptive names are the keys.  If there is no descriptive name,
     * the dataset descriptor is used.
     *
     * @return hashtable of names/desciptors
     *
     * @exception AddeURLException if there was an error reading data
     */
    public Hashtable getDescriptionTable()
        throws AddeURLException
    {
        if (status < 0)
            throw new AddeURLException("No data available");
        return descriptorTable;
    }

    /**
     * Return a sorted list of the dataset descriptors
     *
     * @return sorted list 
     *
     * @exception AddeURLException if there was an error reading data
     */
    public String[] getDescriptors()
        throws AddeURLException
    {
        if (status < 0)
            throw new AddeURLException("No data available");
        String[] list = new String[descriptorTable.size()];
        Enumeration elements = descriptorTable.elements();
        int i = 0;
        while (elements.hasMoreElements())
        {
            list[i] = (String) elements.nextElement();
            i++;
        }
        Arrays.sort(list);
        return list;
    }

    /**
     * Return a formated string of the returned data
     *
     * @return  formatted string representing the data.
     */ 
    public String toString()
    {
        if (status < 0)
            return new String("No data Available");
        String header = new String(
            "Name         NumPos   Content\n" +
            "------------ ------   --------------------------------------\n");
        StringBuffer buf = new StringBuffer(header);
        for (int i = 0; i < data.length/80; i++)
        {
            StringBuffer sb = 
                new StringBuffer("                                          ");
            String line = new String(data, i*80, 80);
            sb.insert(0,line.substring(0,12));
            int brange = 0;
            int erange = 0;
            try {
                brange = Integer.parseInt(line.substring(12,18).trim());
            } catch (NumberFormatException ne) { brange = 0; }
            try {
                erange = Integer.parseInt(line.substring(18,23).trim());
            } catch (NumberFormatException ne) { erange = 0; }
            if (erange == 0) erange = brange;
            int numPos = (erange-brange) + 1;
            int insertPos = 17;
            if (numPos >= 10 && numPos < 100)
                insertPos = 16;
            else if (numPos >= 100 && numPos < 1000)
                insertPos = 15;
            else if (numPos > 1000)
                insertPos = 14;
            sb.insert(insertPos,String.valueOf(numPos));
            int pos = line.indexOf('"');
            if (pos >= 23) sb.insert(22, line.substring(pos+1));
            buf.append(sb.toString().trim());
            buf.append("\n");
        }
        return buf.toString();
    }

    /** test by running 'java edu.wisc.ssec.mcidas.adde.DataSetInfo' */
    public static void main (String[] args)
        throws Exception
    {
        System.out.println("\nDataset Names:\n");

        String request = (args.length == 0)
          ? "adde://adde.unidata.ucar.edu/datasetinfo?group=blizzard&type=image"
          : args[0];
        DataSetInfo dsinfo = new DataSetInfo(request);
        System.out.println(dsinfo.toString());
        String[] descriptors = dsinfo.getDescriptors();
        System.out.println("Sorted list of Descriptors:");
        for (int i = 0; i < descriptors.length; i++)
            System.out.println(descriptors[i]);

    }
}
