//
// AddePointDataReader.java
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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Vector;
import edu.wisc.ssec.mcidas.McIDASUtil;

/** 
 * AddePointDataReader interface for McIDAS ADDE point data sets.   Simulates a
 * McIDAS PTLIST output using an ADDE URL if <code>toString() method is used.
 * 
 * Note that units are ignored by this client, default units are used.
 *
 * <pre>
 * URLs must all have the following format   
 *   adde://host/point?keyword_1=value_1&amp;keyword_2=value_2
 *
 * there can be any valid combination of the following supported keywords:
 *
 *   group=&lt;groupname&gt;         ADDE group name
 *   descr=&lt;descriptor&gt;        ADDE descriptor name
 *   pos=&lt;position&gt;            request an absolute or relative ADDE 
 *                               position number
 *   select=&lt;select clause&gt;    to specify which data is required
 *   param=&lt;param list&gt;        what parameters to return
 *   num=&lt;max&gt;                 maximum number of obs to return
 *   user=&lt;user_id&gt;            ADDE user identification
 *   proj=&lt;proj #&gt;             a valid ADDE project number
 *   trace=&lt;0/1&gt;               setting to 1 tells server to write debug 
 *                               trace file (imagedata, imagedirectory)
 *   version=1                 ADDE version number, currently 1 
 *   
 * the following keywords are required:
 *
 *   group
 *   descr
 *
 * an example URL might look like:
 *   adde://rtds/point?group=neons&amp;type=metar
 * </pre>
 *
 * @author Don Murray - Unidata and James Kelly - BoM
 * 
 */
public class AddePointDataReader {

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

    /**
     * Key for getting data in param order [numParams][numObs]
     */
    public static final int PARAM_ORDER = 0;

    /**
     * Key for getting data in obs order [numObs][numParams]
     */
    public static final int OB_ORDER = 1;

    /**
     * Maximum number of parameters - used as a sanity check
     */
    public static final int MAXNUMPARM = 402;  // number of parameters

    private int status=0;                      // read status
    //private URLConnection urlc;                // URL connection
    private String[] params;       // parameters returned from server
    private int[] ScalingFactors;  // scaling factors returned from server
    private String[] units;        // units returned from server
    private int[][] iData = null;  // data returned from server as array of ints
    private int[][] oData = null;  // data returned from server as array of ints
    private Vector data = null;    // holds the obs
    private int numParams = 0;     // number of parameters
    private boolean debug = false; // set to true for debugging
    
    /**
     * creates an AddePointDataReader object that allows reading ADDE point
     * datasets.
     *
     * @param request ADDE URL to read from.  See class javadoc.
     *
     * <pre>
     * an example URL might look like:
     *   adde://rtds.ho.bom.gov.au/point?group=neons&amp;descr=metar
     * </pre>
     *
     * @exception AddeException if there are no datasets of the particular
     *            type or there is an error reading data
     *
     */
    public AddePointDataReader(String request)
        throws AddeException
    {

        DataInputStream dataInputStream;
        URLConnection urlc;
        try 
        {
            URL url = new URL(request);
            urlc = url.openConnection();
            //InputStream is = urlc.getInputStream();
            dataInputStream = 
                new DataInputStream(
                    new BufferedInputStream(
                        urlc.getInputStream()));
        }
        catch (AddeURLException ae) 
        {
            throw new AddeException("No datasets found " + ae);
        }
        catch (Exception e) 
        {
            throw new AddeException("Error opening connection: " + e);
        }
        //
        //  first get number of bytes for Parameter Names
        //
        int numParamBytes;
        if(urlc instanceof AddeURLConnection) {
          numParamBytes = ((AddeURLConnection) urlc).getInitialRecordSize();
        } else {
           try {
             numParamBytes = dataInputStream.readInt();
           }    catch (IOException e) {
            throw new AddeException("Error reading data: " + e);
           }
        }
        if (debug) System.out.println("numParamBytes = " + numParamBytes);
        if (numParamBytes == 0)
        {
            status = -1;
            throw new AddeException("No data found");
        } else if (numParamBytes/4 > MAXNUMPARM)
        {
            status = -1;
            throw new AddeException("Not an ADDE Point Data set");
        }
        else
        {
            byte[] bParamNames = new byte[numParamBytes];
            numParams = numParamBytes/4;
            params = new String[numParams];
            try
            {
                //
                //  read Parameter names into paramNames
                //
                dataInputStream.readFully(bParamNames, 0, numParamBytes);
                String sParamNames = new String(bParamNames);
                if (debug) System.out.println(" sParamNames = " + sParamNames);
                for (int i = 0; i < numParams; i++)
                    params[i] = sParamNames.substring(i*4, (i+1)*4).trim();

            }
            catch (IOException e) 
            {
                status = -1;
                throw new AddeException("Error reading parameters:" + e);
            }
        } 
        //
        //  next get number of bytes for Unit Names
        //
        try
        {
            int numUnitBytes = dataInputStream.readInt();
            units = new String[numUnitBytes/4];
            if (debug) System.out.println("numUnitBytes = " + numUnitBytes);
            byte[] bUnitNames = new byte[numUnitBytes];
            dataInputStream.readFully(bUnitNames, 0, numUnitBytes);
            String sUnitNames = new String(bUnitNames);
            if (debug) System.out.println("sUnitNames = " + sUnitNames);
            for (int i = 0; i < numUnitBytes/4; i++)
                units[i] = sUnitNames.substring(i*4, (i+1)*4).trim();
        }
        catch (IOException e) 
        {
            status = -1;
            throw new AddeException("Error reading units:" + e);
        }
        //
        //  next get number of bytes for Scaling Factors
        //
        try
        {
            int numScalingBytes = dataInputStream.readInt();
            if (debug) 
                System.out.println("numScalingBytes = " + numScalingBytes);
            ScalingFactors = new int[(int)(numScalingBytes/4)];
            for (int i=0; i < (int) (numScalingBytes/4); i++) {
                ScalingFactors[i] = dataInputStream.readInt();
            }
        }
        catch (IOException e) 
        {
            status = -1;
            throw new AddeException("Error reading scaling factors:" + e);
        }
        //
        //  next get number of bytes for the actual data
        //
        data = new Vector();
        byte[] bThisUnitName = new byte[4];
        try
        {
            int numDataBytes = dataInputStream.readInt();
            while (numDataBytes !=0) {
                if (debug) System.out.println(" i, Param, Unit, Value " );
                int[] dataArray = new int[numParams];
                for (int i=0; i < (int) (numDataBytes/4); i++) {
                     dataArray[i] = dataInputStream.readInt();
                }
                data.addElement(dataArray);
                numDataBytes = dataInputStream.readInt();
                if (debug) System.out.println("numDataBytes = " + numDataBytes);
            }
        }
        catch (IOException e) 
        {
            status = -1;
            throw new AddeException("Error reading data:" + e);
        }
    }

    /**
     * Return the data sent by the server
     *
     * @return  array of the data.  Data is in the format of an integer array
     *          of unscaled integers as returned from the server in parameter
     *          order ([numParams][numObs]).
     *
     * @exception AddeException if there was an error reading data
     */
    public int[][] getData()
        throws AddeException
    {
       return getData(PARAM_ORDER);
    }

    /**
     * Return the data sent by the server in a particular order
     * (PARAM_ORDER, OB_ORDER). 
     * @param   order   order of the data.  (PARAM_ORDER, OB_ORDER)
     *
     * @return  array of the data.  Data is in the format of an integer array
     *          of unscaled integers in the specified order
     *          (PARAM_ORDER = [numParams][numObs],
     *           OB_ORDER = [numObs][numParams])
     *
     * @exception AddeException if there was an error reading data
     */
    public int[][] getData(int order)
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");

        return (order == PARAM_ORDER)
                    ? getParamOrderData()
                    : getObOrderData();
    }

    /**
     * Get the list of parameters
     *
     * @return  array of the parameter names.  The names will be in the same
     *          order as the array of data values in the <code>getData()</code>
     *          method.
     *
     * @exception AddeException if there was an error reading data
     */
    public String[] getParams()
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");
        return params;
    }

    /**
     * Get the list of units
     *
     * @return  array of the unit names.  The names will be in the same
     *          order as the array of data values in the <code>getData()</code>
     *          method.
     *
     * @exception AddeException if there was an error reading data
     */
    public String[] getUnits()
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");
        return units;
    }

    /**
     * Get the list of scaling factors
     *
     * @return  array of the scaling factors (powers of 10).  The scaling 
     *          factors will be in the same order as the array of data 
     *          values in the <code>getData()</code> method.
     *
     * @exception AddeException if there was an error reading data
     */
    public int[] getScales()
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");
        return ScalingFactors;
    }

    /**
     * return the number of parameters
     *
     * @return  number of parameters returned from the server
     */
    public int getNumParams()
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");
        return numParams;
    }

    /**
     * Return an array of data for the particular parameter. 
     *
     * @return array of values for the particular parameter or
     *         null if an invalid parameter was entered. This
     *         will return String[] for parameters with units of CHAR,
     *         float[1][] for an array of non scaled data, and double[1][] for
     */
    public Object[] getData(String parameter)
        throws AddeException
    {
        if (status < 0)
            throw new AddeException("No data available");
        for (int i = 0; i < numParams; i++)
        {
            if (parameter.equalsIgnoreCase(params[i]))
            {
                iData = getParamOrderData();
                if (units[i].equalsIgnoreCase("CHAR"))
                {
                    String[] vals = new String[iData[i].length];
                    for (int j = 0; j < iData[i].length; j++)
                    {
                        vals[j] = McIDASUtil.intBitsToString(iData[i][j]);
                    }
                    return vals;
                }
                else 
                if (ScalingFactors[i] != 0)
                {
                    double[][] vals = new double[1][iData[i].length];
                    for (int j = 0; j < iData[i].length; j++)
                    {
                        vals[0][j] = iData[i][j]/Math.pow(10.0, 
                                                 (double) ScalingFactors[i]);
                    }
                    return vals;
                }
                else
                {
                    float[][] vals = new float[1][iData[i].length];
                    for (int j = 0; j < iData[i].length; j++)
                    {
                        vals[0][j] = (float) iData[i][j];
                    }
                    return vals;
                }
            }
        }
        return null;
    }

    /**
     * Create an array of data in parameter order [numParams][numobs].
     * @return array of data in parameter order
     */
    private int[][] getParamOrderData() {
                
      if (iData == null) {
        // Convert to in array
        iData = new int[numParams][data.size()];
        if (debug) {
          System.out.println("number of data records = " + data.size());
        }
        for (int i = 0; i < data.size(); i++)
        {
            int[] values = (int[]) data.get(i);
            for (int j = 0; j < numParams; j++) iData[j][i] = values[j];
        }
      }
      return iData;
    }

    /**
     * Create an array of data in observation order [numObs][numParams].
     * @return array of data in obs order
     */
    private int[][] getObOrderData() {
                
      if (oData == null) {
        // Convert to in array
        oData = new int[data.size()][numParams];
        if (debug) {
          System.out.println("number of data records = " + data.size());
        }
        for (int i = 0; i < data.size(); i++)
        {
            oData[i] = (int[]) data.get(i);
        }
      }
      return oData;
    }

    /**
     * Return a formated string of the returned data
     *
     * @return  formatted representation of the data ala McIDAS PTLIST command.
     */ 
    public String toString()
    {
        if (status < 0)
            return new String("No data Available");

        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < numParams; i++)
        {
            buf.append(params[i]);
            buf.append("[");
            buf.append(units[i]);
            buf.append("] ");
            buf.append("\t");
        }
        buf.append("\n");
        iData = getParamOrderData();
        for (int i = 0; i < iData[0].length; i++)
        {
            for (int j = 0; j < numParams; j++)
            {
                if (units[j].equalsIgnoreCase("CHAR"))
                {
                     buf.append(McIDASUtil.intBitsToString(iData[j][i]));
                }
                else 
                if (ScalingFactors[j] != 0)
                {
                     buf.append( iData[j][i] == McIDASUtil.MCMISSING
                                   ? "     "
                                   : Double.toString(
                                       iData[j][i]/Math.pow(10.0, 
                                              (double) ScalingFactors[j] )));
                }
                else
                {
                     buf.append( iData[j][i] == McIDASUtil.MCMISSING
                                   ? "     "
                                   : Integer.toString(iData[j][i]));
                }
                buf.append("\t");
            }
            buf.append("\n");
        }
        return buf.toString();
    }

    /** test by running 'java edu.wisc.ssec.mcidas.adde.AddePointDataReader' */
    public static void main (String[] args)
        throws Exception
    {
        System.out.println("\nData Requested:");

        String request = 
            (args.length == 0)
            // ? "adde://servb.ho.bom.gov.au/point?group=neons&descr=metar&num=2&select='id ymml; time 22 24; day 1999317'&parm=id dir spd t[c] td[c] psl&pos=ALL&version=1"
            // Unidata server
            ? "adde://adde.ucar.edu/point?group=rtptsrc&descr=sfchourly&num=2&select='id ypph'&parm=id dir spd t[c] td[c] psl&pos=0&version=1&trace=1"
            : args[0];
        AddePointDataReader ptlist = new AddePointDataReader(request);
        System.out.println(ptlist.toString());
    }
}
