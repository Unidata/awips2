//
// EnhancementTable.java
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

import java.awt.Color;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.File;
import java.net.URL;

/**
 * Class for reading a McIDAS enhancement table (.ET file).  The default
 * constructor creates a grey scale enhancement.
 */
public class EnhancementTable
{
    private int[][] rgbValues = null;
    private DataInputStream dataStream;

    /**
     *  Construct an enhancement table with a grey scale enhancement.
     */
    public EnhancementTable()
    {
        rgbValues = new int[3][256];
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 256; j++) 
            {
                rgbValues[i][j] = j;
            }
        }
    }
        
    /**
     * Construct an enhancement table from a local disk file or
     * URL.
     *
     * @param source     source of the enhancement table (path or URL)
     *
     * @exception  McIDASException  error finding or reading the source.
     */
    public EnhancementTable(String source)
        throws McIDASException
    {
        try
        {
            dataStream = 
                new DataInputStream(
                    new BufferedInputStream(
                        new FileInputStream(source)));
        }
        catch (Exception e)
        {
            try
            {
                dataStream = 
                    new DataInputStream(
                        new BufferedInputStream(
                           new URL(source).openStream()));
            }
            catch (Exception e2)
            {
                throw new McIDASException(
                    "Unable to open enhancement table " + source);
            }
        }
        readRGBValues();
    }

    /**
     * Construct an enhancement table from a file object.
     *
     * @param file     file object representing the enhancement table.
     *
     * @exception  McIDASException  error finding or reading the file.
     */
    public EnhancementTable(File file)
        throws McIDASException
    {
        try
        {
            dataStream = 
                new DataInputStream(
                    new BufferedInputStream(
                        new FileInputStream(file)));
        }
        catch (Exception e)
        {
            throw new McIDASException(
                "Unable to open enhancement table " + file);
        }
        readRGBValues();
    }

    /**
     * Construct an enhancement table from a remote URL object.
     *
     * @param url     URL representing the enhancement table.
     *
     * @exception  McIDASException  error finding or reading the URL.
     */
    public EnhancementTable(URL url)
        throws McIDASException
    {
        try
        {
            dataStream = 
                new DataInputStream(
                    new BufferedInputStream(url.openStream()));
        }
        catch (Exception e)
        {
            throw new McIDASException(
                "Unable to open enhancement table at URL" + url);
        }
        readRGBValues();
    }

    /**
     * read in the values
     */
    private void readRGBValues()
        throws McIDASException
    {
        rgbValues = new int[3][256];
        try
        {
            int reservedWord = dataStream.readInt();
            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < 256; j++) 
                    rgbValues[i][j] = dataStream.readInt();
            }
        }
        catch (Exception e)
        {
            throw new McIDASException("Invalid enhancement table");
        }
    }

    /**
     * Retrieve the data values.  
     *
     * @return  integer array [3][256] of red, green, blue values or null if
     *          the table was not initialized correctly.  Values
     *          range from 0-255.
     */
    public int[][] getRGBValues()
    {
        return rgbValues;
    }

    /**
     * Look up a unique (hopefully) RGB value and return the index
     *
     * @return index value (0-255) or -1 if not found
     *
     */

     public int getIndex(int red, int green, int blue) {
       int inx;
       for (inx=0; inx<256; inx++) {
         if (rgbValues[0][inx] == red &&
             rgbValues[1][inx] == green &&
             rgbValues[2][inx] == blue) {

           return inx;
         }

       }
       return -1;
     }

    /**
     * Print out a pretty table.  Currently lists all values, but will
     * eventually print a format like EU TABLE.
     *
     * @return  table of levels, red, green and blue values
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append(" Brightness  Red       Green      Blue ");
        sb.append("\n");
        sb.append("  min max   min max   min max   min max");
        sb.append("\n");
        sb.append("  --- ---   --- ---   --- ---   --- ---");
        sb.append("\n");
        for (int i = 0; i < 256; i++)
        {
            sb.append(" " + i + "  " + 
                      rgbValues[0][i] + " " + 
                      rgbValues[1][i] + " " +
                      rgbValues[2][i]);
            sb.append("\n");
        }
        return sb.toString();
    }

    /**  
     * Test by running:
     * <UL>
     * <LI>java edu.wisc.ssec.mcidas.EnhancementTable   _OR_
     * <LI>java edu.wisc.ssec.mcidas.EnhancementTable <i>enhancement_file</i>.
     * </UL>
     */
    public static void main(String[] args)
    {
        try
        {
            EnhancementTable et = 
                args.length == 0
                    ? new EnhancementTable()
                    : new EnhancementTable(args[0]);
            System.out.println(et.toString());
        }
        catch (McIDASException e)
        {
            System.out.println(e.toString());
        }
    }
}
                

        
