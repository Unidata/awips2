/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.aviation.climatology;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * This class contains Ceiling/Visibility wind direction data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03 MAR 2008  938        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CigVisByWindDirData implements ICigVisData {

    private int startYear;

    private int endYear;

    private String siteId;

    /**
     * first dimension is visibility, ceiling, joint. second dimension is flight
     * categories. third dimension is wind direction
     */
    private float[][][] dataArray;

    /**
     * WInd directions.
     */
    private String[] winDirsArray = { "  N", "NNE", " NE", "ENE", "  E", "ESE",
            " SE", "SSE", "  S", "SSW", " SW", "WSW", "  W", "WNW", " NW",
            "NNW", "VRB", "  C" };

    private String[] months = { "null", "Jan", "Feb", "Mar", "Apr", "May",
            "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    /**
     * Constructor.
     */
    public CigVisByWindDirData() {
        dataArray = new float[3][4][18];
        startYear = 0;
        endYear = 0;
    }

    /**
     * Get MVFR data array.
     * 
     * @return Array of MVFR data.
     */
    public float[] getMvfrArray(Element element) {
        return dataArray[element.ordinal()][3];
    }

    /**
     * Get IFR data array.
     * 
     * @return Array of IFR data.
     */
    public float[] getIfrArray(Element element) {
        return dataArray[element.ordinal()][2];
    }

    /**
     * Get LIFR data array.
     * 
     * @return Array of LIFR data.
     */
    public float[] getLifrArray(Element element) {
        return dataArray[element.ordinal()][1];
    }

    /**
     * Get VLIFR data array.
     * 
     * @return Array of VLIFR data.
     */
    public float[] getVlifrArray(Element element) {
        return dataArray[element.ordinal()][0];
    }

    public void set(int month, int hour, int windDir, int flightCat, float vis,
            float cig, float jnt) {
        dataArray[0][flightCat][windDir] = vis;
        dataArray[1][flightCat][windDir] = cig;
        dataArray[2][flightCat][windDir] = jnt;
    }

    public void setYears(int startYear, int endYear) {
        this.startYear = startYear;
        this.endYear = endYear;
    }

    public void setSite(String site) {
        siteId = site;
    }

    public int getStartYear() {
        return startYear;
    }

    public int getEndYear() {
        return endYear;
    }

    public String getSite() {
        return siteId;
    }

    private String elementStr(Element element) {
        int ord = element.ordinal();
        if (ord == 0) {
            return "Visibility";
        } else if (ord == 1) {
            return "Ceiling";
        } else {
            return "Flight Category";
        }
    }

    private String monthStr(int startMonth, int endMonth) {
        int numMonths = endMonth - startMonth;

        if (numMonths < 1) {
            numMonths += 12;
        }

        if (numMonths == 1) {
            return months[startMonth];
        } else {
            return String.format("%s-%s", months[startMonth],
                    months[endMonth - 1]);
        }
    }

    private String hourStr(int startHour, int endHour) {
        int numHours = endHour - startHour;

        if (numHours < 1) {
            numHours += 24;
        }

        if (numHours == 1) {
            return Integer.toString(startHour);
        } else {
            return String.format("%02d-%02d", startHour, endHour);
        }
    }

    public void saveStats(String filename, Element element, int startHour,
            int endHour, int startMonth, int endMonth) {
        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf
                    .write(String.format("%s (%d-%d)\n", siteId, startYear,
                            endYear));
            buf.write(String.format("HOUR: %sZ MONTH: %s  %s\n", hourStr(
                    startHour, endHour), monthStr(startHour, endHour),
                    elementStr(element)));
            buf.write("DIR   VLIFR   LIFR    IFR    MVFR   VFR\n");

            float[][] data = dataArray[element.ordinal()];

            for (int i = 0; i < winDirsArray.length; i++) {
                buf.write(String.format("%s%7.2f%8.2f%8.2f%7.2f%7.2f\n",
                        winDirsArray[i], data[0][i], data[1][i], data[2][i],
                        data[3][i],
                        (data[0][i] - data[1][i] - data[2][i] - data[3][i])));
            }

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
