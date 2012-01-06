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
 * This class contains Ceiling/Visibility hour data.
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
public class CigVisByHourData implements ICigVisData {

    private int startYear;

    private int endYear;

    private String siteId;

    /**
     * first dimension is visibility, ceiling, joint second dimension is flight
     * categories third dimension is hours
     */
    private float[][][] dataArray;

    /**
     * Constructor.
     */
    public CigVisByHourData() {
        dataArray = new float[3][4][24];
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
        dataArray[0][flightCat][hour] = vis;
        dataArray[1][flightCat][hour] = cig;
        dataArray[2][flightCat][hour] = jnt;
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

    public void saveStats(String filename, Element element, int startMonth,
            int endMonth) {
        int numMonths = endMonth - startMonth;

        if (numMonths < 1) {
            numMonths += 12;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf
                    .write(String.format("%s (%d-%d)\n", siteId, startYear,
                            endYear));

            for (int i = 0; i < numMonths; i++) {
                int month = startMonth + i;

                if (month >= 12) {
                    month -= 12;
                }

                buf.write(String.format("MONTH: %s        %s\n",
                        months[month + 1], elementStr(element)));
                buf.write("DIR   VLIFR   LIFR    IFR    MVFR   VFR\n");

                float[][] data = dataArray[element.ordinal()];

                for (int j = 0; j < 24; j++) {
                    buf.write(String
                            .format("%02dZ%7.2f%8.2f%8.2f%7.2f%7.2f\n", j,
                                    data[0][j], data[1][j], data[2][j],
                                    data[3][j], (data[0][j] - data[1][j]
                                            - data[2][j] - data[3][j])));
                }

                buf.write("\n");
            }

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
