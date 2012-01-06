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
/**
 * 
 */
package com.raytheon.viz.gfe.contours;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * @author wdougherty
 * 
 */
public class FloatArrayReader {

    public static float[] read(String fname, int size) {
        float[] expectedA = new float[size];

        BufferedReader reader = null;
        try {
            File file = new File(fname);
            FileInputStream fileInStream = new FileInputStream(file);
            InputStreamReader ireader = new InputStreamReader(fileInStream);
            reader = new BufferedReader(ireader);

            String[] valStrings;
            int xxx = 0;
            for (String line = reader.readLine(); line != null; line = reader
                    .readLine()) {
                line = line.replaceAll("[{};]", "");
                valStrings = line.split(",\\s?");
                for (String valString : valStrings) {
                    if (!"".equals(valString.trim())) {
                        expectedA[xxx++] = Float.parseFloat(valString);
                    }
                }
            }

        } catch (IOException e) {
            throw new RuntimeException("Error reading array", e);
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                ; // ignore
            }
        }
        return expectedA;
    }

}
