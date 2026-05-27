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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class RadarInfoDict implements Iterable<RadarInfo> {
    public static final String RADAR_INFO_FILE = "radarInfo.txt";

    private static RadarInfoDict instance = null;

    private Map<Integer, RadarInfo> dict;

    private RadarInfoDict(String dir) {
        dict = new LinkedHashMap<Integer, RadarInfo>();

        File file = new File(dir + File.separator + RADAR_INFO_FILE);

        try {
            BufferedReader in = new BufferedReader(new FileReader(file));

            String s = in.readLine();
            while (s != null) {
                if ((s.length() > 0) && (s.charAt(0) != '#')) {
                    RadarInfo r = new RadarInfo(s);
                    if (r != null) {
                        dict.put(r.getProductCode(), r);
                    }
                }
                s = in.readLine();
            }
            in.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            System.out.println(file.getAbsolutePath());
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Returns the RadarInfo for the specified productCode
     * 
     * @param productCode
     * @return
     */
    public RadarInfo getInfo(int productCode) {
        return dict.get(productCode);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<RadarInfo> iterator() {
        return Collections.unmodifiableCollection(dict.values()).iterator();
    }

    public static synchronized RadarInfoDict getInstance(String dir) {
        if (instance == null) {
            instance = new RadarInfoDict(dir);
        }
        return instance;

    }

    @Override
    public String toString() {
        String s = "";
        for (RadarInfo r : dict.values()) {
            s += "\n" + r;
        }
        return s;
    }

    public static void main(String[] args) {
        RadarInfoDict dict = RadarInfoDict.getInstance(args[0]);
        System.out.println(dict);
        // for (RadarInfo radarInfo : dict) {
        // System.out.println(radarInfo.getProductCode() + ": "
        // + radarInfo.getDescription());
        // }
    }
}