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
package com.raytheon.viz.pointdata;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;

public class StaticPlotInfoPV {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticPlotInfoPV.class);

    private static final String REGEX = "^\\s*(\\d+)\\s*(\\S+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+)\\s*(-?\\d+\\.\\d+)\\s*(\\S*)$";

    private final HashMap<String, SPIEntry> spiList;

    private final HashMap<Integer, SPIEntry> bnList;

    private final String filename;

    public class SPIEntry {
        public int blockNumber;

        public int elevation;

        public Coordinate latlon = new Coordinate();

        public double[] pixel = new double[2];

        public double distance;

        public String accessId;
    }

    protected StaticPlotInfoPV(String filename) {
        this.filename = filename;
        this.spiList = new HashMap<String, SPIEntry>();
        this.bnList = new HashMap<Integer, SPIEntry>();
    }

    public void setSPIEntry(String icao, int bn, double lat, double lon,
            int en, double dt, String ad) {
        SPIEntry entry = new SPIEntry();
        entry.blockNumber = bn;
        entry.distance = dt;
        entry.latlon.y = lat;
        entry.latlon.x = lon;
        entry.accessId = ad;
        entry.elevation = en;
        spiList.put(icao, entry);
        bnList.put(bn, entry);
    }

    public SPIEntry getSPIEntry(String icao) {
        SPIEntry spiEntry = null;
        if (this.spiList.containsKey(icao)) {
            spiEntry = this.spiList.get(icao);
        } else {
            try {
                Integer bnKey = Integer.parseInt(icao + "0");
                if (this.bnList.containsKey(bnKey)) {
                    spiEntry = this.bnList.get(bnKey);
                }
            } catch (NumberFormatException e) {
                // icao is not an integer
            }
        }
        return spiEntry;
    }

    public String getSPIFileName() {
        return this.filename;
    }

    public static StaticPlotInfoPV readStaticPlotInfoPV(String filename) {
        return readStaticPlotInfoPV(filename, false);
    }

    public static StaticPlotInfoPV readStaticPlotInfoPV(String filename,
            boolean isSPI) {

        File spiFileName = null;
        if (isSPI) {
            spiFileName = new File(filename);
        } else {
            spiFileName = PathManagerFactory.getPathManager().getStaticFile(
                    filename);
        }
        if (spiFileName == null || spiFileName.exists() == false) {
            throw new RuntimeException("Could not load file: " + filename);
        }

        BufferedReader input = null;
        StaticPlotInfoPV catalog = new StaticPlotInfoPV(filename);
        try {
            input = new BufferedReader(new FileReader(spiFileName));
            String line = null;
            Pattern p = Pattern.compile(REGEX);
            int lineNum = 0;
            while ((line = input.readLine()) != null) {
                lineNum++;
                try {
                    Matcher match = p.matcher(line);
                    if (!match.find()) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error parsing line " + lineNum + " of file "
                                        + spiFileName.getAbsolutePath() + "\n"
                                        + line);
                        continue;
                    }
                    String icao = match.group(2);
                    int bn = Integer.parseInt(match.group(1));
                    double lat = Double.parseDouble(match.group(3));
                    double lon = Double.parseDouble(match.group(4));
                    int en = Integer.parseInt(match.group(5));
                    double distance = Double.parseDouble(match.group(6));
                    String accessId = match.group(7).length() > 0 ? match
                            .group(7) : icao;
                    catalog.setSPIEntry(icao, bn, lat, lon, en, distance,
                            accessId);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing line " + lineNum + " of file "
                                    + spiFileName.getAbsolutePath() + "\n"
                                    + line, e);
                }
            }
            input.close();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
            return null;
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
        return catalog;
    }

    public HashMap<String, SPIEntry> getSpiList() {
        return spiList;
    }
}
