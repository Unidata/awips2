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
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Should Function exactly like the NSharpHandleArchiveFile but adds support for
 * spc tabular data from http://www.spc.noaa.gov/exper/soundings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class D2DNsharpHandleArchiveFile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DNsharpHandleArchiveFile.class);

    private static final String[] filterExt = { "*.nsp", "*.txt", "*.*", "*" };

    private static final SimpleDateFormat tabularDateFormat = new SimpleDateFormat(
            "yyMMdd/HHmm");

    private static final SimpleDateFormat ncepDataFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    public static void openArchiveFile(Shell shell) {
        FileDialog fd = new FileDialog(shell, SWT.MULTI);// SWT.OPEN);
        fd.setText("Open");
        fd.setFilterExtensions(filterExt);
        if (fd.open() == null) {
            return;
        }
        String[] fileNames = fd.getFileNames();
        if (fileNames == null || fileNames.length == 0) {
            return;
        }
        for (String fileName : fileNames) {
            openArchiveFile(fd.getFilterPath() + File.separator + fileName);
        }
    }

    public static void openArchiveFile(String fileName) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(fileName));
            List<String> lines = new ArrayList<String>();
            String line = reader.readLine();
            boolean isTabular = false;
            while (line != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    line = reader.readLine();
                    continue;
                }
                if (!isTabular && line.equals("%TITLE%")) {
                    isTabular = true;
                }
                lines.add(line);
                line = reader.readLine();
            }
            if (isTabular) {
                openTabularFile(lines);
            } else {
                openNspFile(lines);
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public static void openNspFile(List<String> lines) throws ParseException,
            VizException {
        // It would have been great to reuse some ncep code here but the parsing
        // code is tied to the dialog/loading code.
        if (lines.size() < 3) {
            throw new VizException("Unrecognized file format");
        }
        NsharpStationInfo stninfo = new NsharpStationInfo();

        // First line is all the metadata
        String line = lines.remove(0);
        line = line.replaceAll(" +", " ");
        String[] values = line.split(" ");
        if (values.length == 7) {
            stninfo.setSndType(values[1]);
            String stationId = values[2];
            String yearMonthDay = values[3];
            String hourMinuteSecond = values[4];
            // looks like "LAT=44.847"
            String lat = values[5];
            // looks like "LON=-93.564"
            String lon = values[6];
            String dateStr = yearMonthDay + " " + hourMinuteSecond;
            Date date = ncepDataFormat.parse(dateStr);
            stninfo.setReftime(new Timestamp(date.getTime()));
            stninfo.setRangestarttime(new Timestamp(date.getTime()));
            stninfo.setStnDisplayInfo(stationId + " " + dateStr);
            lat = lat.replace("LAT=", "");
            lon = lon.replace("LON=", "");
            stninfo.setLatitude(Float.parseFloat(lat));
            stninfo.setLongitude(Float.parseFloat(lon));
        } else {
            // TODO it might be possible to look at each token and throw
            // something together
            throw new VizException("Unrecognized data headings");
        }
        // Second line is the headings
        String headingLine = lines.remove(0);
        // The rest of the lines should be data lines.
        List<String> dataLines = new ArrayList<String>();
        line = lines.remove(0);
        while (!lines.isEmpty()) {
            dataLines.add(line);
            line = lines.remove(0);
        }

        List<NcSoundingLayer> layers = parseLayers(headingLine, dataLines);

        load(stninfo, layers);
    }

    public static void openTabularFile(List<String> lines)
            throws ParseException, VizException {
        NsharpStationInfo stninfo = new NsharpStationInfo();

        // Scan for the start of the title
        String line = lines.remove(0);
        while (!lines.isEmpty() && !line.equals("%TITLE%")) {
            line = lines.remove(0);
        }

        if (lines.isEmpty()) {
            throw new VizException("No %TITLE% found in tabular data");
        }
        line = lines.remove(0);
        // First line is the stationId and date.
        line = line.replaceAll(" +", " ");
        String[] values = line.split("\\s");
        String stationId = values[0];
        Date date = tabularDateFormat.parse(values[1]);
        stninfo.setStnDisplayInfo(stationId + " " + ncepDataFormat.format(date));
        stninfo.setSndType("BUFRUA");
        stninfo.setReftime(new Timestamp(date.getTime()));
        stninfo.setRangestarttime(new Timestamp(date.getTime()));
        // Next line is the column headings
        String headingLine = lines.remove(0);
        // Scan for the start of the raw data
        while (!lines.isEmpty() && !line.equals("%RAW%")) {
            line = lines.remove(0);
        }
        if (lines.isEmpty()) {
            throw new VizException("No %RAW% found in tabular data");
        }
        // All layers until end are data lines.
        List<String> dataLines = new ArrayList<String>();
        line = lines.remove(0);
        while (!lines.isEmpty() && !line.equals("%END%")) {
            dataLines.add(line);
            line = lines.remove(0);
        }

        List<NcSoundingLayer> layers = parseLayers(headingLine, dataLines);
        load(stninfo, layers);
    }

    private static List<NcSoundingLayer> parseLayers(String headingLine,
            List<String> lines) {
        int pressureIndex = -1;
        int heightIndex = -1;
        int temperatureIndex = -1;
        int dewpointIndex = -1;
        int windDirectionIndex = -1;
        int windSpeedIndex = -1;
        int omegaIndex = -1;
        headingLine = headingLine.replace("\t", " ");
        headingLine = headingLine.replaceAll(" +", " ");
        String[] headings = headingLine.split(" ");
        for (int i = 0; i < headings.length; i++) {
            if (headings[i].equals("PRESSURE")) {
                pressureIndex = i;
            } else if (headings[i].equals("LEVEL")) {
                pressureIndex = i;
            } else if (headings[i].equals("HGHT")) {
                heightIndex = i;
            } else if (headings[i].equals("TEMP")) {
                temperatureIndex = i;
            } else if (headings[i].equals("DWPT")) {
                dewpointIndex = i;
            } else if (headings[i].equals("WDIR")) {
                windDirectionIndex = i;
            } else if (headings[i].equals("WSPD")) {
                windSpeedIndex = i;
            } else if (headings[i].equals("OMEG")) {
                omegaIndex = i;
            }
        }
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        for (String line : lines) {
            line = line.replace(",", " ");
            line = line.replaceAll(" +", " ");
            String[] values = line.split("\\s");
            if (values.length != headings.length) {
                continue;
            }
            NcSoundingLayer layer = new NcSoundingLayer();
            if (pressureIndex != -1) {
                layer.setPressure(parseFloat(values[pressureIndex]));
            }
            if (heightIndex != -1) {
                layer.setGeoHeight(parseFloat(values[heightIndex]));
            }
            if (temperatureIndex != -1) {
                layer.setTemperature(parseFloat(values[temperatureIndex]));
            }
            if (dewpointIndex != -1) {
                layer.setDewpoint(parseFloat(values[dewpointIndex]));
            }
            if (windDirectionIndex != -1) {
                layer.setWindDirection(parseFloat(values[windDirectionIndex]));
            }
            if (windSpeedIndex != -1) {
                layer.setWindSpeed(parseFloat(values[windSpeedIndex]));
            }
            if (omegaIndex != -1) {
                layer.setOmega(parseFloat(values[omegaIndex]));
            }
            layers.add(layer);
        }
        return layers;
    }

    private static float parseFloat(String string) {
        try {
            return Float.parseFloat(string);
        } catch (NumberFormatException e) {
            return NcSoundingLayer.MISSING;
        }
    }

    private static void load(NsharpStationInfo stninfo,
            List<NcSoundingLayer> layers) throws VizException {
        if (layers.isEmpty()) {
            throw new VizException("No valid sounding layers found in data");
        }
        layers = NsharpDataHandling.organizeSoundingDataForShow(layers, layers
                .get(0).getGeoHeight());
        if (layers.isEmpty()) {
            throw new VizException("No valid sounding layers found in data");
        }

        NsharpSkewTEditor skewtEdt = NsharpSkewTEditor
                .createOrOpenSkewTEditor();
        NsharpSkewTResource skewRsc = skewtEdt.getNsharpSkewTDescriptor()
                .getSkewtResource();
        Map<String, List<NcSoundingLayer>> soundingLysLstMap = new HashMap<String, List<NcSoundingLayer>>();
        soundingLysLstMap.put(stninfo.getStnDisplayInfo(), layers);
        skewRsc.addRsc(soundingLysLstMap, stninfo);
        skewRsc.setSoundingType(stninfo.getSndType());
    }
}
