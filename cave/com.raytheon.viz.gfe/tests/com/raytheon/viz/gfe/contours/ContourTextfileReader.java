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
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.viz.gfe.contours.util.CLine;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author wdougherty
 * 
 */
public class ContourTextfileReader {

    public static List<CLine> readCoordinates(String fname) throws IOException {
        // GeometryFactory factory = new GeometryFactory();
        // CoordinateSequenceFactory seqFactory = factory
        // .getCoordinateSequenceFactory();
        // CoordinateSequence cseq = null;
        CLine contour = null;
        List<CLine> contours = new ArrayList<CLine>();
        float contourLevel = Float.NaN;

        // Writing the contours as code didn't work (64k limit), so parse jtext
        // to create the contours
        File file = new File(fname);
        FileInputStream fileInStream = new FileInputStream(file);
        InputStreamReader ireader = new InputStreamReader(fileInStream);
        BufferedReader reader = new BufferedReader(ireader);
        String line;
        Coordinate[] coords = null;
        Pattern coordArrPattern = Pattern
                .compile("\\QCoordinate[] coords\\E\\d+\\Q = new Coordinate[\\E(\\d+)\\];");
        Matcher coordArrMatcher;
        Pattern coordValPattern = Pattern
                .compile("coords\\d+\\[(\\d+)\\Q] = new Coordinate(\\E(-?\\d+\\.\\d+),\\s*(-?\\d+\\.\\d+)\\);");
        Matcher coordValMatcher;
        Pattern contourLevelPattern = Pattern
                .compile("contourLevel = (-?\\d+.\\d+);");
        Matcher contourLevelMatcher;
        try {
            for (line = reader.readLine(); line != null; line = reader
                    .readLine()) {
                coordArrMatcher = coordArrPattern.matcher(line);
                coordValMatcher = coordValPattern.matcher(line);
                contourLevelMatcher = contourLevelPattern.matcher(line);
                if (coordArrMatcher.lookingAt()) {
                    int size = Integer.parseInt(coordArrMatcher.group(1));
                    coords = new Coordinate[size];
                } else if (coordValMatcher.lookingAt()) {
                    int idx = Integer.parseInt(coordValMatcher.group(1));
                    double x = Double.parseDouble(coordValMatcher.group(2));
                    double y = Double.parseDouble(coordValMatcher.group(3));
                    coords[idx] = new Coordinate(x, y);
                } else if (line
                        .matches("cseq = seqFactory.create\\(coords\\d+\\);\\s*")) {
                    // cseq = seqFactory.create(coords);
                } else if (contourLevelMatcher.lookingAt()) {
                    contourLevel = Float.parseFloat(contourLevelMatcher
                            .group(1));
                } else if ("contour = new CLine(cseq, factory, contourLevel, true);"
                        .equals(line)) {
                    contour = new CLine(coords, contourLevel, false);
                } else if ("contours.add(contour);".equals(line)) {
                    contours.add(contour);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading contours", e);
        } finally {
            try {
                reader.close();
            } catch (Exception e) {
                ; // ignore
            }
        }

        return contours;
    }
}
