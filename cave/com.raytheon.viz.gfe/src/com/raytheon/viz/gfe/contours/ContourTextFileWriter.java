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

import java.io.File;
import java.io.PrintStream;
import java.util.List;

import com.raytheon.viz.gfe.contours.util.CLine;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author wdougherty
 * 
 */
public class ContourTextFileWriter {
    public static void write(List<CLine> contours, String fname) {
        PrintStream out = System.out;
        try {
            File file = new File(fname);
            out = new PrintStream(file);

        } catch (Exception e) {
            throw new RuntimeException("Error opening output stream", e);
        }

        int contourNum = 0;
        for (CLine contour : contours) {
            LineString line = contour.getLineString();
            out.println("Coordinate[] coords" + contourNum
                    + " = new Coordinate[" + line.getNumPoints() + "];");
            int coordNum = 0;
            for (Coordinate coordinate : line.getCoordinates()) {
                out.println(String.format(
                        "coords%d[%d] = new Coordinate(%f,%f);", contourNum,
                        coordNum, coordinate.x, coordinate.y));
                coordNum++;
            }
            out.println(String.format("cseq = seqFactory.create(coords%d);",
                    contourNum));
            out.println(String.format("contourLevel = %f", contour
                    .getContourLevel()));
            out
                    .println("contour = new CLine(cseq, factory, contourLevel, true);");
            out.println("contours.add(contour);");
            contourNum++;
        }

        out.close();
    }
}
