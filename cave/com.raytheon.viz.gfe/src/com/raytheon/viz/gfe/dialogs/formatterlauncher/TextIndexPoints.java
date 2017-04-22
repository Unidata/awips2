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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import org.eclipse.swt.graphics.Point;

public class TextIndexPoints {
    private Point startIndex;

    private Point endIndex;

    private String text = null;

    public TextIndexPoints() {
        startIndex = new Point(0, 0);
        endIndex = new Point(0, 0);
        text = "";
    }

    public void addIndexPointsAndText(int startLine, int startCol, int endLine,
            int endCol, String txt) {
        startIndex.x = startLine;
        startIndex.y = startCol;
        endIndex.x = endLine;
        endIndex.y = endCol;
        text = txt;
    }

    public void setStartIndex(int line, int col) {
        startIndex.x = line;
        startIndex.y = col;
    }

    public void setEndIndex(int line, int col) {
        endIndex.x = line;
        endIndex.y = col;
    }

    public void setText(String txt) {
        text = txt;
    }

    public String getText() {
        return text;
    }

    public Point getStartIndex() {
        return startIndex;
    }

    public Point getEndIndex() {
        return endIndex;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append(startIndex);
        sb.append("-");
        sb.append(endIndex);
        sb.append(":\"");
        sb.append(text);
        sb.append("\"]");
        return sb.toString();
    }

}
