package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 07/10/2012	229			Chin Chen	Initial coding + EBS 
 * 10/01/2012               Chin Chen   Add STP Stats
 * 05/23/2014               Chin Chen   update SHIP, STP Stats based on bigsharp version 2013Jun12
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.sun.jna.ptr.FloatByReference;

public class NsharpSpcGraphsPaneResource extends NsharpAbstractPaneResource {
    private double spcLeftXOrig;

    private double spcRightXOrig;

    private double spcYOrig;

    private double spcYEnd;

    private double spcWidth;

    private double spcFrameWidth;

    private double spcHeight;

    private NsharpConstants.SPCGraph leftGraph = NsharpConstants.SPCGraph.EBS;

    private NsharpConstants.SPCGraph rightGraph = NsharpConstants.SPCGraph.STP;

    private static int left = 0;

    private static int right = 1;

    private double xpos, xstart, xend;

    private double ypos, ystart;

    private double hRatio;

    /* sb supercell mean ebs values by percentage of storm depth */
    /* values in m/s */
    /*
     * s10 = 9.0; s20 = 13.8; s30 = 17.2; s40 = 20.0; s50 = 22.2; s60 = 24.4;
     * s70 = 27.0; s80 = 29.6; s90 = 31.3; s100 = 31.1;
     */
    /* values in kt */
    private float supercell[] = { 18.0f, 27.6f, 34.4f, 40.0f, 44.4f, 48.8f,
            54.0f, 59.2f, 62.6f, 62.2f };

    /* mrgl supercell mean ebs values by percentage of storm depth */
    /* values in m/s */
    /*
     * m10 = 6.2; m20 = 10.2; m30 = 12.8; m40 = 14.3; m50 = 16.1; m60 = 18.0;
     * m70 = 19.9; m80 = 21.8; m90 = 23.8; m100 = 24.2;
     */
    /* values in kt */
    private float mrglSupercell[] = { 12.4f, 20.4f, 25.6f, 28.6f, 32.2f, 36.0f,
            39.8f, 43.6f, 47.6f, 48.4f };

    /* nonsupercell mean ebs values by percentage of storm depth */
    /* values in m/s */
    /*
     * n10 = 4.1; n20 = 6.1; n30 = 7.2; n40 = 7.9; n50 = 8.5; n60 = 10.0; n70 =
     * 11.6; n80 = 13.5; n90 = 15.2; n100 = 16.0;
     */
    /* values in kt */
    private float nonSupercell[] = { 8.2f, 12.2f, 14.4f, 15.8f, 17.0f, 20.0f,
            23.2f, 27.0f, 30.4f, 32.0f };

    private String hailSize[][] = {
            { "", "<1", "1-1.5", "1.75", "2", "2.5", "2.75", "3-4", ">4" },
            { "+1 STD", "1.9", "2.0", "2.3", "2.8", "2.9", "3.0", "3.0", "3.0" },
            { "AVG", "1.5", "1.5", "1.8", "2.3", "2.5", "2.5", "2.6", "2.7" },
            { "-1 STD", "1.1", "1.1", "1.3", "1.7", "2.1", "2.1", "2.2", "2.4" } };

    private RGB white = NsharpConstants.color_white;

    private RGB cyan = NsharpConstants.color_cyan_md;

    private RGB hailSizeColor[][] = {
            { white, white, white, white, white, white, white, white, white },
            { white, cyan, cyan, cyan, cyan, cyan, cyan, cyan, cyan },
            { white, cyan, cyan, cyan, cyan, cyan, cyan, cyan, cyan },
            { white, cyan, cyan, cyan, cyan, cyan, cyan, cyan, cyan } };

    private NsharpNative.NsharpLibrary.FireInfoStr fireInfo = new NsharpNative.NsharpLibrary.FireInfoStr();

    public NsharpSpcGraphsPaneResource(AbstractResourceData resourceData,
            LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
        super(resourceData, loadProperties, desc);
        leftGraph = NsharpPaletteWindow.getLeftGraph();
        rightGraph = NsharpPaletteWindow.getRightGraph();
    }

    private void underDevelopment(int side) throws VizException {
        double xpos;
        if (side == left)
            xpos = spcLeftXOrig + spcFrameWidth / 2;
        else
            xpos = spcRightXOrig + spcFrameWidth / 2;
        DrawableString str = new DrawableString("under development",
                NsharpConstants.color_green);
        str.font = font12;
        str.horizontalAlignment = HorizontalAlignment.LEFT;
        str.verticallAlignment = VerticalAlignment.TOP;
        ypos = spcYOrig + spcHeight / 2;
        str.setCoordinates(xpos, ypos);
        target.drawStrings(str);
    }

    private void setXyStartingPosition(int side) {
        ystart = spcYOrig;
        if (side == left) {
            xstart = spcLeftXOrig + 0.5 * charWidth;
            xend = spcLeftXOrig + spcFrameWidth;
        } else {
            xstart = spcRightXOrig + 0.5 * charWidth;
            xend = spcRightXOrig + spcFrameWidth;
        }
    }

    /*
     * This function is based on show_fire() in xwvid3.c of BigNsharp
     */
    private void plotFire(int side) throws VizException {
        String sfcRh = new String(fireInfo.getSfcRh());
        String sfc = new String(fireInfo.getSfc());
        String zeroOneKmRh = new String(fireInfo.getZeroOneKmRh());
        String zeroOneKmMean = new String(fireInfo.getZeroOneKmMean());
        String blMeanRh = new String(fireInfo.getBlMeanRh());
        String blMean = new String(fireInfo.getBlMean());
        String pw = new String(fireInfo.getPw());
        String blMax = new String(fireInfo.getBlMax());
        String fosberg = new String(fireInfo.getFosberg());
        RGB sfcRhColor = NsharpConstants.gempakColorToRGB.get(fireInfo
                .getSfcRhColor());
        RGB pwColor = NsharpConstants.gempakColorToRGB.get(fireInfo
                .getPwColor());
        RGB blMaxColor = NsharpConstants.gempakColorToRGB.get(fireInfo
                .getBlMaxColor());
        RGB fosbergColor = NsharpConstants.gempakColorToRGB.get(fireInfo
                .getFosbergColor());
        List<DrawableLine> lineList = new ArrayList<DrawableLine>();
        List<DrawableString> strList = new ArrayList<DrawableString>();
        this.font11.setSmoothing(false);
        this.font11.setScaleFont(false);
        this.font12.setSmoothing(false);
        this.font12.setScaleFont(false);
        this.font10.setSmoothing(false);
        setXyStartingPosition(side);
        DrawableString titleStr = new DrawableString("Fire Weather Parameters",
                NsharpConstants.color_white);
        titleStr.font = font12;
        titleStr.horizontalAlignment = HorizontalAlignment.LEFT;
        titleStr.verticallAlignment = VerticalAlignment.TOP;
        xpos = xstart + 0.25 * spcFrameWidth;
        ypos = ystart;
        titleStr.setCoordinates(xpos, ypos);
        strList.add(titleStr);
        DrawableString moistureTStr = new DrawableString("Moisture",
                NsharpConstants.color_lawngreen);
        moistureTStr.font = font12;
        moistureTStr.horizontalAlignment = HorizontalAlignment.LEFT;
        moistureTStr.verticallAlignment = VerticalAlignment.TOP;
        double xleft = xstart + 0.1 * spcFrameWidth;
        ypos = ypos + 2 * charHeight;
        moistureTStr.setCoordinates(xleft, ypos);
        strList.add(moistureTStr);
        DrawableString llWindTStr = new DrawableString("Low-Level Wind",
                NsharpConstants.color_dodgerblue);
        llWindTStr.font = font12;
        llWindTStr.horizontalAlignment = HorizontalAlignment.LEFT;
        llWindTStr.verticallAlignment = VerticalAlignment.TOP;
        double xright = xstart + 0.5 * spcFrameWidth;
        llWindTStr.setCoordinates(xright, ypos);
        strList.add(llWindTStr);
        DrawableLine line1 = new DrawableLine();
        line1.lineStyle = LineStyle.SOLID;
        line1.basics.color = NsharpConstants.color_white;
        line1.width = 1;
        ypos = ypos + 2 * charHeight;
        line1.setCoordinates(xstart, ypos);
        line1.addPoint(xend, ypos);
        lineList.add(line1);
        DrawableString sfcRhStr = new DrawableString(sfcRh.trim(), sfcRhColor);
        sfcRhStr.font = font12;
        sfcRhStr.horizontalAlignment = HorizontalAlignment.LEFT;
        sfcRhStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + charHeight;
        sfcRhStr.setCoordinates(xleft, ypos);
        strList.add(sfcRhStr);
        DrawableString sfcStr = new DrawableString(sfc.trim(),
                NsharpConstants.color_white);
        sfcStr.font = font10;
        sfcStr.horizontalAlignment = HorizontalAlignment.LEFT;
        sfcStr.verticallAlignment = VerticalAlignment.TOP;
        sfcStr.setCoordinates(xright, ypos);
        strList.add(sfcStr);
        DrawableString zeroOneRhStr = new DrawableString(zeroOneKmRh.trim(),
                NsharpConstants.color_white);
        zeroOneRhStr.font = font10;
        zeroOneRhStr.horizontalAlignment = HorizontalAlignment.LEFT;
        zeroOneRhStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + 2 * charHeight;
        zeroOneRhStr.setCoordinates(xleft, ypos);
        strList.add(zeroOneRhStr);
        DrawableString zeroOneKmMeanStr = new DrawableString(
                zeroOneKmMean.trim(), NsharpConstants.color_white);
        zeroOneKmMeanStr.font = font10;
        zeroOneKmMeanStr.horizontalAlignment = HorizontalAlignment.LEFT;
        zeroOneKmMeanStr.verticallAlignment = VerticalAlignment.TOP;
        zeroOneKmMeanStr.setCoordinates(xright, ypos);
        strList.add(zeroOneKmMeanStr);
        DrawableString blMeanRhStr = new DrawableString(blMeanRh.trim(),
                NsharpConstants.color_white);
        blMeanRhStr.font = font10;
        blMeanRhStr.horizontalAlignment = HorizontalAlignment.LEFT;
        blMeanRhStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + 2 * charHeight;
        blMeanRhStr.setCoordinates(xleft, ypos);
        strList.add(blMeanRhStr);
        DrawableString blMeanStr = new DrawableString(blMean.trim(),
                NsharpConstants.color_white);
        blMeanStr.font = font10;
        blMeanStr.horizontalAlignment = HorizontalAlignment.LEFT;
        blMeanStr.verticallAlignment = VerticalAlignment.TOP;
        blMeanStr.setCoordinates(xright, ypos);
        strList.add(blMeanStr);
        DrawableString pwStr = new DrawableString(pw.trim(), pwColor);
        if (pwColor.equals(NsharpConstants.color_red))
            pwStr.font = font12;
        else
            pwStr.font = font10;
        pwStr.horizontalAlignment = HorizontalAlignment.LEFT;
        pwStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + 2 * charHeight;
        pwStr.setCoordinates(xleft, ypos);
        strList.add(pwStr);
        DrawableString blMaxStr = new DrawableString(blMax.trim(), blMaxColor);
        blMaxStr.font = font12;
        blMaxStr.horizontalAlignment = HorizontalAlignment.LEFT;
        blMaxStr.verticallAlignment = VerticalAlignment.TOP;
        blMaxStr.setCoordinates(xright, ypos);
        strList.add(blMaxStr);
        DrawableString derivedTStr = new DrawableString("Derived Indices",
                NsharpConstants.color_orange);
        derivedTStr.font = font12;
        derivedTStr.horizontalAlignment = HorizontalAlignment.LEFT;
        derivedTStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + 3 * charHeight;
        derivedTStr.setCoordinates(xstart + 0.3 * spcFrameWidth, ypos);
        strList.add(derivedTStr);
        DrawableLine line2 = new DrawableLine();
        line2.lineStyle = LineStyle.SOLID;
        line2.basics.color = NsharpConstants.color_orange;
        line2.width = 1;
        ypos = ypos + 1.5 * charHeight;
        line2.setCoordinates(xstart, ypos);
        line2.addPoint(xend, ypos);
        lineList.add(line2);
        DrawableString fosbergStr = new DrawableString(fosberg.trim(),
                fosbergColor);
        fosbergStr.font = font12;
        fosbergStr.horizontalAlignment = HorizontalAlignment.LEFT;
        fosbergStr.verticallAlignment = VerticalAlignment.TOP;
        ypos = ypos + charHeight;
        fosbergStr.setCoordinates(xstart + 0.27 * spcFrameWidth, ypos);
        strList.add(fosbergStr);
        target.drawStrings(strList.toArray(new DrawableString[strList.size()]));
        target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
    }

    /*
     * This function is based on show_ship_stats() in xwvid3.c of BigNsharp
     */
    private void plotSHIP(int side) throws VizException {
        List<DrawableLine> lineList = new ArrayList<DrawableLine>();
        List<DrawableString> strList = new ArrayList<DrawableString>();
        this.font12.setSmoothing(false);
        this.font12.setScaleFont(false);
        this.font10.setSmoothing(false);
        // this.font10.setScaleFont(false);
        setXyStartingPosition(side);
        DrawableString titleStr = new DrawableString(
                "Significant Hail Parameter (SHIP)",
                NsharpConstants.color_white);
        titleStr.font = font12;
        titleStr.horizontalAlignment = HorizontalAlignment.LEFT;
        titleStr.verticallAlignment = VerticalAlignment.TOP;
        xpos = xstart + 0.1 * spcFrameWidth;
        ypos = ystart;
        titleStr.setCoordinates(xpos, ypos);
        strList.add(titleStr);

        // Plot Y-Coord hash marks
        int maxSHIPValue = 7;
        // shipDist = one unit of SHIP distance in Y-axis
        // 7 SHIP unit in total at Y axis
        ypos = ypos + 2 * charHeight;
        double shipDist = (spcYEnd - 2 * charHeight - ypos) / 7.0;
        double ship7Ypos = ypos;
        for (int i = maxSHIPValue; i >= 0; i--) {
            DrawableString lb = new DrawableString(Integer.toString(i),
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.LEFT;
            lb.verticallAlignment = VerticalAlignment.MIDDLE;
            xpos = xstart;
            lb.setCoordinates(xpos, ypos);
            strList.add(lb);
            DrawableLine line = new DrawableLine();
            line.lineStyle = LineStyle.DASHED;
            line.basics.color = NsharpConstants.temperatureColor;
            line.width = 1;
            xpos = xpos + 2 * charWidth;
            line.setCoordinates(xpos, ypos);
            line.addPoint(xend, ypos);
            lineList.add(line);
            ypos = ypos + shipDist;
        }
        // plot hail box and whiskers
        double ship0Ypos = ypos - shipDist;
        double shipHeight = ship0Ypos - ship7Ypos;
        double boxWidth = (xend - xstart) / 5.0;
        // nonsig hail box and whiskers values
        // s90th = 1.6;
        // s75th = 1.0;
        // s25th = 0.4;
        // s10th = 0.3;
        // sig hail box and whiskers values
        // s90th = 3.2;
        // s75th = 2.5;
        // s25th = 1.3;
        // s10th = 1.0;
        String boxName[] = { "< 2in", ">= 2in" };
        double boxWhiskerValue[][] = { { 1.6, 1.0, 0.4, 0.3 },
                { 3.2, 2.5, 1.3, 1.0 } };
        for (int i = 0; i < 2; i++) {
            DrawableString lb = new DrawableString(boxName[i],
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.CENTER;
            lb.verticallAlignment = VerticalAlignment.TOP;
            xpos = xstart + (xend - xstart) / 3.0 * (i + 1);
            lb.setCoordinates(xpos, ship0Ypos + 0.5 * charWidth);
            strList.add(lb);
            DrawableLine upWhiskerline = new DrawableLine();
            upWhiskerline.lineStyle = LineStyle.SOLID;
            upWhiskerline.basics.color = NsharpConstants.color_darkgreen;
            upWhiskerline.width = 3;
            double s90Ypos = ship0Ypos
                    - (boxWhiskerValue[i][0] / 7.0 * shipHeight);
            upWhiskerline.setCoordinates(xpos, s90Ypos);
            double s75Ypos = ship0Ypos
                    - (boxWhiskerValue[i][1] / 7.0 * shipHeight);
            upWhiskerline.addPoint(xpos, s75Ypos);
            lineList.add(upWhiskerline);

            double s25Ypos = ship0Ypos
                    - (boxWhiskerValue[i][2] / 7.0 * shipHeight);
            PixelExtent pixExt1 = new PixelExtent(xpos - boxWidth / 2.0, xpos
                    + boxWidth / 2.0, s75Ypos, s25Ypos);
            target.drawRect(pixExt1, NsharpConstants.color_darkgreen, 3.0f,
                    1.0f);
            DrawableLine lowWiskerline = new DrawableLine();
            lowWiskerline.lineStyle = LineStyle.SOLID;
            lowWiskerline.basics.color = NsharpConstants.color_darkgreen;
            lowWiskerline.width = 3;
            lowWiskerline.setCoordinates(xpos, s25Ypos);
            double s10Ypos = ship0Ypos
                    - (boxWhiskerValue[i][3] / 7.0 * shipHeight);
            lowWiskerline.addPoint(xpos, s10Ypos);
            lineList.add(lowWiskerline);
        }
        float ship = nsharpNative.nsharpLib.cave_ship();
        if (nsharpNative.nsharpLib.qc(ship) == 1) {
            double shipcY = ship0Ypos - (ship / 7.0 * shipHeight);
            RGB shipColor;
            if (ship >= 5)
                shipColor = NsharpConstants.color_magenta; // setcolor(7);
            else if (ship >= 2)
                shipColor = NsharpConstants.color_red;// setcolor(2);
            else if (ship >= 1)
                shipColor = NsharpConstants.color_gold;// setcolor(19);
            else if (ship >= .5)
                shipColor = NsharpConstants.color_white;// setcolor(31);
            else
                shipColor = NsharpConstants.color_brown; // (ship < .5)
                                                         // setcolor(8);

            DrawableLine shipline = new DrawableLine();
            shipline.lineStyle = LineStyle.SOLID;
            shipline.basics.color = shipColor;
            shipline.width = 3;
            shipline.setCoordinates(xstart, shipcY);
            shipline.addPoint(xend, shipcY);
            lineList.add(shipline);
        }
        target.drawStrings(strList.toArray(new DrawableString[strList.size()]));
        target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
    }

    /*
     * This function is based on show_stp_stats() in xwvid3.c of BigNsharp
     */
    private void plotSTP(int side) throws VizException {
        List<DrawableLine> lineList = new ArrayList<DrawableLine>();
        List<DrawableString> strList = new ArrayList<DrawableString>();
        this.font12.setSmoothing(false);
        this.font12.setScaleFont(false);
        this.font10.setSmoothing(false);
        this.font10.setScaleFont(false);
        setXyStartingPosition(side);
        DrawableString titleStr = new DrawableString(
                "Effective-Layer STP (with CIN)", NsharpConstants.color_white);
        titleStr.font = font12;
        titleStr.horizontalAlignment = HorizontalAlignment.LEFT;
        titleStr.verticallAlignment = VerticalAlignment.TOP;
        xpos = xstart + 0.1 * spcFrameWidth;
        ypos = ystart;
        titleStr.setCoordinates(xpos, ypos);
        strList.add(titleStr);

        // Plot Y-Coord hash marks
        int maxSTPValue = 11;
        // stpDist = one STP unit distance in Y-axis
        // 11 STP unit in total at Y axis
        ypos = ypos + 1.5 * charHeight;
        double stpDist = (spcYEnd - 2 * charHeight - ypos) / 11.0;
        double stp11Ypos = ypos;
        for (int i = maxSTPValue; i >= 0; i--) {
            DrawableString lb = new DrawableString(Integer.toString(i),
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.LEFT;
            lb.verticallAlignment = VerticalAlignment.MIDDLE;
            xpos = xstart;
            lb.setCoordinates(xpos, ypos);
            strList.add(lb);
            DrawableLine line = new DrawableLine();
            line.lineStyle = LineStyle.DASHED;
            line.basics.color = NsharpConstants.temperatureColor;
            line.width = 1;
            xpos = xpos + 2 * charWidth;
            line.setCoordinates(xpos, ypos);
            line.addPoint(xend, ypos);
            lineList.add(line);
            ypos = ypos + stpDist;
        }

        // plot tor box and whiskers
        double stp0Ypos = ypos - stpDist;
        double stpHeight = stp0Ypos - stp11Ypos;
        double boxWidth = (xend - xstart) / 9.5;
        // EF4+ box and whiskers values, see show_stp_stats()
        // s90th = 11.0;
        // s75th = 8.3;
        // s50th = 5.3;
        // s25th = 2.8;
        // s10th = 1.2;
        // EF3 box and whiskers values , see show_stp_stats()
        // s90th = 8.4;
        // s75th = 4.5;
        // s50th = 2.4;
        // s25th = 1.0;
        // s10th = 0.2;
        // Ef2 box and whiskers values
        // s90th = 5.6;
        // s75th = 3.7;
        // s50th = 1.7;
        // s25th = 0.6;
        // s10th = 0.0;
        // EF1 box and whiskers values
        // s90th = 4.5;
        // s75th = 2.6;
        // s50th = 1.2;
        // s25th = 0.3;
        // s10th = 0.0;
        // EF0 box and whiskers values
        // s90th = 3.7;
        // s75th = 2.0;
        // s50th = 0.8;
        // s25th = 0.1;
        // s10th = 0.0;
        // nontor box and whiskers values , see show_stp_stats()
        // s90th = 1.5;
        // s75th = 0.7;
        // s50th = 0.2;
        // s25th = 0.0;
        // s10th = 0.0;
        String boxName[] = { "EF4+", "EF3", "EF2", "EF1", "EF0", "NONTOR" };
        double boxWhiskerValue[][] = { { 11.0, 8.3, 5.3, 2.8, 1.2 },
                { 8.4, 4.5, 2.4, 1.0, 0.2 }, { 5.6, 3.7, 1.7, 0.6, 0.0 },
                { 4.5, 2.6, 1.2, 0.3, 0.0 }, { 3.7, 2.0, 0.8, 0.1, 0.0 },
                { 1.5, 0.7, 0.2, 0.0, 0.0 } };
        RGB whiskerColor[] = { NsharpConstants.color_lawngreen,
                NsharpConstants.color_lawngreen,
                NsharpConstants.color_lawngreen, NsharpConstants.color_mdgreen,
                NsharpConstants.color_mdgreen, NsharpConstants.color_darkgreen };
        for (int i = 0; i < boxName.length; i++) {
            DrawableString lb = new DrawableString(boxName[i],
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.CENTER;
            lb.verticallAlignment = VerticalAlignment.TOP;
            xpos = xstart + (xend - xstart) / 7.0 * (i + 1);
            lb.setCoordinates(xpos, stp0Ypos + 0.5 * charWidth);
            strList.add(lb);
            DrawableLine upWhiskerline = new DrawableLine();
            upWhiskerline.lineStyle = LineStyle.SOLID;
            upWhiskerline.basics.color = whiskerColor[i];
            upWhiskerline.width = 3;
            double s90Ypos = stp0Ypos
                    - (boxWhiskerValue[i][0] / 11.0 * stpHeight);
            upWhiskerline.setCoordinates(xpos, s90Ypos);
            double s75Ypos = stp0Ypos
                    - (boxWhiskerValue[i][1] / 11.0 * stpHeight);
            upWhiskerline.addPoint(xpos, s75Ypos);
            lineList.add(upWhiskerline);
            double s50Ypos = stp0Ypos
                    - (boxWhiskerValue[i][2] / 11.0 * stpHeight);
            PixelExtent pixExt = new PixelExtent(xpos - boxWidth / 2.0, xpos
                    + boxWidth / 2.0, s75Ypos, s50Ypos);
            target.drawRect(pixExt, whiskerColor[i], 3.0f, 1.0f);
            double s25Ypos = stp0Ypos
                    - (boxWhiskerValue[i][3] / 11.0 * stpHeight);
            PixelExtent pixExt1 = new PixelExtent(xpos - boxWidth / 2.0, xpos
                    + boxWidth / 2.0, s50Ypos, s25Ypos);
            target.drawRect(pixExt1, whiskerColor[i], 3.0f, 1.0f);
            DrawableLine lowWiskerline = new DrawableLine();
            lowWiskerline.lineStyle = LineStyle.SOLID;
            lowWiskerline.basics.color = whiskerColor[i];
            lowWiskerline.width = 3;
            lowWiskerline.setCoordinates(xpos, s25Ypos);
            double s10Ypos = stp0Ypos
                    - (boxWhiskerValue[i][4] / 11.0 * stpHeight);
            lowWiskerline.addPoint(xpos, s10Ypos);
            lineList.add(lowWiskerline);
        }
        // plot sounding value of STPC first
        /*
         * _lplvalues lpvls; _parcel pcl; lpvls = new _lplvalues();
         * nsharpNative.nsharpLib.get_lpvaluesData(lpvls); float sfctemp,
         * sfcdwpt, sfcpres; sfctemp = lpvls.temp; sfcdwpt = lpvls.dwpt; sfcpres
         * = lpvls.pres; // get parcel data by calling native nsharp parcel()
         * API. value is returned in pcl pcl = new _parcel();
         * nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp,
         * sfcdwpt, pcl);
         */
        // "STP (CIN)"
        float smdir = rscHandler.getSmWindDir();// bkRsc.getSmDir(); #10438
        float smspd = rscHandler.getSmWindSpd();
        float cin = nsharpNative.nsharpLib.sigtorn_cin(smdir, smspd);
        if (cin > maxSTPValue) {
            cin = maxSTPValue;
        }
        // System.out.println("cin ="+cin);
        double stpcY = stp0Ypos - (cin / 11.0 * stpHeight);
        RGB cinColor;
        if (cin >= 8)
            cinColor = NsharpConstants.color_magenta; // setcolor(7);
        else if (cin >= 4)
            cinColor = NsharpConstants.color_red;// setcolor(2);
        else if (cin >= 2)
            cinColor = NsharpConstants.color_gold;// setcolor(19);
        else if (cin >= 1)
            cinColor = NsharpConstants.color_white;// setcolor(31);
        else if (cin >= 0.5)
            cinColor = NsharpConstants.color_darkorange;// setcolor(18);
        else
            cinColor = NsharpConstants.color_brown; // <0.5 ; setcolor(8);
        DrawableLine STPCline = new DrawableLine();
        STPCline.lineStyle = LineStyle.SOLID;
        STPCline.basics.color = cinColor;
        STPCline.width = 3;
        STPCline.setCoordinates(xstart, stpcY);
        STPCline.addPoint(xend, stpcY);
        lineList.add(STPCline);
        target.drawStrings(strList.toArray(new DrawableString[strList.size()]));
        target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));

        // Calculates and plots the probability of an F2+ tornado
        // (given a supercell) based on MLCAPE alone. Probabilities
        // are derived from Thompson et al. 2005 RUC soundings
        // based on prob_sigt_mlcape() of xwvid3.c

        // tornado probability inset box
        PixelExtent tboxExt;
        double tboxStart = xstart + (xend - xstart) * 5.0 / 9.0;
        double tboxValueStart;
        tboxExt = new PixelExtent(tboxStart, xend, stp11Ypos, stp11Ypos + 8.5
                * charHeight);
        // box background and border line
        target.drawShadedRect(tboxExt, NsharpConstants.color_black, 1f, null);
        target.drawRect(tboxExt, NsharpConstants.color_white, 1f, 1f);
        strList.clear();
        lineList.clear();
        DrawableString lb = new DrawableString("Prob EF2+ torn with supercell",
                NsharpConstants.color_white);
        lb.font = font10;
        lb.horizontalAlignment = HorizontalAlignment.LEFT;
        lb.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = stp11Ypos;
        lb.setCoordinates(xpos, ypos);
        strList.add(lb);
        DrawableString lb1 = new DrawableString("Sample CLIMO = .15 sigtor",
                NsharpConstants.color_white);
        lb1.font = font10;
        lb1.horizontalAlignment = HorizontalAlignment.LEFT;
        lb1.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = ypos + charHeight;
        lb1.setCoordinates(xpos, ypos);
        strList.add(lb1);
        DrawableLine divline = new DrawableLine();
        divline.lineStyle = LineStyle.SOLID;
        divline.basics.color = NsharpConstants.color_white;
        divline.width = 1;
        ypos = ypos + 1.2 * charHeight;
        divline.setCoordinates(tboxStart, ypos);
        divline.addPoint(xend, ypos);
        lineList.add(divline);

        short oldlplchoice;
        _parcel pcl = new _parcel();

        _lplvalues lpvls = new _lplvalues();
        nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
        // oldlplchoice = lpvls.flag;
        // lift ML parcel
        nsharpNative.nsharpLib.define_parcel(
                NsharpNativeConstants.PARCELTYPE_MEAN_MIXING,
                NsharpNativeConstants.MML_LAYER);
        lpvls = new _lplvalues();
        nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
        float sfctemp, sfcdwpt, sfcpres;
        sfctemp = lpvls.temp;
        sfcdwpt = lpvls.dwpt;
        sfcpres = lpvls.pres;
        // get parcel data by calling native nsharp parcel() API. value is
        // returned in pcl
        pcl = new _parcel();
        nsharpNative.nsharpLib.parcel(-1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt,
                pcl);
        float mlcape = pcl.bplus;
        String psigt_mlcape;
        RGB mlcapeColor;
        // logic statements for Thompson et al. (2012) WAF sample
        if (mlcape >= 4000) {
            psigt_mlcape = "0.16";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape >= 3000) {
            psigt_mlcape = "0.20";
            mlcapeColor = NsharpConstants.color_gold;// setcolor(19);
        } else if (mlcape >= 2500) {
            psigt_mlcape = "0.18";
            mlcapeColor = NsharpConstants.color_gold;// setcolor(19);
        } else if (mlcape >= 2000) {
            psigt_mlcape = "0.14";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape >= 1500) {
            psigt_mlcape = "0.13";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape >= 1000) {
            psigt_mlcape = "0.15";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape >= 500) {
            psigt_mlcape = "0.16";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape >= 250) {
            psigt_mlcape = "0.14";
            mlcapeColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mlcape > 0) {
            psigt_mlcape = "0.12";
            mlcapeColor = NsharpConstants.color_darkorange;// setcolor(18);
        } else {
            psigt_mlcape = "0.00";
            mlcapeColor = NsharpConstants.color_brown;// setcolor(8);
        }
        DrawableString lbCAPE = new DrawableString("based on CAPE: ",
                NsharpConstants.color_white);
        lbCAPE.font = font10;
        lbCAPE.horizontalAlignment = HorizontalAlignment.LEFT;
        lbCAPE.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        lbCAPE.setCoordinates(xpos, ypos);
        tboxValueStart = tboxStart + (xend - tboxStart) * 0.7;// -
                                                              // target.getStringsBounds(lbCAPE).getWidth()+
                                                              // 12* charWidth;
        strList.add(lbCAPE);
        DrawableString valueCAPE = new DrawableString(psigt_mlcape, mlcapeColor);
        valueCAPE.font = font10;
        valueCAPE.horizontalAlignment = HorizontalAlignment.LEFT;
        valueCAPE.verticallAlignment = VerticalAlignment.TOP;
        valueCAPE.setCoordinates(tboxValueStart, ypos);
        strList.add(valueCAPE);

        // (given a supercell) based on MLLCL alone. Probabilities
        // are derived from Thompson et al. 2005 RUC soundings
        // based on prob_sigt_mllcl() of xwvid3.c
        float mllcl = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib
                .ihght(pcl.lclpres));
        String psigt_mllcl;
        RGB mllclColor;
        /* logic statements for Thompson et al. (2012) WAF sample */
        if (mllcl <= 750) {
            psigt_mllcl = "0.19";
            mllclColor = NsharpConstants.color_gold;// setcolor(19);
        } else if (mllcl <= 1000) {
            psigt_mllcl = "0.19";
            mllclColor = NsharpConstants.color_gold;// setcolor(19);
        } else if (mllcl <= 1250) {
            psigt_mllcl = "0.15";
            mllclColor = NsharpConstants.color_white;// setcolor(31);
        } else if (mllcl <= 1500) {
            psigt_mllcl = "0.10";
            mllclColor = NsharpConstants.color_darkorange;// setcolor(18);
        } else if (mllcl <= 1750) {
            psigt_mllcl = "0.06";
            mllclColor = NsharpConstants.color_brown;// setcolor(8);
        } else if (mllcl <= 2000) {
            psigt_mllcl = "0.06";
            mllclColor = NsharpConstants.color_brown;// setcolor(8);
        } else if (mllcl <= 2500) {
            psigt_mllcl = "0.02";
            mllclColor = NsharpConstants.color_brown;// setcolor(8);
        } else {
            psigt_mllcl = "0.00";
            mllclColor = NsharpConstants.color_brown;// setcolor(8);
        }
        DrawableString lbLCL = new DrawableString("based on LCL: ",
                NsharpConstants.color_white);
        lbLCL.font = font10;
        lbLCL.horizontalAlignment = HorizontalAlignment.LEFT;
        lbLCL.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = ypos + charHeight;
        lbLCL.setCoordinates(xpos, ypos);
        strList.add(lbLCL);
        DrawableString valueLCL = new DrawableString(psigt_mllcl, mllclColor);
        valueLCL.font = font10;
        valueLCL.horizontalAlignment = HorizontalAlignment.LEFT;
        valueLCL.verticallAlignment = VerticalAlignment.TOP;
        valueLCL.setCoordinates(tboxValueStart, ypos);
        strList.add(valueLCL);
        oldlplchoice = rscHandler.getCurrentParcel();
        float pres = NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice);
        nsharpNative.nsharpLib.define_parcel(oldlplchoice, pres);

        // Calculates and plots the probability of an F2+ tornado
        // (given a supercell) based on effective SRH alone.
        // Probabilities are derived from Thompson et al. 2005 RUC soundings
        // based on prob_sigt_esrh() of xwvid3.c
        FloatByReference topPF = new FloatByReference(0);
        FloatByReference botPF = new FloatByReference(0);
        float jh1 = -999, jh2 = -999;
        nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
        float esrh = 0f;
        if (botPF.getValue() > 0) {
            jh1 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(botPF
                    .getValue()));
            jh2 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(topPF
                    .getValue()));

            if (jh1 != -999 && jh2 != -999) {
                FloatByReference fValue = new FloatByReference(0);
                FloatByReference fValue1 = new FloatByReference(0);
                esrh = nsharpNative.nsharpLib.helicity(jh1, jh2, smdir, smspd,
                        fValue, fValue1);
            }
        }
        String psigt_esrh;
        RGB esrhColor;
        // logic statement for Thompson et al. (2012) WAF sample
        // 68 sigtor, 94 non-sigtor supercells
        if (esrh > 700) {
            psigt_esrh = "0.42";
            esrhColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 74 sigtor, 128 non-sigtor supercells
        else if (esrh > 600) {
            psigt_esrh = "0.37";
            esrhColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 130 sigtor, 213 non-sigtor supercells
        else if (esrh > 500) {
            psigt_esrh = "0.38";
            esrhColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 146 sigtor, 391 non-sigtor supercells
        else if (esrh >= 400) {
            psigt_esrh = "0.27";
            esrhColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 180 sigtor, 710 non-sigtor supercells
        else if (esrh >= 300) {
            psigt_esrh = "0.20";
            esrhColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 170 sigtor, 1074 non-sigtor supercells
        else if (esrh >= 200) {
            psigt_esrh = "0.14";
            esrhColor = NsharpConstants.color_white;// setcolor(31);
        }
        // 126 sigtor, 1440 non-sigtor supercells
        else if (esrh >= 100) {
            psigt_esrh = "0.08";
            esrhColor = NsharpConstants.color_darkorange;// setcolor(18);
        }
        // 44 sigtor, 711 non-sigtor supercells
        else if (esrh >= 50) {
            psigt_esrh = "0.06";
            esrhColor = NsharpConstants.color_brown;// setcolor(8);
        }
        // 64 sigtor, 941 non-sigtor supercells
        else {
            psigt_esrh = "0.06";
            esrhColor = NsharpConstants.color_brown;// setcolor(8);
        }
        DrawableString lbesrh = new DrawableString("based on ESRH:",
                NsharpConstants.color_white);
        lbesrh.font = font10;
        lbesrh.horizontalAlignment = HorizontalAlignment.LEFT;
        lbesrh.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = ypos + charHeight;
        lbesrh.setCoordinates(xpos, ypos);
        strList.add(lbesrh);
        DrawableString valueEsrh = new DrawableString(psigt_esrh, esrhColor);
        valueEsrh.font = font10;
        valueEsrh.horizontalAlignment = HorizontalAlignment.LEFT;
        valueEsrh.verticallAlignment = VerticalAlignment.TOP;
        valueEsrh.setCoordinates(tboxValueStart, ypos);
        strList.add(valueEsrh);

        // (given a supercell) based on effective bulk shear alone.
        // Probabilities are derived from Thompson et al. 2005 RUC soundings
        // based on prob_sigt_eshear() of xwvid3.c
        // nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
        // oldlplchoice = lpvls.flag;
        // lift MU parcel
        nsharpNative.nsharpLib.define_parcel(
                NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE,
                NsharpNativeConstants.MU_LAYER);

        float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib
                .ihght(pcl.elpres));
        // float base=jh1 =
        // nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(p_bot));
        float depth = (el - jh1);
        FloatByReference ix1 = new FloatByReference(0);
        FloatByReference ix2 = new FloatByReference(0);
        FloatByReference ix3 = new FloatByReference(0);
        FloatByReference ix4 = new FloatByReference(0);
        nsharpNative.nsharpLib.wind_shear(
                botPF.getValue(),
                nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(jh1
                        + (depth * 0.5f))), ix1, ix2, ix3, ix4);
        float eshear = ix4.getValue();
        /*
         * case of missing EL but effective inflow base exists - default to 0-6
         * km bulk shear
         */
        if (eshear < -99) {
            eshear = 0.0f;
        }
        if (el < 0) {
            nsharpNative.nsharpLib
                    .wind_shear(nsharpNative.nsharpLib
                            .ipres(nsharpNative.nsharpLib.msl(0)),
                            nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib
                                    .msl(6000)), ix1, ix2, ix3, ix4);
            eshear = ix4.getValue();
        }
        String psigt_eshear;
        RGB eshearColor;
        // logic statements for Thompson et al. (2012) WAF sample
        // 17 sigtor, 49 non-sigtor supercells
        if (eshear >= 80) {
            psigt_eshear = "0.26";
            eshearColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 111 sigtor, 300 non-sigtor supercells
        else if (eshear >= 70) {
            psigt_eshear = "0.36";
            eshearColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 251 sigtor, 676 non-sigtor supercells
        else if (eshear >= 60) {
            psigt_eshear = "0.27";
            eshearColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 303 sigtor, 1260 non-sigtor supercells
        else if (eshear >= 50) {
            psigt_eshear = "0.19";
            eshearColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 218 sigtor, 1647 non-sigtor supercells
        else if (eshear >= 40) {
            psigt_eshear = "0.12";
            eshearColor = NsharpConstants.color_darkorange;// setcolor(18);
        }
        // 74 sigtor, 1264 non-sigtor supercells
        else if (eshear >= 30) {
            psigt_eshear = "0.06";
            eshearColor = NsharpConstants.color_brown;// setcolor(8);
        }
        // 23 sigtor, 437 non-sigtor supercells
        else if (eshear >= 20) {
            psigt_eshear = "0.05";
            eshearColor = NsharpConstants.color_brown;// setcolor(8);
        }
        // 5 sigtor, 169 non-sigtor supercells
        else if (eshear > 0) {
            psigt_eshear = "0.03";
            eshearColor = NsharpConstants.color_brown;// setcolor(8);
        } else {
            psigt_eshear = "0.00";
            eshearColor = NsharpConstants.color_brown;// setcolor(8);
        }
        DrawableString lbEshear = new DrawableString("based on EBWD:",
                NsharpConstants.color_white);
        lbEshear.font = font10;
        lbEshear.horizontalAlignment = HorizontalAlignment.LEFT;
        lbEshear.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = ypos + charHeight;
        lbEshear.setCoordinates(xpos, ypos);
        strList.add(lbEshear);
        DrawableString valueEshear = new DrawableString(psigt_eshear,
                eshearColor);
        valueEshear.font = font10;
        valueEshear.horizontalAlignment = HorizontalAlignment.LEFT;
        valueEshear.verticallAlignment = VerticalAlignment.TOP;
        valueEshear.setCoordinates(tboxValueStart, ypos);
        strList.add(valueEshear);

        DrawableLine dashDivline = new DrawableLine();
        dashDivline.lineStyle = LineStyle.DASHED;
        dashDivline.basics.color = NsharpConstants.color_white;
        dashDivline.width = 1;
        ypos = ypos + 1.2 * charHeight;
        dashDivline.setCoordinates(tboxStart, ypos);
        dashDivline.addPoint(xend, ypos);
        lineList.add(dashDivline);
        pres = NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice);
        nsharpNative.nsharpLib.define_parcel(oldlplchoice, pres);

        // (given a supercell) based on the Sigtor Parameter that
        // includes CIN. Probabilities are derived from Thompson et al. 2005 RUC
        // soundings
        // based on prob_sigt_stpc() of xwvid3.c
        String psigt_stpcin;
        RGB stpcColor;
        // logic statements for Thompson et al. (2012) WAF sample
        // 39 sigtor, 28 non-sigtor supercells
        if (cin >= 10) {
            psigt_stpcin = "0.58";
            stpcColor = NsharpConstants.color_magenta;// setcolor(7);
        }
        // 39 sigtor, 32 non-sigtor supercells
        else if (cin >= 8) {
            psigt_stpcin = "0.55";
            stpcColor = NsharpConstants.color_magenta;// setcolor(7);
        }
        // 54 sigtor, 104 non-sigtor supercells
        else if (cin >= 6) {
            psigt_stpcin = "0.34";
            stpcColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 146 sigtor, 305 non-sigtor supercells
        else if (cin >= 4) {
            psigt_stpcin = "0.32";
            stpcColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 219 sigtor, 842 non-sigtor supercells
        else if (cin >= 2) {
            psigt_stpcin = "0.21";
            stpcColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 200 sigtor, 963 non-sigtor supercells
        else if (cin >= 1) {
            psigt_stpcin = "0.17";
            stpcColor = NsharpConstants.color_white;// setcolor(31);
        }
        // 112 sigtor, 823 non-sigtor supercells
        else if (cin >= .5) {
            psigt_stpcin = "0.12";
            stpcColor = NsharpConstants.color_darkorange;// setcolor(18);
        }
        // 84 sigtor, 1026 non-sigtor supercells
        else if (cin > .1) {
            psigt_stpcin = "0.08";
            stpcColor = NsharpConstants.color_darkorange;// setcolor(18);
        }
        // 109 sigtor, 1571 non-sigtor supercells
        else {
            psigt_stpcin = "0.06";
            stpcColor = NsharpConstants.color_brown;// setcolor(8);
        }

        DrawableString lbStpc = new DrawableString("based on STPC:",
                NsharpConstants.color_white);
        lbStpc.font = font10;
        lbStpc.horizontalAlignment = HorizontalAlignment.LEFT;
        lbStpc.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        // ypos = ypos + charHeight;
        lbStpc.setCoordinates(xpos, ypos);
        strList.add(lbStpc);
        DrawableString valueStpc = new DrawableString(psigt_stpcin, stpcColor);
        valueStpc.font = font10;
        valueStpc.horizontalAlignment = HorizontalAlignment.LEFT;
        valueStpc.verticallAlignment = VerticalAlignment.TOP;
        valueStpc.setCoordinates(tboxValueStart, ypos);
        strList.add(valueStpc);

        // (given a supercell) based on the Sigtor Parameter.
        // Probabilities are derived from Thompson et al. 2005 RUC soundings
        // based on prob_sigt_stp() of xwvid3.c
        float stp_nocin = nsharpNative.nsharpLib.sigtorn_fixed(smdir, smspd);
        String psigt_stp;
        RGB stpColor;
        // logic statements for Thompson et al. (2012) WAF sample */
        // 32 sigtor, 23 non-sigtor supercells
        if (stp_nocin >= 9) {
            psigt_stp = "0.58";
            stpColor = NsharpConstants.color_magenta;// setcolor(7);
        }
        // 46 sigtor, 37 non-sigtor supercells
        else if (stp_nocin >= 7) {
            psigt_stp = "0.55";
            stpColor = NsharpConstants.color_magenta;// setcolor(7);
        }
        // 112 sigtor, 172 non-sigtor supercells
        else if (stp_nocin >= 5) {
            psigt_stp = "0.39";
            stpColor = NsharpConstants.color_red;// setcolor(2);
        }
        // 172 sigtor, 515 non-sigtor supercells, >3 same as > 2
        // 177 sigtor, 541 non-sigtor supercells
        else if (stp_nocin >= 2) {
            psigt_stp = "0.25";
            stpColor = NsharpConstants.color_gold;// setcolor(19);
        }
        // 218 sigtor, 1091 non-sigtor supercells
        else if (stp_nocin >= 1) {
            psigt_stp = "0.17";
            stpColor = NsharpConstants.color_white;// setcolor(31);
        }
        // 114 sigtor, 950 non-sigtor supercells
        else if (stp_nocin >= .5) {
            psigt_stp = "0.11";
            stpColor = NsharpConstants.color_darkorange;// setcolor(18);
        }
        // 71 sigtor, 1107 non-sigtor supercells
        else if (stp_nocin >= .1) {
            psigt_stp = "0.06";
            stpColor = NsharpConstants.color_brown;// setcolor(8);
        }
        // 60 sigtor, 1266 non-sigtor supercells
        else {
            psigt_stp = "0.05";
            stpColor = NsharpConstants.color_brown;// setcolor(8);
        }

        DrawableString lbStp = new DrawableString("based on STP_fixed:",
                NsharpConstants.color_white);
        lbStp.font = font10;
        lbStp.horizontalAlignment = HorizontalAlignment.LEFT;
        lbStp.verticallAlignment = VerticalAlignment.TOP;
        xpos = tboxStart + 0.5 * charWidth;
        ypos = ypos + charHeight;
        lbStp.setCoordinates(xpos, ypos);
        strList.add(lbStp);
        DrawableString valueStp = new DrawableString(psigt_stp, stpColor);
        valueStp.font = font10;
        valueStp.horizontalAlignment = HorizontalAlignment.LEFT;
        valueStp.verticallAlignment = VerticalAlignment.TOP;
        valueStp.setCoordinates(tboxValueStart, ypos);
        strList.add(valueStp);

        // reset parcel to previous "oldlplchoice" , TBD
        /*
         * float pres; if(oldlplchoice ==
         * NsharpNativeConstants.PARCELTYPE_USER_DEFINED){
         * if(NsharpParcelDialog.getAccess() != null){ pres =
         * NsharpParcelDialog.getAccess().getUserDefdParcelMb(); } else pres =
         * NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice); } else pres
         * = NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice);
         * nsharpNative.nsharpLib.define_parcel(oldlplchoice,pres);
         */
        target.drawStrings(strList.toArray(new DrawableString[strList.size()]));
        target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));

    }

    /*
     * This function is based on show_ebs_stats() in xwvid3.c of BigNsharp
     */
    private void plotEBS(int side) throws VizException {
        List<DrawableLine> lineList = new ArrayList<DrawableLine>();
        List<DrawableString> strList = new ArrayList<DrawableString>();
        this.font12.setSmoothing(false);
        this.font12.setScaleFont(false);
        setXyStartingPosition(side);
        DrawableString titleStr = new DrawableString(
                "Effective Bulk Wind Difference (kt, y axis)",
                NsharpConstants.color_white);
        titleStr.font = font12;
        titleStr.horizontalAlignment = HorizontalAlignment.LEFT;
        titleStr.verticallAlignment = VerticalAlignment.TOP;
        xpos = xstart + 0.1 * spcFrameWidth;
        ypos = ystart;
        titleStr.setCoordinates(xpos, ypos);
        strList.add(titleStr);
        // target.drawStrings(titleStr);
        DrawableString subTStr1 = new DrawableString(
                "supercell    mrgl supercell (dashed)    ",
                NsharpConstants.color_lawngreen);
        ypos = ypos + 1.5 * charHeight;
        subTStr1.font = font10;
        subTStr1.horizontalAlignment = HorizontalAlignment.LEFT;
        subTStr1.verticallAlignment = VerticalAlignment.TOP;
        subTStr1.setCoordinates(xpos, ypos);
        strList.add(subTStr1);
        DrawableString subTStr2 = new DrawableString("non-supercell",
                NsharpConstants.color_darkorange);
        subTStr2.font = font10;
        subTStr2.horizontalAlignment = HorizontalAlignment.LEFT;
        subTStr2.verticallAlignment = VerticalAlignment.TOP;
        xpos = xpos + (target.getStringsBounds(subTStr1).getWidth()) * hRatio;
        subTStr2.setCoordinates(xpos, ypos);
        strList.add(subTStr2);
        // target.drawStrings(subTStr1, subTStr2);

        ypos = ypos + 1.5 * charHeight;
        // ----- Plot Y-Coordinate hash marks, 0 - 70 kt -----
        int maxval = 70;
        // knotDist = one Knot distance in Y-axis
        // 70 Knots in total at Y axis
        double knotDist = (spcYEnd - 2 * charHeight - ypos) / 70.0;
        for (int i = maxval; i >= 0; i = i - 10) {
            DrawableString lb = new DrawableString(Integer.toString(i),
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.LEFT;
            lb.verticallAlignment = VerticalAlignment.MIDDLE;
            xpos = xstart;
            lb.setCoordinates(xpos, ypos);
            strList.add(lb);
            DrawableLine line = new DrawableLine();
            line.lineStyle = LineStyle.DASHED;
            line.basics.color = NsharpConstants.temperatureColor;
            line.width = 1;
            xpos = xpos + 2 * charWidth;
            line.setCoordinates(xpos, ypos);
            line.addPoint(xend, ypos);
            lineList.add(line);
            ypos = ypos + 10 * knotDist;
        }
        // target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
        // x axis 10 -100
        double xgap = (spcFrameWidth - 5 * charWidth) / 10;
        ypos = spcYEnd - 0.5 * charHeight;
        xpos = xstart;
        double cellYPosStart = spcYEnd - 2 * charHeight;
        // supercell line
        DrawableLine supercellline = new DrawableLine();
        supercellline.lineStyle = LineStyle.SOLID;
        supercellline.basics.color = NsharpConstants.color_lawngreen;
        supercellline.width = 2;
        // meglsupercell line
        DrawableLine mrglSupercellline = new DrawableLine();
        mrglSupercellline.lineStyle = LineStyle.DASHED;
        mrglSupercellline.basics.color = NsharpConstants.color_lawngreen;
        mrglSupercellline.width = 2;// nonsupercell line
        DrawableLine nonSupercellline = new DrawableLine();
        nonSupercellline.lineStyle = LineStyle.SOLID;
        nonSupercellline.basics.color = NsharpConstants.color_darkorange;
        nonSupercellline.width = 2;
        for (int i = 10; i <= 100; i = i + 10) {
            xpos = xpos + xgap;
            // lb for x-axis number
            DrawableString lb = new DrawableString(Integer.toString(i),
                    NsharpConstants.color_white);
            lb.font = font10;
            lb.horizontalAlignment = HorizontalAlignment.CENTER;
            lb.verticallAlignment = VerticalAlignment.BOTTOM;
            lb.setCoordinates(xpos, ypos);
            strList.add(lb);
            int cellIndex = i / 10 - 1;
            nonSupercellline.addPoint(xpos, cellYPosStart
                    - nonSupercell[cellIndex] * knotDist);
            supercellline.addPoint(xpos, cellYPosStart - supercell[cellIndex]
                    * knotDist);
            mrglSupercellline.addPoint(xpos, cellYPosStart
                    - mrglSupercell[cellIndex] * knotDist);
        }
        lineList.add(nonSupercellline);
        lineList.add(supercellline);
        lineList.add(mrglSupercellline);
        // target.drawLine(supercellline,mrglSupercellline,nonSupercellline);
        // call get_topBotPres to set p_top and p_bot
        FloatByReference topPF = new FloatByReference(0);
        FloatByReference botPF = new FloatByReference(0);
        nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
        float base = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib
                .ihght(botPF.getValue()));

        short oldlplchoice;
        _parcel pcl = new _parcel();
        ;
        _lplvalues lpvls = new _lplvalues();
        nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
        // oldlplchoice = lpvls.flag;

        nsharpNative.nsharpLib.define_parcel(
                NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE, 400f);

        lpvls = new _lplvalues();
        nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
        float sfctemp, sfcdwpt, sfcpres;
        sfctemp = lpvls.temp;
        sfcdwpt = lpvls.dwpt;
        sfcpres = lpvls.pres;
        // get parcel data by calling native nsharp parcel() API. value is
        // returned in pcl
        nsharpNative.nsharpLib.parcel(-1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt,
                pcl);
        float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib
                .ihght(pcl.elpres));
        if (botPF.getValue() < 0 || pcl.bplus < 100.0
                || el == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
            xpos = xstart + 0.2 * spcFrameWidth;
            DrawableString lb = new DrawableString("No Effective Inflow Layer",
                    NsharpConstants.color_yellow);
            lb.font = font12;
            lb.horizontalAlignment = HorizontalAlignment.LEFT;
            lb.verticallAlignment = VerticalAlignment.TOP;
            lb.setCoordinates(xpos, cellYPosStart - 7 * knotDist);
            strList.add(lb);
        } else {
            float depth = (el - base);
            xpos = xstart;
            FloatByReference ix1 = new FloatByReference(0);
            FloatByReference ix2 = new FloatByReference(0);
            FloatByReference ix3 = new FloatByReference(0);
            FloatByReference ix4 = new FloatByReference(0);
            DrawableLine ebsline = new DrawableLine();
            ebsline.lineStyle = LineStyle.SOLID;
            ebsline.basics.color = NsharpConstants.color_yellow;
            ebsline.width = 3;
            for (int i = 10; i <= 100; i = i + 10) {
                xpos = xpos + xgap;
                nsharpNative.nsharpLib.wind_shear(botPF.getValue(),
                        nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib
                                .msl(base + (depth * 0.1f * (i / 10)))), ix1,
                        ix2, ix3, ix4);
                float ebs = ix4.getValue();
                if (ebs > 70)
                    ebs = 70;
                ebsline.addPoint(xpos, cellYPosStart - ebs * knotDist);
            }
            lineList.add(ebsline);
        }
        float pres;
        /*
         * if(oldlplchoice == NsharpNativeConstants.PARCELTYPE_USER_DEFINED){
         * if(NsharpParcelDialog.getAccess() != null){ pres =
         * NsharpParcelDialog.getAccess().getUserDefdParcelMb(); } else pres =
         * NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice); } else
         */
        oldlplchoice = rscHandler.getCurrentParcel();
        pres = NsharpNativeConstants.parcelToLayerMap.get(oldlplchoice);
        nsharpNative.nsharpLib.define_parcel(oldlplchoice, pres);
        target.drawStrings(strList.toArray(new DrawableString[strList.size()])); // x-axis
                                                                                 // mark
                                                                                 // number
        target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        // defineCharHeight(font10);
        if (rscHandler == null || rscHandler.getSoundingLys() == null)
            return;
        this.font10.setSmoothing(false);
        this.font10.setScaleFont(false);
        hRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        DrawableLine line = new DrawableLine();
        line.lineStyle = LineStyle.SOLID;
        line.basics.color = NsharpConstants.color_white;
        line.width = 1;
        line.setCoordinates(spcRightXOrig, spcYOrig);
        line.addPoint(spcRightXOrig, spcYOrig + spcHeight);
        target.drawLine(line);
        PixelExtent spcExt = new PixelExtent(new Rectangle((int) spcLeftXOrig,
                (int) spcYOrig, (int) spcWidth, (int) spcHeight));
        target.drawRect(spcExt, NsharpConstants.color_white, 1f, 1f); // box
                                                                      // border
                                                                      // line
        PixelExtent extent = new PixelExtent(new Rectangle((int) spcLeftXOrig,
                (int) spcYOrig, (int) spcFrameWidth, (int) spcHeight));
        // target.setupClippingPlane(extent);
        switch (leftGraph) {
        case EBS:
            plotEBS(left);
            break;
        case STP:
            plotSTP(left);
            break;
        case SHIP:
            plotSHIP(left);
            break;
        case FIRE:
            plotFire(left);
            break;
        case WINTER:
            underDevelopment(0);// plotWinter(left);
            break;
        case HAIL:
            underDevelopment(0);// plotHail(left);
            break;
        case SARS:
            underDevelopment(0);// plotSars(left);
            break;
        }
        // target.clearClippingPlane();
        extent = new PixelExtent(new Rectangle((int) spcRightXOrig,
                (int) spcYOrig, (int) spcFrameWidth, (int) spcHeight));
        // target.setupClippingPlane(extent);
        switch (rightGraph) {
        case EBS:
            plotEBS(right);
            break;
        case STP:
            plotSTP(right);
            break;
        case SHIP:
            plotSHIP(right);
            break;
        case FIRE:
            plotFire(right);
            break;
        case WINTER:
            underDevelopment(1);// plotWinter(right);
            break;
        case HAIL:
            underDevelopment(1);// plotHail(right);//
            break;
        case SARS:
            underDevelopment(1);// plotSars(right);
            break;
        }
        // target.clearClippingPlane();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
    }

    /*
     * private void disposeEbsShape(){ if(ebsBkgLblShape != null)
     * ebsBkgLblShape.dispose(); if(ebsBkgLineShape != null)
     * ebsBkgLineShape.dispose(); if(ebsSupercellShape != null)
     * ebsSupercellShape.dispose(); if(ebsMrglSupShape != null)
     * ebsMrglSupShape.dispose(); if(ebsNonSupSgape != null)
     * ebsNonSupSgape.dispose(); }
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
    }

    @Override
    public void handleResize() {

        super.handleResize();
        IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
        ext.reset();
        this.rectangle = new Rectangle((int) ext.getMinX(),
                (int) ext.getMinY(), (int) ext.getWidth(),
                (int) ext.getHeight());
        pe = new PixelExtent(this.rectangle);
        getDescriptor().setNewPe(pe);
        defineCharHeight(font10);
        spcLeftXOrig = (ext.getMinX());
        spcYOrig = ext.getMinY();
        spcWidth = (ext.getWidth());
        spcFrameWidth = spcWidth / 2;
        spcRightXOrig = spcLeftXOrig + spcFrameWidth;
        spcHeight = ext.getHeight();
        spcYEnd = ext.getMaxY();
    }

    public NsharpConstants.SPCGraph getLeftGraph() {
        return leftGraph;
    }

    public void setGraphs(NsharpConstants.SPCGraph leftGraph,
            NsharpConstants.SPCGraph rightGraph) {
        this.leftGraph = leftGraph;
        this.rightGraph = rightGraph;
        getSpcGraphsInfo();
        rscHandler.refreshPane();
    }

    public NsharpConstants.SPCGraph getRightGraph() {
        return rightGraph;
    }

    public void getSpcGraphsInfo() {

        if (leftGraph == NsharpConstants.SPCGraph.FIRE
                || rightGraph == NsharpConstants.SPCGraph.FIRE) {
            nsharpNative.nsharpLib.getFireInfo(fireInfo);
        }

    }
}
