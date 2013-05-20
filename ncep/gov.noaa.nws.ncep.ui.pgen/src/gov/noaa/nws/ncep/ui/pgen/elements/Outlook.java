/*
 * gov.noaa.nws.ncep.ui.pgen.elements.Outlook
 * 
 * 23 April 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.ListIterator;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for Jet element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/10			?		B. Yin   	Initial Creation.
 * 07/10			#215	J. Wu   	Added support for graph-to-grid
 * 08/10			#215	J. Wu   	Added Contours' attributes
 * 02/11			?		B. Yin		Create text from lines without labels
 * 04/13        #977        S. Gilbert  PGEN Database support
 * </pre>
 * 
 * @author B. Yin
 */
@ElementOperations({ Operation.COPY_MOVE, Operation.EXTRAPOLATE })
public class Outlook extends Contours {

    // Name string of DECollections for line groups
    public static String OUTLOOK_LINE_GROUP = "OutlookLineGroup";

    // Name string of DECollection for labeled lines
    public static String OUTLOOK_LABELED_LINE = "OutlookLine";

    private String outlookType;

    private String forecaster;

    private String days;

    private Calendar issueTime;

    private Calendar expirationTime;

    private String lineInfo;

    // flag that indicates if lines in a group has been sorted
    private boolean linesSorted;

    protected Outlook(String name) {
        super(name);
        linesSorted = false;
    }

    /**
     * If lines have not been sorted, sort line in each group so that the line
     * with east-most starting point be the first line. If lines have been
     * sorted, move the first line to the back of the list.
     */
    public void reorderLines() {

        if (!linesSorted) {
            orderLines();
            linesSorted = true;
        } else {

            Iterator<AbstractDrawableComponent> it = compList.iterator();
            while (it.hasNext()) {
                AbstractDrawableComponent adc = it.next();
                if (adc.getName().equalsIgnoreCase(Outlook.OUTLOOK_LINE_GROUP)) {
                    ListIterator<AbstractDrawableComponent> listIt = ((DECollection) adc)
                            .getComponentListIterator();
                    while (listIt.nextIndex() < ((DECollection) adc).size()) {
                        listIt.next();
                    }

                    DECollection lastLn = null;
                    while (listIt.hasPrevious() && lastLn == null) {
                        AbstractDrawableComponent adc1 = listIt.previous();
                        if (adc1.getName().equalsIgnoreCase(
                                Outlook.OUTLOOK_LABELED_LINE)) {
                            Iterator<AbstractDrawableComponent> it1 = ((DECollection) adc1)
                                    .getComponentIterator();
                            while (it1.hasNext()) {
                                AbstractDrawableComponent de = it1.next();
                                if (de instanceof Line) {
                                    lastLn = (DECollection) adc1;
                                    break;
                                }
                            }
                        }
                    }

                    if (lastLn != null) {
                        ((DECollection) adc).remove(lastLn);
                        ((DECollection) adc).add(0, lastLn);
                    }
                }
            }

        }

    }

    /**
     * Sort lines in every group so that in each group the line with the
     * east-most starting point is the first in the list.
     */
    private void orderLines() {

        // Loop through every group and put all lines in the group into a array
        // list.
        // Remove every line from the group.
        // Sort the array list of the group
        // Add the sorted lines into the group
        Iterator<AbstractDrawableComponent> it = compList.iterator();
        while (it.hasNext()) {
            AbstractDrawableComponent adc = it.next();
            if (adc.getName().equalsIgnoreCase(Outlook.OUTLOOK_LINE_GROUP)) {
                ArrayList<DECollection> grp = new ArrayList<DECollection>();
                Iterator<AbstractDrawableComponent> itGrp = ((DECollection) adc)
                        .getComponentIterator();
                while (itGrp.hasNext()) {
                    AbstractDrawableComponent labeledLine = itGrp.next();
                    Iterator<AbstractDrawableComponent> itLabeledLine = ((DECollection) labeledLine)
                            .getComponentIterator();
                    while (itLabeledLine.hasNext()) {
                        AbstractDrawableComponent de = itLabeledLine.next();
                        if (de instanceof Line) {
                            grp.add((DECollection) labeledLine);
                            // ((DECollection)adc).remove(labeledLine);
                            itGrp.remove();
                        }
                    }

                }
                Collections.sort(grp, new EastMost());
                ((DECollection) adc).add(grp);
            }
        }

    }

    /**
     * Implementation of a comparator class for sorting lines by the east-most
     * starting point.
     * 
     * @author bingfan
     * 
     */
    private class EastMost implements Comparator<DECollection> {

        @Override
        public int compare(DECollection labeledLn1, DECollection labeledLn2) {

            Iterator<AbstractDrawableComponent> itLabeledLine1 = ((DECollection) labeledLn1)
                    .getComponentIterator();
            Line ln1 = null;
            while (itLabeledLine1.hasNext()) {
                AbstractDrawableComponent de = itLabeledLine1.next();
                if (de instanceof Line) {
                    ln1 = (Line) de;
                }
            }

            Iterator<AbstractDrawableComponent> itLabeledLine2 = ((DECollection) labeledLn2)
                    .getComponentIterator();
            Line ln2 = null;
            while (itLabeledLine1.hasNext()) {
                AbstractDrawableComponent de = itLabeledLine2.next();
                if (de instanceof Line) {
                    ln2 = (Line) de;
                }
            }

            if (ln1 == null || ln2 == null)
                return 0;

            if (ln1.getPoints().get(0).x > ln2.getPoints().get(0).x) {
                return -1;
            } else if (ln1.getPoints().get(0).x < ln2.getPoints().get(0).x) {
                return 1;
            } else {
                return 0;
            }
        }

    }

    @Override
    /**
     * Deep copy of the collection
     */
    public Outlook copy() {
        Outlook newOtlk = new Outlook(getName());
        Iterator<AbstractDrawableComponent> iterator = getComponentIterator();

        while (iterator.hasNext()) {
            newOtlk.addElement(iterator.next().copy());
        }

        iterator = newOtlk.getComponentIterator();
        while (iterator.hasNext()) {
            iterator.next().setParent(newOtlk);
        }

        newOtlk.setParm(this.getParm());
        newOtlk.setLevel(this.getLevel());
        newOtlk.setTime1(this.getTime1());
        newOtlk.setTime2(this.getTime2());
        newOtlk.setCint(this.getCint());

        newOtlk.setPgenCategory(this.getPgenCategory());
        newOtlk.setPgenType(this.getPgenType());
        newOtlk.setParent(this.getParent());
        newOtlk.setOutlookType(this.getOutlookType());
        newOtlk.setDays(this.getDays());
        newOtlk.setIssueTime(this.getIssueTime());
        newOtlk.setExpirationTime(this.getExpirationTime());
        newOtlk.setForecaster(this.getForecaster());

        return newOtlk;
    }

    /**
     * Remove a line from outlook
     * 
     * @param ln
     */
    public void removeLine(Line ln) {
        DECollection dec = (DECollection) ln.getParent();

        if (dec.getParent().equals(this)) {
            this.remove(dec);
        } else if (dec.getParent().getParent().equals(this)) {
            DECollection grp = (DECollection) dec.getParent();
            grp.remove(dec);
            if (grp.size() == 0)
                this.remove(grp);
        }
    }

    /**
     * Return the number of DEs in the outlook
     * 
     * @return
     */
    public int getDEs() {

        Iterator<DrawableElement> it = createDEIterator();
        int num = 0;
        while (it.hasNext()) {
            it.next();
            num++;
        }
        return num;

    }

    public void setOutlookType(String outlookType) {
        this.outlookType = outlookType;
    }

    public String getOutlookType() {
        return outlookType;
    }

    public void setForecaster(String forecaster) {
        this.forecaster = forecaster;
    }

    public String getForecaster() {
        return forecaster;
    }

    public void setDays(String days) {
        this.days = days;
    }

    public String getDays() {
        return days;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setExpirationTime(Calendar expirationTime) {
        this.expirationTime = expirationTime;
    }

    public Calendar getExpirationTime() {
        return expirationTime;
    }

    public void setLineInfo(String lineInfo) {
        this.lineInfo = lineInfo;
    }

    public String getLineInfo() {
        return lineInfo;
    }

    /**
     * Save the watch to an XML file
     * 
     * @param filename
     */
    public void saveToFile(String filename) {

        Layer defaultLayer = new Layer();
        defaultLayer.addElement(this);

        Product defaultProduct = new Product();
        defaultProduct.addLayer(defaultLayer);

        ArrayList<Product> prds = new ArrayList<Product>();
        prds.add(defaultProduct);
        Products filePrds = ProductConverter.convert(prds);

        FileTools.write(filename, filePrds);
    }

    /**
     * Save this element to EDEX
     * 
     * @param filename
     */
    public String storeProduct(String label) {

        String dataURI;

        Layer defaultLayer = new Layer();
        defaultLayer.addElement(this);
        ArrayList<Layer> layerList = new ArrayList<Layer>();
        layerList.add(defaultLayer);

        ProductTime refTime = new ProductTime(getIssueTime());

        Product defaultProduct = new Product("", "OUTLOOK", forecaster, null,
                refTime, layerList);

        // Product defaultProduct = new Product();
        // defaultProduct.addLayer(defaultLayer);

        defaultProduct.setOutputFile(label);
        defaultProduct.setCenter(PgenUtil.getCurrentOffice());

        try {
            dataURI = StorageUtils.storeProduct(defaultProduct);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
            return null;
        }

        return dataURI;
    }

    /**
     * Add the input line into a group so all the lines in the group will be
     * showed as continued lines when creating the text message.
     * 
     * @param ln
     * @param grp
     */
    public void addLineToGroup(Line ln, DECollection grp) {
        if (ln != null && this.contains(ln)
                && (grp == null || this.contains(grp))) {
            // if the input line is not in a group yet
            if (ln.getParent().getParent().equals(this)) {
                if (grp == null) {
                    grp = new DECollection(Outlook.OUTLOOK_LINE_GROUP);
                    this.remove(ln.getParent());
                    this.add(grp);
                    grp.add(ln.getParent());
                } else {
                    this.remove(ln.getParent());
                    grp.add(ln.getParent());
                }
            } else if (ln.getParent().getParent().getName()
                    .equalsIgnoreCase(Outlook.OUTLOOK_LINE_GROUP)
                    && grp != null) {
                // if the input line is in a group that has only one line,
                // then the line can be added to the current group.
                // otherwise do nothing.

                // check if the line's parent has only one line in it.
                Iterator<DrawableElement> it = ((DECollection) ln.getParent()
                        .getParent()).createDEIterator();
                int lns = 0;
                while (it.hasNext()) {
                    DrawableElement de = it.next();
                    if (de instanceof Line) {
                        lns++;
                    }
                }

                if (lns == 1) {
                    this.remove(ln.getParent().getParent());
                    grp.add(ln.getParent());
                }
            }
        }
    }

    /**
     * Remove the input line from the input group.
     * 
     * @param ln
     * @param grp
     */
    public void rmLineFromGroup(Line ln, DECollection grp) {
        if (ln != null && this.contains(ln)
                && (grp != null && this.contains(grp))) {
            if (ln.getParent().getParent() == grp) {
                // remove the line from the group
                grp.remove(ln.getParent());

                // remove the group if it is empty.
                if (grp.size() == 0) {
                    this.remove(grp);
                }

                // add the line to the outlook
                this.add(ln.getParent());
            }
        }

    }

    /**
     * Get the nearest line in the outlook from the input location.
     * 
     * @param loc
     *            - location
     * @return - nearest line
     */

    public Line getNearestLine(Coordinate loc) {
        Line nearestLine = null;
        double minDistance = -1;

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        gc.setStartingGeographicPoint(loc.x, loc.y);

        Iterator<DrawableElement> iterator = this.createDEIterator();
        while (iterator.hasNext()) {

            DrawableElement element = iterator.next();
            if (element instanceof Line) {

                for (Coordinate pts : element.getPoints()) {

                    gc.setDestinationGeographicPoint(pts.x, pts.y);

                    double dist = 0;

                    try {
                        dist = gc.getOrthodromicDistance();
                    } catch (IllegalStateException ise) {
                        ise.printStackTrace();

                    } catch (ArithmeticException ae) {
                        ae.printStackTrace();
                        continue;
                    }

                    if (minDistance < 0 || dist < minDistance) {

                        minDistance = dist;
                        nearestLine = (Line) element;

                    }
                }
            }

        }

        return nearestLine;
    }

    /**
     * @return a list of ContourLines converted from all "outlookLine"s
     */
    public ArrayList<ContourLine> getContourLines() {

        ArrayList<ContourLine> lines = new ArrayList<ContourLine>();

        findContourLines(this, lines);

        return lines;

    }

    /**
     * @return a list of all ContourMinmaxs converted from all "outlookMinmax"s?
     */
    public ArrayList<ContourMinmax> getContourMinmaxs() {

        return new ArrayList<ContourMinmax>();

    }

    /**
     * @return a new Outlook from a set of lines generated from a grid.
     */
    public Outlook createContours(int nContours, int[] nContourPts,
            double[] latlons, float[] contourValue, Color clr) {

        /*
         * Create a new Outlook element from the contour lines
         */
        Outlook gridContours = this.copy();

        int tPts = 0;

        ArrayList<ContourLine> cline = getContourLines();
        Line lineTemp = cline.get(0).getLine();
        Text txtTemp = cline.get(0).getLabels().get(0);

        gridContours.clear();

        for (int mm = 0; mm < nContours; mm++) {

            ArrayList<Coordinate> linePts = new ArrayList<Coordinate>();

            for (int nn = 0; nn < nContourPts[mm]; nn++) {
                Coordinate point = new Coordinate();
                point.x = latlons[tPts + nn * 2];
                point.y = latlons[tPts + nn * 2 + 1];

                linePts.add(point);
            }

            Collections.reverse(linePts);

            tPts = tPts + nContourPts[mm] * 2;

            Line oneLine = (Line) lineTemp.copy();
            oneLine.setPointsOnly(linePts);
            if (clr != null)
                oneLine.setColors(new Color[] { clr });

            DECollection dec = new DECollection(OUTLOOK_LABELED_LINE);
            oneLine.setParent(dec);
            dec.add(oneLine);

            int nLabels = 1;
            Coordinate cdt = linePts.get(linePts.size() - 1);
            for (int ii = 0; ii < nLabels; ii++) {
                Text oneText = (Text) txtTemp.copy();
                oneText.setText(new String[] { "" + contourValue[mm] });

                oneText.setLocationOnly(new Coordinate(cdt.x + 2, cdt.y + 1));
                if (clr != null)
                    oneText.setColors(new Color[] { clr });
                oneText.setParent(dec);
                dec.add(oneText);
            }

            dec.setParent(gridContours);
            gridContours.add(dec);

        }

        return gridContours;

    }

    /**
     * @return a new Outlook from a set of lines generated from a grid.
     */
    public Outlook createContours(int nContours, int[] nContourPts,
            double[] latlons, String[] contourValue, Color clr) {

        /*
         * Create a new Outlook element from the contour lines
         */
        Outlook gridContours = this.copy();

        int tPts = 0;

        ArrayList<ContourLine> cline = getContourLines();
        Line lineTemp = cline.get(0).getLine();
        Text txtTemp = cline.get(0).getLabels().get(0);

        gridContours.clear();

        for (int mm = 0; mm < nContours; mm++) {

            ArrayList<Coordinate> linePts = new ArrayList<Coordinate>();

            for (int nn = 0; nn < nContourPts[mm]; nn++) {
                Coordinate point = new Coordinate();
                point.x = latlons[tPts + nn * 2];
                point.y = latlons[tPts + nn * 2 + 1];

                linePts.add(point);
            }

            Collections.reverse(linePts);

            tPts = tPts + nContourPts[mm] * 2;

            Line oneLine = (Line) lineTemp.copy();
            oneLine.setPointsOnly(linePts);
            if (clr != null)
                oneLine.setColors(new Color[] { clr });

            DECollection dec = new DECollection(OUTLOOK_LABELED_LINE);
            oneLine.setParent(dec);
            dec.add(oneLine);

            int nLabels = 1;
            Coordinate cdt = linePts.get(linePts.size() - 1);
            for (int ii = 0; ii < nLabels; ii++) {
                Text oneText = (Text) txtTemp.copy();
                oneText.setText(new String[] { contourValue[mm] });

                oneText.setLocationOnly(new Coordinate(cdt.x + 2, cdt.y + 1));
                if (clr != null)
                    oneText.setColors(new Color[] { clr });
                oneText.setParent(dec);
                dec.add(oneText);
            }

            dec.setParent(gridContours);
            gridContours.add(dec);

        }

        return gridContours;

    }

    /**
     * @return a list of ContourLines converted from "outlookLine"s in an
     *         Outlook.
     */
    private void findContourLines(DECollection dec, ArrayList<ContourLine> lines) {

        if (lines == null) {
            lines = new ArrayList<ContourLine>();
        }

        Iterator<AbstractDrawableComponent> iterator = dec
                .getComponentIterator();

        while (iterator.hasNext()) {

            AbstractDrawableComponent adc = iterator.next();

            if (adc.getName().equals(OUTLOOK_LINE_GROUP)) {
                findContourLines((DECollection) adc, lines);
            } else if (adc.getName().equals(OUTLOOK_LABELED_LINE)) {

                Iterator<DrawableElement> deit = adc.createDEIterator();
                Line ln = null;
                Text txt = null;
                while (deit.hasNext()) {
                    DrawableElement de = deit.next();
                    if (de instanceof Line) {
                        ln = (Line) de;
                    } else if (de instanceof Text) {
                        txt = (Text) de;
                    }

                }

                ContourLine cline = new ContourLine(ln, txt, 1);
                cline.setParent(this);
                lines.add(cline);
            }
        }
    }

}