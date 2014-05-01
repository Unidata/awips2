/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.CcfpTimeDlg
 * 
 * 20 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.awt.geom.Point2D;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * CCFP utility and storage class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ---------	--------	----------	--------------------------
 * 09/10		322			G. Zhang 	Initial Creation.
 * 04/13        #977        S. Gilbert  PGEN Database support
 * 12/13        TTR751      J. Wu       Update getCcfpTxt2().
 * </pre>
 * 
 * @author gzhang
 */

public class CcfpInfo {

    /**
     * CCFP Pgen type constant
     */
    public static final String PGEN_TYPE_CCFP = "CCFP_SIGMET";

    /**
     * String for seperating the Azimuth and distance
     */
    public static final String TEXT_SEPERATOR = ":::";

    /**
     * elements to be used for text product
     */
    private static final ArrayList<AbstractDrawableComponent> sigs = new ArrayList<AbstractDrawableComponent>();

    /**
     * 16-points compass direction to degree map
     */
    public static final Map<String, Double> DIR_AZI_MAP = getDirAziMap();

    /*
     * initializer of DIR_AZI_MAP
     * 
     * @return
     */
    private static Map<String, Double> getDirAziMap() {
        Map<String, Double> map = new HashMap<String, Double>();

        map.put("N", 0.0);
        map.put("NNE", 22.5);
        map.put("NE", 45.0);
        map.put("ENE", 67.5);

        map.put("E", 90.0);
        map.put("ESE", 112.5);
        map.put("SE", 135.0);
        map.put("SSE", 157.5);

        map.put("S", 180.0);
        map.put("SSW", -157.5);
        map.put("SW", -135.0);
        map.put("WSW", -112.5);

        map.put("W", -90.0);
        map.put("WNW", -67.5);
        map.put("NW", -45.0);
        map.put("NNW", -22.5);

        return map;
    }

    /**
     * Creates a formatted string comprising of the contents of the XML file, to
     * which formatting information is applied from the style-sheet.
     * 
     * @param xmlFileName
     *            - Name of the XML file
     * @param xltFileName
     *            - Name of the style-sheet
     * @return A <tt>String</tt> with the formatted contents of the XML file.
     */
    public static String convertXml2Txt(String xml, String xltFileName) {
        String res = "";

        Source xmlSource = new StreamSource(new StringReader(xml));
        Source xsltSource = new StreamSource(xltFileName);

        TransformerFactory transFact = TransformerFactory.newInstance();

        try {
            Transformer trans = transFact.newTransformer(xsltSource);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            trans.transform(xmlSource, new StreamResult(baos));

            res = new String(baos.toByteArray());
        } catch (Exception e) {
            System.out.println("Error: File is corrupt");
        }

        return res;
    }

    /**
     * save CCFP Products to an xml file
     * 
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     * @return: saved file name
     */
    public static String saveCcfpXmlFile(String it, String vt) {

        Products filePrds = getCcfpFilePrds(it, vt);
        String fileName = PgenUtil.getPgenActivityTextProdPath()
                + File.separator + getCcfpFileName();

        FileTools.write(fileName, filePrds);

        // avoid duplicate elements
        sigs.clear();

        return fileName;

    }

    /**
     * save CCFP Products to EDEX
     * 
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     * @return: saved file name
     */
    public static String storeCcfpXmlFile(Product prod) {
        String dataURI = null;

        String label = getCcfpFileName();
        prod.setOutputFile(label);

        // FileTools.write(fileName, filePrds);
        try {
            dataURI = StorageUtils.storeProduct(prod);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
        }

        // avoid duplicate elements
        sigs.clear();

        return dataURI;

    }

    /**
     * return all CCFP Products.
     * 
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     * @return CCFP Products
     */
    public static Products getCcfpFilePrds(String it, String vt) {

        PgenResource rsc = PgenSession.getInstance().getPgenResource();
        ArrayList<Product> prds = (ArrayList<Product>) rsc.getProducts();

        Products filePrds = ProductConverter.convert(getCcfpPrds(prds, it, vt));

        return filePrds;
    }

    /**
     * filter out non CCFP Products
     * 
     * @param prds
     *            : Product list to be filtered
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     * @return CCFP Products list
     */
    public static ArrayList<Product> getCcfpPrds(ArrayList<Product> prds,
            String it, String vt) {
        ArrayList<Product> list = new ArrayList<Product>();

        for (Product p : prds) {
            for (Layer l : p.getLayers()) {
                for (AbstractDrawableComponent adc : l.getDrawables()) {
                    if (PGEN_TYPE_CCFP.equalsIgnoreCase(adc.getPgenType())) {
                        Ccfp c = (Ccfp) adc;
                        Sigmet s = c.getSigmet();
                        s.setEditableAttrStartTime(it);
                        s.setEditableAttrEndTime(vt);
                        c.setCollectionNameWithSigmet(s);
                        sigs.add((Sigmet) ((Ccfp) adc).getSigmet().copy());
                    } // sigs.add(((Sigmet)adc).copy());
                }
            }
        }

        setIssueValidTimes(sigs, it, vt);

        Layer l = new Layer();
        l.add(sigs);
        Product p = new Product();
        p.addLayer(l);
        list.add(p);

        return list;
    }

    /**
     * filter out non CCFP Products from current resource
     * 
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     * @return CCFP Products list
     */
    public static Product getCcfpPrds(String it, String vt) {

        PgenResource rsc = PgenSession.getInstance().getPgenResource();
        List<Product> prds = rsc.getProducts();
        ArrayList<AbstractDrawableComponent> sigmets = new ArrayList<AbstractDrawableComponent>();

        for (Product p : prds) {
            for (Layer l : p.getLayers()) {
                for (AbstractDrawableComponent adc : l.getDrawables()) {
                    if (PGEN_TYPE_CCFP.equalsIgnoreCase(adc.getPgenType())) {
                        Ccfp c = (Ccfp) adc;
                        Sigmet s = c.getSigmet();
                        s.setEditableAttrStartTime(it);
                        s.setEditableAttrEndTime(vt);
                        c.setCollectionNameWithSigmet(s);
                        sigmets.add((Sigmet) ((Ccfp) adc).getSigmet().copy());
                    } // sigs.add(((Sigmet)adc).copy());
                }
            }
        }

        setIssueValidTimes(sigmets, it, vt);

        Layer l = new Layer();
        l.add(sigmets);
        Product p = new Product();
        p.addLayer(l);
        p.setType(PGEN_TYPE_CCFP);

        return p;
    }

    /**
     * set Issue and Valid times for a list of Sigmets
     * 
     * @param sigs
     *            : a list of Sigmet
     * @param it
     *            : Issue time String
     * @param vt
     *            : Valid time String
     */
    private static void setIssueValidTimes(
            ArrayList<AbstractDrawableComponent> sigs, String it, String vt) {
        if (sigs == null)
            return;

        for (AbstractDrawableComponent adc : sigs) {

            ((Sigmet) adc).setEditableAttrStartTime(it);
            ((Sigmet) adc).setEditableAttrEndTime(vt);
        }

    }

    /**
     * return the CCFP xml file name
     * 
     * @return xml file name
     */
    public static String getCcfpFileName() {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddhhmmss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        return sdf.format(new Date()) + "_CCFP.xml";

    }

    /**
     * return the CCFP Text for Area type
     * 
     * @param sig
     *            : the Sigmet with text
     * @return: String[] containing the text
     */
    public static String[] getCcftTxt(Sigmet sig) {
        // String tops = "TOPS "+sig.getEditableAttrPhenom2();
        // String gwth = "GWTH "+sig.getEditableAttrPhenomLon();
        // String sigConf = sig.getEditableAttrPhenomLat();
        // String conf = "CONF "+ ( sigConf==null ? "LOW" :
        // sigConf.contains("49")? "LOW":"HIGH") ;
        // String cvrg = "CVRG "+sig.getEditableAttrPhenom();
        return getCcfpTxt2(sig);
        // return new String[]{tops,gwth,conf,cvrg};//sb.toString();
    }

    public static double getDirPts2(String dir, double x, double y) {
        Double deg = DIR_AZI_MAP.get(dir);

        double r2d = 57.2957795;

        if (deg == null)
            return y;

        double d = deg.doubleValue() / r2d;

        return x * Math.sin(d) + y * Math.cos(d);
    }

    public static Coordinate getDirMostCoor(String dir, Coordinate[] coors,
            IMapDescriptor md) {
        if (coors == null)
            return null;

        TreeMap<Double, Coordinate> tm = new TreeMap<Double, Coordinate>();

        for (Coordinate c : coors) {

            tm.put(getDirPts2(dir, c.x, c.y), c);
        }

        // double d = DIR_AZI_MAP.get(dir);

        return tm.get(tm.lastKey());
    }

    public static ArrayList<Coordinate> getDirMostPts(String dir,
            Coordinate[] coors, IMapDescriptor md) {
        GeodeticCalculator gc = new GeodeticCalculator(md.getCRS());// DefaultEllipsoid.WGS84);
        Coordinate c1 = getDirMostCoor(dir, coors, md);

        // arrow part
        Point2D pts = null;
        try {
            gc.setStartingGeographicPoint(c1.x, c1.y);
            gc.setDirection(DIR_AZI_MAP.get(dir), 70 * 1852);// 30 nautical
                                                             // miles
            pts = gc.getDestinationGeographicPoint();
        } catch (Exception e) {
        }

        ArrayList<Coordinate> list = new ArrayList<Coordinate>();
        if (pts == null)
            return list;

        list.add(c1);
        list.add(new Coordinate(pts.getX(), pts.getY()));

        return list;

    }

    /**
     * get the CCFP with speed direction Text's Coordinate
     * 
     * @param dir
     *            : the direction of the speed
     * @param coors
     *            : the Coordinate of the CCFP
     * @param md
     *            : the MapDescriptor
     * @return: the Coordinate of the CCFP speed direction's Text
     */
    public static Coordinate getDirMostTxt(String dir, Coordinate[] coors,
            IMapDescriptor md) {
        GeodeticCalculator gcTxt = new GeodeticCalculator(md.getCRS());// DefaultEllipsoid.WGS84);
        Coordinate c1 = getDirMostCoor(dir, coors, md);

        gcTxt.setStartingGeographicPoint(c1.x, c1.y);
        gcTxt.setDirection(DIR_AZI_MAP.get(dir), 90 * 1852);//
        Point2D spdTxt = gcTxt.getDestinationGeographicPoint();

        return new Coordinate(spdTxt.getX(), spdTxt.getY());

    }

    /**
     * get the Centroid of the Sigmet Area
     * 
     * @param sig
     *            : the Sigmet of which the Centroid to be calculated
     * @return: the Coordinate for the Centroid
     */
    public static Coordinate getSigCentroid(Sigmet sig) {
        if (sig.getLinePoints().length < 2) {// .getPoints().size() < 2){
            return null;
        }

        GeometryFactory factory = new GeometryFactory();
        LineString g = factory.createLineString(sig.getLinePoints());// a);
        Point p = g.getCentroid();

        return p.getCoordinate();
    }

    /**
     * check if the Sigmet is crossing the line of Longitude 180/-180
     * 
     * @param sig
     *            : the Sigmet for which the check to be performed
     * @return: true: the Sigmet crosses the Lon 180/-180, false otherwise
     */
    public static boolean isCrossingLon180(Sigmet sig) {
        if (sig.getLinePoints().length < 2) {// .getPoints().size() < 2){
            return false;
        }

        GeometryFactory factory = new GeometryFactory();
        LineString g = factory.createLineString(sig.getLinePoints());// a);

        GeometryFactory f2 = new GeometryFactory();
        LineString lon180 = f2.createLineString(new Coordinate[] {
                new Coordinate(180, 90), new Coordinate(180, -90) });

        GeometryFactory f3 = new GeometryFactory();
        LineString lon180x = f3.createLineString(new Coordinate[] {
                new Coordinate(-180, 90), new Coordinate(-180, -90) });

        return g.crosses(lon180) || g.crosses(lon180x);
    }

    /**
     * get the CCFP Text's distance and direction from the Centroid
     * 
     * @param ctxt
     *            : the Coordinate of the Text
     * @param sig
     *            : the Sigmet
     * @return: double array containing direction first element, distance second
     */
    public static double[] getCcfpTxtAziDir(Coordinate ctxt, Sigmet sig) {

        if (ctxt == null)
            return null;

        Coordinate c = getSigCentroid(sig);

        if (c == null)
            return null;

        double[] d = new double[2];

        GeodeticCalculator gc = new GeodeticCalculator(PgenSession
                .getInstance().getPgenResource().getCoordinateReferenceSystem());// DefaultEllipsoid.WGS84);

        try {
            gc.setStartingGeographicPoint(c.x, c.y);
            gc.setDestinationGeographicPoint(ctxt.x, ctxt.y);
            d[0] = gc.getAzimuth();
            d[1] = gc.getOrthodromicDistance();
        } catch (Exception e) {
            System.out.println("___________ Exception: " + e.getMessage());
        }

        // save the Arrow head and Text Coordinates: required by vgf-xml
        // converter
        saveArrwTxtPts(ctxt, c, sig);

        return d;// new double[]{gc.getAzimuth(), gc.getOrthodromicDistance()};
    }

    /**
     * check if a given point is inside the Area of the given Sigmet
     * 
     * @param sig
     *            : the Sigmet with Area
     * @param pts
     *            : the Point's Coordinate
     * @return: true: point inside the Sigmet Area; false otherwise
     */
    public static boolean isPtsInArea(AbstractSigmet sig, Coordinate pts) {
        if (sig == null || sig.getLinePoints() == null
                || sig.getLinePoints().length < 3 || pts == null) {
            return false;
        }

        Coordinate[] c = new Coordinate[sig.getLinePoints().length + 1];
        c = Arrays.copyOf(sig.getLinePoints(), c.length);
        c[c.length - 1] = c[0];

        GeometryFactory f = new GeometryFactory();

        return f.createPolygon(f.createLinearRing(c), null).contains(
                new GeometryFactory().createPoint(pts));
    }

    /**
     * 
     * @param sig
     * @param md
     * @return
     */
    public static Coordinate getSigCentroid2(Sigmet sig, IMapDescriptor md) {

        if (sig == null || sig.getLinePoints() == null
                || sig.getLinePoints().length < 2)
            return null;

        Coordinate[] sigCoors = sig.getLinePoints();
        double[][] list = PgenUtil.latlonToPixel(sigCoors, md);

        double x = 0, y = 0;

        for (double[] dd : list) {
            x += dd[0];
            y += dd[1];
        }

        double avgX = x / sigCoors.length;
        double avgY = y / sigCoors.length;

        double[] ptw = md.pixelToWorld(new double[] { avgX, avgY });

        return new Coordinate(ptw[0], ptw[1]);
    }

    /**
     * check if a given point is inside the Area of the given Sigmet
     * 
     * @param sig
     *            : the Sigmet with Area
     * @param pts
     *            : the Point's Coordinate
     * @return: true: point inside the Sigmet Area; false otherwise
     */
    public static boolean isPtsInArea2(AbstractSigmet sig, Coordinate pts,
            IMapDescriptor md) {

        if (sig == null || sig.getLinePoints() == null
                || sig.getLinePoints().length < 3 || pts == null) {
            return false;
        }

        Coordinate[] sigCoors = sig.getLinePoints();
        double[][] list = PgenUtil.latlonToPixel(sigCoors, md);

        TreeSet<Double> xset = new TreeSet<Double>(), yset = new TreeSet<Double>();

        for (double[] dd : list) {
            xset.add(dd[0]);
            yset.add(dd[1]);
        }

        double[][] dpts = PgenUtil.latlonToPixel(new Coordinate[] { pts }, md);

        return (dpts[0][0] >= xset.first()) && (dpts[0][0] <= xset.last())
                && (dpts[0][1] >= yset.first()) && (dpts[0][1] <= yset.last());
    }

    /**
     * Save the CCFP Text and Arrow End Points to the Sigmet
     * 
     * @param ctxt
     *            : Text Coordinate;
     * @param cArrw
     *            : Arrow head Coordinate:
     * @param sig
     *            : The Sigmet element.
     */
    private static void saveArrwTxtPts(Coordinate cTxt, Coordinate cArrw,
            Sigmet sig) {

        try {

            // Arrow head lon-x, and lat-y
            sig.setEditableAttrLevelInfo2("" + cArrw.x);
            sig.setEditableAttrLevelInfo1("" + cArrw.y);

            // Text lon-x, and lat-y
            sig.setEditableAttrLevelText2("" + cTxt.x);
            sig.setEditableAttrLevelText1("" + cTxt.y);

        } catch (Exception e) {
            System.out.println("___________ Exception: " + e.getMessage());
        }

    }

    /**
     * calculate Text Arrow Azimuth and distance; for the convenience of CCFP
     * vgf-xml converter.
     * 
     * @param sigmet
     */
    public static void calAziDist4TxtArrw(Sigmet sigmet) {

        if (sigmet == null)
            return;

        String tLon = sigmet.getEditableAttrLevelText2();
        String tLat = sigmet.getEditableAttrLevelText1();
        if (tLon == null || tLat == null || tLon.isEmpty() || tLat.isEmpty())
            return;

        double[] ad = null;

        try {

            double dLon = Double.parseDouble(tLon);
            double dLat = Double.parseDouble(tLat);

            ad = CcfpInfo.getCcfpTxtAziDir(new Coordinate(dLon, dLat), sigmet);

        } catch (Throwable t) {
            System.out.println("___________ Error: " + t.getMessage());
        }

        if (ad == null)
            return;

        sigmet.setEditableAttrFreeText("" + ad[0] + TEXT_SEPERATOR + ad[1]);// ":::"+ad[1]);
        sigmet.setEditableAttrFromLine(PGEN_TYPE_CCFP);

    }

    /**
     * check if the CCFP is with Text Arrow fields
     * 
     * @param sigmet
     * @return
     */
    public static boolean isTxtArrwExst(Sigmet sigmet) {

        if (sigmet == null)
            return false;

        String fline = sigmet.getEditableAttrFromLine();
        String ftext = sigmet.getEditableAttrFreeText();

        if (fline != null && (!fline.isEmpty()) && ftext != null
                && (!ftext.isEmpty())) {

            if (PGEN_TYPE_CCFP.equalsIgnoreCase(fline.trim())
                    && ftext.contains(TEXT_SEPERATOR))
                return true;
        }

        return false;
    }

    public static String[] getCcfpTxt2(Sigmet sig) {// used @239
        // TTR 751 - when top > 400, display as ">400" also remove
        // leading/trailing white spaces.
        // For growth - display whatever it is.

        String tops = sig.getEditableAttrPhenom2();
        String gwth = sig.getEditableAttrPhenomLon();

        if (tops.contains("-"))
            tops = "" + tops.substring(tops.indexOf("-") + 1);
        else if (tops.contains("+"))
            tops = ">" + tops.replace("+", "");

        // if (gwth.contains("+"))
        // tops = tops + " " + gwth;
        // else
        // tops = tops + "   ";

        tops = tops + " " + gwth;

        return new String[] { tops.trim() };
    }
}
