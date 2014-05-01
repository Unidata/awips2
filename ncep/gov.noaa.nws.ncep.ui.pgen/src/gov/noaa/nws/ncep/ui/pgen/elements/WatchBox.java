/*
 * gov.noaa.nws.ncep.ui.pgen.elements.WatchBox
 * 
 * 5 December 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IWatchBox;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Implementation of Pgen watch box element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#159		B. Yin   	Initial Creation.
 * 03/10		#159		B. Yin   	Added issued watch info.	
 * 03/10        #137        Q. Zhou     Add condition for standalone VOR
 * 09/10		?			B. Yin		Added status history
 * 04/11		?			B. Yin		handle counties that belongs to more than one WFOs
 * 04/11		?			B. Yin		Use Geometry instead of MultiPolygon for county shapes
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 07/11        #450        G. Hull     NcPathManager
 * 03/12		?			B. Yin		Added another county cluster table(permclust)
 * 04/13        #977        S. Gilbert  PGEN Database support
 * 12/13		TTR904		B. Yin		Increased county name column to 17 characters
 * 12/13   		TTR800		B. Yin		Added original county list
 * </pre>
 * 
 * @author B. Yin
 */
@ElementOperations({ Operation.COPY_MOVE, Operation.EXTRAPOLATE })
public class WatchBox extends MultiPointElement implements IWatchBox {

    /*
     * default half width for watch box in statute miles.
     */
    public final static float HALF_WIDTH = 60f;

    // State names
    private static HashMap<String, String> stateName;

    public static enum WatchShape {
        NS, EW, ESOL
    };

    public static String dirs[] = { "N", "NNE", "NE", "ENE", "E", "ESE", "SE",
            "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N" };

    private WatchShape boxShape;

    private String symbolType;

    private float symbolWidth;

    private double symbolSize;

    private boolean fillFlag;

    private Color fillColor;

    private Station anchors[];

    private List<SPCCounty> countyList;
    private List<SPCCounty> originalCountyList;

    // Watch Issue Information
    private String issueStatus;

    private Calendar issueTime;

    private Calendar expTime;

    private String severity;

    private String timeZone;

    private float hailSize;

    private int gust;

    private int top;

    private int moveDir;

    private int moveSpeed;

    private String statesIncl;

    private String adjAreas;

    private int replWatch;

    private String contWatch;

    // issue flag: issued = 1; canceled = -1; un-issued = 0;
    private int issueFlag;

    private String watchType;

    private String forecaster;

    private int watchNumber;

    private String endPointAnc;

    private String endPointVor;

    private int halfWidthSm;

    private int halfWidthNm;

    private int wathcAreaNm;

    private String cntyInfo;

    private String dataURI;

    // Watch Status Info
    private List<WatchStatus> statusHistory;

    public WatchBox() {
        anchors = new Station[2];
        countyList = new ArrayList<SPCCounty>();
        issueFlag = 0;
    }

    @Override
    /**
     * Update the watch box by the input attributes
     */
    public void update(IAttribute iattr) {
        if (iattr instanceof IWatchBox) {
            IWatchBox attr = (IWatchBox) iattr;
            setColors(attr.getColors());
            fillFlag = attr.getFillFlag();
            fillColor = attr.getFillColor();
            boxShape = attr.getWatchBoxShape();
            symbolType = attr.getWatchSymbolType();
            symbolWidth = attr.getWatchSymbolWidth();
            symbolSize = attr.getWatchSymbolSize();
        }
    }

    @Override
    /**
     * Make a copy of the watch box.
     * County list is NOT copied
     */
    public AbstractDrawableComponent copy() {
        /*
         * create a new Line object and initially set its attributes to this
         * one's
         */
        WatchBox newWatchBox = new WatchBox();

        newWatchBox.setAnchors(anchors[0], anchors[1]);
        newWatchBox.update(this);

        /*
         * new Coordinates points are created and set, so we don't just set
         * references
         */
        ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
        for (int i = 0; i < this.getPoints().size(); i++) {
            ptsCopy.add(new Coordinate(this.getPoints().get(i)));
        }
        newWatchBox.setPoints(ptsCopy);

        /*
         * new colors are created and set, so we don't just set references
         */
        Color[] colorCopy = new Color[this.getColors().length];
        for (int i = 0; i < this.getColors().length; i++) {
            colorCopy[i] = new Color(this.getColors()[i].getRed(),
                    this.getColors()[i].getGreen(),
                    this.getColors()[i].getBlue());
        }
        newWatchBox.setColors(colorCopy);
        if (fillColor != null) {
            newWatchBox.setFillColor(new Color(fillColor.getRed(), fillColor
                    .getGreen(), fillColor.getBlue()));
        }
        /*
         * new Strings are created for Type and LinePattern
         */
        newWatchBox.setPgenCategory(new String(this.getPgenCategory()));
        newWatchBox.setPgenType(new String(this.getPgenType()));
        newWatchBox.setParent(this.getParent());
        newWatchBox.setWatchBoxShape(boxShape);
        newWatchBox.setWatchSymbolType(symbolType);
        newWatchBox.setWatchSymbolSize(symbolSize);
        newWatchBox.setWatchSymbolWidth(symbolWidth);
        
    	newWatchBox.setIssueStatus(getIssueStatus());
        newWatchBox.setWatchNumber(watchNumber);
        newWatchBox.setIssueFlag(getIssueFlag());
        newWatchBox.setForecaster(forecaster);
		newWatchBox.setIssueTime(getIssueTime());
        newWatchBox.setExpTime(this.getExpTime());
		newWatchBox.setSeverity(this.getSeverity());
		newWatchBox.setTimeZone(this.getTimeZone());
		newWatchBox.setHailSize(this.getHailSize());
		newWatchBox.setGust(this.getGust());
		newWatchBox.setTop(this.getTop());
		newWatchBox.setMoveDir(this.getMoveDir());
		newWatchBox.setMoveSpeed(this.getMoveSpeed());
		newWatchBox.setStatesIncl(this.getStatesIncl());
		newWatchBox.setAdjAreas( this.getAdjAreas() );
		newWatchBox.setWatchType(getWatchType());
		
		newWatchBox.setEndPointAnc(this.getEndPointAnc());
		newWatchBox.setEndPointVor(this.getEndPointVor());
		newWatchBox.setHalfWidthSm(this.getHalfWidthSm());
		newWatchBox.setHalfWidthNm(this.getHalfWidthNm());
		newWatchBox.setWathcAreaNm(this.getWathcAreaNm());
		newWatchBox.setCntyInfo(this.getCntyInfo());
		
		newWatchBox.setContWatch( this.getContWatch());
		newWatchBox.setReplWatch(this.getReplWatch());
        
		newWatchBox.setCountyList(new ArrayList<SPCCounty>( this.getCountyList()));
		if ( this.getOriginalCountyList() != null ){
			newWatchBox.setOriginalCountyList( this.getOriginalCountyList());
		}
		
        return newWatchBox;
    }

    @Override
    /**
     * Get the color to fill county maps
     */
    public Color getFillColor() {
        return fillColor;
    }

    /**
     * Set the color to fill county maps
     * 
     * @param color
     */
    public void setFillColor(Color color) {
        this.fillColor = color;
    }

    @Override
    /**
     * Get the shape of the watch box
     */
    public WatchShape getWatchBoxShape() {
        return boxShape;
    }

    /**
     * Set shape of the watch box
     * 
     * @param ws
     */
    public void setWatchBoxShape(WatchShape ws) {
        boxShape = ws;
    }

    public void setWatchBoxShape(String ws) {
        boxShape = WatchShape.valueOf(ws);
    }

    /**
     * Set the anchors for the watch box
     * 
     * @param anchor1
     * @param anchor2
     */
    public void setAnchors(Station anchor1, Station anchor2) {
        anchors[0] = anchor1;
        anchors[1] = anchor2;
    }

    @Override
    /**
     * Return the anchor array of the watch box
     */
    public Station[] getAnchors() {

        return anchors;

    }

    /**
     * Return half width of the watch box in meters
     * 
     * @return
     */
    public double getHalfWidth() {

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        gc.setStartingGeographicPoint(linePoints.get(0).x, linePoints.get(0).y);
        gc.setDestinationGeographicPoint(linePoints.get(1).x,
                linePoints.get(1).y);

        return gc.getOrthodromicDistance();
    }

    /**
     * Return the watch specification information.
     * 
     * @return
     */
    public String getSpec() {
        String spec;

        spec = String.format("%1$5.2f%2$10.2f", linePoints.get(0).y,
                linePoints.get(0).x)
                + "   "
                + getRelative(linePoints.get(0), anchors[0])
                + "   "
                + getRelative(linePoints.get(0),
                        getNearestVor(linePoints.get(0)))
                + "\n"
                + String.format("%1$5.2f%2$10.2f", linePoints.get(4).y,
                        linePoints.get(4).x)
                + "   "
                + getRelative(linePoints.get(4), anchors[1])
                + "   "
                + getRelative(linePoints.get(4),
                        getNearestVor(linePoints.get(4)))
                + "\n"
                + "ORIENT: "
                + boxShape.toString()
                + "  - HALF WIDTH: "
                + String.format("%1$4.0f", getHalfWidth() / PgenUtil.SM2M)
                + "sm ("
                + Math.round(getHalfWidth() / PgenUtil.NM2M / 5.0)
                * 5
                + " nm)\n"
                + "AREA(sq nautical miles):  "
                + String.format("%1$-8.0f", getWatchArea()) + "\n";

        return spec;
    }

    /**
     * Return a string that indicates the relative location from the input
     * location to the input station
     * 
     * @param pt
     * @param st
     * @return
     */
    public String getRelative(Coordinate pt, Station st) {

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude());
        gc.setDestinationGeographicPoint(pt.x, pt.y);

        long dist = Math.round(gc.getOrthodromicDistance() / PgenUtil.SM2M);
        long dir = Math.round(gc.getAzimuth());
        if (dir < 0)
            dir += 360;
        String str = String.format("%1$4d%2$5s%3$5s", dist,
                dirs[(int) Math.round(dir / 22.5)], st.getStid());
        // String str = dist + " " + dirs[(int)Math.round(dir/22.5)]+ " " +
        // st.getStid() ;
        return str;
    }

    /**
     * Return the nearest VOr from the input location
     * 
     * @param loc
     * @return
     */
    public Station getNearestVor(Coordinate loc) {

        return PgenStaticDataProvider.getProvider().getVorTbl()
                .getNearestStation(loc);

    }

    /**
     * Return the area of the watch box (nautical sq. miles)
     * 
     * @return
     */
    public double getWatchArea() {
        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        gc.setStartingGeographicPoint(linePoints.get(0).x, linePoints.get(0).y);
        gc.setDestinationGeographicPoint(linePoints.get(4).x,
                linePoints.get(4).y);

        double base = gc.getOrthodromicDistance();

        Coordinate intrPt = new Coordinate();
        getDistanceFromLine(linePoints.get(1), linePoints.get(0),
                linePoints.get(4), intrPt);

        gc.setStartingGeographicPoint(linePoints.get(1).x, linePoints.get(1).y);
        gc.setDestinationGeographicPoint(intrPt.x, intrPt.y);

        double height = gc.getOrthodromicDistance();

        return base * height * 2.0 / (PgenUtil.NM2M * PgenUtil.NM2M);

    }

    /**
     * Calculate the distance from a point to a line
     * 
     * @param point
     *            - input location
     * @param lnPt1
     *            - first point of the line
     * @param lnPt2
     *            - second point of the line
     * @param intrsctPt
     *            - the nearest point on the line
     * @return - the distance from the input point to the input line
     */
    private double getDistanceFromLine(Coordinate point, Coordinate lnPt1,
            Coordinate lnPt2, Coordinate intrsctPt) {
        if (lnPt1.x == lnPt2.x) {
            intrsctPt.x = lnPt1.x;
            intrsctPt.y = point.y;
            return Math.abs(point.x - lnPt1.x);
        } else if (lnPt1.y == lnPt2.y) {
            intrsctPt.x = point.x;
            intrsctPt.y = lnPt1.y;
            return Math.abs(point.y - lnPt1.y);
        } else {
            double mi = (lnPt2.y - lnPt1.y) / (lnPt2.x - lnPt1.x);
            double bi = lnPt1.y - mi * lnPt1.x;
            double m = -1.0 / mi;
            double b = point.y - m * point.x;
            intrsctPt.x = (b - bi) / (mi - m);
            intrsctPt.y = m * (intrsctPt.x) + b;
            double d = (intrsctPt.x - point.x) * (intrsctPt.x - point.x)
                    + (intrsctPt.y - point.y) * (intrsctPt.y - point.y);
            return Math.sqrt(d);
        }
    }

    /**
     * Add the input county in the county list
     * 
     * @param cnty
     */
    public void addCounty(SPCCounty cnty) {
        countyList.add(cnty);
    }

    public void removeCounty(SPCCounty cnty) {
        countyList.remove(cnty);
    }

    /**
     * Get the county list
     */
    public List<SPCCounty> getCountyList() {
        return countyList;
    }

    /**
     * Make a copy of the input list and set the county to it
     * 
     * @param cl
     */
    public void setCountyList(List<SPCCounty> cl) {
        List<SPCCounty> newList = new ArrayList<SPCCounty>();
        for (SPCCounty cnty : cl) {
            newList.add(cnty);
        }
        countyList = newList;
    }

    /**
     * Clear the county list
     */
    public void clearCntyList() {
        countyList.clear();
    }

    /**
     * Get the symbol type for counties
     */
    public String getWatchSymbolType() {
        return symbolType;
    }

    /**
     * Get width of the symbol for counties
     */
    public float getWatchSymbolWidth() {
        return symbolWidth;
    }

    /**
     * Get size of the symbol for counties
     */
    public double getWatchSymbolSize() {
        return symbolSize;
    }

    /**
     * Set the symbol type for counties
     * 
     * @param str
     */
    public void setWatchSymbolType(String str) {
        symbolType = str;
    }

    /**
     * Set the width of the symbol for counties
     * 
     * @param width
     */
    public void setWatchSymbolWidth(float width) {
        symbolWidth = width;
    }

    /**
     * Set the size of the symbol for counties
     * 
     * @param size
     */
    public void setWatchSymbolSize(double size) {
        symbolSize = size;
    }

    /**
     * Return a list of WFOs(String) which are in the watch box
     * 
     * @return
     */
    public List<String> getWFOs() {
        ArrayList<String> wfos = new ArrayList<String>();

        if (countyList != null && !countyList.isEmpty()) {
            for (SPCCounty cnty : countyList) {

                String wfo = cnty.getWfo();
                // wfo can be more than one
                if (wfo != null) {
                    for (int ii = 0; ii < wfo.length(); ii += 3) {
                        String wfoStr = wfo.substring(ii,
                                wfo.length() > ii + 3 ? ii + 3 : wfo.length());
                        if (!wfos.contains(wfoStr))
                            wfos.add(wfoStr);
                    }
                }

            }
        }

        return wfos;
    }

    /**
     * Return a list of state names(String), which are in the watch box
     * 
     * @return
     */
    public List<String> getStates() {
        ArrayList<String> states = new ArrayList<String>();
        if (countyList != null && !countyList.isEmpty()) {
            for (SPCCounty cnty : countyList) {

                if (cnty.getState() != null
                        && !states.contains(cnty.getState())) {
                    states.add(cnty.getState());
                    // System.out.println(obj[2]+"...");

                }

            }
        }

        return states;
    }

    /**
     * remove all counties of a state from the county list
     * 
     * @param state
     */
    public void removeState(String state) {
        if (countyList != null && !countyList.isEmpty()) {
            Iterator<SPCCounty> it = countyList.iterator();
            while (it.hasNext()) {
                SPCCounty cnty = it.next();
                if (cnty.getState() != null
                        && cnty.getState().equalsIgnoreCase(state)) {
                    it.remove();
                }
            }
        }
    }

    /**
     * Remove all counties of an CWA from the county list
     * 
     * @param cwa
     */
    public void removeCwa(String cwa) {
        if (countyList != null && !countyList.isEmpty()) {
            Iterator<SPCCounty> it = countyList.iterator();
            while (it.hasNext()) {
                SPCCounty cnty = it.next();
                if (cnty.getWfo() != null
                        && cnty.getWfo().equalsIgnoreCase(cwa)) {
                    it.remove();
                }
            }
        }
    }

    /**
     * Add all counties of an CWA into the county list of the watch box
     * 
     * @param cwa
     */
    public void addCwa(String cwa) {
        List<SPCCounty> allCounties = PgenStaticDataProvider.getProvider()
                .getSPCCounties();

        if (allCounties != null) {
            for (SPCCounty cnty : allCounties) {
                if (cnty.getWfo() != null
                        && cnty.getWfo().equalsIgnoreCase(cwa)
                        && !countyList.contains(cnty)) {
                    countyList.add(cnty);
                }
            }
        }
    }

    /**
     * @param fillFlag
     *            the fillFlag to set
     */
    public void setFillFlag(boolean fillFlag) {
        this.fillFlag = fillFlag;
    }

    /**
     * @return the fillFlag
     */
    public boolean getFillFlag() {
        return fillFlag;
    }

    /**
     * get a list of inactive counties inside of any polygon of the active
     * county union
     * 
     * @return
     */
    public List<SPCCounty> getInactiveCountiesInWB() {

        if (countyList == null || countyList.isEmpty()) {
            return null;
        }

        List<SPCCounty> rt = new ArrayList<SPCCounty>();
        Geometry union = getCountyUnion();
        GeometryFactory gf = new GeometryFactory();

        // loop through all polygons in the union
        for (int ii = 0; ii < union.getNumGeometries(); ii++) {

            Polygon poly = (Polygon) union.getGeometryN(ii);

            // loop through all holes of the polygon
            for (int jj = 0; jj < poly.getNumInteriorRing(); jj++) {

                LinearRing lr = (LinearRing) poly.getInteriorRingN(jj);

                Polygon p = gf.createPolygon(lr, null);
                double area = p.getArea();

                // skip small holes
                if (area < 0.01)
                    continue;

                // Check any county centeriod inside the hole
                for (SPCCounty cnty : PgenStaticDataProvider.getProvider()
                        .getSPCCounties()) {

                    if (p.contains(gf.createPoint(cnty.getCentriod()))) {
                        rt.add(cnty);
                    }

                }
            }
        }

        return rt;
    }

    /**
     * Get a list of active counties outside of the main watch box
     * 
     * @return
     */
    public List<SPCCounty> getActiveCountiesOutsideWB() {

        if (countyList == null || countyList.isEmpty()) {
            return null;
        }

        // initialization
        List<SPCCounty> rt = new ArrayList<SPCCounty>();
        Geometry union = getCountyUnion();
        GeometryFactory gf = new GeometryFactory();
        Polygon largestPoly = null;

        // get the largest polygon in the county union
        for (int ii = 0; ii < union.getNumGeometries(); ii++) {
            Polygon poly = (Polygon) union.getGeometryN(ii);
            if (largestPoly == null) {
                largestPoly = poly;
            } else if (poly.getArea() > largestPoly.getArea()) {
                largestPoly = poly;
            }

        }

        // loop through the active county list and check if
        // any county is outside of the largest polygon
        for (SPCCounty cnty : countyList) {

            if (!largestPoly.contains(gf.createPoint(cnty.getCentriod()))) {
                rt.add(cnty);
            }

        }

        return rt;
    }

    /**
     * Get the union polygon for all counties in the watch box
     * 
     * @return
     */
    public Geometry getCountyUnion() {

        Collection<Geometry> gCollection = new ArrayList<Geometry>();
        ;

        for (SPCCounty cnty : countyList) {
            Geometry countyGeo = cnty.getShape();
            if (countyGeo != null) {
                if (!countyGeo.isValid()) {
                    System.out.println("Invalid county: " + cnty.getFips());
                }
                gCollection.add(countyGeo.buffer(.04));
            }

        }

        GeometryFactory gf = new GeometryFactory();

        GeometryCollection geometryCollection = (GeometryCollection) gf
                .buildGeometry(gCollection);

        return geometryCollection.union();

    }

    public String getBoxShape() {
        return boxShape.toString();
    }

    public void setIssueStatus(String issueStatus) {
        this.issueStatus = issueStatus;
    }

    public String getIssueStatus() {
        return issueStatus;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setExpTime(Calendar expTime) {
        this.expTime = expTime;
    }

    public Calendar getExpTime() {
        return expTime;
    }

    public void setSeverity(String severity) {
        this.severity = severity;
    }

    public String getSeverity() {
        return severity;
    }

    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    public String getTimeZone() {
        return timeZone;
    }

    public void setHailSize(float hailSize) {
        this.hailSize = hailSize;
    }

    public float getHailSize() {
        return hailSize;
    }

    public void setGust(int gust) {
        this.gust = gust;
    }

    public int getGust() {
        return gust;
    }

    public void setTop(int top) {
        this.top = top;
    }

    public int getTop() {
        return top;
    }

    public void setMoveDir(int moveDir) {
        this.moveDir = moveDir;
    }

    public int getMoveDir() {
        return moveDir;
    }

    public void setMoveSpeed(int moveSpeed) {
        this.moveSpeed = moveSpeed;
    }

    public int getMoveSpeed() {
        return moveSpeed;
    }

    public void setStatesIncl(String statesIncl) {
        this.statesIncl = statesIncl;
    }

    public String getStatesIncl() {
        return statesIncl;
    }

    public void setAdjAreas(String adjAreas) {
        this.adjAreas = adjAreas;
    }

    public String getAdjAreas() {
        return adjAreas;
    }

    public void setReplWatch(int replWatch) {
        this.replWatch = replWatch;
    }

    public int getReplWatch() {
        return replWatch;
    }

    public void setIssueFlag(int issueFlag) {
        this.issueFlag = issueFlag;
    }

    public int getIssueFlag() {
        return issueFlag;
    }

    public void setWatchType(String watchType) {
        this.watchType = watchType;
    }

    public String getWatchType() {
        return watchType;
    }

    public void setForecaster(String forecaster) {
        this.forecaster = forecaster;
    }

    public String getForecaster() {
        return forecaster;
    }

    public void setWatchNumber(int watchNumber) {
        this.watchNumber = watchNumber;
    }

    public int getWatchNumber() {
        return watchNumber;
    }

    public String convert2Text() {

        String msg = "";
        msg += String.format("WATCH NUMBER:\t\t%1$s\n", getWatchNumber());
        msg += String.format("TYPE:\t\t\t%1$s\n", getWatchType());
        msg += String.format("PDS/Normal:\t\t%1$s\n", getSeverity());
        msg += "ISSUE TIME:\t\tXX XX XXXX XXXX\n";
        msg += "VALID TIME:\t\tXX XX XXXX XXXX\n";
        msg += String.format("EXPIRATION TIME:\t%1$tm %1$td %1$tY %1$tY\n",
                getExpTime());
        msg += String.format("ENDPOINT (ANC,sm):\t%1$s - %2$s\n",
                getRelative(getPoints().get(0), getAnchors()[0]).trim(),
                getRelative(getPoints().get(4), getAnchors()[1]).trim());
        msg += String.format(
                "ENDPOINT (VOR,nm):\t%1$s - %2$s\n",
                getRelative(getPoints().get(0),
                        getNearestVor(getPoints().get(0))).trim(),
                getRelative(getPoints().get(4),
                        getNearestVor(getPoints().get(4))).trim());
        msg += String.format("ATTRIB (ANC,sm):\t%1$-4.0f\n", getHalfWidth()
                / PgenUtil.SM2M);
        msg += String.format("ATTRIB (VOR,nm):\t%1$-4d\n",
                Math.round(getHalfWidth() / PgenUtil.NM2M / 5.0) * 5);
        msg += String.format("WATCH CORNER POINT:\t%1$-6.2f %2$-6.2f\n",
                getPoints().get(1).y, getPoints().get(1).x);
        msg += String.format("WATCH CORNER POINT:\t%1$-6.2f %2$-6.2f\n",
                getPoints().get(3).y, getPoints().get(3).x);
        msg += String.format("WATCH CORNER POINT:\t%1$-6.2f %2$-6.2f\n",
                getPoints().get(5).y, getPoints().get(5).x);
        msg += String.format("WATCH CORNER POINT:\t%1$-6.2f %2$-6.2f\n",
                getPoints().get(7).y, getPoints().get(7).x);
        msg += String.format("HAIL SIZE (in):\t\t%1$-4.2f\n", getHailSize());
        msg += String.format("MAX GUSTS (kts):\t%1$-4d\n", getGust());
        msg += String.format("MAX TOPS (100s ft):\t%1$-6d\n", getTop());
        msg += String.format("MOTION (deg,kts):\t%1$-4d %2$-4d\n",
                getMoveDir(), getMoveSpeed());
        msg += String.format("TIME ZONE:\t\t%1$s\n", getTimeZone());
        msg += String.format("REPL WATCH NUMBER:\t%1$s\n", getReplWatch());
        msg += String.format("STATES INCLUDED:\t%1$s\n", getStatesIncl());
        msg += String.format("STATUS:\t\t\t%1$s\n", getIssueStatus());
        msg += String.format("FORECASTER:\t\t%1$s\n", getForecaster());
        msg += String.format("WATCH AREA (sq nm):\t%1$-8.0f\n", getWatchArea());
        msg += "UGC   State County Name   Lat/Long    CntyFips WFO\n";
        msg += formatCountyInfo(getCountyList());

        return msg;
    }

    public String generateWOU() {

        String res = "";
        String wbFile = "ww0002.xml";
        String xsltFile = "wou.xlt";

        /*
         * Convert XML string into xmlSourse
         */
        Source xmlSource = new StreamSource(wbFile);

        /*
         * Construct xsltSource from xslfFile
         */
        Source xsltSource = new StreamSource(xsltFile);

        /*
         * Use the factory for XSLT transformer
         */
        TransformerFactory transFact = TransformerFactory.newInstance();
        try {
            Transformer trans = transFact.newTransformer(xsltSource);

            /*
             * Create object for the transformation product
             */
            ByteArrayOutputStream baos = new ByteArrayOutputStream();

            trans.transform(xmlSource, new StreamResult(baos));
            /*
             * Convert transformation product to string
             */
            res = new String(baos.toByteArray());
        } catch (Exception e) {
            /*
             * Catch invalid control characters in the report
             */
            e.printStackTrace();

        }

        System.out.print(res);

        return res;
    }

    /**
     * Check if there are status lines for the watch element
     */
    public boolean hasStatusLine() {

        AbstractDrawableComponent adc = this.getParent();
        if (adc instanceof DECollection
                && adc.getName().equalsIgnoreCase("Watch")) {
            Iterator<DrawableElement> it = ((DECollection) adc)
                    .createDEIterator();
            while (it.hasNext()) {
                DrawableElement de = it.next();
                if (de instanceof Line
                        && de.getPgenType().equalsIgnoreCase("POINTED_ARROW")) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Save the watch to an XML file
     * 
     * @param filename
     */
    public void saveToFile(String filename) {

        Layer defaultLayer = new Layer();
        // add watch collection(box and status line)
        defaultLayer.addElement(this.getParent());

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

        Layer defaultLayer = new Layer();
        //defaultLayer.addElement(this.getParent());
        
         DECollection dec = new DECollection("Watch");
		 dec.setPgenType("WatchBox");
		 dec.setPgenCategory("MET");
		 dec.add( this.copy() );
		 defaultLayer.addElement(dec);
		 
        
        ArrayList<Layer> layerList = new ArrayList<Layer>();
        layerList.add(defaultLayer);

        ProductTime refTime = new ProductTime(getIssueTime());

        Product defaultProduct = new Product("WatchBox", "WATCHBOX", forecaster, null,
                refTime, layerList);

        defaultProduct.setOutputFile(label);
        defaultProduct.setCenter(PgenUtil.getCurrentOffice());

        try {
            dataURI = StorageUtils.storeProduct(defaultProduct, true);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
            return null;
        }

        return dataURI;
    }

    public void setContWatch(String contWatch) {
        this.contWatch = contWatch;
    }

    public String getContWatch() {
        return contWatch;
    }

    public void setEndPointAnc(String endPointAnc) {
        this.endPointAnc = endPointAnc;
    }

    public String getEndPointAnc() {
        return endPointAnc;
    }

    public void setEndPointVor(String endPointVor) {
        this.endPointVor = endPointVor;
    }

    public String getEndPointVor() {
        return endPointVor;
    }

    public void setHalfWidthSm(int halfWidthSm) {
        this.halfWidthSm = halfWidthSm;
    }

    public int getHalfWidthSm() {
        return halfWidthSm;
    }

    public void setHalfWidthNm(int halfWidthNm) {
        this.halfWidthNm = halfWidthNm;
    }

    public int getHalfWidthNm() {
        return halfWidthNm;
    }

    public void setWathcAreaNm(int wathcAreaNm) {
        this.wathcAreaNm = wathcAreaNm;
    }

    public int getWathcAreaNm() {
        return wathcAreaNm;
    }

    public void setCntyInfo(String cntyInfo) {
        this.cntyInfo = cntyInfo;
    }

    public String getCntyInfo() {
        return cntyInfo;
    }

    // public void setFromLine(String fromLine) {
    // this.fromLine = fromLine;
    // }

    public String getFromLine() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            return statusHistory.get(statusHistory.size() - 1).fromLine;
        } else
            return null;
    }

    // public void setDiscussion(int discussion) {
    // this.discussion = discussion;
    // }

    public int getDiscussion() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            return statusHistory.get(statusHistory.size() - 1).discussion;
        } else
            return 0;
    }

    // public void setStatusValidTime(Calendar statusValidTime) {
    // this.statusValidTime = statusValidTime;
    // }

    public Calendar getStatusValidTime() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            return statusHistory.get(statusHistory.size() - 1).statusValidTime;
        } else
            return null;
    }

    // public void setStatusExpTime(Calendar statusExpTime) {
    // this.statusExpTime = statusExpTime;
    // }

    public Calendar getStatusExpTime() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            return statusHistory.get(statusHistory.size() - 1).statusExpTime;
        } else
            return null;
    }

    // public void setStatusForecaster(String statusForecaster) {
    // if ( statusForecaster != null )
    // this.statusForecaster = statusForecaster.toUpperCase();
    // }

    public String getStatusForecaster() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            return statusHistory.get(statusHistory.size() - 1).statusForecaster;
        } else
            return null;
    }

    public String getDataURI() {
        return dataURI;
    }

    /**
     * Snap the input location onto the input anchor point This function takes
     * an input location and an anchor point, returns a location along a 16-pt
     * compass direction with a distance rounded to the nearest 5 statute miles
     * from the anchor point.
     * 
     * @param anchor
     * @param point
     * @return
     */
    public static Coordinate snapOnAnchor(Station anchor, Coordinate point) {

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
        gc.setStartingGeographicPoint(anchor.getLongitude(),
                anchor.getLatitude());
        gc.setDestinationGeographicPoint(point.x, point.y);

        double dis = Math
                .round((float) (gc.getOrthodromicDistance() / PgenUtil.SM2M) / 5) * 5.0;
        double angle = Math.round(gc.getAzimuth() / 22.5) * 22.5;

        gc.setDirection(angle, dis * PgenUtil.SM2M);
        Point2D pt1 = gc.getDestinationGeographicPoint();

        return new Coordinate(pt1.getX(), pt1.getY());

    }

    /**
     * Get the nearest anchor points from the input location
     * 
     * @param pt
     *            - input location
     * @param anchorList
     *            - a list of anchor points
     * @return - the nearest acnhor point
     */
    public static Station getNearestAnchorPt(Coordinate pt,
            List<Station> anchorList) {

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
        gc.setStartingGeographicPoint(pt.x, pt.y);

        double dist = 0;
        double minDist = -1;
        Station anchor = null;
        ;

        for (Station stn : anchorList) {
            gc.setDestinationGeographicPoint(stn.getLongitude(),
                    stn.getLatitude());

            try {
                dist = gc.getOrthodromicDistance();
            } catch (Exception e) {
                dist = Double.MAX_VALUE;
            }

            if (minDist < 0 || dist < minDist) {
                minDist = dist;
                anchor = stn;
            }
        }

        return anchor;
    }

    /**
     * Generate a watch box's eight points from the two input locations, half
     * width of the watch box, and the shape of the watch box
     * 
     * @param ws
     *            : shape of the watch box
     * @param halfWidth
     *            : half width of the watch box
     * @param point1
     *            : first input location
     * @param point2
     *            : second input location
     * @return
     */
    public static ArrayList<Coordinate> generateWatchBoxPts(WatchShape ws,
            double halfWidth, Coordinate point1, Coordinate point2) {

        ArrayList<Coordinate> watchBoxPts;

        if (ws == WatchShape.NS) {
            watchBoxPts = generateWatchBoxPts(0, halfWidth, point1, point2);
        } else if (ws == WatchShape.EW) {
            watchBoxPts = generateWatchBoxPts(90, halfWidth, point1, point2);
        } else if (ws == WatchShape.ESOL) {

            double dir;
            if (Math.abs(point1.x - point2.x) < 0.0001) {
                dir = 90;
            } else {
                dir = 180
                        - Math.atan((point2.y - point1.y)
                                / (point2.x - point1.x)) * 180 / Math.PI;
                if (dir > 180)
                    dir -= 360;

            }

            watchBoxPts = generateWatchBoxPts(dir, halfWidth, point1, point2);

        } else {
            watchBoxPts = null;
        }

        return watchBoxPts;

    }

    /**
     * Generate the eight points of a watch box from the two input locations,
     * half width, and the direction from the first point to the second point.
     * 
     * @param direction
     * @param halfWidth
     * @param point1
     * @param point2
     * @return
     */
    private static ArrayList<Coordinate> generateWatchBoxPts(double direction,
            double halfWidth, Coordinate point1, Coordinate point2) {

        // get direction from point2 to point 1
        double dir1 = direction + 180;
        if (dir1 > 180)
            dir1 -= 360;

        halfWidth = Math.round((halfWidth / PgenUtil.SM2M / 5.0)) * 5;
        halfWidth = halfWidth * PgenUtil.SM2M;

        ArrayList<Coordinate> watchBoxPts = new ArrayList<Coordinate>();

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        Coordinate pt1 = new Coordinate(point1.x, point1.y);

        gc.setStartingGeographicPoint(point1.x, point1.y);
        gc.setDirection(dir1, halfWidth);
        java.awt.geom.Point2D pt = gc.getDestinationGeographicPoint();

        Coordinate pt2 = new Coordinate(pt.getX(), pt.getY());

        gc.setDirection(direction, halfWidth);
        pt = gc.getDestinationGeographicPoint();
        Coordinate pt8 = new Coordinate(pt.getX(), pt.getY());

        gc.setStartingGeographicPoint(point2.x, point2.y);
        gc.setDirection(dir1, halfWidth);
        pt = gc.getDestinationGeographicPoint();
        Coordinate pt4 = new Coordinate(pt.getX(), pt.getY());

        Coordinate pt3 = new Coordinate((pt2.x + pt4.x) / 2,
                (pt2.y + pt4.y) / 2);

        Coordinate pt5 = new Coordinate(point2.x, point2.y);

        gc.setDirection(direction, halfWidth);
        pt = gc.getDestinationGeographicPoint();
        Coordinate pt6 = new Coordinate(pt.getX(), pt.getY());

        Coordinate pt7 = new Coordinate((pt6.x + pt8.x) / 2,
                (pt6.y + pt8.y) / 2);

        watchBoxPts.add(pt1);
        watchBoxPts.add(pt2);
        watchBoxPts.add(pt3);
        watchBoxPts.add(pt4);
        watchBoxPts.add(pt5);
        watchBoxPts.add(pt6);
        watchBoxPts.add(pt7);
        watchBoxPts.add(pt8);

        return watchBoxPts;

    }

    /**
     * Generate the points of the new watch box
     * 
     * @param ptIdx
     *            - the point being edited
     * @param loc
     *            - new location of the editing point.
     * @return - points of the new watch box
     */
    public ArrayList<Coordinate> createNewWatchBox(int ptIdx, Coordinate loc,
            WatchShape ws) {

        Coordinate newPt0 = new Coordinate();
        Coordinate newPt4 = new Coordinate();

        double newWidth = this.getNewHalfWidth(ptIdx, loc, newPt0, newPt4);
        return WatchBox.generateWatchBoxPts(ws, newWidth, newPt0, newPt4);

    }

    /**
     * Calculate the new half width of the watch box being edited. Re-compute
     * the locations of point 0 and point 4.
     * 
     * @param wb
     *            - watch box being edited.
     * @param ptIdx
     *            - the index of point being edited.
     * @param loc
     *            - the new location of the editing point.
     * @param newWbPt0
     *            - new location for point 0
     * @param newWbPt4
     *            - new location for point 4
     * @return - half width of the new watch box
     */
    private double getNewHalfWidth(int ptIdx, Coordinate loc,
            Coordinate newWbPt0, Coordinate newWbPt4) {

        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
        Coordinate newPt0 = new Coordinate();
        Coordinate newPt4 = new Coordinate();
        Coordinate newPtx = new Coordinate();

        Coordinate pt0 = this.getPoints().get(0);
        Coordinate pt4 = this.getPoints().get(4);
        Coordinate pt5 = this.getPoints().get(5);
        Coordinate pt7 = this.getPoints().get(7);
        Coordinate pt1 = this.getPoints().get(1);
        Coordinate pt3 = this.getPoints().get(3);

        double halfWidth = 0;
        Point2D pt02d;
        Point2D pt42d;

        if (ptIdx == 0) {

            gc.setStartingGeographicPoint(pt0.x, pt0.y);
            gc.setDestinationGeographicPoint(pt1.x, pt1.y);

            halfWidth = gc.getOrthodromicDistance();

            newWbPt0.x = loc.x;
            newWbPt0.y = loc.y;
            newWbPt4.x = pt4.x;
            newWbPt4.y = pt4.y;

        } else if (ptIdx == 4) {

            gc.setStartingGeographicPoint(pt0.x, pt0.y);
            gc.setDestinationGeographicPoint(pt1.x, pt1.y);

            halfWidth = gc.getOrthodromicDistance();

            newWbPt0.x = pt0.x;
            newWbPt0.y = pt0.y;
            newWbPt4.x = loc.x;
            newWbPt4.y = loc.y;

        } else if (ptIdx == 1 || ptIdx == 7) {

            if (this.getWatchBoxShape() == WatchBox.WatchShape.NS) {
                if (pt0.x == pt4.x) {
                    newPt0.y = loc.y;
                } else {
                    newPt0.y = pt4.y + (loc.x - pt4.x) * (pt4.y - pt0.y)
                            / (pt4.x - pt0.x);
                }

                newPt0.x = loc.x;

            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.EW) {
                if (pt0.y == pt4.y) {
                    newPt0.x = loc.x;
                } else {
                    newPt0.x = pt4.x + (loc.y - pt4.y) * (pt4.x - pt0.x)
                            / (pt4.y - pt0.y);
                }
                newPt0.y = loc.y;
            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.ESOL) {
                getDistanceFromLine(loc, pt0, pt4, newPt0);
            }

            gc.setStartingGeographicPoint(loc.x, loc.y);
            gc.setDestinationGeographicPoint(newPt0.x, newPt0.y);

            halfWidth = gc.getOrthodromicDistance();

            newWbPt0.x = newPt0.x;
            newWbPt0.y = newPt0.y;

            newWbPt4.x = pt4.x;
            newWbPt4.y = pt4.y;
        } else if (ptIdx == 3 || ptIdx == 5) {

            if (this.getWatchBoxShape() == WatchBox.WatchShape.NS) {
                if (pt0.x == pt4.x) {
                    newPt4.y = loc.y;
                } else {
                    newPt4.y = pt0.y + (loc.x - pt0.x) * (pt4.y - pt0.y)
                            / (pt4.x - pt0.x);
                }

                newPt4.x = loc.x;

            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.EW) {
                if (pt0.y == pt4.y) {
                    newPt4.x = loc.x;
                } else {
                    newPt4.x = pt0.x + (loc.y - pt0.y) * (pt4.x - pt0.x)
                            / (pt4.y - pt0.y);
                }
                newPt4.y = loc.y;
            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.ESOL) {
                getDistanceFromLine(loc, pt0, pt4, newPt4);
            }

            gc.setStartingGeographicPoint(loc.x, loc.y);
            gc.setDestinationGeographicPoint(newPt4.x, newPt4.y);

            halfWidth = gc.getOrthodromicDistance();

            newWbPt4.x = newPt4.x;
            newWbPt4.y = newPt4.y;

            newWbPt0.x = pt0.x;
            newWbPt0.y = pt0.y;

        } else if (ptIdx == 2) {

            if (this.getWatchBoxShape() == WatchBox.WatchShape.NS) {

                if (pt5.x == pt7.x) {
                    newPtx.y = loc.y;
                } else {
                    newPtx.y = pt5.y + (loc.x - pt5.x) * (pt7.y - pt5.y)
                            / (pt7.x - pt5.x);
                }
                newPtx.x = loc.x;

            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.EW) {
                if (pt5.y == pt7.y) {
                    newPtx.x = loc.x;
                } else {
                    newPtx.x = pt5.x + (loc.y - pt5.y) * (pt7.x - pt5.x)
                            / (pt7.y - pt5.y);
                }

                newPtx.y = loc.y;
            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.ESOL) {
                getDistanceFromLine(loc, pt5, pt7, newPtx);

            }

            gc.setStartingGeographicPoint(loc.x, loc.y);
            gc.setDestinationGeographicPoint(newPtx.x, newPtx.y);

            halfWidth = gc.getOrthodromicDistance() / 2;

            gc.setStartingGeographicPoint(pt7.x, pt7.y);
            gc.setDestinationGeographicPoint(pt1.x, pt1.y);
            double angle = gc.getAzimuth();
            gc.setDirection(angle, halfWidth);

            pt02d = gc.getDestinationGeographicPoint();
            newWbPt0.x = pt02d.getX();
            newWbPt0.y = pt02d.getY();

            gc.setStartingGeographicPoint(pt5.x, pt5.y);
            gc.setDestinationGeographicPoint(pt3.x, pt3.y);
            angle = gc.getAzimuth();
            gc.setDirection(angle, halfWidth);

            pt42d = gc.getDestinationGeographicPoint();

            newWbPt4.x = pt42d.getX();
            newWbPt4.y = pt42d.getY();
        } else if (ptIdx == 6) {

            if (this.getWatchBoxShape() == WatchBox.WatchShape.NS) {

                if (pt1.x == pt3.x) {
                    newPtx.y = loc.y;
                } else {
                    newPtx.y = pt1.y + (loc.x - pt1.x) * (pt3.y - pt1.y)
                            / (pt3.x - pt1.x);
                }
                newPtx.x = loc.x;

            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.EW) {
                if (pt1.y == pt3.y) {
                    newPtx.x = loc.x;
                } else {
                    newPtx.x = pt1.x + (loc.y - pt1.y) * (pt3.x - pt1.x)
                            / (pt3.y - pt1.y);
                }

                newPtx.y = loc.y;
            } else if (this.getWatchBoxShape() == WatchBox.WatchShape.ESOL) {
                getDistanceFromLine(loc, pt1, pt3, newPtx);

            }

            gc.setStartingGeographicPoint(loc.x, loc.y);
            gc.setDestinationGeographicPoint(newPtx.x, newPtx.y);

            halfWidth = gc.getOrthodromicDistance() / 2;

            gc.setStartingGeographicPoint(pt1.x, pt1.y);
            gc.setDestinationGeographicPoint(pt7.x, pt7.y);
            double angle = gc.getAzimuth();

            gc.setDirection(angle, halfWidth);

            pt02d = gc.getDestinationGeographicPoint();
            newWbPt0.x = pt02d.getX();
            newWbPt0.y = pt02d.getY();

            gc.setStartingGeographicPoint(pt3.x, pt3.y);
            gc.setDestinationGeographicPoint(pt5.x, pt5.y);
            angle = gc.getAzimuth();
            gc.setDirection(angle, halfWidth);

            pt42d = gc.getDestinationGeographicPoint();
            newWbPt4.x = pt42d.getX();
            newWbPt4.y = pt42d.getY();

        }

        return halfWidth;
    }

    /**
     * Get the nearby WFOs
     * 
     * @return
     */
    public List<String> getNearbyWFOs() {
        Geometry union = this.getCountyUnion();
        Geometry bUnion = union.buffer(0.01);

        List<SPCCounty> counties = new ArrayList<SPCCounty>();

        counties.addAll(PgenStaticDataProvider.getProvider()
                .getCountiesInGeometry(bUnion));
        ArrayList<String> nWFOs = new ArrayList<String>();

        List<String> wfos = this.getWFOs();
        for (SPCCounty cnty : counties) {
            String wfo = cnty.getWfo();
            // wfo can be more than one
            if (wfo != null) {
                for (int ii = 0; ii < wfo.length(); ii += 3) {
                    String wfoStr = wfo.substring(ii,
                            wfo.length() > ii + 3 ? ii + 3 : wfo.length());
                    if (!wfos.contains(wfoStr) && !nWFOs.contains(wfoStr))
                        nWFOs.add(wfoStr);
                }
            }

        }

        return nWFOs;

    }

    /**
     * Get the state name hash map
     * 
     * @return
     */
    public HashMap<String, String> getStateName() {
        return PgenStaticDataProvider.getProvider().getStateAbrvMap();
    }

    /**
     * Create the counties of one state for WCL
     * 
     * @param state
     * @param countyList
     * @return
     */
    public String createCountyInfo(String state, Calendar exp) {

        if (stateName == null)
            stateName = getStateName();

        String ugcStr = "";
        String oneLine = "";
        String counties = "";
        String cities = "";
        String waters = "";

        int lnLength = 65;

        int iCiti = 0;
        int iCnty = 0;

        for (SPCCounty county : countyList) {
            if (county.getState() != null
                    && state.equalsIgnoreCase(county.getState())) {
                if (ugcStr.isEmpty()) {
                    ugcStr = county.getUgcId();
                } else {
                    if (ugcStr.contains("\n")) {
                        oneLine = ugcStr
                                .substring(ugcStr.lastIndexOf('\n') + 1);
                    } else {
                        oneLine = ugcStr;
                    }

                    if (oneLine.length() >= lnLength - 4) {
                        ugcStr += "\n";
                    }

                    ugcStr += "-" + county.getFips().substring(2);
                }

                if (ugcStr.charAt(2) == 'Z') {
                    // for coastal waters
                    waters += "\n"
                            + county.getZoneName().toUpperCase()
                                    .replaceAll("_", " ") + "\n";
                } else if (Integer.valueOf(county.getFips().substring(2)) > 509) {
                    if (iCiti == 3) {
                        cities += "\n";
                        iCiti = 0;
                    }

                    String citi = county.getName().toUpperCase();
                    if (iCiti == 0) {
                        cities += String.format("%1$-21s",
                                citi.replaceAll("CITY OF ", ""));
                    } else {
                        cities += String.format("%1$-20s",
                                citi.replaceAll("CITY OF ", ""));
                    }
                    iCiti++;

                } else {
                    if (iCnty == 3) {
                        counties += "\n";
                        iCnty = 0;
                    }

                    if (iCnty == 0) {
                        counties += String.format("%1$-21s", county.getName());
                    } else {
                        counties += String.format("%1$-20s", county.getName());
                    }

                    iCnty++;

                }
            }
        }

        if (ugcStr.substring(ugcStr.lastIndexOf('\n') + 1).length() >= lnLength - 7) {
            ugcStr += "\n";
        }

        ugcStr += String.format("-%1td%1$tH%1$tM-\n\n", exp);

        if (!ugcStr.endsWith("\n"))
            ugcStr += "\n";

        if (!cities.isEmpty())
            cities += "\n";
        counties += "\n";

        String cntyStr = "";
        if (!ugcStr.isEmpty()) {

            if (ugcStr.charAt(2) == 'Z') {
                // for coastal waters
                cntyStr += "CW" + "\n\n" + ".    "
                        + "ADJACENT COASTAL WATERS INCLUDED ARE:\n" + waters;
            } else {
                String stName = stateName.get(state).toUpperCase();

                if (stName != null) {
                    if (state.equalsIgnoreCase("LA")) {
                        cntyStr += state + "\n\n" + ".    " + stName
                                + " PARISHES INCLUDED ARE:\n\n"
                                + counties.toUpperCase();
                    } else {
                        cntyStr += state + "\n\n" + ".    " + stName
                                + " COUNTIES INCLUDED ARE:\n\n"
                                + counties.toUpperCase();
                    }

                    if (!cities.isEmpty()) {
                        cntyStr += "\n\n" + stName
                                + " INDEPENDENT CITIES INCLUDED ARE:\n\n"
                                + cities.toUpperCase();
                    }
                }
            }
        }

        return ugcStr.concat(cntyStr);
    }

    /**
     * Find counties in a clustering
     * 
     * @param fips
     * @return
     */
    private Set<String> findCntyInClst(String fips) {

        Set<String> rt = PgenStaticDataProvider.getProvider().getClstTbl()
                .get(fips);
        return (rt == null) ? new HashSet<String>(Arrays.asList(fips)) : rt;

    }

    /**
     * Remove counties from the watch box
     * 
     * @param county
     */
    public void rmClstCnty(SPCCounty county) {
        if (county.getFips().isEmpty()
                || county.getFips().equalsIgnoreCase("00000")) {
            removeCounty(county);
        }
        for (String fips : findCntyInClst(county.getFips())) {
            removeCounty(PgenStaticDataProvider.getProvider().findCounty(fips));
        }
    }
        
    /**
     * Add clustering counties to the watch box
     * 
     * @param county
     */
    public void addClstCnty(SPCCounty county) {
        if (county.getFips().isEmpty()
                || county.getFips().equalsIgnoreCase("00000")) {
            addCounty(county);
        } else {
            for (String fips : findCntyInClst(county.getFips())) {
                SPCCounty cnty = PgenStaticDataProvider.getProvider()
                        .findCounty(fips);
                if (cnty != null && !countyList.contains(cnty))
                    addCounty(cnty);
            }
        }
    }

    /**
     * Add a new status
     * 
     * @param fromLine
     *            - 'from line' string
     * @param dNum
     *            - discussion number
     * @param vTime
     *            - valid time
     * @param eTime
     *            - expiration time
     * @param name
     *            - forecaster name
     */

    public void addStatus(String fromLine, int dNum, Calendar vTime,
            Calendar eTime, String name) {
        if (statusHistory == null) {
            statusHistory = new ArrayList<WatchStatus>();
        }

        statusHistory.add(new WatchStatus(fromLine, dNum, vTime, eTime, name));

    }

    /**
     * Remove the last staus
     */
    public void rmLastStatus() {
        if (statusHistory != null && !statusHistory.isEmpty()) {
            statusHistory.remove(statusHistory.size() - 1);
        }
    }

    /**
     * Get a list of all statuses
     * 
     * @return
     */
    public List<WatchStatus> getStatusHistory() {
        return statusHistory;
    }

    /**
     * Format the county list information
     * 
     * @param countyList
     * @return
     */
    public String formatCountyInfo(List<SPCCounty> cntyList) {

        String cntyInfo = "";
        if (cntyList != null && !cntyList.isEmpty()) {

            for (SPCCounty cnty : cntyList) {
                String cntyName = cnty.getName().replaceAll("City of ", "")
                        .replaceAll(" City", "").replaceAll(" ", "_").replaceAll("'", "").replaceAll("\\.", "");
                cntyInfo += String.format(
                        "%1$-7s%2$-5s%3$-17s%4$5.2f%5$8.2f%6$7s %7$-5s",
                        cnty.getUgcId(), cnty.getState(), cntyName,
                        cnty.getCentriod().y, cnty.getCentriod().x,
                        cnty.getFips(), cnty.getWfo());
                cntyInfo += "\n";
            }
        } else {
            cntyInfo = "None...None...None...\n";
        }

        return cntyInfo;

    }
    
    public List<SPCCounty> getOriginalCountyList() {
		return originalCountyList;
	}
    
    public void makeOriginalCountyList(List<SPCCounty> countyList) {
		this.originalCountyList = new ArrayList<SPCCounty>(countyList);
	}
	
	public void setOriginalCountyList(List<SPCCounty> originalCountyList) {
		this.originalCountyList = originalCountyList;
	}
	
    /**
     * Class to hold watch status information
     * 
     * @author bingfan
     * 
     */
    public class WatchStatus {

        private String fromLine;

        private int discussion;

        private Calendar statusValidTime;

        private Calendar statusExpTime;

        private String statusForecaster;

        private WatchStatus(String fromLine, int dNum, Calendar vTime,
                Calendar eTime, String name) {
            this.fromLine = fromLine;
            this.discussion = dNum;
            this.statusValidTime = vTime;
            this.statusExpTime = eTime;
            this.statusForecaster = name;
        }

        public String getFromLine() {
            return fromLine;
        }

        public void setFromLine(String fromLine) {
            this.fromLine = fromLine;
        }

        public int getDiscussion() {
            return discussion;
        }

        public void setDiscussion(int discussion) {
            this.discussion = discussion;
        }

        public Calendar getStatusValidTime() {
            return statusValidTime;
        }

        public void setStatusValidTime(Calendar statusValidTime) {
            this.statusValidTime = statusValidTime;
        }

        public Calendar getStatusExpTime() {
            return statusExpTime;
        }

        public void setStatusExpTime(Calendar statusExpTime) {
            this.statusExpTime = statusExpTime;
        }

        public String getStatusForecaster() {
            return statusForecaster;
        }

        public void setStatusForecaster(String statusForecaster) {
            this.statusForecaster = statusForecaster;
        }

    }

	
    
}
