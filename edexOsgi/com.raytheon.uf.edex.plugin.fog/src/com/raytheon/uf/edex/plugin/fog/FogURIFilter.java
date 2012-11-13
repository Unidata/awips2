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
package com.raytheon.uf.edex.plugin.fog;

import java.util.regex.Pattern;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * Oct 31, 2012 15464      zhao        updated satellite URIfilters
 * 
 * </pre>
 * 
 */

public class FogURIFilter extends URIFilter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogURIFilter.class);

    /**
     * 
     */
    private static final long serialVersionUID = -7693738314059883681L;

    public FogURIFilter(String name) {
        super(name);
        statusHandler.info("FogFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "satellite" });
        // this will come from the localization bundle
        setCwa(name);
        setExclude(false);
        // setup for satellite processing
		cwaGeometry = AdjacentWfoMgr.getCwaGeomtry(name);

        setMatchURIs();
    }

    /** cwa */
    protected String cwa = null;

    protected Geometry cwaGeometry = null;

    /** static id for NESDIS **/
    public static String NESDIS = "NESDIS";

    /** sat plugin name **/
    public static String PLUGIN = "satellite";

    /** IR 3.9 um */
    public static String ir3_9 = "Imager_3.9_micron_IR";

    /** IR 10.7 um */
    public static String ir10_7 = "Imager_11_micron_IR";

    /** VIS */
    public static String vis = "Imager_Visible";

    public String satRegion = null;
    
    /*
     * new field: satellite coverage ID 
     */
    public String satCoverageIdIR = null;
    public String satCoverageIdVis = null;

    /** URI pattern for VIS */
    public Pattern visURIpattern = null;

    /** URI pattern for IR 3.9 */
    public Pattern ir3_9URIpattern = null;

    /** URI pattern for IR 10.7 */
    public Pattern ir10_7URIpattern = null;

    @Override
    public void setMatchURIs() {

        setSatRegion();

        setVISPattern();
        setIR3_9Pattern();
        setIR10_7Pattern();

        long duration = 60 * 1000l * 5; // 5 mins
        // look for / how long before new
        getMatchURIs().put(getVISPattern(), duration);
        getMatchURIs().put(getIR3_9Pattern(), duration);
        getMatchURIs().put(getIR10_7Pattern(), duration);

    }

    @Override
    protected String applyWildCards(String key) {

        String[] comps = key.split(uriSeperator);
        StringBuffer newKey = new StringBuffer();

        for (int i = 1; i < comps.length; i++) {
            if (i == 2) {
                // second comp is time, wildcarded for most
                newKey.append(uriSeperator + wildCard);
            } else if (i == 0) {
                // do nothing
            } else {
                newKey.append(uriSeperator + comps[i]);
            }
        }
        return newKey.toString();
    }

    @Override
    protected String removeWildCards(String key) {

        String[] comps = key.split(uriSeperator);
        StringBuffer newKey = new StringBuffer();
        String time = getDateFormatter().format(
                getMatchedURIs().get(key).getTime());

        for (int i = 1; i < comps.length; i++) {
            if (i == 2) {
                // second comp is time, wildcarded for most
                newKey.append(uriSeperator + time);
            } else if (i == 0) {
                // do nothing
            } else {
                newKey.append(uriSeperator + comps[i]);
            }
        }
        return newKey.toString();
    }

    /**
     * Filter By CWA
     * 
     * @return
     */
    public String getCwa() {
        return cwa;
    }

    /**
     * Set the filtering ICAOs
     * 
     * @param icaos
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    /**
     * Sets the VIS URI Pattern
     * 
     * @return
     */
    public void setVISPattern() {
        visURIpattern = Pattern.compile("/satellite/" + wildCard + uriSeperator
                + NESDIS + uriSeperator + getSatRegion() + uriSeperator + vis
                + uriSeperator + satCoverageIdVis);
    }

    /**
     * Gets the VIS pattern
     * 
     * @return
     */
    public Pattern getVISPattern() {
        return visURIpattern;
    }

    /**
     * Sets the IR 3_9 URI Pattern
     * 
     * @return
     */
    public void setIR3_9Pattern() {
        ir3_9URIpattern = Pattern.compile("/satellite/" + wildCard
                + uriSeperator + NESDIS + uriSeperator + getSatRegion()
                + uriSeperator + ir3_9 + uriSeperator + satCoverageIdIR);
    }

    /**
     * Gets the IR3_9 pattern
     * 
     * @return
     */
    public Pattern getIR3_9Pattern() {
        return ir3_9URIpattern;
    }

    /**
     * Sets the IR 10_7 URI Pattern
     * 
     * @return
     */
    public void setIR10_7Pattern() {
        ir10_7URIpattern = Pattern.compile("/satellite/" + wildCard
                + uriSeperator + NESDIS + uriSeperator + getSatRegion()
                + uriSeperator + ir10_7 + uriSeperator + satCoverageIdIR );
    }

    /**
     * Gets the IR 10_7 pattern
     * 
     * @return
     */
    public Pattern getIR10_7Pattern() {
        return ir10_7URIpattern;
    }

    /**
     * Sets the satellite region
     * 
     * @param cwaCenterCoor
     */
    public void setSatRegion() {

        Coordinate cwaCenterCoor = cwaGeometry.getCentroid().getCoordinate();
        // -122.53428764092014, 47.662021408089124 for SEW
        if (cwaCenterCoor != null) {
            if (cwaCenterCoor.y > 30.0 && cwaCenterCoor.x < -130.0) {
                //satRegion = "GOES-12\\(M\\)/Alaska_Regional";
            	satRegion = "GOES-15\\(P\\)/Alaska_Regional";
            	satCoverageIdIR = "328892060";
            	satCoverageIdVis = "1160112258";
            } else if (cwaCenterCoor.y < 30.0 && cwaCenterCoor.x < -130.0) {
                //satRegion = "GOES-12\\(M\\)/Hawaii_Regional";
            	satRegion = "GOES-15\\(P\\)/Hawaii_Regional";
            	satCoverageIdIR = "1162224264";
            	satCoverageIdVis = "880359024";
            } else if (cwaCenterCoor.x < -105.0) {
                //satRegion = "GOES-11\\(L\\)/West_CONUS";
                // satRegion = "GOES-12\\(M\\)/West_CONUS";
            	satRegion = "GOES-15\\(P\\)/West_CONUS";
            	satCoverageIdIR = "667897505";
            	satCoverageIdVis = "371138769";
            } else if (cwaCenterCoor.x > -105.0) {
                satRegion = "GOES-13\\(N\\)/East_CONUS";
                //satRegion = "GOES-14\\(O\\)/East_CONUS";
                satCoverageIdIR = "553646295";
                satCoverageIdVis = "-668648557";
            }
        } else {

            satRegion = "GOES-13\\(N\\)/East_CONUS";
            //satRegion = "GOES-14\\(O\\)/East_CONUS";
            satCoverageIdIR = "553646295";
            satCoverageIdVis = "-668648557";            
            statusHandler
                    .error("FogFilter "
                    + name
                    + " Could not find CWA Center Point.  Defaulting to East_CONUS...");
        }
    }

    /**
     * Gets the satellite region
     * 
     * @return
     */
    public String getSatRegion() {
        return satRegion;
    }

    /**
     * Gets the CWA outline
     * 
     * @return
     */
    public Geometry getCwaGeometry() {
        return cwaGeometry;
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        // TODO Auto-generated method stub
        return new FogURIGenerateMessage(this);
    }
}
