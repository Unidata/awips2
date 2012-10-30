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

package com.raytheon.edex.uengine.web;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;

import com.raytheon.edex.msg.ResponseMessageASCII;
import com.raytheon.edex.msg.ResponseMessageInline;
import com.raytheon.edex.msg.ResponseMessageNull;
import com.raytheon.edex.msg.ResponseMessageSubscription;
import com.raytheon.edex.msg.ResponseMessageURI;
import com.raytheon.edex.msg.ResponseMessageValidate;
import com.raytheon.edex.msg.ResponseMessageXML;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.XMLUtils;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Servlet implementation class for Servlet: RunAction
 * 
 * @web.servlet name="RunAction" display-name="RunAction" description="takes an
 *              Action script, runs it, and displays the result"
 * 
 * @web.servlet-mapping url-pattern="/runAction.jas"
 */
public class RunAction extends javax.servlet.http.HttpServlet implements
        javax.servlet.Servlet {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private static final Log theLogger = LogFactory.getLog(RunAction.class);

    /*
     * (non-Java-doc)
     * 
     * @see javax.servlet.http.HttpServlet#HttpServlet()
     */
    public RunAction() {
        super();
    }

    /*
     * (non-Java-doc)
     * 
     * @see javax.servlet.http.HttpServlet#doGet(HttpServletRequest request,
     * HttpServletResponse response)
     */
    @Override
    protected void doPost(HttpServletRequest request,
            HttpServletResponse response) throws ServletException, IOException {
        String requestType = request.getParameter("requesttype");
        String actionXML = request.getParameter("actionXML");
        String receiveTime = request.getParameter("receiveTime");
        long receive = 5000;
        boolean jsEngine = false;
        theLogger.info("request type is " + Util.printString(requestType));
        if (requestType != null) {
            if (requestType.equals("ascii")) {
                actionXML = createActionASCII(request);
            } else if (requestType.equalsIgnoreCase("image")) {
                actionXML = createActionImage(request);
            } else if (requestType.equalsIgnoreCase("grid")) {
                actionXML = createActionGrib(request);
            } else if (requestType.equalsIgnoreCase("radar")) {
                actionXML = createActionRadar(request);
            } else if (requestType.equalsIgnoreCase("unsubscribe")) {
                actionXML = createActionUnsubscribe(request);
            } else if (requestType.equalsIgnoreCase("javascript")) {
                jsEngine = true;
                actionXML = createActionJavaScript(request, actionXML);
            } else if (requestType.equalsIgnoreCase("jsunsubscribe")) {
                jsEngine = true;
                actionXML = createActionJSUnsubscribe(request);
            }
        }

        if (actionXML != null) {
            // run action, return output
            String responseStr = "";
            if (receiveTime != null) {
                try {
                    receive = Long.parseLong(receiveTime);
                } catch (NumberFormatException nfe) {
                    theLogger.info("receiveTime(" + receiveTime
                            + ") not a number, defaulting to " + receive);
                }
            }

            try {
                responseStr = RequestTestDriver.requestHTTP(actionXML);
                System.out.println(responseStr);
            } catch (EdexException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            if (responseStr.indexOf("<responseASCII>") != -1) {
                handleAsciiResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseNULL>") != -1) {
                handleNullResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseInline>") != -1) {
                handleProductResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseURI>") != -1) {
                handleServiceResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseError>") != -1) {
                handleErrorResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseValidate>") != -1) {
                handleValidateResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseSubscription>") != -1) {
                handleSubscriptionResponse(response, responseStr);
            } else if (responseStr.indexOf("<responseNGATS>") != -1) {
                handleNGATSResponse(response, responseStr);
            } else if (responseStr.indexOf("responseXML") != -1) {
                handleXMLResponse(response, responseStr);
            } else if (responseStr.indexOf("ProgramOutput") != -1) {
                handleProgramOutput(response, responseStr);
            } else {
                htmlOutput(response, "Cannot process response.");
            }
        } else {
            htmlOutput(response, "Action <b>NULL</b>, no output.");
        }
    }

    private void handleNGATSResponse(HttpServletResponse response,
            String responseStr) {
        PrintWriter out = null;
        try {
            out = response.getWriter();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        out.println(responseStr);
    }

    /**
     * Creates an unbscribe action script. The script is wrapped in a canonical
     * message. The exact contents of the script is determined by the parameters
     * in the request object.
     * 
     * @param request
     *            the request object
     * 
     * @return the unsubscribe script
     */
    private String createActionUnsubscribe(HttpServletRequest request) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("scriptname");
        if (name == null || name.equals("")) {
            name = "Image Request";
        }
        String function = request.getParameter("function");
        String type = request.getParameter("datatype");
        String dataURI = "";
        try {
            if (type.equalsIgnoreCase("images")) {
                dataURI = makeImageDataURI(request);
            } else if (type.equalsIgnoreCase("ascii")) {
                dataURI = makeAsciiDataURI(request);
            } else if (type.equalsIgnoreCase("grid")) {
                dataURI = makeGribDataURI(request);
            } else if (type.equalsIgnoreCase("radar")) {
                dataURI = makeRadarDataURI(request);
            } else {
                theLogger
                        .warn("unable to create subscription key for data type "
                                + type);
                dataURI = "";
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        /*
         * create the XML message - use brute force to simplify the war
         */
        String eol = System.getProperty("line.separator");
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"uri\" value=\"" + dataURI + "\" />"
                + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<body />" + eol);
        action.append("</message>" + eol);

        return action.toString();
    }

    private String createActionRadar(HttpServletRequest request) {
        StringBuffer action = new StringBuffer();
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        String name = request.getParameter("scriptname");
        if (Util.isEmptyString(name)) {
            name = "";
        }
        String function = request.getParameter("function");
        // String plugin = request.getParameter("plugin");
        String location = request.getParameter("station");
        String productCode = request.getParameter("productcode");
        String elevation = request.getParameter("elevation");
        // String colorMap = request.getParameter("colormap");
        String imageFormat = request.getParameter("imageformat");
        String reproject = request.getParameter("reproject");
        String count = request.getParameter("count");
        if (Util.isEmptyString(count)) {
            count = "1";
        }
        boolean subscription = function.equalsIgnoreCase("subscribe");
        String eol = System.getProperty("line.separator");
        // create the action script message
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<![CDATA[" + eol);
        // create the JS Action Script...
        action.append("include(\"RadarRequest.js\");" + eol);
        action.append("var dataRequest = new RadarRequest();" + eol);
        action.append("dataRequest.requestImage(true);" + eol);
        action.append("dataRequest.setCount(" + count + ");" + eol);
        if (!Util.isEmptyString(productCode)) {
            action.append("dataRequest.addList(\"productCode\",\""
                    + productCode + "\");");
        }
        if (!Util.isEmptyString(location)) {
            action.append("dataRequest.addParameter(\"icao\",\"" + location
                    + "\");");
        }
        if (!Util.isEmptyString(elevation)) {
            action.append("dataRequest.addParameter(\"primaryElevationAngle\",\""
                    + elevation + "\");");
        }
        action.append("dataRequest.setSortValue(\"time_stamp\");");
        if (isCheckboxOn(reproject)) {
            action.append("dataRequest.reprojectImage(true);" + eol);
        } else {
            action.append("dataRequest.reprojectImage(false);" + eol);
        }
        action.append("dataRequest.setFormat(\"" + imageFormat + "\");" + eol);
        if (subscription) {
            action.append("dataRequest.enableSubscription();" + eol);
        }
        action.append("dataRequest.execute();" + eol);
        action.append("]]>" + eol);
        action.append("</message>" + eol);

        return action.toString();
    }

    /**
     * Creates an image retrieval action script. The script is wrapped in a
     * canonical message. The exact contents of the script is determined by the
     * parameters in the request object.
     * 
     * @param request
     *            the request object
     * 
     * @return the action script.
     */
    private String createActionImage(HttpServletRequest request) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("scriptname");
        if (Util.isEmptyString(name)) {
            name = "";
        }
        String function = request.getParameter("function");
        String plugin = request.getParameter("plugin");
        String dataType = request.getParameter("datatype");
        String location = request.getParameter("location");
        String parameter = request.getParameter("parameter");
        String colorMap = request.getParameter("colormap");
        String imageFormat = request.getParameter("imageformat");
        String reproject = request.getParameter("reproject");
        String count = request.getParameter("count");

        if (Util.isEmptyString(count)) {
            count = "1";
        }
        boolean subscription = function.equalsIgnoreCase("subscribe");
        String eol = System.getProperty("line.separator");
        // create the action script message
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<![CDATA[" + eol);
        // crete the JS action script
        action.append("include(\"SatelliteRequest.js\");" + eol);
        action.append("var dataRequest = new SatelliteRequest();" + eol);
        action.append("dataRequest.requestImage(true);" + eol);
        action.append("dataRequest.setCount(" + count + ");" + eol);
        action.append("dataRequest.addParameter(\"goes\",\"" + parameter
                + "\")" + eol);
        action.append("dataRequest.addParameter(\"product_type\",\"" + plugin
                + "\")" + eol);
        action.append("dataRequest.addParameter(\"datatype\",\"" + dataType
                + "\")" + eol);
        action.append("dataRequest.addParameter(\"area_subtype\",\"" + location
                + "\");");
        action.append("dataRequest.setColormap(\"" + colorMap + "\");" + eol);
        if (isCheckboxOn(reproject)) {
            action.append("dataRequest.reprojectImage(true);" + eol);
        } else {
            action.append("dataRequest.reprojectImage(false);" + eol);
        }
        action.append("dataRequest.setFormat(\"" + imageFormat + "\");" + eol);
        if (subscription) {
            action.append("dataRequest.enableSubscription();" + eol);
        }
        action.append("dataRequest.execute();" + eol);
        action.append("]]>" + eol);
        action.append("</message>" + eol);
        return action.toString();
    }

    /**
     * Creates a GRIB product retrieval action script. The script is wrapped in
     * a canonical message. The exact contents of the script is determined by
     * the parameters in the request object.
     * 
     * @param request
     *            the request object
     * 
     * @return the action script.
     */
    private String createActionGrib(HttpServletRequest request) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("scriptname");
        if (Util.isEmptyString(name)) {
            name = "GRIB Request";
        }

        String function = request.getParameter("function");
        String parameter = request.getParameter("parameter");
        String forecast = request.getParameter("forecast");
        String level1 = request.getParameter("level1");
        String level2 = request.getParameter("level2");
        String units = request.getParameter("unit");
        String colorMap = request.getParameter("colormap");
        String imageFormat = request.getParameter("imageformat");
        String reproject = request.getParameter("reproject");
        String count = request.getParameter("count");
        String scalefactor = request.getParameter("scalefactor");

        /* create the value for the levelinfo query. */
        StringBuffer levelInfo = new StringBuffer();
        if (!Util.isEmptyString(level2)) {
            levelInfo.append(level2);
        }
        if (!Util.isEmptyString(level1)) {
            if (!Util.isEmptyString(level2)) {
                levelInfo.append("-");
            }
            levelInfo.append(level1);
        }
        if (!Util.isEmptyString(units)) {
            if (levelInfo.length() != 0) {
                levelInfo.append("_").append(units);
            }
        }

        if (Util.isEmptyString(count)) {
            count = "1";
        }
        // boolean subscription = function.equalsIgnoreCase("subscribe");
        String eol = System.getProperty("line.separator");
        // create the action script message
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<![CDATA[" + eol);
        action.append("include(\"GridRequest.js\");" + eol);
        action.append("var dataRequest = new GridRequest();" + eol);
        action.append("dataRequest.requestImage(true);" + eol);
        action.append("dataRequest.setCount(" + count + ");");
        action.append("dataRequest.addParameter(\"paramid\",\"" + parameter
                + "\");" + eol);
        if (!Util.isEmptyString(levelInfo.toString())) {
            action.append("dataRequest.addParameter(\"levelinfo\",\""
                    + levelInfo.toString() + "\");" + eol);
        }
        if (!Util.isEmptyString(forecast)) {
            action.append("dataRequest.addParameter(\"forecasttime\",\""
                    + forecast + "\");" + eol);
        }
        action.append("dataRequest.setColormap(\"" + colorMap + "\");" + eol);
        if (isCheckboxOn(reproject)) {
            action.append("dataRequest.reprojectImage(true);" + eol);
        } else {
            action.append("dataRequest.reprojectImage(false);" + eol);
        }
        action.append("dataRequest.setFormat(\"" + imageFormat + "\");" + eol);
        action.append("dataRequest.setScaleFactor(" + scalefactor + ");" + eol);
        action.append("dataRequest.enableSubscription();" + eol);
        action.append("dataRequest.execute();" + eol);
        action.append("]]>" + eol);
        action.append("</message>" + eol);
        return action.toString();
    }

    /**
     * Creates an ASCII products retrieval action script. The script is wrapped
     * in a canonical message. The exact contents of the script is determined by
     * the parameters in the request object.
     * 
     * @param request
     *            the request object
     * 
     * @return the action script.
     */
    private String createActionASCII(HttpServletRequest request) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("name");
        if (name == null) {
            name = "";
        }
        String function = request.getParameter("function");
        String type = request.getParameter("type");

        String eol = System.getProperty("line.separator");
        // create the action script message
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<![CDATA[" + eol);
        if (!type.equalsIgnoreCase("taf")) {
            action.append(createObScript(request, function, type));
        } else {
            action.append(createTafScript(request, function, type));
        }
        action.append("]]>" + eol);
        action.append("</message>" + eol);
        return action.toString();
    }

    /**
     * Creates the JS uEngine script for ob requests.
     * 
     * @param request
     *            the request object
     * @param function
     *            script function
     * @param type
     *            report type
     * 
     * @return the JS Script
     */
    private String createObScript(HttpServletRequest request, String function,
            String type) {
        StringBuffer script = new StringBuffer();
        String validTime = request.getParameter("time");
        String station = request.getParameter("station");
        String timeType = request.getParameter("refTime");
        String count = request.getParameter("count");

        if (Util.isEmptyString(count)) {
            count = "1";
        }
        boolean subscription = function.equalsIgnoreCase("subscribe");

        script.append("include(\"ObsRequest.js\");");
        script.append("var dataRequest = new ObsRequest();");
        script.append("dataRequest.setCount(" + count + ");");
        script.append("dataRequest.addParameter(\"reporttype\",\"" + type
                + "\");");
        if (station != null && (!station.equals(""))) {
            script.append("dataRequest.addList(\"stationid\",\"" + station
                    + "\");");
        }
        if (validTime != null && (!validTime.equals(""))) {
            try {
                if (validTime.length() == 6) {
                    validTime = String.format("%1$tY%<tm%<td%<tH%<tM00000",
                            Util.findCurrentTime(validTime));
                }
                if (timeType.equalsIgnoreCase("base")) {
                    script.append("dataRequest.addParameter(\"timeObs\",\""
                            + validTime + "\");");
                } else if (timeType.equalsIgnoreCase("reference")) {
                    script.append("dataRequest.addParameter(\"refTime\",\""
                            + validTime + "\");");
                } else {
                    script.append("dataRequest.addParameter(\"refHour\",\""
                            + validTime + "\");");
                }
            } catch (Exception e) {
                // do nothing - just skip the query
            }
        }
        if (subscription) {
            script.append("dataRequest.enableSubscription();");
        }
        script.append("dataRequest.execute();");
        return script.toString();
    }

    /**
     * Creates the JS uEngine script for taf requests.
     * 
     * @param request
     *            the request object
     * @param function
     *            script function
     * @param type
     *            report type (not currently used)
     * 
     * @return the JS Script
     */
    private String createTafScript(HttpServletRequest request, String function,
            String type) {
        StringBuffer script = new StringBuffer();
        String validTime = request.getParameter("time");
        String station = request.getParameter("station");
        String count = request.getParameter("count");

        if (Util.isEmptyString(count)) {
            count = "1";
        }
        boolean subscription = function.equalsIgnoreCase("subscribe");

        script.append("include(\"TafRequest.js\");");
        script.append("var dataRequest = new TafRequest();");
        script.append("dataRequest.setCount(" + count + ");");
        // script.append("dataRequest.addParameter(\"reporttype\",\""+type+"\");");
        if (station != null && (!station.equals(""))) {
            script.append("dataRequest.addList(\"stationid\",\"" + station
                    + "\");");
        }
        if (validTime != null && (!validTime.equals(""))) {
            try {
                if (validTime.length() == 6) {
                    validTime = String.format("%1$tY%<tm%<td%<tH%<tM00000",
                            Util.findCurrentTime(validTime));
                }
                script.append("dataRequest.addParameter(\"issue_time\",\""
                        + validTime + "\");");
            } catch (Exception e) {
                // do nothing - just skip the query
            }
        }
        if (subscription) {
            script.append("dataRequest.enableSubscription();");
        }
        script.append("dataRequest.execute();");
        return script.toString();
    }

    /**
     * Creates an message wrapped action for a JScript script.
     * 
     * @param request
     *            the request object
     * 
     * @return the action message.
     */
    private String createActionJavaScript(HttpServletRequest request,
            String actionXML) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("name");
        if (name == null) {
            name = "";
        }
        String function = request.getParameter("function");

        // boolean subscription = function.equalsIgnoreCase("subscribe");
        String eol = System.getProperty("line.separator");
        // create the action script message
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<![CDATA[" + eol);
        action.append(actionXML + eol);
        action.append("]]>" + eol);
        action.append("</message>" + eol);
        return action.toString();
    }

    /**
     * Creates the canonical message for a JScript unsubscribe.
     * 
     * @param request
     *            the request object
     * 
     * @return the action message.
     */
    private String createActionJSUnsubscribe(HttpServletRequest request) {
        Calendar cal = Calendar.getInstance();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", cal);
        StringBuffer action = new StringBuffer();
        String name = request.getParameter("scriptname");
        if (name == null || name.equals("")) {
            name = "JScript Test Case";
        }
        String function = request.getParameter("function");
        String dataURI = request.getParameter("scripturi");
        String eol = System.getProperty("line.separator");
        action.append("<message>\n<header>" + eol);
        action.append("<property name=\"uri\" value=\"" + dataURI + "\" />"
                + eol);
        action.append("<property name=\"id\" value=\"" + name + "\" />" + eol);
        action.append("<property name=\"time\" value=\"" + now + "\" />" + eol);
        action.append("<property name=\"function\" value=\"" + function
                + "\" />" + eol);
        action.append("</header>" + eol);
        action.append("<body />" + eol);
        action.append("</message>" + eol);

        return action.toString();
    }

    /**
     * Extracts the response from the body of the message. The message is in
     * canonical message format.
     * 
     * @param message
     *            the canonical message
     * @param responseName
     *            the name of the response to extract
     * 
     * @return the requested response
     * 
     * @throws Exception
     *             if an error occurs
     */
    private String extractResponseFromMessage(String message,
            String responseName) throws Exception {
        Document document = XMLUtils.scanXMLtoDOM(message);
        Document responseMsg = XMLUtils.getSubDocument(document, responseName);

        return XMLUtils.transformXMLDocument(responseMsg);
    }

    /**
     * Extracts all response messages from the body of the message. The message
     * is in canonical message format.
     * 
     * @param message
     *            the canonical message
     * @param responseName
     *            the response to extract
     * 
     * @return array of responses as strings
     * 
     * @throws Exception
     *             if an error occurs.
     */
    private String[] extractAllResponses(String message, String responseName)
            throws Exception {
        Document document = XMLUtils.scanXMLtoDOM(message);
        List<Document> responses = XMLUtils.getAllSubDocumentsByTag(document,
                responseName);
        List<String> xml = new ArrayList<String>();
        for (Document doc : responses) {
            xml.add(XMLUtils.transformXMLDocument(doc));
        }
        return xml.toArray(new String[0]);
    }

    /**
     * Extracts a header property from a message. The message is in canonical
     * message format.
     * 
     * @param message
     *            the canonical message
     * @param property
     *            the property
     * 
     * @return the property value
     * 
     * @throws Exception
     *             in an error occurs
     */
    private String extractPropertyFromResponse(String message, String property)
            throws Exception {
        Document document = XMLUtils.scanXMLtoDOM(message);
        return XMLUtils.getPropertyFromXML(document, property);
    }

    private void handleProgramOutput(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        String[] responses;
        StringBuffer htmlStr = new StringBuffer();
        try {
            responses = extractAllResponses(responseStr, "ProgramOutput");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        setContentType(response, "ascii");
        htmlStr.append("<html>\n");
        htmlStr.append("<head><title>RunAction Response</title></head>\n");
        htmlStr.append("<body>\n");
        htmlStr.append("<center><table width=\"100%\"><tr><td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("<td><h2>&mu;Engine Response &ndash; External Application Execution</h2></td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("</tr></table></center>\n");
        for (String xmlDoc : responses) {
            htmlStr.append("<hr>\n");
            htmlStr.append("<pre>\n")
                    .append(xmlDoc.replaceAll("<", "&lt;").replaceAll(">",
                            "&gt;")).append("\n</pre></td></tr>\n");

        }
        htmlStr.append("<hr>\n");
        htmlStr.append("</body>\n");
        htmlStr.append("</html>\n");
        PrintWriter out = response.getWriter();
        out.println(htmlStr.toString());
    }

    /**
     * 
     * @param response
     * @param responseStr
     * @throws ServletException
     * @throws IOException
     */
    private void handleXMLResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        String[] responses;
        StringBuffer htmlStr = new StringBuffer();
        try {
            responses = extractAllResponses(responseStr, "responseXML");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        setContentType(response, "ascii");
        htmlStr.append("<html>\n");
        htmlStr.append("<head><title>RunAction Response</title></head>\n");
        htmlStr.append("<body>\n");
        htmlStr.append("<center><table width=\"100%\"><tr><td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("<td><h2>&mu;Engine Response &ndash; XML Product Retrieval</h2></td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("</tr></table></center>\n");
        for (String xmlDoc : responses) {
            ResponseMessageXML servMsg = null;
            try {
                // IBindingFactory bfact = BindingDirectory
                // .getFactory(ResponseMessageXML.class);
                // IUnmarshallingContext uctx =
                // bfact.createUnmarshallingContext();
                // servMsg = (ResponseMessageXML) uctx
                // .unmarshalDocument(new StringReader(xmlDoc));
            } catch (Throwable e) {
                throw new ServletException(e);
            }
            if (servMsg != null) {
                // theLogger.info("Processing XML Message ("
                // + servMsg.getContents() + ") to output.");
                setContentType(response, servMsg.getFileType());
                String time = servMsg.getValidTime().toString();
                htmlStr.append("<hr>\n");
                htmlStr.append("<table>\n");
                htmlStr.append("<tr><td><b>Time:</b></td><td>" + time
                        + "</td></tr>\n");
                // try {
                // htmlStr
                // .append("<tr><td valign=top><b>Response:</b></td><td>"
                // + "<pre>"
                // + SerializationUtil.marshalToXml(
                // servMsg.getContents()).replaceAll(
                // "<", "&lt;")
                // .replaceAll(">", "&gt;")
                // + "</pre></td></tr>\n");
                // } catch (JAXBException e) {
                // // TODO Auto-generated catch block
                // e.printStackTrace();
                // }
                htmlStr.append("</table>\n");
            }
        }
        htmlStr.append("<hr>\n");
        htmlStr.append("</body>\n");
        htmlStr.append("</html>\n");
        PrintWriter out = response.getWriter();
        out.println(htmlStr.toString());
    }

    /**
     * Extracts the subscription result from the response string.
     * 
     * @param responseStr
     *            the response string
     * 
     * @return the subscription message
     * 
     * @throws Exception
     *             in the event on an error
     */
    private String getSubscriptionMsg(String responseStr) throws Exception {
        StringBuffer retVal = new StringBuffer();
        String id = null;
        String dataURI = null;
        String message = null;
        String[] subscripts;
        ResponseMessageSubscription respMsg = null;

        subscripts = extractAllResponses(responseStr, "responseSubscription");
        id = extractPropertyFromResponse(responseStr, "id");
        dataURI = extractPropertyFromResponse(responseStr, "uri");
        theLogger.info("Processing Subscription Message (" + dataURI + ":" + id
                + ") to output.");
        for (String response : subscripts) {
            // IBindingFactory bfact = BindingDirectory
            // .getFactory(ResponseMessageSubscription.class);
            // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
            // respMsg = (ResponseMessageSubscription) uctx
            // .unmarshalDocument(new StringReader(response));
            // message = respMsg.getStatusMessage();
            // retVal.append("<tr><td><b>Data URI:</b></td><td>").append(dataURI)
            // .append("</td></tr>");
            // retVal.append("<tr><td><b>Subscription ID:</b></td><td>")
            // .append(id).append("</td></tr>");
            // retVal.append("<tr><td><b>Result:</b></td><td>").append(message)
            // .append("</td></tr>");
        }
        return retVal.toString();
    }

    /**
     * Handles a subscription response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleSubscriptionResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        String message = "";
        try {
            message = getSubscriptionMsg(responseStr);
        } catch (Exception e) {
            throw new ServletException(e);
        }
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        out.println("<html>");
        out.println("<head><title>RunAction Response</title></head>");
        out.println("<body>");
        out.println("<table>");
        out.println("<TR><TD><img src=\"rayAWIPS.jpg\" align=middle>");
        out.println("<TD>");
        out.println("<center><h1>&mu;Engine Response</h1></center>");
        out.println("<center><h2>Product Subscription Result</h4></center>");
        out.println("<TD>");
        out.println("<TD><img src=\"rayAWIPS.jpg\" align=middle></TD>");
        out.println("</table>");
        out.println("<hr>");
        out.println("<table>");
        out.println(message);
        out.println("</table>");
        out.println("</body>");
        out.println("</html>");
    }

    /**
     * Handles the NULL Data Response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleNullResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        System.out.println(responseStr);
        try {
            responseStr = extractResponseFromMessage(responseStr,
                    "responseNULL");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        ResponseMessageNull servMsg = null;
        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageNull.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // servMsg = (ResponseMessageNull) uctx
        // .unmarshalDocument(new StringReader(responseStr));
        // } catch (JiBXException e) {
        // e.printStackTrace();
        // throw new ServletException(e);
        // } catch (Throwable e) {
        // e.printStackTrace();
        // }
        if (servMsg != null) {
            theLogger.info("Writing error message(" + servMsg.getMessage()
                    + ") to output.");

            setContentType(response, servMsg.getFileType());
            PrintWriter out = response.getWriter();
            out.println("<html>");
            out.println("<head><title>RunAction NULL Data</title></head>");
            out.println("<body>");
            out.println("<center><h2>No Data Available</h2></center><hr>");
            try {
                String subresp = getSubscriptionMsg(responseStr);
                if (!Util.isEmptyString(subresp)) {
                    out.println("<table>\n");
                    out.println("<tr><th colspan=2>Subscription results:</th></tr>\n");
                    out.println(subresp);
                    out.println("</table>\n");
                }
            } catch (Exception e) {

            }
            out.println("<b>Data URI:</b> " + servMsg.getDataURI() + "<p>");
            out.println("<b>Time:</b> " + servMsg.getTime() + "<p>");
            out.println("<b>Message:</b> " + servMsg.getMessage() + "<p>");
            out.println("<HR>");
            out.println("<em>See system log for more information.</em>");
            out.println("</body></html>");

        } else {
            htmlOutput(response,
                    "NULL response could not be parsed, no output.");
        }
    }

    /**
     * Handles an ASCII product response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleAsciiResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        String[] responses;
        StringBuffer htmlStr = new StringBuffer();
        try {
            responses = extractAllResponses(responseStr, "responseASCII");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        setContentType(response, "ascii");
        htmlStr.append("<html>\n");
        htmlStr.append("<head><title>RunAction Response</title></head>\n");
        htmlStr.append("<body>\n");
        htmlStr.append("<center><table width=\"100%\"><tr><td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("<td><h2>&mu;Engine Response &ndash; ASCII Product Retrieval</h2></td>\n");
        htmlStr.append("<td><img src=\"rayAWIPS.jpg\"></td>\n");
        htmlStr.append("</tr></table></center>\n");
        try {
            String subresp = getSubscriptionMsg(responseStr);
            if (!Util.isEmptyString(subresp)) {
                htmlStr.append("<table>\n");
                htmlStr.append("<tr><th colspan=2>Subscription results:</th></tr>\n");
                htmlStr.append(subresp);
                htmlStr.append("</table>\n");
            }
        } catch (Exception e) {

        }

        for (String xmlDoc : responses) {
            ResponseMessageASCII servMsg = null;
            // try {
            // IBindingFactory bfact = BindingDirectory
            // .getFactory(ResponseMessageASCII.class);
            // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
            // servMsg = (ResponseMessageASCII) uctx
            // .unmarshalDocument(new StringReader(xmlDoc));
            // } catch (JiBXException e) {
            // e.printStackTrace();
            // throw new ServletException(e);
            // } catch (Throwable e) {
            // e.printStackTrace();
            // }
            if (servMsg != null) {
                theLogger.info("Processing ASCII message("
                        + servMsg.getContents() + ") to output.");
                setContentType(response, servMsg.getFileType());
                String month = "";
                String day = "";
                String hour = "";
                String minute = "";
                String station = "N/A";
                if (!servMsg.getStation().equals("")
                        && !servMsg.getStation().equalsIgnoreCase("null")) {
                    station = servMsg.getStation();
                }
                String time = "N/A";
                String strTime = servMsg.getTime();
                theLogger.debug(strTime);
                if (strTime.length() == 6) {
                    day = strTime.substring(0, 2);
                    hour = strTime.substring(2, 4);
                    minute = strTime.substring(4, 6);
                    time = String.format("%1$s %2$s:%3$s", day, hour, minute);
                } else if (strTime.length() == 7) {
                    month = strTime.substring(0, 2);
                    day = strTime.substring(2, 4);
                    hour = strTime.substring(4, 7);
                    time = String.format("%1$s %2$s:%3$s", month, day, hour);
                } else if (strTime.length() == 17) {
                    month = strTime.substring(6, 8);
                    day = strTime.substring(8, 10);
                    hour = strTime.substring(10, 12);
                    time = String.format("%1$s %2$s:%3$s", month, day, hour);
                } else if (strTime.length() == 21) {
                    day = strTime.substring(8, 10);
                    hour = strTime.substring(11, 13);
                    minute = strTime.substring(14, 16);
                    time = String.format("%1$s %2$s:%3$s", day, hour, minute);
                }
                htmlStr.append("<hr>\n");
                htmlStr.append("<table>\n");
                htmlStr.append("<tr><td colspan=2><b>Station:</b> " + station
                        + "\n");
                htmlStr.append("<b>Report Type:</b> " + servMsg.getType()
                        + "\n");
                htmlStr.append("<b>Report Time:</b> " + time + "</td></tr>\n");
                htmlStr.append("<tr><td valign=top><b>Message:</b></td><td>"
                        + "<pre>"
                        + servMsg.getContents().replaceAll("<", "&lt;")
                                .replaceAll(">", "&gt;") + "</pre></td></tr>\n"
                        + "</td></tr>");
                htmlStr.append("</table>\n");
            }
        }
        htmlStr.append("<hr>\n");
        htmlStr.append("</body>\n");
        htmlStr.append("</html>\n");
        PrintWriter out = response.getWriter();
        out.println(htmlStr.toString());
    }

    /**
     * Handles a script validation response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleValidateResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        ResponseMessageValidate servMsg = null;
        try {
            responseStr = extractResponseFromMessage(responseStr,
                    "responseValidate");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageError.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // servMsg = (ResponseMessageValidate) uctx
        // .unmarshalDocument(new StringReader(responseStr));
        // } catch (JiBXException e) {
        // e.printStackTrace();
        // throw new ServletException(e);
        // } catch (Throwable e) {
        // e.printStackTrace();
        // }
        if (servMsg != null) {
            theLogger.info("Writing validate message(" + servMsg.getMessage()
                    + ") to output.");

            setContentType(response, servMsg.getFileType());
            PrintWriter out = response.getWriter();
            out.println("<html>");
            out.println("<head><title>RunAction Validation</title></head>");
            out.println("<body>");
            out.println("<center><h2>" + servMsg.getErrorSource()
                    + " Results</h2></center><hr>");
            out.println("<table><tr><td valign=top><b>Script:<b></td><td>");
            out.println("<textarea name=\"actionXML\" cols=\"80\" rows=\"22\" style=\"background-color:aqua\">");
            out.println(servMsg.getScript());
            out.println("</textarea>");
            out.println("</td></tr></table>");
            out.println("<HR><b>Result:</b> " + servMsg.getMessage()
                    + "</b><p>");
            if (!servMsg.getResult()) {
                out.println("<b>Exception:</b> ");
                if (!servMsg.getErrorCause().equals("")) {
                    out.println(servMsg.getErrorCause().toString());
                } else {
                    out.println("No exception available");
                }
                out.println("<P>");
                String[] chain = servMsg.getErrorChain();
                out.println("<B>Causes:</B>");
                if (chain != null && chain.length > 0 && !chain[0].equals("")) {
                    out.println("<OL>");
                    for (String msg : chain) {
                        out.println("<LI>" + msg);
                    }
                    out.println("</OL>");
                } else {
                    out.println("No additional causes available.");
                }
            }
            out.println("<HR>");
            out.println("<em>See system log for more information.</em>");

        }

    }

    /**
     * Hangles the Error Response (<code>ResponseMessageError</code>) from the
     * micro engine.
     * 
     * @param response
     *            response object
     * @param responseStr
     *            message from the micro engine
     * 
     * @throws ServletException
     *             - for problems unmarshaling message
     * @throws IOException
     *             - for problems geting print writer
     */
    private void handleErrorResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        try {
            responseStr = extractResponseFromMessage(responseStr,
                    "responseError");
        } catch (Exception e) {
            throw new ServletException(e);
        }
        ResponseMessageError servMsg = null;
        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageError.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // servMsg = (ResponseMessageError) uctx
        // .unmarshalDocument(new StringReader(responseStr));
        // } catch (JiBXException e) {
        // e.printStackTrace();
        // throw new ServletException(e);
        // } catch (Throwable e) {
        // e.printStackTrace();
        // }
        if (servMsg != null) {
            theLogger.info("Writing error message(" + servMsg.getErrorMsg()
                    + ") to output.");

            setContentType(response, servMsg.getFileType());
            PrintWriter out = response.getWriter();

            out.println("<html>");
            out.println("<head><title>RunAction Error</title></head>");
            out.println("<body>");
            out.println("<center><h2>" + servMsg.getErrorSource()
                    + " 404 Error</h2></center><hr>");
            out.println("<b>ERROR:</b> " + servMsg.getErrorMsg() + "<p>");
            out.println("<b>Exception:</b> ");
            if (!servMsg.getErrorCause().equals("")) {
                out.println(servMsg.getErrorCause().toString());
            } else {
                out.println("No exception available");
            }
            out.println("<P>");
            String[] chain = servMsg.getErrorChain();
            out.println("<B>Causes:</B>");
            if (chain != null && chain.length > 0 && !chain[0].equals("")) {
                out.println("<OL>");
                for (String msg : chain) {
                    out.println("<LI>" + msg);
                }
                out.println("</OL>");
            } else {
                out.println("No additional causes available.");
            }
            out.println("<HR>");
            out.println("<em>See system log for more information.</em>");

            out.println("</body></html>");

        } else {
            htmlOutput(response,
                    "Error response could not be parsed, no output.");
        }

    }

    /**
     * Handles a URI product response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleServiceResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        ResponseMessageURI servMsg = null;
        try {
            responseStr = extractResponseFromMessage(responseStr, "responseURI");
        } catch (Exception e) {
            throw new ServletException(e);
        }

        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageURI.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // servMsg = (ResponseMessageURI) uctx
        // .unmarshalDocument(new StringReader(responseStr));
        // } catch (JiBXException e) {
        // e.printStackTrace();
        // throw new ServletException(e);
        // } catch (Throwable e) {
        // e.printStackTrace();
        // }
        if (servMsg != null) {
            theLogger.info("processing URI response " + servMsg.getDataURI()
                    + ":" + servMsg.getValidTime());
            String type = servMsg.getFileType();
            URI[] uriArray = servMsg.getProductURI();
            for (int counter = 0; counter < 1; counter++) {
                String path = uriArray[counter].getPath();

                if (type.startsWith("tile-")) {
                    theLogger.info("Tiled image!");
                    type = type.substring(5); // chop off "tile-"
                    path += "/0/000000." + type; // grab the first tile
                }

                setContentType(response, type);

                theLogger.info("Writing image(" + type + ") to output.");

                OutputStream out = response.getOutputStream();
                File imgFile = new File(path);
                out.write(FileUtil.file2bytes(imgFile));
            }
        } else {
            htmlOutput(response,
                    "Service response could not be parsed, no output.");
        }
    }

    /**
     * Handles a in line product response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleProductResponse(HttpServletResponse response,
            String responseStr) throws ServletException, IOException {
        ResponseMessageInline prodMsg = null;
        try {
            responseStr = extractResponseFromMessage(responseStr,
                    "responseInline");
        } catch (Exception e) {
            throw new ServletException(e);
        }

        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageInline.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // prodMsg = (ResponseMessageInline) uctx
        // .unmarshalDocument(new StringReader(responseStr));
        // } catch (JiBXException e) {
        // throw new ServletException(e);
        // }

        if ((prodMsg != null) && (prodMsg.getZippedData() != null)
                && (prodMsg.getZippedData().size() > 0)) {
            theLogger.info("processing inline response " + prodMsg.getDataURI()
                    + ":" + prodMsg.getValidTime());

            setContentType(response, prodMsg.getFileType());

            theLogger.info("Writing image(" + prodMsg.getFileType()
                    + ") to output.");

            OutputStream out = response.getOutputStream();

            // Just return the first response in multi-image responses
            for (int counter = 0; counter < 1; counter++) {
                ZipInputStream zis = new ZipInputStream(
                        new ByteArrayInputStream(prodMsg.getZippedData().get(
                                counter)));
                ZipEntry entry = zis.getNextEntry();
                while (entry != null) {
                    if (!entry.getName().endsWith(".wld")) {
                        byte[] temp = new byte[1024];
                        long read = zis.read(temp);
                        while (read > 0) {
                            out.write(temp, 0, (int) read);
                            read = zis.read(temp);
                        }
                        break; // only do one
                    }
                    entry = zis.getNextEntry();
                }

            }
        } else {
            htmlOutput(response,
                    "Product response could not be parsed, no output");
        }
    }

    /**
     * Sets the content type of the returned message.
     * 
     * @param response
     *            the servlet response object
     * @param fileType
     *            the file type of the response
     */
    private void setContentType(HttpServletResponse response, String fileType) {
        if ("png".equalsIgnoreCase(fileType)) {
            response.setContentType("image/png");
        } else if (("jpg".equalsIgnoreCase(fileType))
                || ("jpeg".equalsIgnoreCase(fileType))) {
            response.setContentType("image/jpeg");
        } else if ("gif".equalsIgnoreCase(fileType)) {
            response.setContentType("image/gif");
        } else if (("htm".equalsIgnoreCase(fileType))
                || ("html".equalsIgnoreCase(fileType))) {
            response.setContentType("text/html");
        } else if ("ascii".equalsIgnoreCase(fileType)) {
            response.setContentType("text/html");
        } else if ("tiff".equalsIgnoreCase(fileType)) {
            response.setContentType("image/tiff");
        }
        // TODO: add cases for other output types
    }

    /**
     * Wraps content in HTML. The HTML is returned using the response object's
     * print writer. This is intended to return error messages.
     * 
     * @param response
     *            the servlet response object
     * @param msg
     *            the message to return
     * 
     * @throws IOException
     *             if an IO error occurs
     */
    private void htmlOutput(HttpServletResponse response, String msg)
            throws IOException {
        theLogger.info(msg);

        response.setContentType("text/html");
        PrintWriter out = response.getWriter();

        out.println("<html>");
        out.println("<head><title>RunAction</title></head>");
        out.println("<body>");

        out.println(msg);

        out.println("</body></html>");
    }

    /**
     * Determines if a checkbox was checked.
     * 
     * @param checkBox
     *            the value of the checkbox from the form
     * 
     * @return true if the check box is checked
     */
    private boolean isCheckboxOn(String checkBox) {
        return (checkBox != null && checkBox.equalsIgnoreCase("on"));
    }

    /*
     * The next methods create the subscription key based on the information
     * available from the request. These only work with the "form based"
     * unsubscribe web pages.
     */
    /*
     * Note: the web client does not have access to the plug-ins, so any change
     * to the data URI defined in the Satellite plug-in will require some
     * recoding here.
     */
    private String makeImageDataURI(HttpServletRequest request)
            throws Exception {
        StringBuffer dataUri = new StringBuffer();
        String plugin = request.getParameter("plugin");
        String dataType = request.getParameter("datatype");
        String location = request.getParameter("location");
        String parameter = request.getParameter("parameter");
        dataUri.append("/").append(plugin);
        dataUri.append("/").append(plugin);
        dataUri.append("/").append(dataType);
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(location);
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(parameter);
        dataUri.append("/").append(".+");
        return dataUri.toString().replaceAll(" ", "_")
                .replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)");
    }

    /*
     * Note: the web client does not have access to the plug-ins, so any change
     * to the data URI defined in the METAR plug-in will require some recoding
     * here.
     */
    private String makeAsciiDataURI(HttpServletRequest request)
            throws Exception {
        // /.+?/METAR/KABQ/.+ or /.+?/METAR/.+?/.+
        StringBuffer dataUri = new StringBuffer();
        String type = request.getParameter("type");
        String station = request.getParameter("station");
        dataUri.append("/").append(
                (type.equalsIgnoreCase("taf") ? "taf" : "obs"));
        dataUri.append("/.+?");
        dataUri.append("/").append(type);
        if (Util.isEmptyString(station)) {
            dataUri.append("/.+?");
        } else {
            dataUri.append("/").append(station);
        }
        dataUri.append("/").append(".+");
        System.out.println(dataUri);
        return dataUri.toString().replaceAll(" ", "_")
                .replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)");

    }

    /*
     * Note: the web client does not have access to the plug-ins, so any change
     * to the data URI defined in the GRIB plug-in will require some recoding
     * here.
     */
    private String makeGribDataURI(HttpServletRequest request) throws Exception {
        // /.+?/.+?/.+?/.+?/.+?/TMP/2/.+?/0
        String parameter = request.getParameter("parameter");
        String level1 = request.getParameter("level1");
        String level2 = request.getParameter("level2");
        String units = request.getParameter("unit");
        String forecast = request.getParameter("forecast");
        if (Util.isEmptyString(forecast)) {
            forecast = ".+";
        }
        StringBuffer levelInfo = new StringBuffer();
        if (!Util.isEmptyString(level2)) {
            levelInfo.append(level2);
        }
        if (!Util.isEmptyString(level1)) {
            if (!Util.isEmptyString(level2)) {
                levelInfo.append("-");
            }
            levelInfo.append(level1);
        }
        if (!Util.isEmptyString(units)) {
            if (levelInfo.length() != 0) {
                levelInfo.append("_").append(units);
            }
        }
        StringBuffer dataUri = new StringBuffer();
        dataUri.append("/grid");
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(parameter);
        dataUri.append("/").append(".+?");
        dataUri.append("/").append(levelInfo);
        dataUri.append("/").append(forecast);
        return dataUri.toString().replaceAll(" ", "_")
                .replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)");
    }

    /*
     * Note: the web client does not have access to the plug-ins, so any change
     * to the data URI defined in the Radar plug-in will require some recoding
     * here.
     */
    private String makeRadarDataURI(HttpServletRequest request)
            throws Exception {
        String location = request.getParameter("station");
        if (Util.isEmptyString(location)) {
            location = ".+?";
        }
        String productCode = request.getParameter("productcode");
        if (Util.isEmptyString(productCode)) {
            productCode = ".+?";
        }
        String elevation = request.getParameter("elevation");
        if (Util.isEmptyString(elevation)) {
            elevation = ".+?";
        }
        StringBuffer dataUri = new StringBuffer();
        dataUri.append("/radar");
        dataUri.append("/").append(location);
        dataUri.append("/").append(productCode);
        dataUri.append("/").append(elevation);
        dataUri.append("/").append(".+");
        return dataUri.toString().replaceAll(" ", "_")
                .replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)");
    }
}
