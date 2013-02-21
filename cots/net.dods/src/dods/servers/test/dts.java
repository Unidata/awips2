/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//
/////////////////////////////////////////////////////////////////////////////


/* $Id: dts.java,v 1.15.2.3 2004/08/26 21:47:49 ndp Exp $
*
*/


package dods.servers.test;

import java.io.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.zip.DeflaterOutputStream;


import dods.servlet.DODSServlet;

import dods.dap.*;
import dods.dap.parser.ParseException;
import dods.dap.Server.*;
import dods.servers.test.*;
import dods.servlet.GuardedDataset;
import dods.servlet.ReqState;


/**
 * <b>Purpose:</b><br>
 * This is the DODS Test servlet (dts). It allows the owner of the server
 * to deliver data in ANY valid DDS to a client. This DDS will be
 * filled with invented data if the client requests a DataDDS.
 * This kind of test fixture is useful for evaluating a clients
 * ability to handle the various complexities of the DODS data
 * types.
 * <p/>
 * <b>Configuration:</b><br>
 * The DODSServlet relies on the javax.servlet.ServletConfig
 * interface (in particular the getInitParameter() method)
 * to retrieve configuration information used by the servlet.
 * <b>InitParameters:</b>
 * <p/>
 * <ul>
 * <li>
 * DebugOn - This controls ouput to the terminal from which
 * the servlet engine was launched. The value is a list of
 * flags that turn on debugging instrumentation in different
 * parts of the code. Values are:
 * <ul>
 * <li>showRequest  - Show information about the clients request. </li>
 * <li>showResponse - Show information about the servlets response.</li>
 * <li>probeRequest - Show an exhaustive amount of information about
 * the clients request object.</li>
 * </ul>
 * </li><br>
 * <p/>
 * <li>
 * INFOcache - This is should be set to the directory containing the
 * files used by the ".info" service for the servlet. This directory
 * should contain any dataset specific "over-ride" files (see below),
 * any dataset specific additional information files (see below), and any
 * servlet specific information files(see below).
 * </li><br>
 * <p/>
 * <li>
 * DDScache - This is should be set to the directory containing the DDS
 * files for the datasets used by the servlet. Some servlets have been
 * developed that do not use DDS's that are cached on the disk, however
 * the default behaviour is for the servlet to load DDS images from disk.
 * </li><br>
 * <p/>
 * <li>
 * DAScache - This is should be set to the directory containing the DAS
 * files for the datasets used by the servlet. Some servlets have been
 * developed that do not use DAS's that are cached on the disk, however
 * the default behaviour is for the servlet to load DAS images from disk.
 * </li><br>
 * <p/>
 * </ul>
 * Here is an example entry from the web.xml file (for tomcat3.3a) for
 * the DODS Test Server (DTS):
 * <p/>
 * <pre>
 *         &lt;servlet&gt;
 *            &lt;servlet-name&gt;
 *                dts
 *            &lt;/servlet-name&gt;
 * <p/>
 *            &lt;servlet-class&gt;
 *                dods.servers.test.dts
 *            &lt;/servlet-class&gt;
 * <p/>
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DebugOn&lt;/param-name&gt;
 *                &lt;param-value&gt;showRequest showResponse &lt;/param-value&gt;
 *            &lt;/init-param&gt;
 * <p/>
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;INFOcache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/info/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 * <p/>
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DDScache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/dds/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 * <p/>
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DAScache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/das/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *        &lt;/servlet&gt;
 * </pre>
 *
 * @author Nathan David Potter
 * @version $Revision: 1.15.2.3 $
 */


public class dts extends DODSServlet {


    private static FunctionLibrary functionLibrary =
            new FunctionLibrary("dods.servers.test.SSF");


    /**
     * ************************************************************************
     * DODS Server Version for the test server...
     *
     * @serial Ostensibly it's a serializable item...
     */

    private String ServerVersion = "DODS/3.2";


    /**
     * ************************************************************************
     * This method returns a String containing the DODS Server Version...
     */
    public String getServerVersion() {
        return (ServerVersion);
    }

    /**
     * ************************************************************************
     * We override this crucial method from the parent servlet in order to
     * force the client not to cache. This is achieved by setting the header
     * tag "Last-Modified" to the current date and time.
     */
    public void doGet(HttpServletRequest request,
                      HttpServletResponse response)
            throws IOException, ServletException {


        response.setHeader("Last-Modified", (new Date()).toString());

        super.doGet(request, response);

    }

    /**
     * ************************************************************************
     * Default handler for the client's data request. Requires the getDDS()
     * method implemented by each server localization effort.
     * <p/>
     * <p>Once the DDS has been parsed, the data is read (using the class in the
     * localized server factory etc.), compared to the constraint expression,
     * and then sent to the client.
     *
     * @param request              The client's <code> HttpServletRequest</code> request
     *                             object.
     * @param response             The server's <code> HttpServletResponse</code> response
     *                             object.
     * @param rs   The ReqState object containing th details of this client request.
     */
    public void doGetDODS(HttpServletRequest request,
                          HttpServletResponse response,
                          ReqState rs)
            throws IOException, ServletException {


        System.out.println("Sending DODS Data For: " + rs.getDataSet());

        response.setContentType("application/octet-stream");
        response.setHeader("XDODS-Server", getServerVersion());
        response.setHeader("Content-Description", "dods_data");


        ServletOutputStream sOut = response.getOutputStream();
        OutputStream bOut, eOut;


        if (rs.getAcceptsCompressed()) {
            response.setHeader("Content-Encoding", "deflate");
            bOut = new DeflaterOutputStream(sOut);
        } else {
            // Commented out because of a bug in the DODS C++ stuff...
            //response.setHeader("Content-Encoding", "plain");
            bOut = new BufferedOutputStream(sOut);
        }


        try {

            GuardedDataset ds = getDataset(rs);
            // Utilize the getDDS() method to get       a parsed and populated DDS
            // for this server.
            ServerDDS myDDS = (ServerDDS) ds.getDDS();

            cacheArrayShapes(myDDS);


            //myDDS.print(System.out);






            // Instantiate the CEEvaluator
            CEEvaluator ce =
                    new CEEvaluator(myDDS,
                            new ClauseFactory(functionLibrary));

            // and parse the constraint expression
            ce.parseConstraint(rs.getConstraintExpression());







            // Send the constrained DDS back to the client
            PrintWriter pw = new PrintWriter(new OutputStreamWriter(bOut));
            myDDS.printConstrained(pw);
            //myDDS.printConstrained(System.out);

            // Send the Data delimiter back to the client
            //pw.println("Data:"); // JCARON CHANGED
            pw.flush();
            bOut.write("\nData:\n".getBytes()); // JCARON CHANGED
            bOut.flush();

            // Send the binary data back to the client
            DataOutputStream sink = new DataOutputStream(bOut);

            testEngine te = new testEngine();
            ce.send(myDDS.getName(), sink, te);
            sink.flush();

            // Finish up tsending the compressed stuff, but don't
            // close the stream (who knows what the Servlet may expect!)
            if (rs.getAcceptsCompressed())
                ((DeflaterOutputStream) bOut).finish();

        } catch (DODSException de) {
            dodsExceptionHandler(de, response);
        } catch (ParseException pe) {
            parseExceptionHandler(pe, response);
        }


        response.setStatus(response.SC_OK);


    }

    /**
     * ***********************************************************************
     */


    private void cacheArrayShapes(ServerDDS sdds) {

        Enumeration e = sdds.getVariables();

        while (e.hasMoreElements()) {
            BaseType bt = (BaseType) e.nextElement();
            cAS(bt);
        }
    }


    private void cAS(BaseType bt) {

        if (bt instanceof DConstructor) {
            Enumeration e = ((DConstructor) bt).getVariables();
            while (e.hasMoreElements()) {
                BaseType tbt = (BaseType) e.nextElement();
                cAS(tbt);
            }
        } else if (bt instanceof test_SDArray) {
            ((test_SDArray) bt).cacheShape();
        }


    }

    protected GuardedDataset getDataset(ReqState rs) throws DODSException, IOException, ParseException {
        return new testDataset(rs);
    }


}



