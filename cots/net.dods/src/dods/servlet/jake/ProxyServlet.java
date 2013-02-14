/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1998, California Institute of Technology.  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Jake Hamby, NASA/Jet Propulsion Laboratory
//         Jake.Hamby@jpl.nasa.gov
/////////////////////////////////////////////////////////////////////////////

package dods.servlet.jake;
import dods.dap.*;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Proxy servlet.  This servlet searches its parameter list for a server URL
 * which matches the first component of the path, and if it finds one,
 * acts as a proxy for DODS requests on the remote server.  This can be used
 * by DODS applet clients to bypass the default applet security restrictions,
 * which only allow an applet to connect to the host from which they are
 * downloaded.
 * <P>
 * Note: to use this servlet, you'll need to add servlet parameters of the form:
 * <code>manta=http://manta.jpl.nasa.gov/cgi-bin/nph-hdf</code>
 * <P>
 * This will convert a URL of the form:
 * <BR>
 * <code>http://machine/servlets/proxy/manta/foo.hdf</code>
 * <BR>
 * to:
 * <BR>
 * <code>http://manta.jpl.nasa.gov/cgi-bin/nph-hdf/foo.hdf</code>
 *
 * @version $Revision: 1.2 $
 * @author jehamby
 * @see DispatchServlet
 */
public class ProxyServlet extends DispatchServlet {
  /**
   * Get the DODS DAS.
   * @param req the HttpServletRequest to use
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public void getDAS(HttpServletRequest req, HttpServletResponse res, String path, String ce)
       throws ServletException, IOException
  {
    ServletOutputStream out = res.getOutputStream();
    try {
      DConnect con = proxyConnect(path, ce);
      DAS das = con.getDAS();
      das.print(out);
    }
    catch (DODSException e) {
      res.setHeader("Content-Description", "dods_error");
      e.print(out);
    }
    catch (Exception e) {
      res.setHeader("Content-Description", "dods_error");
      DODSException de = new DODSException(DODSException.UNKNOWN_ERROR, e.getMessage());
      de.print(out);
    }
  }

  /**
   * Get the DODS DDS.
   * @param req the HttpServletRequest to use
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public void getDDS(HttpServletRequest req, HttpServletResponse res, String path, String ce)
       throws ServletException, IOException
  {
    ServletOutputStream out = res.getOutputStream();
    try {
      DConnect con = proxyConnect(path, ce);
      DDS dds = con.getDDS();
      dds.print(out);
    }
    catch (DODSException e) {
      res.setHeader("Content-Description", "dods_error");
      e.print(out);
    }
    catch (Exception e) {
      res.setHeader("Content-Description", "dods_error");
      DODSException de = new DODSException(DODSException.UNKNOWN_ERROR, e.getMessage());
      de.print(out);
    }
  }

  /**
   * Get the DODS Dataset.
   * @param req the HttpServletRequest to use
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @param compress whether to compress the DODS output
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public void getData(HttpServletRequest req, HttpServletResponse res, String path, String ce, boolean compress)
       throws ServletException, IOException
  {
    ServletOutputStream out = res.getOutputStream();
    try {
      DConnect con = proxyConnect(path, ce);
      // XXX in the future, we could use StatusUI to abort downloads which
      // take too long
      DataDDS dds = con.getData(null);
      dds.externalize(out, compress, false);
    }
    catch (DODSException e) {
      res.setContentType("text/plain");
      res.setHeader("Content-Description", "dods_error");
      res.setHeader("Content-Encoding", "none");
      e.print(out);
    }
    catch (Exception e) {
      res.setHeader("Content-Description", "dods_error");
      DODSException de = new DODSException(DODSException.UNKNOWN_ERROR, e.getMessage());
      de.print(out);
    }
  }

  /**
   * Establish a proxy connection to the remote server.  Note:  this
   * implementation is naive and doesn't attempt to cache frequently accessed
   * URL's.  If you want to add this feature, this is the method in which to
   * add it.
   *
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @return a new DConnect object
   */
  private DConnect proxyConnect(String path, String ce) throws DODSException {
    // strip off first component of path and search for it in parameters
    int secondSlashIndex = path.indexOf('/', 1);
    String server = path.substring(1, secondSlashIndex);
    String newPath = path.substring(secondSlashIndex); // include leading slash

    java.util.Enumeration names = getInitParameterNames();
    while(names.hasMoreElements()) {
      String paramName = (String)names.nextElement();
      if(paramName.equals(server)) {
	// get initial part of URL (including http://) from parameter
	String url = getInitParameter(paramName) + newPath + "?" + ce;
	try {
	  DConnect con = new DConnect(url, true);
	  return con;
	}
	catch (FileNotFoundException e) {
	  throw new DODSException(DODSException.NO_SUCH_FILE, "file not found: " + url);
	}
      }
    }
    throw new DODSException(DODSException.NO_SUCH_FILE, "proxy path not found: " + path);
  }
}
