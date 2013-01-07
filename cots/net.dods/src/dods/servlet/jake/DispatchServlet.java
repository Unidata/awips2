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
import dods.dap.ServerVersion;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * DODS Dispatch servlet.  This abstract class preprocesses the query string,
 * simplifying development of DODS servlets.
 *
 * @version $Revision: 1.2 $
 * @author jehamby
 */
public abstract class DispatchServlet extends HttpServlet {
  /**
   * Handle a GET request.  This calls the <code>getDAS</code>,
   * <code>getDDS</code>, or <code>getData</code> method, which is
   * implemented by each derived DODS servlet class.
   *
   * @param req HttpServletRequest that encapsulates the request to the servlet
   * @param res HttpServletResponse that encapsulates the response from the servlet
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public void doGet(HttpServletRequest req, HttpServletResponse res)
       throws ServletException, IOException
  {
    String path = req.getPathInfo();
    String ce = req.getQueryString();

    // Set headers
    res.setHeader("Server", ServerVersion.getCurrentVersion());

    int typeIndex = path.lastIndexOf('.');
    if (typeIndex != -1) {
      String requestType = path.substring(typeIndex+1);
      path = path.substring(0, typeIndex);
      if(requestType.equals("das")) {
	res.setContentType("text/plain");
	res.setHeader("Content-Description", "dods_das");
	getDAS(req, res, path, ce);
      } else if(requestType.equals("dds")) {
	res.setContentType("text/plain");
	res.setHeader("Content-Description", "dods_dds");
	getDDS(req, res, path, ce);
      } else if(requestType.equals("dods")) {
	res.setContentType("application/octet-stream");
	res.setHeader("Content-Description", "dods_data");
	boolean compress = false;
	String encoding = req.getHeader("Accept-Encoding");
	if(encoding != null && encoding.equals("deflate")) {
	  compress = true;
	  res.setHeader("Content-Encoding", "deflate");
	}
	getData(req, res, path, ce, compress);
      } else
	printURLError(res, path, ce);
    } else {
      printURLError(res, path, ce);
    }
  }

  /**
   * Print an error message for bad URL's.
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @exception IOException if detected when handling the request
   */
  private void printURLError(HttpServletResponse res, String path, String ce)
       throws IOException
  {
    res.setContentType("text/html");
    ServletOutputStream out = res.getOutputStream();
    out.println("<html>");
    out.println("<head><title>Error in URL</title></head>");
    out.println("<body>");
    out.println("<h1>Error in URL</h1>");
    out.println("path = " + path);
    out.println("<br>ce = " + ce);
    out.println("</body></html>");
  }

  /**
   * Get the DODS DAS.
   * @param req the HttpServletRequest to use
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public abstract void getDAS(HttpServletRequest req, HttpServletResponse res,
			      String path, String ce)
       throws ServletException, IOException;

  /**
   * Get the DODS DDS.
   * @param req the HttpServletRequest to use
   * @param res the HttpServletResponse to use
   * @param path the DODS file path in the URL
   * @param ce the DODS constraint expression in the URL
   * @exception IOException if detected when handling the request
   * @exception ServletException if the request could not be handled
   */
  public abstract void getDDS(HttpServletRequest req, HttpServletResponse res,
			      String path, String ce)
       throws ServletException, IOException;

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
  public abstract void getData(HttpServletRequest req, HttpServletResponse res,
			       String path, String ce, boolean compress)
       throws ServletException, IOException;
}
