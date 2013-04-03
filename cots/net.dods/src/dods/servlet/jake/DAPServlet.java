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
 * DAP servlet.  This servlet reads binary DAP files, along with ASCII
 * descriptions of the DAS and DDS, and serves them out.
 *
 * @version $Revision: 1.2.4.1 $
 * @author jehamby
 */
public class DAPServlet extends DispatchServlet {
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
    String docPath = getServletContext().getRealPath(path) + ".das";
    try {
      InputStream is = new FileInputStream(docPath);
      DAS das = new DAS();
      das.parse(is);
      das.print(out);
    }
    catch (FileNotFoundException e) {
      res.setHeader("Content-Description", "dods_error");
      DODSException de = new DODSException(DODSException.NO_SUCH_FILE,
						 "file not found: " + docPath);
      de.print(out);
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
    String docPath = getServletContext().getRealPath(path) + ".dds";
    try {
      InputStream is = new FileInputStream(docPath);
      DDS dds = new DDS();
      dds.parse(is);
      dds.print(out);
    }
    catch (FileNotFoundException e) {
      res.setHeader("Content-Description", "dods_error");
      DODSException de = new DODSException(DODSException.NO_SUCH_FILE,
						 "file not found: " + docPath);
      de.print(out);
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
    String docPath = getServletContext().getRealPath(path) + ".dods";
    try {
      DConnect con = new DConnect(docPath, true);
      DataDDS dds = con.getData(null);
      dds.externalize(out, compress, false);
    }
    catch (FileNotFoundException e) {
      res.setContentType("text/plain");
      res.setHeader("Content-Description", "dods_error");
      res.setHeader("Content-Encoding", "none");
      DODSException de = new DODSException(DODSException.NO_SUCH_FILE,
						 "file not found: " + docPath);
      de.print(out);
    }
    catch (Exception e) {
      res.setContentType("text/plain");
      res.setHeader("Content-Description", "dods_error");
      res.setHeader("Content-Encoding", "none");
      DODSException de = new DODSException(DODSException.UNKNOWN_ERROR, e.getMessage());
      de.print(out);
    }
  }
}
