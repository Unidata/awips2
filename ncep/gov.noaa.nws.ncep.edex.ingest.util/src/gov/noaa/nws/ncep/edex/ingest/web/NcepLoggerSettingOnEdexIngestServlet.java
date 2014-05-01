package gov.noaa.nws.ncep.edex.ingest.web;

import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;

import java.io.IOException;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class NcepLoggerSettingOnEdexIngestServlet extends HttpServlet implements Servlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5217319305966593070L;

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp)
		throws ServletException, IOException {
        String logName = req.getParameter("log-name");
        String logLevel = req.getParameter("log-level");
    	NcepLoggerManager.setNcepLoggerLevel(logName, logLevel); 
    	
 //   	RequestDispatcher rd = null;
    	resp.sendRedirect("index.html"); 
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp)
		throws ServletException, IOException {
    	doGet(req, resp); 
    }


}
