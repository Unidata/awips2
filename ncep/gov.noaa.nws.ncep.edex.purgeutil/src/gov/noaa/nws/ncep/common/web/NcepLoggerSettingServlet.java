package gov.noaa.nws.ncep.common.web;

import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;

import java.io.IOException;

//import javax.servlet.RequestDispatcher;
import javax.servlet.Servlet;
//import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
//import javax.servlet.ServletRequest;
//import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class NcepLoggerSettingServlet extends HttpServlet implements Servlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8785343518113487908L;

//	@Override
//	public void init(ServletConfig config) throws ServletException {
//		// TODO Auto-generated method stub
//		
//	}
//
//	@Override
//	public ServletConfig getServletConfig() {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public void service(ServletRequest req, ServletResponse res)
//			throws ServletException, IOException {
//		// TODO Auto-generated method stub
//		
//	}
//
//	@Override
//	public String getServletInfo() {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public void destroy() {
//		// TODO Auto-generated method stub
//		
//	}

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp)
		throws ServletException, IOException {
        String logName = req.getParameter("log-name");
        String logLevel = req.getParameter("log-level");
    	NcepLoggerManager.setNcepLoggerLevel(logName, logLevel); 
    	
 //   	RequestDispatcher rd = null;
    	resp.sendRedirect("ncepEdexRequestLoggerSetting.html"); 
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp)
		throws ServletException, IOException {
    	doGet(req, resp); 
    }


}
