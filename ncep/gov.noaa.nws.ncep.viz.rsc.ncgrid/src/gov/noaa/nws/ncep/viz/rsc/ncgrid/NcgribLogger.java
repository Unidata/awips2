package gov.noaa.nws.ncep.viz.rsc.ncgrid;


public class NcgribLogger {
	
	
	private boolean enableRscLogs;
	
	private boolean enableDiagbosticLogs;
	
	private boolean enableConturLogs;
	
	private boolean enableTotalTimeLogs;
	
	private static NcgribLogger instance = null;
	public static NcgribLogger getInstance() {
		if (instance == null) {
			instance = new NcgribLogger();	
		}
		return instance;
	}

	
   	public NcgribLogger() {
		this.enableRscLogs = false;
		this.enableDiagbosticLogs = false;
		this.enableConturLogs = false;
		this.enableTotalTimeLogs = false;
	}
       	
   	public void setEnableRscLogs ( boolean enable) {
   		this.enableRscLogs = enable;
   	}
   	
   	public boolean enableRscLogs () {
   		return this.enableRscLogs;
   	}
   	
   	public void setEnableDiagnosticLogs ( boolean enable) {
   		this.enableDiagbosticLogs = enable;
   	}
   	
   	public boolean enableDiagnosticLogs () {
   		return this.enableDiagbosticLogs;
   	}
   	
   	public void setEnableCntrLogs ( boolean enable) {
   		this.enableConturLogs = enable;
   	}
   	
   	public boolean enableCntrLogs () {
   		return this.enableConturLogs;
   	}
   	
   	public void setEnableTotalTimeLogs ( boolean enable) {
   		this.enableTotalTimeLogs = enable;
   	}
   	
   	public boolean enableTotalTimeLogs () {
   		return this.enableTotalTimeLogs;
   	}
}
