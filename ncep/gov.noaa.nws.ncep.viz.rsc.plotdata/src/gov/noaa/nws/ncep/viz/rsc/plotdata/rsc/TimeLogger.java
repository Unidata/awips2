package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

public class TimeLogger {

	private static TimeLogger instance;
	private StringBuffer sb;
	public static TimeLogger getInstance(){
		if(instance == null ){
			instance = new TimeLogger();
		}
		return instance;
	}
	
	private TimeLogger(){
		sb = new StringBuffer();
	}
	
	public synchronized void append(String string){
		sb.append(string);
	}
	
	public String toString(){
		return sb.toString();
	}
	
	public void clearLog(){
		sb.delete(0,sb.length() - 1);
	}
	
	public synchronized StringBuffer getStringBuffer(){
		return sb;
	}
}
