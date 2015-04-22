package gov.noaa.nws.ncep.viz.rsc.plotdata.queue;

import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.NcPlotResource2.Station;

import java.util.Collection;

import com.raytheon.uf.common.time.DataTime;

public class QueueEntry {
	DataTime time;
	Collection<Station> stations;
	
	public QueueEntry(DataTime time,Collection<Station> stations ){
		this.time     = time;
		this.stations = stations; 
	}
	
	public  DataTime getDataTime(){
		return time;
	}
	
	public  Collection<Station> getStations(){
		return stations;
	}
}
