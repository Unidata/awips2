package gov.noaa.nws.ncep.viz.ui.locator;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import com.vividsolutions.jts.geom.Coordinate;


public class LocatorCoorBean implements PropertyChangeListener, Serializable{
	
	private static final long serialVersionUID = 1L;
	
	private Coordinate coor;
	
	public LocatorCoorBean(){
		gov.noaa.nws.ncep.viz.common.CoorBean.getInstance().addPropertyChangeListener(this);
	}	


	public Coordinate getCoor() {
		return coor;
	}

	public void setCoor(Coordinate coor) {
		this.coor = coor;
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {		
		Coordinate c = (Coordinate)evt.getNewValue();
		coor = c;		
		gov.noaa.nws.ncep.viz.ui.locator.LocatorDisplay.setPosition(c); 
	}

}