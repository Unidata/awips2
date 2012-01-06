package gov.noaa.nws.ncep.viz.common;

import java.util.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;

import com.vividsolutions.jts.geom.Coordinate;


public class CoorBean implements Serializable{
	
	private static final long serialVersionUID = 1L;

	private Coordinate coor;
	
	private static CoorBean instance;
	
    private final PropertyChangeSupport pcs = new PropertyChangeSupport( this );
	
	public CoorBean(){
		
	}
	
	public static CoorBean getInstance(){
		if(instance == null)
			instance = new CoorBean();
		return instance;
	}

	public Coordinate getCoor() {
		return coor;
	}

	public void setCoor(Coordinate c) {		
				
		this.pcs.firePropertyChange("coor", this.coor, c);
		this.coor = c;
	}
	
    public void addPropertyChangeListener( PropertyChangeListener listener ) {
        this.pcs.addPropertyChangeListener( listener );
    }

    public void removePropertyChangeListener( PropertyChangeListener listener ){
        this.pcs.removePropertyChangeListener( listener );
    }


}
