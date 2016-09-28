package com.raytheon.viz.awipstools.ui.action;

import java.awt.geom.Point2D;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

public class LapsToolsData {

    private Coordinate gridCenter = new Coordinate();

    private Integer nx;

    private Integer ny;

    private Integer nz;

    private Double gridSpacing;

    private Double lat;

    private Double lon;
    
    private Envelope cwaArea;

    private Envelope validArea;
    
    private Envelope validAreaOrig;
    
    private Envelope validAreaDefault;

    private Envelope gridArea;

	private String areaCoverageString;

	private Boolean belowLimits;

    public Coordinate getGridCenter() {
        return gridCenter;
    }

    public Coordinate getCwaCenter() {
        return cwaArea.centre();
    }

    public Integer getNx() {
        return nx;
    }

    public Integer getNy() {
        return ny;
    }

    public Integer getNz() {
        return nz;
    }

    public Double getGridSpacing() {
        return gridSpacing;
    }

    public String getAreaCoverageString() {
		double nxDist = getNx()*getGridSpacing()/1000;
		double nyDist = getNy()*getGridSpacing()/1000;
        double areaCoverage = nxDist * nyDist;
        this.areaCoverageString = String.format("%.1f km by %.1f km = %.2f km2", 
        		nxDist, nyDist, areaCoverage);
        double maximumArea = 14140140.;
        double minimumArea =   150000.;

       	if(areaCoverage > maximumArea) {
    		System.out.print("LAPS Tools Data: problem with this domain exceeding the RUC 236 background\n");
            MessageDialog.openWarning(getShell(), "Domain Size Error",
              "The size of new domain would exceed the maximum limits \n\tdefined by model background (the RAP40 grid).\n");
    	} else if(areaCoverage < minimumArea) {
    		System.out.print("LAPS Tools Data: problem with this domain coverage area being smaller than the CWA coverage area\n");
            MessageDialog.openWarning(getShell(), "Domain Size Error",
              "The size of new domain area would LESS than the minimum limits \n\tdefined by the CWA coverage area.");
    	}

       	
		return areaCoverageString;
    }
    
    private Shell getShell() {
		// TODO Auto-generated method stub
		return null;
	}

	public Double getLat() {
        return lat;
    }

    public Double getLon() {
        return lon;
    }

    public Envelope getCwaArea() {
        return cwaArea;
    }

    public void setLimits(Boolean belowLimits) {
        this.belowLimits = belowLimits;
    }
    
    public Boolean getLimits() {
        return belowLimits;
    }

    public Envelope getValidArea() {
        if (validArea == null) {
        	double buffer = .1; // tenth of a degree buffer around CWA (in km)
            double cosBuffer = Math.abs(2*Math.cos(cwaArea.centre().y));
            double cwa_dist_lat = Math.abs(cwaArea.getHeight() / 2) + (2 * buffer);
            double cwa_dist_lon = Math.abs(cwaArea.getWidth() / 2) + (2 * buffer * cosBuffer);
            validArea = new Envelope(cwaArea.centre());
            validArea.expandBy(cwa_dist_lon, cwa_dist_lat); 
            
            double z = Math.abs(cwaArea.getHeight() / cwaArea.getWidth()) ;
            System.out.print("LAPS Tools Data: cwa height = "+cwaArea.getHeight()+"\n");
            System.out.print("cwa width = "+cwaArea.getWidth()+"\n");
            System.out.print("z ratio = "+z+"\n");
        }
        return validArea;
    }
    
    public Envelope getValidAreaOrig() {
        if (validAreaOrig == null) {
            double cwa_dist_lat = Math.abs(cwaArea.getHeight() / 2);
            double cwa_dist_lon = Math.abs(cwaArea.getWidth() / 2);
            validAreaOrig = new Envelope(cwaArea.centre());
            validAreaOrig.expandBy(cwa_dist_lon, cwa_dist_lat);        
        } 
        return validAreaOrig;
    }

    public Envelope getValidAreaDefault() {
        if (validAreaDefault == null) {
        	double buffer = 1; // half degree buffer around CWA (in km)
            double cosBuffer = Math.abs(2*Math.cos(cwaArea.centre().y));
            double cwa_dist_lat = Math.abs(cwaArea.getHeight() / 2) + (2 * buffer);
            double cwa_dist_lon = Math.abs(cwaArea.getWidth() / 2) + (2 * buffer * cosBuffer);
            validAreaDefault = new Envelope(cwaArea.centre());
            validAreaDefault.expandBy(cwa_dist_lon, cwa_dist_lat); 
        }
        return validAreaDefault;
    }
    
    public Envelope getGridArea() {
        if (gridArea == null) {
            double width = getGridSpacing() * nx;
            double height = getGridSpacing() * ny;
            GeodeticCalculator gc = new GeodeticCalculator();
            gc.setStartingGeographicPoint(gridCenter.x, gridCenter.y);
            gc.setDirection(0.0, height / 2);
            Point2D top = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(gridCenter.x, gridCenter.y);
            gc.setDirection(180.0, height / 2);
            Point2D bot = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(top);
            gc.setDirection(90.0, width / 2);
            Point2D NE = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(top);
            gc.setDirection(-90.0, width / 2);
            Point2D NW = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(bot);
            gc.setDirection(90.0, width / 2);
            Point2D SE = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(bot);
            gc.setDirection(-90.0, width / 2);
            Point2D SW = gc.getDestinationGeographicPoint();

            gridArea = new Envelope();
            gridArea.expandToInclude(NE.getX(), NE.getY());
            gridArea.expandToInclude(NW.getX(), NW.getY());
            gridArea.expandToInclude(SE.getX(), SE.getY());
            gridArea.expandToInclude(SW.getX(), SW.getY());
        }
        return gridArea;
    }
    
    public void setCwaArea(Envelope cwaArea) {
        validArea = null;
        this.cwaArea = cwaArea;
    }

    public void setGridCenterLon(double lon) {
        gridArea = null;
        gridCenter.x = lon;
    }

    public void setGridCenterLat(double lat) {
        gridArea = null;
        gridCenter.y = lat;
    }

    public void setGridCenter(Coordinate gridCenter) {
        gridArea = null;
        this.gridCenter = gridCenter;
    }

    public void setNx(Integer nx) {
        gridArea = null;
        this.nx = nx;
    }

    public void setNy(Integer ny) {
        gridArea = null;
        this.ny = ny;
    }

    public void setGridSpacing(Double gridSpacing) {
        gridArea = null;
        this.gridSpacing = gridSpacing;
    }

    public void setNz(Integer nz) {
        this.nz = nz;
    }

    public void setLat(Double lat) {
        this.lat = lat;
    }

    public void setLon(Double lon) {
        this.lon = lon;
    }

    //LAPS domain.xml has <domain> tags and <NX_L> tags, etc
    @XmlRootElement(name="domain", namespace="")
    public static class LapsDomain {

        private Integer nx;
        private Integer ny;
        private Integer nz;
        private Double gridSpacing;
        private Double lat;
        private Double lon;

        public int getNx() {
              return nx;
        }
        @XmlElement(name = "NX_L")
        public void setNx(int nx) {
              this.nx = nx;
        }

        public int getNy() {
              return ny;
        }
        @XmlElement(name = "NY_L")
        public void setNy(int ny) {
              this.ny = ny;
        }

        public int getNz() {
              return nz;
        }
        @XmlElement(name = "NK_LAPS")
        public void setNz(int nz) {
              this.nz = nz;
        }
        
        public Double getGridSpacing() {
            return gridSpacing;
        }
        @XmlElement(name = "GRID_SPACING_M")
        public void setGridSpacing(Double gridSpacing) {
            this.gridSpacing = gridSpacing;
        }
       
        public Double getGridCenLat() {
           return lat;
        }
        @XmlElement(name = "GRID_CEN_LAT")
        public void setGridCenLat(Double lat) {
           this.lat = lat;
        }
      
        public Double getGridCenLon() {
           return lon;
        }
        @XmlElement(name = "GRID_CEN_LON")
        public void setGridCenLon(Double lon) {
           this.lon = lon;
       }
    }
}
