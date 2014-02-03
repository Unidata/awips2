package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

public class ContourAttributes {

	private String glevel;
	
	private String gvcord;
	
	private String skip;
	
	private String filter; 
		
	private String scale;
	
	private String gdpfun;
	
	private String type;
	
	private String cint;
	
	private String line;
	
	private String fint;
	
	private String fline;
	
	private String hilo;
	
	private String hlsym;
	
	private String wind;
	
	private String marker;
	
	private String clrbar;
	
	private String text;

	public String getGlevel() {
		return glevel;
	}

	public void setGlevel(String glevel) {
		this.glevel = glevel;
	}

	public String getGvcord() {
		return gvcord;
	}

	public void setGvcord(String gvcord) {
		this.gvcord = gvcord;
	}

	public String getSkip() {
		return skip;
	}

	public void setSkip(String skip) {
		this.skip = skip;
	}

	public String getFilter() {
		return filter;
	}

	public void setFilter(String filter) {
		this.filter = filter;
	}

	public String getScale() {
		return scale;
	}

	public void setScale(String scale) {
		this.scale = scale;
	}

	public String getGdpfun() {
		return gdpfun;
	}

	public void setGdpfun(String gdpfun) {
		this.gdpfun = gdpfun;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getCint() {
		return cint;
	}

	public void setCint(String cint) {
		this.cint = cint;
	}

	public String getLine() {
		return line;
	}

	public void setLine(String line) {
		this.line = line;
	}

	public String getFint() {
		return fint;
	}

	public void setFint(String fint) {
		this.fint = fint;
	}

	public String getFline() {
		return fline;
	}

	public void setFline(String fline) {
		this.fline = fline;
	}

	public String getHilo() {
		return hilo;
	}

	public void setHilo(String hilo) {
		this.hilo = hilo;
	}

	public String getHlsym() {
		return hlsym;
	}

	public void setHlsym(String hlsym) {
		this.hlsym = hlsym;
	}

	public String getWind() {
		return wind;
	}

	public void setWind(String wind) {
		this.wind = wind;
	}

	public String getMarker() {
		return marker;
	}

	public void setMarker(String marker) {
		this.marker = marker;
	}
    
	/**
	 * @return the clrbar
	 */
	public final String getClrbar() {
		return clrbar;
	}

	/**
	 * @param clrbar the clrbar to set
	 */
	public final void setClrbar(String clrbar) {
		this.clrbar = clrbar;
	}
	
	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public boolean isMatch ( ContourAttributes attr) {
		boolean match = false;
		if ( this.glevel.trim().equalsIgnoreCase(attr.getGlevel())&&
			 this.gvcord.trim().equalsIgnoreCase(attr.getGvcord()) &&
			 this.skip.trim().equalsIgnoreCase(attr.getSkip()) &&
			 this.filter.trim().equalsIgnoreCase(attr.getFilter()) &&
			 this.scale.trim().equalsIgnoreCase(attr.getScale()) &&
			 this.gdpfun.trim().equalsIgnoreCase(attr.getGdpfun()) &&
			 this.type.trim().equalsIgnoreCase(attr.getType())&&
			 this.cint.trim().equalsIgnoreCase(attr.getCint()) &&
			 this.line.trim().equalsIgnoreCase(attr.getLine()) &&
			 this.fint.trim().equalsIgnoreCase(attr.getFint()) &&
			 this.fline.trim().equalsIgnoreCase(attr.getFline()) &&
			 this.hilo.trim().equalsIgnoreCase(attr.getHilo()) &&
			 this.hlsym.trim().equalsIgnoreCase(attr.getHlsym()) &&
			 this.wind.trim().equalsIgnoreCase(attr.getWind()) &&
			 this.marker.trim().equalsIgnoreCase(attr.getMarker()) &&
			 this.clrbar.trim().equalsIgnoreCase(attr.getClrbar()) &&
			 this.text.trim().equalsIgnoreCase(attr.getText())) {
			match = true;
		}
		return match;
	}
}
