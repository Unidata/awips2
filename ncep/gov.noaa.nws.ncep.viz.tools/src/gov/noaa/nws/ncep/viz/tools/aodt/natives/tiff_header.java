package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Structure;

public class tiff_header extends Structure{
	 /// Byte order
    public int order;
    /// Version
    public int version;
    /// Pointer
    public int point;
    public tiff_header() {
            super();
    }
    /**
     * @param order Byte order<br>
     * @param version Version<br>
     * @param point Pointer
     */
    public tiff_header(int order, int version, int point) {
            super();
            this.order = order;
            this.version = version;
            this.point = point;
    }
    protected ByReference newByReference() { return new ByReference(); }
    protected ByValue newByValue() { return new ByValue(); }
    protected tiff_header newInstance() { return new tiff_header(); }
    public static class ByReference extends tiff_header implements com.sun.jna.Structure.ByReference {}
    public static class ByValue extends tiff_header implements com.sun.jna.Structure.ByValue {}

}
