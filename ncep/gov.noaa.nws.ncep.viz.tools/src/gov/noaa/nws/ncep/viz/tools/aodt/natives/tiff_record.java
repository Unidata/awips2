package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Structure;

public class tiff_record extends Structure{
	 /// TIFF tag
    public int tag;
    /// Data type
    public int type;
    /// Length
    public int length;
    /// Pointer or value
    public int voff;
    public tiff_record() {
            super();
    }
    /**
     * @param tag TIFF tag<br>
     * @param type Data type<br>
     * @param length Length<br>
     * @param voff Pointer or value
     */
    public tiff_record(int tag, int type, int length, int voff) {
            super();
            this.tag = tag;
            this.type = type;
            this.length = length;
            this.voff = voff;
    }
    protected ByReference newByReference() { return new ByReference(); }
    protected ByValue newByValue() { return new ByValue(); }
    protected tiff_record newInstance() { return new tiff_record(); }
    public static class ByReference extends tiff_record implements com.sun.jna.Structure.ByReference {}
    public static class ByValue extends tiff_record implements com.sun.jna.Structure.ByValue {}

}
