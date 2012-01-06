package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Structure;

public class dis_vars extends Structure{
	/// Output number of lines
    public double xrectl;
    /// Output number of elems
    public double xrecte;
    public dis_vars() {
            super();
    }
    /**
     * @param xrectl Output number of lines<br>
     * @param xrecte Output number of elems
     */
    public dis_vars(double xrectl, double xrecte) {
            super();
            this.xrectl = xrectl;
            this.xrecte = xrecte;
    }
    protected ByReference newByReference() { return new ByReference(); }
    protected ByValue newByValue() { return new ByValue(); }
    protected dis_vars newInstance() { return new dis_vars(); }
    public static class ByReference extends dis_vars implements com.sun.jna.Structure.ByReference {}
    public static class ByValue extends dis_vars implements com.sun.jna.Structure.ByValue {}
}
