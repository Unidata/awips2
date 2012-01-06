package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Structure;

public class ringdata extends Structure {
	public float dist;
    public float angle;
    public float temp;
    public ringdata.ByReference nextrec;
    public ringdata() {
            super();
    }
    public ringdata(float dist, float angle, float temp, ringdata.ByReference nextrec) {
            super();
            this.dist = dist;
            this.angle = angle;
            this.temp = temp;
            this.nextrec = nextrec;
    }
    protected ByReference newByReference() { return new ByReference(); }
    protected ByValue newByValue() { return new ByValue(); }
    protected ringdata newInstance() { return new ringdata(); }
    public static class ByReference extends ringdata implements com.sun.jna.Structure.ByReference {}
    public static class ByValue extends ringdata implements com.sun.jna.Structure.ByValue {}

}
