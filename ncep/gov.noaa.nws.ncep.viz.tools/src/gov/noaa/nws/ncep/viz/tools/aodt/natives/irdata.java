package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class irdata extends Structure {
	public static class ByReference extends irdata implements
			Structure.ByReference {
	}
    public int date;
    public int time;
    public float TrawO;
    public float Traw;
    public float Tfinal;
    public float Tfinal3;
    public float CI;
    public float eyet;
    public float warmt;
    public float cloudt;
    public float cloudt2;
    public float cwcloudt;
    public float latitude;
    public float longitude;
    public float warmlatitude;
    public float warmlongitude;
    public float eyesize;
    public float eyestdv;
    public float cloudsymave;
    public int sattype;
    public int eyescene;
    public int cloudscene;
    public int eyesceneold;
    public int cloudsceneold;
    public int rule9;
    public int rule8;
    public int land;
    public int eyefft;
    public int cloudfft;
    public int cwring;
    public int ringcb;
    public int ringcbval;
    public int ringcbvalmax;
    public float ringcblatmax;
    public float ringcblonmax;
    public float CIadjp;
    public float sst;
    public float TIEraw;
    public float TIEavg;
    public int TIEflag;
    public int autopos;
    public int LBflag;
    public float rmw;
    public byte[] comment = new byte[(50)];
    public irdata() {
            super();
    }
    public irdata(Pointer p) {
        useMemory(p);
    }
    protected ByReference newByReference() { return new ByReference(); }
    protected ByValue newByValue() { return new ByValue(); }
    protected irdata newInstance() { return new irdata(); }
    //public static class ByReference extends irdata implements ByReference {}
    public static class ByValue extends irdata implements com.sun.jna.Structure.ByValue {}
}
