package gov.noaa.nws.ncep.common.dataplugin.nctaf.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

/**
 * TODO Parse visibility provided in different formats.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/03/2011              sgurung     Initial creation
 *
 * </pre>
 *
 * @author sgurung
 * @version 1.0	
 */

public class VisibilityParser {

    private String visibility;
    private double prevail_vsbySM;
    private String vsby_Dir;
    private Pattern digitsOnly;
    private UnitConverter miles2Meters;
    private UnitConverter miles2Km;
    
    // MAX value taken from AWIPSI code
    private static final double MAX = 2147483647;
    
    public VisibilityParser() {
        this.miles2Meters = NonSI.MILE.getConverterTo(SI.METER);
        this.miles2Km = NonSI.MILE.getConverterTo(SI.KILOMETER);
        this.visibility = new String();
        this.prevail_vsbySM = 0.0;
        this.vsby_Dir = new String();
        this.digitsOnly = Pattern.compile("^\\d+$");
    }
    
    public String getVisibility() {
        return visibility;
    }

    public double getPrevail_vsbySM() {
        return prevail_vsbySM;
    }
    
    public double getPrevail_vsbyKM() {
        return this.miles2Km.convert(prevail_vsbySM);
    }
    
    public double getPrevail_vsbyM() {
        return this.miles2Meters.convert(prevail_vsbySM);
    }

    public String getVsby_Dir() {
        return vsby_Dir;
    }
    
    public boolean decode(String visString) {
        
        int offset = 0;
        int charOffset = 0;
        
        String[] vis = visString.split(" ");
        
        if(vis == null || vis.length == 0) {
            return false;
        }
        
        /****************************************/
        /* CHECK FOR VISIBILITY MEASURED <1/4SM */
        /****************************************/
        if(vis[0].equals("M1/4SM") || vis[0].equals("<1/4SM")) {
            this.prevail_vsbySM = 0.0;
            this.visibility = vis[0];
            return true;
        }
        /****************************************/
        /* CHECK FOR VISIBILITY MEASURED >6SM */
        /****************************************/
        else if(vis[0].equals("P6SM")) {
            this.prevail_vsbySM = 6.0;
            this.visibility = vis[0];
            return true;
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /* IN A FRACTION OF A STATUTE MILE */
        /***********************************/
       if( (charOffset = vis[0].indexOf('/')) != -1 &&
                (offset = vis[0].indexOf("SM")) != -1 ) {
            Matcher num = digitsOnly.matcher(vis[0].substring(0, charOffset));
            Matcher den = 
                digitsOnly.matcher(vis[0].substring(charOffset+1, offset));
            if(num.matches() && den.matches()) {
                this.prevail_vsbySM = prevailVSBY(vis[0]);
                this.visibility = vis[0];
                return true;
            }
            else
                return false;
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /*     IN WHOLE STATUTE MILES      */
        /***********************************/
        else if((offset = vis[0].indexOf("SM")) != -1) {
            Matcher sm = digitsOnly.matcher(vis[0].substring(0, offset));
            if(sm.matches()) {
                prevail_vsbySM = prevailVSBY(vis[0]);
                this.visibility = vis[0];
                return true;
            }
            else {
                return false;
            }
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /* IN WHOLE AND FRACTIONAL STATUTE */
        /*             MILES               */
        /***********************************/
        else if( vis[0].length() < 4 ) {
            Matcher wholeNumber = digitsOnly.matcher(vis[0]);
            if(!wholeNumber.matches()) {
                return false;
            }
            String save_token = vis[0];
            
            if(vis.length <= 1 || vis[1] == null) {
                return false;
            }
            if( (charOffset = vis[1].indexOf('/')) != -1 &&
                    (offset = vis[1].indexOf("SM")) != -1 ) {
                Matcher num = 
                    digitsOnly.matcher(vis[1].substring(0, charOffset));
                Matcher den = 
                    digitsOnly.matcher(vis[1].substring(charOffset+1, offset));
                if( num.matches() && den.matches() )
                {
                    prevail_vsbySM = prevailVSBY(vis[1]);
                    prevail_vsbySM += Double.valueOf(save_token);
                    this.visibility = vis[0] + " " + vis[1];
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        } 
        return false;
    }
        
    private Double prevailVSBY(String vis) {   
        int Slash_ptr, SM_KM_ptr;
        if(vis == null || vis.length() == 0) {
            return MAX;
        }
        if( (SM_KM_ptr = vis.indexOf("SM")) == -1) {
            SM_KM_ptr = vis.indexOf("KM");
        }
        Slash_ptr = vis.indexOf('/');
        if( Slash_ptr == -1 )
        {
            return Double.valueOf(vis.substring(0, SM_KM_ptr));
        }
        else
        {
            String numerator = vis.substring(0, Slash_ptr);
            String denominator = vis.substring(Slash_ptr+1, SM_KM_ptr);
            return (Double.valueOf(numerator)/
                    Double.valueOf(denominator));
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String[] visibilities = {"10SM", "1 3/8SM","5000NW","3/16SM","9999NDV", "1 1/2SM", "P6SM", "2 1/2SM", "M1/4SM", ""};
        VisibilityParser parser = new VisibilityParser();
        for(String vis : visibilities) {
            if(parser.decode(vis)) {
                System.out.println("Found vis info: " + parser.getVisibility() + " " + parser.getPrevail_vsbySM());
            }
            else {
                System.out.println("The decode method didn't find anything");
            }
        }

    }

}
