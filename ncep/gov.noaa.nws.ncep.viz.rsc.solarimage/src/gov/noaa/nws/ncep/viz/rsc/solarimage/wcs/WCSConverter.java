package gov.noaa.nws.ncep.viz.rsc.solarimage.wcs;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import nom.tam.fits.Header;
import nom.tam.fits.TruncatedFileException;
import nom.tam.util.BufferedDataInputStream;

/**
 * Provides methods for fits wcs transformation.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------  --------------------------
 *                             
 *  02/14/2013   #958       q.zhou      Convert original fits to wcs format.  
 *  									Convert pixel to real world coordinates
 *                                         
 * </pre>
 * 
 * @author q.zhou
 * @version 2
 */
public class WCSConverter {

    private Header header;

    private int dim = 0;

    private double[] crpix;

    private double[] cdelt;

    private double[] crval;

    private double[] crota;
    
    private double a = 0.0;

    private double[][] pc;
    
    private double[][] cd;
    
    private double[][] pcInv, cdInv; 
    
    //private double[][] m, mInv;
    //private double[][] pv, pvInv;

    /**
     * @param header
     */
    public WCSConverter(Header header) {
        this.header = header;
        init();
    }

    public WCSConverter(byte[] data) throws TruncatedFileException,
            IOException {
        ByteArrayInputStream bis = new ByteArrayInputStream(data);
        BufferedDataInputStream stream = new BufferedDataInputStream(bis);
        this.header = new Header(stream);
        init();
    }

    /*
     * Initialize and Convert original fits to wcs 
     */
    private void init() {
    	dim = header.getIntValue("NAXIS");
        crpix = new double[dim];
        cdelt = new double[dim];
        crval = new double[dim];
        crota = new double[dim];
        pc = new double[dim][dim];
        cd = new double[dim][dim];
        pcInv = new double[dim][dim];
		cdInv = new double[dim][dim];
	
        for (int n = 0; n < dim; n++) {
            String ij = Integer.toString(n + 1);
            crpix[n] = header.getDoubleValue("CRPIX" + ij, 0.0); //if null, 0.0
            cdelt[n] = header.getDoubleValue("CDELT" + ij, 1.0);
            crval[n] = header.getDoubleValue("CRVAL" + ij, 0.0);
            crota[n] = header.getDoubleValue("CROTA" + ij, 0.0);
        }
                         
        for (int m = 0; m < dim; m++) {
        	String i = Integer.toString(m + 1);
        	for (int n = 0; n < dim; n++) {
        		String j = Integer.toString(n + 1);
        		pc[m][n] = header.getDoubleValue("PC" +i +"_" +j, 0.0);
        		cd[m][n] = header.getDoubleValue("CD" +i +"_" +j, 0.0); 
        		pcInv[m][n] = pc[m][n];
        		cdInv[m][n] = cd[m][n];
        	}
        }
                      
        // Convert original to pc. Do not convert original fits to cd. 
        if (!header.containsKey("PC1_1") && !header.containsKey("CD1_1")) {      	
        	if ( crota[1] != 0.0) //CROTA2
        		a = crota[1];
        	else if ( crota[0] != 0.0)
        		a = crota[0];
        		
        	if (dim == 2 ) {  
        		for (int i = 0; i < dim; i++) {
                    for (int j = 0; j < dim; j++) {
                        if (i == j) {
                            pc[i][j] = Math.cos(Math.toRadians( a));                           
                        }
                        else if (i < j) {
                            pc[i][j] = (-Math.sin(Math.toRadians( a))) * cdelt[1]/cdelt[0];
                        }
                        else if (i > j) {
                            pc[i][j] = (Math.sin(Math.toRadians( a))) * cdelt[0]/cdelt[1];
                        }
                    }
                }
        	}
        } 
        
    	// inverse
        if (dim == 2 ) {  
    		for (int i = 0; i < dim; i++) {
                for (int j = 0; j < dim; j++) {
                    if (i == j) {
                        pcInv[i][j] = pc[i][j];
                        cdInv[i][j] = cd[i][j];
                    }
                    else {
                        pcInv[i][j] = -pc[i][j];
                        cdInv[i][j] = -cd[i][j];
                    }
                }
            }
        }
        
 //System.out.println("here "+ pc[0][0] +" "+pc[0][1]+" " +pcInv[0][0] +" "+pcInv[0][1]+" ");       
//        dim = header.getIntValue("NAXIS");
//        crpix = new double[dim];
//        cdelt = new double[dim];
//        crval = new double[dim];
//        crota = new double[dim];
//        for (int n = 0; n < dim; n++) {
//            String ij = Integer.toString(n + 1);
//            crpix[n] = header.getDoubleValue("CRPIX" + ij, 0.0);
//            cdelt[n] = header.getDoubleValue("CDELT" + ij, 1.0);
//            crval[n] = header.getDoubleValue("CRVAL" + ij, 0.0);
//            crota[n] = header.getDoubleValue("CROTA" + ij, 0.0);
//        }
//
//        /* Set up rotation matrix (and its inverse) */
//        m = new double[dim][dim];
//        mInv = new double[dim][dim];
//        if (dim == 2 && header.containsKey("CROTA1")) {
//            double sine = Math.sin(Math.toRadians(crota[0]));
//            double cosine = Math.cos(Math.toRadians(crota[0]));
//            m[0][0] = cosine;
//            m[0][1] = -sine;
//            m[1][0] = sine;
//            m[1][1] = cosine;
//            mInv[0][0] = cosine;
//            mInv[0][1] = sine;
//            mInv[1][0] = -sine;
//            mInv[1][1] = cosine;
//        } else {
//            for (int i = 0; i < dim; i++) {
//                for (int j = 0; j < dim; j++) {
//                    if (i == j)
//                        m[i][j] = 1.0;
//                    else
//                        m[i][j] = 0.0;
//                }
//            }
//            mInv = m;
//        }
    }

    public double[] imageToWorld(double[] image) {
    	double[] cs = new double[dim];
        double[] tmp = new double[dim];
        
        for (int j = 0; j < dim; j++) {
        	cs[j] = image[j] - crpix[j];
        }
//        double x = crval[0] + cdelt[0] * (pc[0][0] * cs[0] + pc[0][1] * cs[1]);
//        double y = crval[1] + cdelt[1] * (pc[1][0] * cs[0] + pc[1][1] * cs[1]);
	    for (int i = 0; i < dim; i++) {
	        tmp[i] = 0.0;
	        for (int j = 0; j < dim; j++) {
	            if (dim == 2 && header.containsKey("PC1_1")) 
	            	tmp[i] += pc[i][j] * cs[j] * cdelt[i];
	            else if (dim == 2 && header.containsKey("CD1_1"))
	            	tmp[i] += cd[i][j] * cs[j];
	            else
	            	tmp[i] += pc[i][j] * cs[j] * cdelt[i];
	        }
	    }
	    
        for (int i = 0; i < dim; i++) {
        	cs[i] = tmp[i];
            cs[i] += crval[i];
        }
        //System.out.println("***xy "+ x +" "+ y);
        //System.out.println("***cs "+ cs[0] +" "+cs[1]);
       
        return cs;
        
//        double[] wcs = new double[dim];
//        double[] tmp = new double[dim];
//        for (int j = 0; j < dim; j++) {
//        	System.out.println("***imgOrig "+ image[0] +" "+image[1]);
//            wcs[j] = image[j] - crpix[j];
//        }
//        for (int i = 0; i < dim; i++) {
//            tmp[i] = 0.0;
//            for (int j = 0; j < dim; j++) {
//                tmp[i] += m[i][j] * wcs[j];
//            }
//        }
//        for (int i = 0; i < dim; i++) {
//            wcs[i] = cdelt[i] * tmp[i];
//            wcs[i] += crval[i];
//        }
//        System.out.println("***wcs "+ wcs[0] +" "+wcs[1]);
//        return wcs;
    }

    public double[] WorldToImage(double[] cs) {
    	double[] image = new double[dim];
        double[] tmp = new double[dim];
        
        for (int i = 0; i < dim; i++) {
            image[i] = (cs[i] - crval[i]); // / cdelt[i];
        }
        
	    for (int i = 0; i < dim; i++) {
	        tmp[i] = 0.0;
	        for (int j = 0; j < dim; j++) {
	        	if (dim == 2 && header.containsKey("PC1_1")) 
	                tmp[i] += pcInv[i][j] * image[j] / cdelt[i]; 
	        	else if (dim == 2 && header.containsKey("CD1_1"))
	        		tmp[i] += cdInv[i][j] * image[j];
	        	else 
	        		tmp[i] += pcInv[i][j] * image[j] / cdelt[i]; 
	        }
	    }
	        
        for (int j = 0; j < dim; j++) {
            image[j] = tmp[j] + crpix[j];
        }
        
        
//        for (int i = 0; i < dim; i++) {
//            tmp[i] = 0.0;
//            for (int j = 0; j < dim; j++) {
//                tmp[i] += mInv[i][j] * image[j];
//            }
//        }
//        for (int j = 0; j < dim; j++) {
//            image[j] = tmp[j] + crpix[j];
//        }
        
        //System.out.println("******image "+ image[0] +" "+image[1]);
        return image;
    }

}
