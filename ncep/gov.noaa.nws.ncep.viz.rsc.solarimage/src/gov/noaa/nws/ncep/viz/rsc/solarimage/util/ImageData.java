package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

import com.raytheon.uf.viz.core.exception.VizException;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.Header;
import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
/**
 * Represents the image data of a SolarImageRecord object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- --------------    --------------------------
 * 02/07/2013   958        sgurung, qzhou    Initial creation.
 * </pre>
 * 
 * @author sgurung, qzhou
 * @version 1.0
 */
public class ImageData {
		
	private int nx;    
    private int ny;
   
    private int bitpix;   
    private double bscale;
    private double bzero;
 
    private float[] imageValues;  
    private float[][] imageArrays;
    private Header header;

    /**
     * @param record
     */
    public ImageData(SolarImageRecord record) throws VizException {
    	
    	try {
	    	BasicHDU hdu = SolarImageUtil.getImageHDU(record);
	    	
	    	if (hdu != null) {
	    		int[] axes = hdu.getAxes();
	    		
	    		if (axes != null && axes.length != 2) {
	                // Expecting 2 dimensional image at this time               
	                throw new VizException(
	                        "The record does not contain a 2 dimensional image.");
	            }
	    		
	    		this.header = hdu.getHeader();
	    		this.nx = axes[1];
	    		this.ny = axes[0];
	    		this.bitpix = hdu.getBitPix();
	    		this.bscale = hdu.getBScale();
	    		this.bzero = hdu.getBZero();
	    		
	    		Object imgData = hdu.getKernel();
	    		
	    		float[][] array = null;
		        if (imgData instanceof float[][]) {
		            array = (float[][]) imgData;
		        } else if (imgData instanceof int[][]) {
		            array = new float[ny][nx];
		            int[][] tmp = (int[][]) imgData;	            
		            for (int i = 0; i < ny; i++) {
		                for (int j = 0; j < nx; j++) {
		                    array[i][j] = (float) (bscale * tmp[i][j] + bzero);
		                }
		            }
	
		        } else if (imgData instanceof short[][]) {
		            array = new float[ny][nx];
		            short[][] tmp = (short[][]) imgData;
		            for (int i = 0; i < ny; i++) {
		                for (int j = 0; j < nx; j++) {
		                    array[i][j] = (float) (bscale * tmp[i][j] + bzero);
		                }
		            }
		        } else if (imgData instanceof byte[][]) {
		            array = new float[ny][nx];
		            byte[][] tmp = (byte[][]) imgData;
		            for (int i = 0; i < ny; i++) {
		                for (int j = 0; j < nx; j++) {
		                    array[i][j] = (float) (bscale * tmp[i][j] + bzero);
		                }
		            }
		        } else {
		            String msg = "SolarImageData: Unrecognized imgDatatype: "
		                    + imgData.getClass().getCanonicalName();
		            throw new VizException(msg);
		        }
		     
		        int n = 0;
		        float[] imgDataRec = new float[nx * ny];
		        for (int i = ny - 1; i >= 0; i--) { // Reverse order of rows
		            for (int j = 0; j < nx; j++) {
		                imgDataRec[n++] = array[i][j];
		            }
		        }
		        
		        this.imageValues = imgDataRec;
		        this.imageArrays = array;
	    	}
    	}
    	catch (Exception e) {
    		throw new VizException("Error getting SolarImageData from SolarImageRecord.");
    	}
        
    }
    
    /**
     * @return the nx
     */
    public int getNx() {
        return nx;
    }

    /**
     * @param nx
     *            the nx to set
     */
    public void setNx(int nx) {
        this.nx = nx;
    }

    /**
     * @return the ny
     */
    public int getNy() {
        return ny;
    }

    /**
     * @param ny
     *            the ny to set
     */
    public void setNy(int ny) {
        this.ny = ny;
    }

    /**
     * @return the bitpix
     */
    public int getBitpix() {
        return bitpix;
    }

    /**
     * @param bitpix
     *            the bitpix to set
     */
    public void setBitpix(int bitpix) {
        this.bitpix = bitpix;
    }

    /**
     * @return the bscale
     */
    public double getBscale() {
        return bscale;
    }

    /**
     * @param bscale
     *            the bscale to set
     */
    public void setBscale(double bscale) {
        this.bscale = bscale;
    }

    /**
     * @return the bzero
     */
    public double getBzero() {
        return bzero;
    }

    /**
     * @param bzero
     *            the bzero to set
     */
    public void setBzero(double bzero) {
        this.bzero = bzero;
    }


    /**
     * @return the imageValues
     */
    public float[] getImageValues() {
        return imageValues;
    }

    /**
     * @param imageValues
     *            the imageValues to set
     */
    public void setImageValues(float[] imageValues) {
        this.imageValues = imageValues;
    }
    
    /**
     * @return the imageValues
     */
    public float[][] getImageArrays() {
        return imageArrays;
    }

    /**
     * @param imageValues
     *            the imageValues to set
     */
    public void setImageArrays(float[][] imageArrays) {
        this.imageArrays = imageArrays;
    }
    
    /**
     * @return the header
     */
    public Header getHeader() {
        return header;
    }

    /**
     * @param header
     *            the header to set
     */
    public void setHeader(Header header) {
        this.header = header;
    }

}
